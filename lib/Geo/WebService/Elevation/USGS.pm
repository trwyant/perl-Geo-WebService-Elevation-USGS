=head1 NAME

Geo::WebService::Elevation::USGS - Elevation queries against USGS web services.

=head1 SYNOPSIS

 use Geo::WebService::Elevation::USGS;
 
 my $eq = Geo::WebService::Elevation::USGS->new();
 print "The elevation of the White House is ",
   $eq->getElevation(38.898748, -77.037684)->{Elevation},
   " feet above sea level.\n";

=head1 DESCRIPTION

This module executes elevation queries against the United States
Geological Survey's web server. You provide the latitude and longitude
in degrees, with south latitude and west longitude being negative. The
return is typically a hash containing the data you want. Query errors
are exceptions by default, though the object can be configured to signal
an error by an undef response, with the error retrievable from the
'error' attribute.

For documentation on the underlying web service, see
L<http://gisdata.usgs.gov/XMLWebServices/TNM_Elevation_Service.php>.

For all methods, the input latitude and longitude are documented at the
above web site as being WGS84, which for practical purposes I understand
to be equivalent to NAD83. The vertical reference is not documented
under the above link, but correspondence with the USGS says that it is
derived from the National Elevation Dataset (NED; see
L<http://ned.usgs.gov>). This is referred to NAD83 (horizontal) and
NAVD88 (vertical). NAVD88 is based on geodetic leveling surveys, B<not
the WGS84/NAD83 ellipsoid,> and takes as its zero datum sea level at
Father Point/Rimouski, in Quebec, Canada. Alaska is an exception, and is
based on NAD27 (horizontal) and NAVD29 (vertical).

Anyone interested in the gory details may find the paper I<Converting
GPS Height into NAVD88 Elevation with the GEOID96 Geoid Height Model> by
Dennis G. Milbert, Ph.D. and Dru A. Smith, Ph.D helpful. This is
available at L<http://www.ngs.noaa.gov/PUBS_LIB/gislis96.html>. This
paper states that the difference between ellipsoid and geoid heights
ranges between -75 and +100 meters globally, and between -53 and -8
meters in "the conterminous United States."

B<Caveat:> This module relies on the documented behavior of the
above-referred-to USGS web service, as well as certain undocumented but
observed behaviors and the proper functioning of the USGS' hardware and
software and the network connecting you to them. Changes or failures in
any of these will probably cause this module not to work.

I have not (I think) gone out of my way to use undocumented behavior,
but there are a couple cases where I felt forced into it.

First, the documentation says that if a point is not in a requested
source data set, the returned elevation will be -1.79769313486231E+308.
In practice, the getAllElevations web service returns 'BAD_EXTENT' in
this case, and the getElevation web service returns error 'ERROR: No
Elevation value was returned from servers.' This affects the behavior of
the is_valid() method, but more importantly it convinced me to try to
convert the corresponding getElevation error back into a successful call
of the getElevation() method, returning 'BAD_EXTENT' as the elevation.

Second, experimentation shows that, although the source IDs are
generally documented as upper-case, in fact the getElevation web service
queries are not sensitive to the case of the provided source ID. Because
of this, this package normalizes source IDs (by converting them to upper
case) before using them in a comparison. This affects the behavior of
the elevation() method when the 'source' attribute is an array or hash
reference and the source is being used to select results from a
getAllElevations query. It also affects the construction of the
'BAD_EXTENT' packet in response to a getElevation web service failure,
since the source name is obtained by making a call to getAllElevations()
(or the cached results of such a call) and finding the name
corresponding to the given source ID.

=head2 Methods

The following public methods are provided:

=cut

package Geo::WebService::Elevation::USGS;

use 5.008;

use strict;
use warnings;

use Carp;
use Scalar::Util 1.10 qw{ blessed looks_like_number };
use SOAP::Lite;

our $VERSION = '0.007';

use constant BEST_DATA_SET => -1;

my $using_time_hires;
{
    our $THROTTLE;
    my $mark;
    if ( eval {
	    require Time::HiRes;
	    Time::HiRes->can( 'time' ) && Time::HiRes->can( 'sleep' );
	} ) {
	*_time = \&Time::HiRes::time;
	*_sleep = \&Time::HiRes::sleep;
	$using_time_hires = 1;
    } else {
	*_time = sub { return time };
	*_sleep = sub { return sleep $_[0] };
    }

    $mark = _time();
    sub _pause {
	my ( $self ) = @_;
	defined $THROTTLE
	    and croak '$THROTTLE revoked - use ', __PACKAGE__,
		'->set( throttle => value ) instead';
	my $now = _time();
	while ( $now < $mark ) {
	    _sleep( $mark - $now );
	    $now = _time();
	}
	$mark = $now + __PACKAGE__->get( 'throttle' );
	return;
    }
}

=head3 $eq = Geo::WebService::Elevation::USGS->new();

This method instantiates a query object. If any arguments are given,
they are passed to the set() method. The instantiated object is
returned.

=cut

sub new {
    my ($class, @args) = @_;
    ref $class and $class = ref $class;
    $class or croak "No class name specified";
    shift;
    my $self = {
	carp	=> 0,
	croak	=> 1,
	default_ns	=> 'http://gisdata.usgs.gov/XMLWebServices2/',
	error	=> undef,
	places	=> undef,
	proxy	=>
	    'http://gisdata.usgs.gov/xmlwebservices2/elevation_service.asmx',
	retry	=> 0,
	retry_hook => sub {},
	source	=> BEST_DATA_SET,
	timeout	=> 30,
	trace	=> undef,
	units	=> 'FEET',
	use_all_limit => 5,
    };
    bless $self, $class;
    @args and $self->set(@args);
    return $self;
}

my %mutator = (
    croak	=> \&_set_literal,
    carp	=> \&_set_literal,
    default_ns	=> \&_set_literal,
    error	=> \&_set_literal,
    places	=> \&_set_integer_or_undef,
    proxy	=> \&_set_literal,
    retry	=> \&_set_unsigned_integer,
    retry_hook	=> \&_set_hook,
    source	=> \&_set_source,
    throttle	=> \&_set_throttle,
    timeout	=> \&_set_integer_or_undef,
    trace	=> \&_set_literal,
    units	=> \&_set_literal,
    use_all_limit => \&_set_use_all_limit,
);

my %access_type = (
    throttle	=> \&_only_static_attr,
);

foreach my $name ( keys %mutator ) {
    exists $access_type{$name}
	or $access_type{$name} = \&_no_static_attr;
}

=head3 %values = $eq->attributes();

This method returns a list of the names and values of all attributes of
the object. If called in scalar context it returns a hash reference.

=cut

sub attributes {
    my $self = shift;
    my %attr;
    foreach (keys %mutator) {
	$attr{$_} = $self->{$_};
    }
    return wantarray ? %attr : \%attr;
}

=head3 $rslt = $usgs->elevation($lat, $lon, $valid);

This method queries the data sets defined in the 'source' attribute for
the elevation at the given latitude and longitude, returning the results
in the given array reference. If called in list context the array itself
is returned. The returned array contains hashes identical to that
returned by getElevation().

You can also pass a Geo::Point, GPS::Point, or Net::GPSD::Point object
in lieu of the $lat and $lon arguments. If you do this, $valid becomes
the second argument, rather than the third.

If the 'source' is undef or -1, getElevation() is called to get the
'best' data set representing the given coordinates. The result is an
array (or array reference) whose sole element is the hash returned by
GetElevation().

If the 'source' is any other scalar, getElevation() is called to get the
named data set. The result is an array (or array reference) whose sole
element is the hash returned by GetElevation().

If the 'source' is a reference to an empty array or an empty hash,
getAllElevations() is called, and its output (or a reference to it) is
returned.

If the 'source' is a reference to a non-empty array with at least as
many entries as the value of the 'use_all_limit' attribute, B<and> none
of the entries begins with a '*' (what the USGS calls 'best available
subset' syntax) it is made into a hash by using the contents of the
array as keys and 1 as the value for all keys. Then getAllElevations()
is called, and the array (or a reference to it) of all source data sets
whose Source_ID values appear in the hash are returned. An error will be
declared if there are any source IDs specified in the array which are
not returned by getAllElevations().

If the 'source' is a reference to a non-empty array and none of the
other conditions of the previous paragraph apply, getElevation() is
called for each element of the array, and the array of all results (or a
reference to it) is returned.

B<Please note> that the use of wildcard source IDs (either the magic
'-1' or anything beginning with '*') in an array (or hash, see below) is
not supported. Users will find that the current behavior is to error
out with an invalid source name if the query is directed to
getAllElevations. If the query gets handled by iterating with
getElevation(), it succeeds or errors out under the same conditions that
the getElevation() method would. But I make no commitment to retain this
functionality. Instead, I hope that use of the module will clarify what
its behavior should be.

If the 'source' is a reference to a non-empty hash, it is handled pretty
much as though the 'source' were a reference to an array containing the
sorted keys of the hash.

If the 'source' is a reference to a regular expression,
getAllElevations() is called, and items whose Data_ID does not match the
regular expression are eliminated from its output. The resultant array
(or a reference to it) is returned.

If the 'source' is a reference to code, getAllElevations() is called.
For each item in the returned array, the code is called, with the
Geo::WebService::Elevation::USGS object as the first argument, and the
array item (which, remember, is a hash reference like that returned by
getElevation()) as the second argument. If the code returns true the
item is included in the output; if the code returns false the item is
excluded. The resultant array (or a reference to it) is returned.

For example,

 $ele->set(source => sub {$_[1]{Data_ID} =~ m/CONUS/i});

will retain all items whose Data_ID contains the string 'CONUS', and
therefore has the same result as

 $ele->set(source => qr{CONUS}i);

If the optional C<$valid> argument to elevation() is specified, data
with invalid elevations are eliminated before the array is returned.
Note that this may result in an empty array.

=cut

{

    my %filter = (
	CODE => sub {
	    sub {$_[1]->($_[0], $_[2])}
	},
	HASH => sub {%{$_[0]} ?
	    sub {delete $_[1]{_normalize_id($_[2]{Data_ID})}} :
	    sub {1}
	},
	Regexp => sub {
	    sub {$_[2]{Data_ID} =~ $_[1]}
	},
    );

    sub elevation {
	my ($self, $lat, $lon, $valid) = _latlon( @_ );
	my $ref = ref (my $source = $self->_get_source());
	my $rslt;
	if ($ref eq 'ARRAY') {
	    $rslt = [];
	    foreach (@$source) {
		push @$rslt, $self->getElevation($lat, $lon, $_);
		$self->get('error') and return;
	    }
	} elsif ($ref) {
	    $filter{$ref}
		or croak "Source $ref ref not understood";
	    my $check = $filter{$ref}->($source);
	    ref (my $raw = $self->getAllElevations($lat, $lon)) eq 'ARRAY'
		or return;
	    $rslt = [grep {$check->($self, $source, $_)} @$raw];
  	    if ($ref eq 'HASH') {
		foreach (sort keys %$source) {
		    $self->_error( "Source Data_ID $_ not found" );
		    $self->{croak} and croak $self->{error};
		    return;
		}
	    }
	} else {
	    ref (my $raw = $self->getElevation($lat, $lon, $source)) eq
	    'HASH'
		or return;
	    $rslt = [$raw];
	}
	if ($valid) {
	    $rslt = [grep {is_valid($_)} @$rslt];
	}
	return wantarray ? @$rslt : $rslt;
    }
}

=head3 $value = $eq->get($attribute);

This method returns the value of the given attribute. It will croak if
the attribute does not exist.

=cut

sub get {
    my ($self, $name) = @_;
    $access_type{$name}
	or croak "No such attribute as '$name'";
    my $holder = $access_type{$name}->( $self, $name );
    return $holder->{$name};
}

=head3 $rslt = $eq->getAllElevations($lat, $lon, $valid);

This method executes the getAllElevations query, which returns the
elevation of the given point as recorded in all available data sets. The
results are returned as a reference to an array containing hashes
representing the individual data sets. Each data set hash is structured
like the one returned by getElevation(). If a failure occurs, the method
will croak if the 'croak' attribute is true, or return undef otherwise.
The arguments are WGS84 latitude and longitude, in degrees, with south
latitude and west longitude being negative. The elevations returned are
NAVD88 elevations.

You can also pass a Geo::Point, GPS::Point, or Net::GPSD::Point object
in lieu of the $lat and $lon arguments.

If the elevation is not available from a given source, that source will
still appear in the output, but the Elevation will either be a
non-numeric value (e.g. 'BAD_EXTENT', though this is nowhere documented
that I can find), or a very large negative number (documented as
-1.79769313486231E+308).

If the optional C<$valid> argument to getAllElevations() is specified,
data with invalid elevations are eliminated before the array is
returned. Note that this may result in an empty array.

=cut

sub getAllElevations {
    my ($self, $lat, $lon, $valid) = _latlon( @_ );
    my $soap = $self->_soapdish();
    my $retry_limit = $self->get( 'retry' );
    my $retry = 0;

    while ( $retry++ <= $retry_limit ) {

	$self->{error} = undef;

	$self->_pause();

	my $raw;
	eval {
	    if ( exists $self->{_hack_result} ) {
		$raw = delete $self->{_hack_result}

	    } else {

		local $SOAP::Constants::DO_NOT_USE_CHARSET = 1;

		$raw = $soap->call(
		    SOAP::Data->name ('getAllElevations')->attr (
			{xmlns => $self->{default_ns}}) =>
		    SOAP::Data->new(name => 'X_Value', type => 'string',
			value => $lon),
		    SOAP::Data->new(name => 'Y_Value', type => 'string',
			value => $lat),
		    SOAP::Data->new(name => 'Elevation_Units', type => 'string',
			value => $self->{units}),
		);
	    }

	    1;
	} or do {
	    $self->_error( $@ );
	    next;
	};

	my $cooked = $self->_digest($raw);
	ref $cooked eq 'HASH' or next;
	my @rslt;
	my $ref = ref $cooked->{Data_ID};
	if ($ref eq 'ARRAY') {
###	    my $limit = @{$cooked->{Data_ID}} - 1;
	    foreach my $inx (0 .. (scalar @{$cooked->{Data_ID}} - 1)) {
		push @rslt, {
		    Data_Source => $cooked->{Data_Source}[$inx],
		    Data_ID => $cooked->{Data_ID}[$inx],
		    Elevation => $cooked->{Elevation}[$inx],
		    Units => $cooked->{Units}[$inx],
		};
	    }
	} elsif ($ref) {
	    $self->_error( "Unexpected $ref reference in {Data_ID}" );
	    next;
	} else {	# One of the ActiveState MSWin32 variants seems to do this.
	    push @rslt, $cooked;
	}
	if ($valid) {
	    @rslt = grep {is_valid($_)} @rslt;
	}
	return \@rslt;

    } continue {

	$retry <= $retry_limit
	    and $self->get( 'retry_hook' )->( $self, $retry,
	    getAllElevations => $lat, $lon );

    }

    $self->{croak} and croak $self->{error};
    return;
}

=head3 $rslt = $eq->getElevation($lat, $lon, $source, $elevation_only);

This method executes the getElevation query, requesting elevation for
the given WGS84 latitude and longitude (in degrees, with south latitude
and west longitude being negative) from the source data set. The
returned elevation is NAVD88. If a failure occurs, the method will croak
if the 'croak' attribute is true, or return undef otherwise. Either way,
the error if any will be available in the 'error' attribute.

You can also pass a Geo::Point, GPS::Point, or Net::GPSD::Point object
in lieu of the $lat and $lon arguments. If you do this, $source becomes
the second argument, rather than the third, and $elevation_only becomes
the third argument rather than the fourth.

If the $source argument is omitted, undef, or -1, data comes from the
'best' data set for the given position. If the $source argument begins
with an asterisk ('*') you get data from the 'best' data set whose name
matches the given name. In either case, you get an error (not success
with an invalid {Elevation}) if the given position is not covered by any
of the selected data sets.

The $elevation_only argument is optional. If provided and true (in the
Perl sense) it causes the return on success to be the numeric value of
the elevation, rather than the hash reference described below.

Assuming success and an unspecified or false $elevation_only, $rslt
will be a reference to a hash containing the data. The USGS documents
the following keys:

 Data_Source: name or description of data source
 Data_ID: string identifier of data source
 Elevation: the elevation of the point
 Units: the units of the Elevation

The contents of $rslt will then be something like:

 {
     Data_Source => 'source name'
     Data_ID => 'source ID',
     Elevation => elevation from given source,
     Units => 'units from source',
 }

If the elevation is not available, the Elevation will either be a
non-numeric value (e.g. 'BAD_EXTENT', though this is nowhere documented
that I can find), or a very large negative number (documented as
-1.79769313486231E+308).

B<Caveat:> The USGS web service upon which this code is based throws an
error ('ERROR: No Elevation value was returned from servers.') if the
requested latitude and longitude do not appear in the specified data
set(s). If a specific data source name was specified, this code attempts
to trap this error and return a hash with Elevation => 'BAD_EXTENT', the
way getAllElevations does, in order to get more desirable behavior from
the elevation() method. If the behavior of the USGS web service changes,
the change may be visible to users of this software, either as an error,
as an unexpected value in the 'Elevation' key, or some other way.

B<Another caveat:> If you have not specified a data source (or if you
have specified a 'wildcard' data source such as '-1' or anything
beginning with '*'), and no data source covers the point you have
specified, an error will occur. This will be thrown as an exception if
the 'carp' attribute is true; otherwise undef will be returned and the
error will be in the 'error' attribute. This seems inconsistent with the
behavior of the previous C<caveat>, but it is also unclear what to do
about it.

=cut

my $bad_extent_re = _make_matcher(
    q{Conversion from string "BAD_EXTENT" to type 'Double'} );

sub getElevation {
    my ($self, $lat, $lon, $source, $only) = _latlon( @_ );
    defined $source or $source = BEST_DATA_SET;
    my $soap = $self->_soapdish();
    my $retry_limit = $self->get( 'retry' );
    my $retry = 0;

    while ( $retry++ <= $retry_limit ) {

	$self->{error} = undef;

	$self->_pause();

	my $rslt;
	eval {

	    if ( exists $self->{_hack_result} ) {
		$rslt = delete $self->{_hack_result};
	    } else {

		local $SOAP::Constants::DO_NOT_USE_CHARSET = 1;

		$rslt = $soap->call(SOAP::Data->name ('getElevation')->attr (
			{xmlns => $self->{default_ns}}) =>
		    SOAP::Data->new(name => 'X_Value', type => 'string',
			value => $lon),
		    SOAP::Data->new(name => 'Y_Value', type => 'string',
			value => $lat),
		    SOAP::Data->new(name => 'Source_Layer', type => 'string', 
			value => $source),
		    SOAP::Data->new(name => 'Elevation_Units', type => 'string',
			value => $self->{units}),
		    SOAP::Data->new(name => 'Elevation_Only', type => 'string',
			value => $only ? 'true' : 'false'),
		);
	    }
	    1;
	} or do {
	    $self->_error( $@ );
	    next;
	};

	if ( _instance( $rslt, 'SOAP::SOM' ) && $rslt->fault &&
		$rslt->faultstring =~ m/ $bad_extent_re /smxo ) {
	    if (my $hash = $self->_get_bad_extent_hash($source)) {
		return $hash;
	    }
	}

	my $info = $self->_digest( $rslt, $source ) or next;
	return $info;

    } continue {

	$retry <= $retry_limit
	    and $self->get( 'retry_hook' )->( $self, $retry,
	    getElevation => $lat, $lon, $source, $only );

    }

    $self->{croak} and croak $self->{error};
    return;

}

=head3 $boolean = $eq->is_valid($elevation);

This method (which can also be called as a static method or as a
subroutine) returns true if the given datum represents a valid
elevation, and false otherwise. A valid elevation is a number having a
value greater than -1e+300. The input can be either an elevation value
or a hash whose {Elevation} key supplies the elevation value.

=cut

sub is_valid {
    my $ele = pop;
    my $ref = ref $ele;
    if ($ref eq 'HASH') {
	$ele = $ele->{Elevation};
    } elsif ($ref) {
	croak "$ref reference not understood";
    }
    return looks_like_number($ele) && $ele > -1e+300;
}

=head3 $eq = $eq->set($attribute => $value ...);

This method sets the value of the given attribute. Multiple
attribute/value pairs may be specified. The object itself is returned,
to allow call chaining. An attempt to set a non-existent attribute will
result in an exception being thrown.

=cut


{

    my %clean_soapdish;	# Attributes that are used in _soapdish();

    sub set {	## no critic (ProhibitAmbiguousNames)
	my ($self, @args) = @_;
	my $clean;
	while (@args) {
	    my ( $name, $val ) = splice @args, 0, 2;
	    $access_type{$name}
		or croak "No such attribute as '$name'";
	    exists $mutator{$name}
		or croak "Attribute '$name' is read-only";
	    my $holder = $access_type{$name}->( $self, $name );
	    $mutator{$name}->( $holder, $name, $val );
	    $clean ||= $clean_soapdish{$name};
	}
	$clean and delete $self->{_soapdish};
	return $self;
    }

    sub _set_clean_soapdish {
	%clean_soapdish = map { $_ => 1 } @_;
	return;
    }

}

sub _set_hook {
    my ( $self, $name, $val ) = @_;
    ref $val eq 'CODE'
	or croak "Attribute $name must be a code reference";
    return( $self->{$name} = $val );
}

sub _set_integer {
    my ($self, $name, $val) = @_;
    (!defined $val || $val !~ m/ \A [-+]? \d+ \z /smx)
	and croak "Attribute $name must be an integer";
    return ($self->{$name} = $val + 0);
}

sub _set_integer_or_undef {
    my ($self, $name, $val) = @_;
    (defined $val && $val !~ m/ \A \d+ \z /smx)
	and croak "Attribute $name must be an unsigned integer or undef";
    return ($self->{$name} = $val);
}

sub _set_literal {
    return $_[0]{$_[1]} = $_[2];
}

{
    my %supported = map {$_ => 1} qw{ARRAY CODE HASH Regexp};
    sub _set_source {
	my ($self, $name, $val) = @_;
	my $ref = ref $val;
	($ref && !$supported{$ref})
	    and croak "Attribute $name may not be a $ref ref";
	delete $self->{_source_cache};
	return ($self->{$name} = $val);
    }
}

sub _set_throttle {
    my ( $self, $name, $val ) = @_;
    if ( defined $val ) {
	looks_like_number( $val )
	    and $val >= 0
	    or croak "The $name attribute must be undef or a ",
		'non-negative number';
	$using_time_hires
	    or $val >= 1
	    or $val == 0
	    or $val = 1;
    } else {
	$val = 0;
    }
    return( $self->{$name} = $val );
}

sub _set_unsigned_integer {
    my ($self, $name, $val) = @_;
    ( !defined $val || $val !~ m/ \A \d+ \z /smx )
	and croak "Attribute $name must be an unsigned integer";
    return ($self->{$name} = $val + 0);
}

sub _set_use_all_limit {
    _set_integer(@_);
    delete $_[0]{_source_cache};
    return;
}

########################################################################
#
#	Private methods
#
#	The author reserves the right to change these without notice.

#	$rslt = $ele->_digest($rslt, $source);
#
#	This method takes the results of an elevation query and turns
#	them into the desired hash. If an error occurred, it is recorded
#	in the 'error' attribute, and either an exception is thrown (if
#	'croak' is true) or undef is returned (otherwise). If no error
#	occurred, we burrow down into the SOAP response, find the actual
#	query results, and return that.
#
#	The optional $source argument, if defined, is appended to any
#	error reported in the SOAP packet in lieu of the actual data
#	(i.e. when the no SOAP error was reported).

my $no_elevation_value_re = _make_matcher(
    q{ERROR: No Elevation value was returned} );

sub _digest {
    my ($self, $rslt, $source) = @_;
    if ( _instance( $rslt, 'SOAP::SOM' ) ) {
	if ($rslt->fault) {
	    return $self->_error($rslt->faultstring);
	} else {
	    $rslt = $rslt->result;
	}
    }
    $self->{trace} and SOAP::Trace->import('-all');
    my $round;
    {
	my $prec = $self->{places};
	if (defined $prec) {
	    $round = sub {
		is_valid($_[0]) ? sprintf ("%.${prec}f", $_[0])
		: $_[0]}
	}
    }

    my $error;
    if (ref $rslt eq 'HASH') {
	# The following line is because we may be handling an 'elevation
	# only' request.
	exists $rslt->{double}
	    and return $round ?
		$round->($rslt->{double}) :
		$rslt->{double};
	foreach my $key (
	    qw{USGS_Elevation_Web_Service_Query Elevation_Query}) {
	    (ref $rslt eq 'HASH' && exists $rslt->{$key}) or do {
		$error = "Elevation result is missing tag <$key>";
		last;
	    };
	    $rslt = $rslt->{$key};
	}
	unless (ref $rslt) {
	    if (defined $source &&
		$rslt =~ m/ $no_elevation_value_re /smxio &&
		(my $hash = $self->_get_bad_extent_hash($source))) {
		return $hash;
	    }
	    $rslt =~ m/ [.?!] \z /smx or $rslt .= '.';
	    $error = defined $source ?
		"$rslt Source = '$source'" : $rslt;
	}
    } else {
	$error = "No data found in SOAP result";
    }

    $error
	and return $self->_error( $error );

    if ($rslt && $round) {
	if (ref $rslt->{Elevation}) {
	    $rslt->{Elevation} = [
		map {$round->($_)} @{$rslt->{Elevation}}];
	} else {
	    $rslt->{Elevation} = $round->($rslt->{Elevation});
	}
    }
    return $rslt;
}

#	$ele->_error($text);
#
#	Set the error attribute, and croak if the croak attribute is
#	true. If croak is false, just return, carping if the carp
#	attribute is true.

sub _error {
    my ($self, @args) = @_;
    $self->{error} = join '', @args;
##  $self->{croak} and croak $self->{error};
    $self->{croak} and return;
    $self->{carp} and carp $self->{error};
    return;
}

#	$hash_ref = $ele->_get_bad_extent_hash($data_id);
#
#	Manufacture and return a data hash for the given data ID, with
#	'BAD_EXTENT' filled in for the elevation. If the $data_id does
#	not represent a known source, we return undef in scalar context,
#	or an empty list in list context.

sub _get_bad_extent_hash {
    my $self = shift;
    my ($id, $src) = $self->_get_source_info(shift) or return;
    return {
	Data_Source => $src,
	Data_ID	=> $id,
	Elevation	=> 'BAD_EXTENT',
	Units	=> $self->{units},
    };
}

#	$source => $ele->_get_source()
#
#	This method returns the source attribute, massaged for ease of
#	use.
#
#	If the source is a code reference, it is called, with $ele as
#	its argument. If the result is also a code reference, this is
#	repeated up to 10 times, after which we croak.
#
#	If the source is not defined, we return BEST_DATA_SET.
#
#	If the source is a scalar, we return it.
#
#	If the source is a reference to an array, we return it
#	unmodified if it has at least one element but not more than
#	'use_all_limit'. If it is empty, we return a reference to an
#	empty hash.  Otherwise we map it into a hash keyed on the array,
#	with value 1 for all entries, and return a reference to that
#	hash.
#
#	If the source is a reference to a hash, we return a reference to
#	the sorted keys if it has at least one element but not more than
#	'use_all_limit'. If it is empty, we return a reference to a
#	different empty hash. Otherwise we duplicate it and return a
#	reference to the duplicate.
#
#	If the source is anything else, we return it.
#
#	Note that because the actual computation is fairly complex, I
#	have decided to try to compute (when needed) and cache a
#	subroutine that returns the desired source when called. Any
#	attribute mutators which would (or might) cause the desired code
#	to change should clear the cache. Inside the comments is the
#	original code.

sub _get_source {
    return ($_[0]{_source_cache} ||= _get_source_cache(@_))->();
}

sub _get_source_cache {
    my ($self) = @_;
    defined (my $source = $self->{source})
	or return \&BEST_DATA_SET;
    my $ref = ref $source
	or return sub {$source};
    if ($ref eq 'ARRAY') {
    } elsif ($ref eq 'HASH') {
	$source = [sort keys %$source];
    } else {
	return sub {$source};
    }
    @$source or return sub {{}};
    my $limit = $self->get('use_all_limit');
####    $limit < 0 || @$source < $limit || grep m/^\*/i, @$source
    ($limit < 0 || @$source < $limit)
	and return sub {$source};
    $source = [map {_normalize_id($_)} @$source];
    return sub {
	my %src = map {$_ => 1} @$source;
	return \%src;
    };
}

#	($id, $name) = $self->_get_source_info($data_id);
#
#	This subroutine returns id in canonical case and the data source
#	name for the given Data_ID. If called in scalar context, a
#	reference to the array is returned. If the given data source can
#	not be found, we simply return (i.e. undef in scalar context and
#	an empty array in list context).

{
    my %name;

    sub _get_source_info {
	my ($self, $id) = @_;
	$id = _normalize_id($id);
	%name or $self->__get_source_info_hash();
	return unless $name{$id};
	return wantarray ? @{$name{$id}} : $name{$id};
    }

    sub __get_source_info_hash {
	my $self = shift || __PACKAGE__->new();
	foreach my $data (@{$self->getAllElevations(40, -90)}) {
	    $name{_normalize_id($data->{Data_ID})} = [
		$data->{Data_ID},
		$data->{Data_Source},
	    ];
	}
	return;
    }
}

#	_instance( $object, $class )
#	    and print "\$object isa $class\n";
#
#	Return true if $object is an instance of class $class, and false
#	otherwise. Unlike UNIVERSAL::isa, this is false if the first
#	object is not a reference.

sub _instance {
    my ( $object, $class ) = @_;
    blessed( $object ) or return;
    return $object->isa( $class );
}

#	my ($self, $lat, $lon, @_) = _latlon(@_);
#
#	Strip the object reference, latitude, and longitude off the
#	argument list. If the first argument is a Geo::Point,
#	GPS::Point, or Net::GPSD::Point object the latitude and
#	longitude come from it.  Otherwise the first argument is assumed
#	to be latitude, and the second to be longitude.

{

    my %known = (
	'Geo::Point' => sub {$_[0]->latlong('wgs84')},
	'GPS::Point' => sub {$_[0]->latlon()},
	'Net::GPSD::Point' => sub {$_[0]->latlon()},
    );

    sub _latlon {
	my ($self, $obj, @args) = @_;
	foreach my $class (keys %known) {
	    if (_instance( $obj, $class ) ) {
		return ($self, $known{$class}->($obj), @args);
	    }
	}
	return ($self, $obj, @args);
    }
}

sub _make_matcher {	## no critic (RequireArgUnpacking)
    my $string = join ' ', @_;
    $string =~ s/ [ ] / [ ] /smxg;
    $string =~ m/ \A \w /smx
	and substr $string, 0, 0, '\b ';
    $string =~ m/ \w \z /smx
	and $string .= ' \b';
    return qr{ $string }smx;
}

{
    my %static = (	# Static attribute values.
	throttle => 0,
    );

#	$self->_no_static_attr( $name );
#
#	Croaks if the invocant is not a reference. The message assumes
#	the method was called trying to access an attribute, whose name
#	is $name.

    sub _no_static_attr {
	my ( $self, $name ) = @_;
	ref $self
	    or croak "Attribute $name may not be accessed statically";
	return $self;
    }

#	$self->_only_static_attr( $name );
#
#	Croaks if the invocant is a reference. The message assumes the
#	method was called trying to access an attribute, whose name is
#	$name.

    sub _only_static_attr {
	my ( $self, $name ) = @_;
	ref $self
	    and croak "Attribute $name may only be accessed statically";
	return \%static;
    }

}

#	$id = _normalize_id($id)
#
#	This subroutine normalizes a Source_ID by uppercasing it. It
#	exists to centralize this operation.

sub _normalize_id {return uc $_[0]}

#	$soap_object = _soapdish ()

#	Manufacture a SOAP::Lite object from the object's attributes.
#	We need this because SOAP::Lite has changed significantly since
#	the 2002 version that as of 2006 was still bundled with
#	ActivePerl.
#
#	We also set the error attribute and $@ to undef in preparation
#	for a query.

_set_clean_soapdish( qw{ default_ns proxy timeout } );

sub _soapdish {
    my $self = shift;
    $self->{trace} and SOAP::Trace->import ('all');
    $self->{_soapdish} and return $self->{_soapdish};
    my $conn = '';	# Other possibility is '#'.
    my $act = sub {join $conn, @_};
    my $soap = SOAP::Lite->can ('default_ns') ?
	SOAP::Lite
	    ->default_ns ($self->{default_ns})
	    ->on_action ($act)
	    ->proxy ($self->{proxy}, timeout => $self->{timeout}) :
	SOAP::Lite
	    ->envprefix ('soap')
	    ->on_action ($act)
	    ->uri ($self->{default_ns})
	    ->proxy ($self->{proxy}, timeout => $self->{timeout});
    return ( $self->{_soapdish} = $soap );
}


1;

__END__

=head2 Attributes

=head3 carp (boolean)

This boolean attribute determines whether the data acquisition methods carp on
encountering an error. If false, they silently return undef. Note,
though, that the L<croak|/croak> attribute trumps this one.

If L<retry|/retry> is set to a number greater than 0, you will get a
carp on each failed query, provided L<croak|/croak> is false. If
L<croak|/croak> is true, no retries will be carped.

This attribute was introduced in Geo::WebService::Elevation::USGS
version 0.005_01.

The default is 0 (i.e. false).

=head3 croak (boolean)

This attribute determines whether the data acquisition methods croak on
encountering an error. If false, they return undef on an error.

If L<retry|/retry> is set to a number greater than 0, the data
acquisition method will not croak until all retries are exhausted.

The default is 1 (i.e. true).

=head3 default_ns (string)

This attribute records the XML namespace used by the SOAP query. This
must agree with the targetNamespace value given in the USGS' WSDL found
at
L<http://gisdata.usgs.gov/XMLWebServices/TNM_Elevation_service.asmx?WSDL>.

This attribute should not ordinarily need to be modified, but the
desperate user may be able to use it to get him- or herself going again
if the USGS changes the WSDL and this module has not been modified to
track the change.

The default is 'http://gisdata.usgs.gov/XMLWebServices2/'.

=head3 error (string)

This attribute records the error returned by the last query operation,
or undef if no error occurred. This attribute can be set by the user,
but will be reset by any query operation.

The default (before any queries have occurred) is undef.

=head3 places (integer)

If this attribute is set to a non-negative integer, elevation results
will be rounded to this number of decimal places by running them through
sprintf "%.${places}f".

The default is undef.

=head3 proxy (string)

This attribute specifies the actual url to which the SOAP query is
posted. It must agree with the soap:address location value given for
wsdl:port name "Elevation_ServiceSoap" given in the USGS' WSDL found
at
L<http://gisdata.usgs.gov/XMLWebServices/TNM_Elevation_service.asmx?WSDL>.

This attribute should not ordinarily need to be modified, but the
desperate user may be able to use it to get him- or herself going again
if the USGS changes the WSDL and this module has not been modified to
track the change.

The default is
'http://gisdata.usgs.gov/xmlwebservices2/elevation_service.asmx'.

=head3 retry (unsigned integer)

This attribute specifies the number of retries to be done by
C<getAllElevations()> and C<getElevation()> when an error is
encountered. The first try is not considered a retry, so if you set this
to 1 you get a maximum of two queries (the try and the retry).

Retries are done only on actual errors, not on bad extents. They are
also subject to the L</throttle> setting if any.

The default is 0, i.e. no retries.

=head3 retry_hook (code reference)

This attribute specifies a piece of code to be called before retrying.
The code will be called before a retry takes place, and will be passed
the Geo::WebService::Elevation::USGS object, the number of the retry
(from 1), the name of the method being retried (C<'getAllElevations'> or
C<'getElevation'>), and the arguments to that method. If the position
was passed as an object, the hook gets the latitude and longitude
unpacked from the object. The hook will B<not> be called before the
first try, nor after the last retry.

Examples:

 # To sleep 5 seconds between retries:
 $eq->set( retry_hook => sub { sleep 5 } );
 
 # To sleep 1 second before the first retry, 2 seconds
 # before the second, and so on:
 $eq->set( retry_hook => sub { sleep $_[1] } );
 
 # To do nothing between retries:
 $eq->set( retry_hook => sub {} );

The default is the null subroutine, i.e. C<sub {}>.

=head3 source

This attribute specifies the ID of the source layer to be queried by
the elevation() method. Valid layer IDs are documented at
L<http://gisdata.usgs.gov/XMLWebServices/TNM_Elevation_Service_Methods.php>.

A legal value is a scalar, or an ARRAY, CODE, HASH, or Regexp reference.
Please see the elevation() method's documentation for how these are
used.

The default is '-1', which requests a response from the 'best' data
source for the given point.

=head3 throttle (non-negative number, or undef)

 Geo::WebService::Elevation::USGS->set( throttle => 5 );

This attribute, if defined and positive, specifies the minimum interval
between queries, in seconds. This attribute may be set statically only,
and the limit applies to all queries, not just the ones from a given
object. If L<Time::HiRes|Time::HiRes> can be loaded, then sub-second
intervals are supported, otherwise not.

This functionality, and its implementation, are experimental, and may be
changed or retracted without notice. Heck, I may even go back to
C<$TARGET>, though I don't think so.

=head3 timeout (integer, or undef)

This attribute specifies the timeout for the SOAP query in seconds.

The default is 30.

=head3 trace (boolean)

If true, this attribute requests a SOAP::Lite trace of any queries made.
This should only be used for troubleshooting, and the author makes no
representation about and has no control over what output you get if you
set this true.

The default is undef (i.e. false).

=head3 units (string)

This attribute specifies the desired units for the resultant elevations.
The USGS documents 'FEET' and 'METERS' as valid values. Experimentation
shows that undocumented values (e.g. 'CUBITS') return results in feet,
which is documented as the default.

The default is 'FEET'.

=head3 use_all_limit (integer)

This attribute is used to optimize the behavior of the elevation()
method when the 'source' attribute is an array or hash reference. If the
number of elements in the array or hash is greater than or equal to this,
elevation() gets its data by calling getAllElevations() and then
dropping unwanted data. If the number of elements is less than
this number, elevation() iterates over the elements of the array or the
sorted keys of the hash, calling getElevation() on each.

Note that setting this to 0 causes getAllElevations() to be used always.
Setting this to -1 (or any negative number) is special-cased to cause
getElevation() to be used whenever the 'source' array or hash has any
entries at all, no matter how many it has.

The default is 5, which was chosen based on timings of the two methods.

=head2 Globals

=head3 $Geo::WebService::Elevation:USGS::THROTTLE

B<This interface is deprecated;> use the L</throttle> static method
instead. It will be revoked in release C<0.006>, and at that time
setting C<$THROTTLE> will become a fatal error. The short deprecation
cycle is because this interface has never been released in a production
release, and because you were warned when it was introduced. Until the
next production release, the next query after setting C<$THROTTLE> will
cause a warning message to be issued, and its value to be passed to
L</throttle>.

If set to a positive number, this symbol limits the rate at which
queries can be issued. The value represents the minimum interval between
queries (in seconds), which is enforced by having the query methods
C<sleep()> as needed. This minimum is enforced among objects, not just
within a single object.

If L<Time::HiRes|Time::HiRes> can be loaded, its C<time()> and
C<sleep()> will be used, and fractional minimum intervals can be
specified. If it can not be loaded, the core C<time()> and C<sleep()>
will be used. With the core functions, the resolution is a second, and
a C<$THROTTLE> value between 0 and 1 will be taken to be 1.

The behavior of this functionality when C<$THROTTLE> is set to a
non-numeric value is undefined. In practice, you will probably get an
error of some sort when you issue the query, but the author makes no
commitments. Same for quasi-numbers such as C<Inf> and C<NaN>.

The default is C<undef>, which means that this functionality is
disabled.

=head1 ACKNOWLEDGMENTS

The author wishes to acknowledge the following individuals and groups.

The members of the geo-perl mailing list provided valuable suggestions
and feedback, and generally helped me thrash through such issues as how
the module should work and what it should actually be called.

Michael R. Davis provided prompt and helpful feedback on a testing
problem in my first module to rely heavily on Test::More.

=head1 BUGS

Bugs can be reported to the author by mail, or through
L<http://rt.cpan.org/>.

=head1 SEE ALSO

=head1 AUTHOR

Thomas R. Wyant, III; F<wyant at cpan dot org>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2008-2010, Thomas R. Wyant, III

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl 5.10.0. For more details, see the full text
of the licenses in the directory LICENSES.

This program is distributed in the hope that it will be useful, but
without any warranty; without even the implied warranty of
merchantability or fitness for a particular purpose.

=cut

# ex: set textwidth=72 :
