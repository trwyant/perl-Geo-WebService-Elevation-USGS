package main;

use strict;
use warnings;

use Test::More;

_skip_it(eval {require Geo::WebService::Elevation::USGS},
    'Unable to load Geo::WebService::Elevation::USGS');

_skip_it(eval {require LWP::UserAgent},
    'Unable to load LWP::UserAgent (should not happen)');

my $ele = _skip_it(eval {Geo::WebService::Elevation::USGS->new(places => 2)},
    'Unable to instantiate GEO::Elevation::USGS');

{
    my $ua = _skip_it(eval {LWP::UserAgent->new()},
	'Unable to instantiate LWP::UserAgent (should not happen)');

    my $pxy = _skip_it(eval {$ele->get('proxy')},
	'Unable to retrieve proxy setting');

    my $rslt = _skip_it(eval {$ua->get($pxy)},
	'Unable to execute GET (should not happen)');

    _skip_it($rslt->is_success(),
	"Unable to access $pxy");
}

plan (tests => 163);

my $ele_ft = '54.70';	# Expected elevation in feet.
my $ele_mt = '16.67';	# Expected elevation in meters.

my $rslt;

SKIP: {
    $rslt = eval {$ele->getElevation(38.898748, -77.037684)};
    _skip_on_server_error($ele, 6);
    ok(!$@, 'getElevation succeeded') or diag($@);
    ok($rslt, 'getElevation returned a result');
    is(ref $rslt, 'HASH', 'getElevation returned a hash');
    is($rslt->{Data_ID}, 'NED.CONUS_NED_13E',
	'Data came from NED.CONUS_NED_13E');
    is($rslt->{Units}, 'FEET', 'Elevation is in feet');
    is($rslt->{Elevation}, $ele_ft, "Elevation is $ele_ft");
}

SKIP: {
    $rslt = eval {$ele->getElevation(38.898748, -77.037684, undef, 1)};
    _skip_on_server_error($ele, 2);
    ok(!$@, 'getElevation (only) succeeded') or diag($@);
    is($rslt, $ele_ft, "getElevation (only) returned $ele_ft");
}

SKIP: {
    $rslt = eval {$ele->elevation(38.898748, -77.037684)};
    _skip_on_server_error($ele, 7);
    ok(!$@, 'elevation() succeeded') or diag($@);
    is(ref $rslt, 'ARRAY', 'elevation() returned an array');
    ref $rslt eq 'ARRAY' or $rslt = [];	# To keep following from blowing up.
    cmp_ok(scalar @$rslt, '==', 1, 'elevation() returned a single result');
    is(ref ($rslt->[0]), 'HASH', 'elevation\'s only result was a hash');
    is($rslt->[0]{Data_ID}, 'NED.CONUS_NED_13E',
	'Data came from NED.CONUS_NED_13E');
    is($rslt->[0]{Units}, 'FEET', 'Elevation is in feet');
    is($rslt->[0]{Elevation}, $ele_ft, "Elevation is $ele_ft");
}

SKIP: {
    $rslt = eval {
	$ele->getElevation(38.898748, -77.037684, 'SRTM.C_SA_3', 1)};

    _skip_on_server_error($ele, 2);
    ok(!$@, 'getElevation does not fail when data has bad extent')
	or diag($@);
    ok(!$ele->is_valid($rslt),
	'getElevation does not return a valid elevation when given a bad extent');

=begin comment

#	This code represents behavior if we are allowing the behavior of
#	the USGS web server in this case to be visible to the caller. I
#	decided not to do this even though changes made in the service
#	on or about 1-Jan-2009 indicate that this is the USGS' intent.

    ok($@, 'getElevation fails when data has bad extent');
    like($@, qr{ERROR: No Elevation value was returned from servers\.}i,
	'getElevation returns expected message when data has bad extent');

=end comment

=cut

}
$ele->set(source => []);
is(ref ($ele->get('source')), 'ARRAY', 'Source can be set to an array ref');

SKIP: {
    $rslt = eval {$ele->elevation(38.898748, -77.037684)};
    _skip_on_server_error($ele, 7);
    ok(!$@, 'elevation() still succeeds') or diag($@);
    is(ref $rslt, 'ARRAY', 'elevation() still returns an array');
    ref $rslt eq 'ARRAY' or $rslt = [];	# To keep following from blowing up.
    cmp_ok(scalar @$rslt, '>', 1, 'elevation() returned multiple results');
    ok(!(grep {ref $_ ne 'HASH'} @$rslt),
	'elevation\'s results are all hashes');
    $rslt = {map {$_->{Data_ID} => $_} @$rslt};
    ok($rslt->{'NED.CONUS_NED_13E'}, 'We have results from NED.CONUS_NED_13E');
    is($rslt->{'NED.CONUS_NED_13E'}{Units}, 'FEET', 'Elevation is in feet');
    is($rslt->{'NED.CONUS_NED_13E'}{Elevation}, $ele_ft, "Elevation is $ele_ft");
}

$ele->set(source => {});
is(ref ($ele->get('source')), 'HASH', 'Source can be set to a hash ref');

SKIP: {
    $rslt = eval {$ele->elevation(38.898748, -77.037684)};
    _skip_on_server_error($ele, 7);
    ok(!$@, 'elevation() with hash source still succeeds') or diag($@);
    is(ref $rslt, 'ARRAY',
	'elevation() with hash source still returns an array');
    ref $rslt eq 'ARRAY' or $rslt = [];	# To keep following from blowing up.
    cmp_ok(scalar @$rslt, '>', 1, 'elevation() returned multiple results');
    ok(!(grep {ref $_ ne 'HASH'} @$rslt),
	'elevation\'s results are all hashes');
    $rslt = {map {$_->{Data_ID} => $_} @$rslt};
    ok($rslt->{'NED.CONUS_NED_13E'}, 'We have results from NED.CONUS_NED_13E');
    is($rslt->{'NED.CONUS_NED_13E'}{Units}, 'FEET', 'Elevation is in feet');
    is($rslt->{'NED.CONUS_NED_13E'}{Elevation}, $ele_ft, "Elevation is $ele_ft");
}

SKIP: {
    $ele->set(
	source => ['NED.CONUS_NED_13E', 'NED.CONUS_NED'],
	use_all_limit => 5,
    );
    $rslt = eval {$ele->elevation(38.898748, -77.037684)};
    _skip_on_server_error($ele, 7);
    ok(!$@, 'elevation() still succeeds') or diag($@);
    is(ref $rslt, 'ARRAY', 'elevation() still returns an array');
    ref $rslt eq 'ARRAY' or $rslt = [];	# To keep following from blowing up.
    cmp_ok(scalar @$rslt, '==', 2, 'elevation() returned two results');
    ok(!(grep {ref $_ ne 'HASH'} @$rslt),
	'elevation\'s results are all hashes');
    $rslt = {map {$_->{Data_ID} => $_} @$rslt};
    ok($rslt->{'NED.CONUS_NED_13E'}, 'We have results from NED.CONUS_NED_13E');
    is($rslt->{'NED.CONUS_NED_13E'}{Units}, 'FEET', 'Elevation is in feet');
    is($rslt->{'NED.CONUS_NED_13E'}{Elevation}, $ele_ft, "Elevation is $ele_ft");
}

SKIP: {
    $ele->set(
##	source => ['NED.CONUS_NED_13E', 'NED.CONUS_NED', 'SRTM.C_SA_3'],
	source => ['NED.CONUS_NED_13E', 'NED.CONUS_NED', 'NED.AK_NED'],
	use_all_limit => 0,
    );
    $rslt = eval {$ele->elevation(38.898748, -77.037684)};
    _skip_on_server_error($ele, 7);
    my $err = $@;
    ok(!$err, 'elevation() done by iteration succeeds') or do {
	diag ("Error: $err");
	skip("Elevation by iteration failed: $err", 6);
    };
    is(ref $rslt, 'ARRAY', 'elevation() still returns an array');
    ref $rslt eq 'ARRAY' or $rslt = [];	# To keep following from blowing up.
    cmp_ok(scalar @$rslt, '==', 3, 'elevation() returned three results');
    ok(!(grep {ref $_ ne 'HASH'} @$rslt),
	'elevation\'s results are all hashes');
    $rslt = {map {$_->{Data_ID} => $_} @$rslt};
    ok($rslt->{'NED.CONUS_NED_13E'}, 'We have results from NED.CONUS_NED_13E');
    is($rslt->{'NED.CONUS_NED_13E'}{Units}, 'FEET', 'Elevation is in feet');
    is($rslt->{'NED.CONUS_NED_13E'}{Elevation}, $ele_ft, "Elevation is $ele_ft");
}

SKIP: {
    $rslt = eval {$ele->elevation(38.898748, -77.037684, 1)};
    _skip_on_server_error($ele, 7);
    my $err = $@;
    ok(!$err, 'elevation(valid) succeeds') or do {
	diag ("Error: $err");
	skip("Elevation(valid) failed: $err", 6);
    };
    is(ref $rslt, 'ARRAY', 'elevation(valid) still returns an array');
    ref $rslt eq 'ARRAY' or $rslt = [];	# To keep following from blowing up.
    cmp_ok(scalar @$rslt, '==', 2, 'elevation(valid) returned two results')
	or warn "\$@ = $@";
    ok(!(grep {ref $_ ne 'HASH'} @$rslt),
	'elevation\'s results are all hashes');
    $rslt = {map {$_->{Data_ID} => $_} @$rslt};
    ok($rslt->{'NED.CONUS_NED_13E'}, 'We have results from NED.CONUS_NED_13E');
    is($rslt->{'NED.CONUS_NED_13E'}{Units}, 'FEET', 'Elevation is in feet');
    is($rslt->{'NED.CONUS_NED_13E'}{Elevation}, $ele_ft, "Elevation is $ele_ft");
}

{
    my $msg;
    local $SIG{__WARN__} = sub {$msg = $_[0]};
    my $bogus = $ele->new();
    ok($bogus, 'Call new() as normal method');
    isnt($bogus, $ele, 'They are different objects');

    # CAVEAT:
    # Direct manipulation of the attribute hash is UNSUPPORTED! I can't
    # think why anyone would want a public interface for {_hack_result}
    # anyway. If you do, contact me, and if I can't talk you out of it
    # we will come up with something.
    $bogus->{_hack_result} = {
	double => 58.6035683399111,
    };
    $rslt = eval {$bogus->getElevation(38.898748, -77.037684, undef, 1)};
    ok(!$@, 'getElevation (only) succeeded') or diag($@);
    is($rslt, '58.6035683399111',
	'getElevation (only) returned 58.6035683399111');

    $bogus->{_hack_result} = _get_bad_som();
    $rslt = eval {$bogus->getElevation(38.898748, -77.037684, undef, 1)};
    ok($bogus->get('error'),
	'getElevation() SOAP failures other than BAD_EXTENT conversion are errors.');

    $bogus->set(
	source => ['FUBAR'],
	use_all_limit => 0,
    );
    SKIP: {
	$rslt = eval {$bogus->elevation(38.898748, -77.037684)};
	_skip_on_server_error($bogus, 2);
	like($@, qr{^Source Data_ID FUBAR not found},
	    'Expect error from getAllElevations');
	ok(!$rslt, 'Expect no results from source \'FUBAR\'');
    }

    $bogus->set(
	use_all_limit => -1,
    );
    SKIP: {
	$rslt = eval {$bogus->elevation(38.898748, -77.037684)};
	_skip_on_server_error($bogus, 2);
	like($@, qr{^ERROR: Input Source Layer was invalid\.},
	    'Expect error from getElevation');
	ok(!$rslt, 'Expect no results from source \'FUBAR\'');
    }

    $bogus->set(
	source => sub {$_[1]{Data_ID} eq 'NED.CONUS_NED_13E'},
	use_all_limit => 0,
    );
    is(ref $bogus->get('source'), 'CODE', 'Can set source to code ref');
    SKIP: {
	$rslt = eval {$bogus->elevation(38.898748, -77.037684)};
	_skip_on_server_error($bogus, 5);
	ok(!$@, 'elevation succeeded using code ref as source') or diag($@);
	ok($rslt, 'Got a result when using code ref as source');
	is(ref $rslt, 'ARRAY', 'Got array ref when using code ref as source');
	ref $rslt eq 'ARRAY' or $rslt = [];	# To keep following from blowing up.
	cmp_ok(scalar @$rslt, '==', 1,
	    'Got exactly one result when using code ref as source');
	is($rslt->[0]{Data_ID}, 'NED.CONUS_NED_13E',
	    'Got correct Data_ID when using code ref as source');
    }

    $bogus->set(source => []);
    $bogus->{_hack_result} = undef;
    $rslt = eval {$bogus->elevation(38.898748, -77.037684)};
    like($@, qr{^No data found in SOAP result},
	'No data error when going through getAllElevations');

    $bogus->set(croak => 0, carp => 1);
    $bogus->{_hack_result} = undef;
    $rslt = eval {$bogus->elevation(38.898748, -77.037684)};
    ok(!$@, 'Should not throw an error on bad result if croak is false')
	or diag($@);
    like( $msg, qr{ \A No \s data \s found \b }smx,
	'Should warn if croak is false but carp is true' );
    ok(!$rslt, 'Should return undef on bad result if croak is false');
    like($bogus->get('error'), qr{^No data found in SOAP result},
	'No data error when going through getAllElevations');

    $msg = undef;
    $bogus->set(carp => 0);
    $bogus->{_hack_result} = undef;
    $rslt = eval {$bogus->elevation(38.898748, -77.037684)};
    ok(!$@, 'Should not throw an error on bad result if croak is false')
	or diag($@);
    ok( ! defined $msg, 'Should not warn if carp is false' );
    ok(!$rslt, 'Should return undef on bad result if croak is undef');
    like($bogus->get('error'), qr{^No data found in SOAP result},
	'No data error when going through getAllElevations');

    SKIP: {
	$bogus->set(
	    source => {'SRTM.C_SA_3' => 1},
	    use_all_limit => 5,
	);
	$rslt = eval {$bogus->elevation(38.898748, -77.037684)};
	_skip_on_server_error($bogus, 2);
	my $err = $bogus->get('error');
	$err =~ m/Input Source Layer was invalid/i
	    and skip($err, 2);
	ok(!$err,
	    'Query of SRTM.C_SA_3 still is not an error')
	    or diag($bogus->get('error'));
	ok(!$bogus->is_valid($rslt->[0]),
	    'SRTM.C_SA_3 still does not return a valid elevation');
    }

    $bogus->{_hack_result} = _get_bad_som();
    $rslt = eval {$bogus->elevation(38.898748, -77.037684)};
    ok($bogus->get('error'),
	'SOAP failures other than conversion of BAD_EXTENT are still errors.');

    $bogus->set(croak => 1);
    $bogus->{_hack_result} = _get_bad_som();
    $rslt = eval {$bogus->elevation(38.898748, -77.037684)};
    ok($@,
	'SOAP failures other than conversion are fatal with croak => 1');
    ok($bogus->get('error'),
	'SOAP failures should set {error} even if fatal');

    $bogus->set(
	source => ['FUBAR'],
	use_all_limit => 0,
	croak => 0,
    );
    SKIP: {
	$rslt = eval {$bogus->elevation(38.898748, -77.037684)};
	_skip_on_server_error($bogus, 1);
	like($bogus->get('error'),
####	    qr{ERROR: Input Source Layer was invalid},
	    qr{Source Data_ID FUBAR not found},
	    'Data set FUBAR is still an error');
    }

    $bogus->set(source => undef, croak => 1);
    $bogus->{_hack_result} = undef;
    $rslt = eval {$bogus->elevation(38.898748, -77.037684)};
    like($@, qr{^No data found in SOAP result},
	'No data error when going through getElevations');

    $bogus->set(croak => 0);
    $bogus->{_hack_result} = undef;
    $rslt = eval {$bogus->elevation(38.898748, -77.037684)};
    ok(!$@, 'Should not throw an error on bad result if croak is false')
	or diag($@);
    ok(!$rslt, 'Should return undef on bad result if croak is false');
    like($bogus->get('error'), qr{^No data found in SOAP result},
	'No data error when going through getElevation');

    $bogus->{_hack_result} = {};
    $rslt = eval {$bogus->elevation(38.898748, -77.037684)};
    ok(!$@, 'Should not throw an error on bad result if croak is false')
	or diag($@);
    like($bogus->get('error'), qr{^Elevation result is missing tag},
	'Missing tag error when going through getElevation');

    $bogus->{_hack_result} = {USGS_Elevation_Web_Service_Query => []};
    $rslt = eval {$bogus->elevation(38.898748, -77.037684)};
    ok(!$@, 'Should not throw an error on bad result if croak is false')
	or diag($@);
    like($bogus->get('error'), qr{^Elevation result is missing tag},
	'Missing tag error when going through getElevation');

    $bogus->{_hack_result} = {
	USGS_Elevation_Web_Service_Query => {
	    Elevation_Query => 'Something bad happened',
	},
    };
    $rslt = eval {$bogus->elevation(38.898748, -77.037684)};
    ok(!$@, 'Should not throw an error on bad result if croak is false')
	or diag($@);
    like($bogus->get('error'), qr{^Something bad happened},
	'Missing data error when going through getElevation');

    $bogus->{_hack_result} = {
	USGS_Elevation_Web_Service_Query => {
	    Elevation_Query => {
		Data_Source => 'NED Contiguous U. S. 1/3E arc second elevation data',
		Data_ID	=> 'NED.CONUS_NED_13E',
		Elevation => 58.6035683399111,
		Units => 'FEET',
	    },
	},
    };
    $rslt = eval {$bogus->getAllElevations(38.898748, -77.037684)};
    ok(!$bogus->get('error'),
	'Should not declare an error processing an individual point');
    is(ref $rslt, 'ARRAY', 'Result should still be an array ref')
	or $rslt = [];
    cmp_ok(scalar @$rslt, '==', 1, 'getAllelevations() returned one result');
    ok(!(grep {ref $_ ne 'HASH'} @$rslt),
	'elevation\'s results are all hashes');
    $rslt = {map {$_->{Data_ID} => $_} @$rslt};
    ok($rslt->{'NED.CONUS_NED_13E'}, 'We have results from NED.CONUS_NED_13E');
    is($rslt->{'NED.CONUS_NED_13E'}{Units}, 'FEET', 'Elevation is in feet');
    is($rslt->{'NED.CONUS_NED_13E'}{Elevation}, '58.6035683399111',
	'Elevation is 58.6035683399111');


    $bogus->{_hack_result} = {
	USGS_Elevation_Web_Service_Query => {
	    Elevation_Query => {
		Data_Source => 'NED Contiguous U. S. 1/3E arc second elevation data',
		Data_ID	=> {},	# Force error
		Elevation => 58.6035683399111,
		Units => 'FEET',
	    },
	},
    };
    $rslt = eval {$bogus->getAllElevations(38.898748, -77.037684)};
    like($bogus->get('error'), qr{Unexpected HASH reference},
	'Should declare an error if {Data_ID} is a hash reference');

    $bogus->set(proxy => $bogus->get('proxy') . '_xyzzy');
    SKIP: {
	$rslt = eval {$bogus->elevation(38.898748, -77.037684)};
	_skip_on_server_error($bogus, 2);
	ok(!$@, 'Should not throw an error on bad proxy if croak is false')
	    or diag($@);
	like($bogus->get('error'), qr{^404\b},
	    'SOAP error when going through getElevation');
    }

    $bogus->set(source => []);
    SKIP: {
	$rslt = eval {$bogus->elevation(38.898748, -77.037684)};
	_skip_on_server_error($bogus, 2);
	ok(!$@, 'Should not throw an error on bad proxy if croak is false')
	    or diag($@);
	like($bogus->get('error'), qr{^404\b},
	    'SOAP error when going through getAllElevations');
    }

    $bogus->set(croak => 1);
    SKIP: {
	$rslt = eval {$bogus->elevation(38.898748, -77.037684)};
	_skip_on_server_error($bogus, 2);
	ok(($msg = $@), 'Should throw an error on bad proxy if croak is true');
	like($msg, qr{^404\b},
	    'SOAP error when going through getAllElevations');
    }

}

{
    my $retries;
    my $bogus = $ele->new(
	places => 2,
	retry => 1,	# Do a single retry
	retry_hook => sub { $retries++ },	# Just count them
    );

    SKIP: {
	$retries = 0;
	$bogus->{_hack_result} = _get_bad_som();
	$rslt = eval {$bogus->getElevation(38.898748, -77.037684)};
	ok( $retries, 'A retry was performed' );
	_skip_on_server_error($bogus, 6);
	ok(!$@, 'getElevation succeeded on retry') or diag($@);
	ok($rslt, 'getElevation returned a result on retry');
	is(ref $rslt, 'HASH', 'getElevation returned a hash on retry');
	is($rslt->{Data_ID}, 'NED.CONUS_NED_13E',
	    'Data came from NED.CONUS_NED_13E on retry');
	is($rslt->{Units}, 'FEET', 'Elevation is in feet on retry');
	is($rslt->{Elevation}, $ele_ft, "Elevation is $ele_ft on retry");
    }

    SKIP: {
	$retries = 0;
	$bogus->{_hack_result} = _get_bad_som();
	$rslt = eval {$bogus->getAllElevations(38.898748, -77.037684)};
	ok( $retries, 'A retry was performed' );
	_skip_on_server_error($bogus, 6);
	ok(!$@, 'getAllElevations succeeded on retry') or diag($@);
	ok($rslt, 'getAllElevations returned a result on retry');
	is(ref $rslt, 'ARRAY', 'getAllElevations returned an array on retry');
	my %hash = map { $_->{Data_ID} => $_ } @{ $rslt };
	ok( $hash{'NED.CONUS_NED_13E'},
	    'Results contain NED.CONUS_NED_13E on retry' );
	is($hash{'NED.CONUS_NED_13E'}{Units}, 'FEET',
	    'Elevation is in feet on retry');
	is($hash{'NED.CONUS_NED_13E'}{Elevation}, $ele_ft,
	    "Elevation is $ele_ft on retry");
    }

}

$ele->set(
    croak => 1,
    source => undef,
    units => 'METERS'
);

SKIP: {
    $rslt = eval {$ele->getElevation(38.898748, -77.037684)};
    _skip_on_server_error($ele, 6);
    ok(!$@, 'getElevation again succeeded') or diag($@);
    ok($rslt, 'getElevation again returned a result');
    is(ref $rslt, 'HASH', 'getElevation again returned a hash');
    is($rslt->{Data_ID}, 'NED.CONUS_NED_13E', 'Data again came from NED.CONUS_NED_13E');
    is($rslt->{Units}, 'METERS', 'Elevation is in meters');
    is($rslt->{Elevation}, $ele_mt, "Elevation is $ele_mt");
}

SKIP: {
    $rslt = eval {$ele->getElevation(38.898748, -77.037684, undef, 1)};
    _skip_on_server_error($ele, 2);
    ok(!$@, 'getElevation(only) succeeded') or diag($@);
    is($rslt, $ele_mt, "getElevation (only) returned $ele_mt");
}

SKIP: {
    $rslt = eval {[$ele->elevation(38.898748, -77.037684)]};
    _skip_on_server_error($ele, 7);
    ok(!$@, 'elevation() succeeded in list context') or diag($@);
    is(ref $rslt, 'ARRAY', 'elevation() returns an array in list context');
    ref $rslt eq 'ARRAY' or $rslt = [];	# To keep following from blowing up.
    cmp_ok(scalar @$rslt, '==', 1, 'elevation() returned a single result');
    is(ref ($rslt->[0]), 'HASH', 'elevation\'s only result was a hash');
    is($rslt->[0]{Data_ID}, 'NED.CONUS_NED_13E',
	'Data came from NED.CONUS_NED_13E');
    is($rslt->[0]{Units}, 'METERS', 'Elevation is in meters');
    is($rslt->[0]{Elevation}, $ele_mt, "Elevation is $ele_mt");
}

eval {$ele->set(source => \*STDOUT)};
like($@, qr{^Attribute source may not be a GLOB ref},
    'Can not set source as a glob ref');
# NOTE that direct modification of object attributes like this is UNSUPPORTED.
$ele->{source} = \*STDOUT;	# Bypass validation
delete $ele->{_source_cache};	# Clear cache
$rslt = eval {[$ele->elevation(38.898748, -77.037684)]};
like($@, qr{^Source GLOB ref not understood},
    'Bogus source reference gets caught in use');
$ele->set(source => qr{^NED\.CONUS_NED}i);
is(ref $ele->get('source'), 'Regexp', 'Can set source as a regexp ref');

SKIP: {
    $rslt = eval {[$ele->elevation(38.898748, -77.037684)]};
    _skip_on_server_error($ele, 6);
    ok(!$@, 'elevation() succeeded with regexp source') or diag($@);
    is(ref $rslt, 'ARRAY', 'Get an array back from regexp source');
    ref $rslt eq 'ARRAY' or $rslt = [];	# To keep following from blowing up.
    cmp_ok(scalar @$rslt, '>=', 2, 'Should have at least two results');
    $rslt = {map {$_->{Data_ID} => $_} @$rslt};
    ok($rslt->{'NED.CONUS_NED_13E'}, 'We have results from NED.CONUS_NED_13E');
    is($rslt->{'NED.CONUS_NED_13E'}{Units}, 'METERS', 'Elevation is in meters');
    is($rslt->{'NED.CONUS_NED_13E'}{Elevation}, $ele_mt, "Elevation is $ele_mt");
}

my $gp = {};
bless $gp, 'Geo::Point';
$ele->set(source => {'NED.CONUS_NED_13E' => 1});
is(ref $ele->get('source'), 'HASH', 'Can set source as a hash');

SKIP: {
    $rslt = eval {$ele->elevation($gp)};
    _skip_on_server_error($ele, 7);
    ok(!$@, 'elevation(Geo::Point) succeeded') or diag($@);
    is(ref $rslt, 'ARRAY',
	'elevation(Geo::Point) returns an array from getAllElevations');
    ref $rslt eq 'ARRAY' or $rslt = [];	# To keep following from blowing up.
    cmp_ok(scalar @$rslt, '==', 1,
	'elevation(Geo::Point) returned a single result');
    is(ref ($rslt->[0]), 'HASH', 'elevation\'s only result was a hash');
    is($rslt->[0]{Data_ID}, 'NED.CONUS_NED_13E',
	'Data came from NED.CONUS_NED_13E');
    is($rslt->[0]{Units}, 'METERS', 'Elevation is in meters');
    is($rslt->[0]{Elevation}, $ele_mt, "Elevation is $ele_mt");
}

SKIP: {
    $ele->set(use_all_limit => -1);	# Force iteration.
    $rslt = eval {$ele->elevation($gp)};
    _skip_on_server_error($ele, 2);
    ok(!$@, 'elevation(Geo::Point) via getElevation succeeded') or diag($@);
    is(ref $rslt, 'ARRAY',
	'elevation(Geo::Point) returns an array from getElevation');
}

SKIP: {
    my $kind;
    if (eval {require GPS::Point}) {
	$gp = GPS::Point->new();
	$gp->lat(38.898748);
	$gp->lon(-77.037684);
	$gp->alt(undef);
	$kind = 'real GPS::Point';
    } else {
	$gp = {};
	bless $gp, 'GPS::Point';
	no warnings qw{once};
	*GPS::Point::latlon = sub {
	    return (38.898748, -77.037684)
	};
	$kind = 'dummy GPS::Point';
    }
    $ele->set(use_all_limit => 0);	# Force getAllElevations
    $rslt = eval {$ele->elevation($gp)};
    _skip_on_server_error($ele, 7);
    ok(!$@, "elevation($kind GPS::Point) via getAllElevations succeeded")
	or diag($@);
    is(ref $rslt, 'ARRAY',
	"elevation($kind) returns an array from getAllElevations");
    ref $rslt eq 'ARRAY' or $rslt = [];	# To keep following from blowing up.
    cmp_ok(scalar @$rslt, '==', 1,
	"elevation($kind) returned a single result");
    is(ref ($rslt->[0]), 'HASH', "$kind elevation's only result was a hash");
    is($rslt->[0]{Data_ID}, 'NED.CONUS_NED_13E',
	"$kind data came from NED.CONUS_NED_13E");
    is($rslt->[0]{Units}, 'METERS', "$kind elevation is in meters");
    is($rslt->[0]{Elevation}, $ele_mt, "$kind elevation is $ele_mt");
}

_skip_on_server_summary();

# I need to mung the argument list before use because the idea is to
# call this with an indication of whether to skip the whole test and
# a reason for skipping. The first argument may be computed inside an
# eval{}, which returns () in list context on failure.
#
sub _skip_it {
    my @args = @_;
    @args > 1
	or unshift @args, undef;  # Because eval{} returns () in list context.
    my ($check, $reason) = @args;
    unless ($check) {
	plan (skip_all => $reason);
	exit;
    }
    return $check;
}

{
    my $skips;

    sub _skip_on_server_error {
	my ($ele, $how_many) = @_;
	local $_ = $ele->get('error') or return;
	(m/^5\d\d\b/ ||
	    m/^ERROR: No Elevation values were returned/i ||
	    m/^ERROR: No Elevation value was returned/i ||
	    m/System\.Web\.Services\.Protocols\.SoapException/i
	) or return;
	$skips += $how_many;
	my ($pkg, $file, $line) = caller(0);
	diag("Skipping $how_many tests: $_ at $file line $line");
	return skip ($_, $how_many);
    }

    sub _skip_on_server_summary {
	$skips and diag(<<eod);

Skipped $skips tests due to apparent server errors.

eod
	return;
    }

}

sub Geo::Point::latlong {
    return (38.898748, -77.037684)
}

my $VAR1;
sub _get_bad_som {
    return ($VAR1 ||= bless( {
                 '_content' => [
                                 'soap:Envelope',
                                 {
                                   'xmlns:xsi' => 'http://www.w3.org/2001/XMLSchema-instance',
                                   'xmlns:xsd' => 'http://www.w3.org/2001/XMLSchema',
                                   'xmlns:soap' => 'http://schemas.xmlsoap.org/soap/envelope/'
                                 },
                                 [
                                   [
                                     'soap:Body',
                                     {},
                                     [
                                       [
                                         'soap:Fault',
                                         {},
                                         [
                                           [
                                             'faultcode',
                                             {},
                                             'soap:Server',
                                             undef,
                                             'soap:Server',
                                             'faultcode',
                                             {}
                                           ],
                                           [
                                             'faultstring',
                                             {},
                                             'System.Web.Services.Protocols.SoapException: Server was unable to process request. ---> Bogus error injected into system to test error handling.',
                                             undef,
                                             'System.Web.Services.Protocols.SoapException: Server was unable to process request. ---> Bogus error injected into system to test error handling.',
                                             'faultstring',
                                             {}
                                           ],
                                           [
                                             'detail',
                                             {},
                                             '',
                                             undef,
                                             '',
                                             'detail',
                                             {}
                                           ]
                                         ],
                                         undef,
                                         {
                                           'detail' => '',
                                           'faultcode' => 'soap:Server',
                                           'faultstring' => 'System.Web.Services.Protocols.SoapException: Server was unable to process request. ---> Bogus error injected into system to test error handling.',
                                         },
                                         '{http://schemas.xmlsoap.org/soap/envelope/}Fault',
                                         {}
                                       ]
                                     ],
                                     undef,
                                     {
                                       'Fault' => $VAR1->{'_content'}[2][0][2][0][4]
                                     },
                                     '{http://schemas.xmlsoap.org/soap/envelope/}Body',
                                     {}
                                   ]
                                 ],
                                 undef,
                                 {
                                   'Body' => $VAR1->{'_content'}[2][0][4]
                                 },
                                 '{http://schemas.xmlsoap.org/soap/envelope/}Envelope',
                                 {}
                               ],
                 '_context' => bless( {
                                        '_on_nonserialized' => sub { "DUMMY" },
                                        '_deserializer' => bless( {
                                                                    '_ids' => $VAR1->{'_content'},
                                                                    '_xmlschemas' => {
                                                                                       'http://www.w3.org/2003/05/soap-encoding' => 'SOAP::Lite::Deserializer::XMLSchemaSOAP1_2',
                                                                                       'http://xml.apache.org/xml-soap' => 'SOAP::XMLSchemaApacheSOAP::Deserializer',
                                                                                       'http://www.w3.org/2001/XMLSchema' => 'SOAP::Lite::Deserializer::XMLSchema2001',
                                                                                       'http://www.w3.org/1999/XMLSchema' => 'SOAP::Lite::Deserializer::XMLSchema1999',
                                                                                       'http://schemas.xmlsoap.org/soap/encoding/' => 'SOAP::Lite::Deserializer::XMLSchemaSOAP1_1'
                                                                                     },
                                                                    '_context' => $VAR1->{'_context'},
                                                                    '_hrefs' => {},
                                                                    '_parser' => bless( {
                                                                                          '_done' => $VAR1->{'_content'},
                                                                                          '_values' => undef,
                                                                                          '_parser' => bless( {
                                                                                                                'Non_Expat_Options' => {
                                                                                                                                         'NoLWP' => 1,
                                                                                                                                         'Non_Expat_Options' => 1,
                                                                                                                                         '_HNDL_TYPES' => 1,
                                                                                                                                         'Handlers' => 1,
                                                                                                                                         'Style' => 1
                                                                                                                                       },
                                                                                                                'Pkg' => 'SOAP::Parser',
                                                                                                                'Handlers' => {
                                                                                                                                'End' => undef,
                                                                                                                                'Final' => undef,
                                                                                                                                'Char' => undef,
                                                                                                                                'Start' => undef,
                                                                                                                                'ExternEnt' => undef
                                                                                                                              },
                                                                                                                '_HNDL_TYPES' => {
                                                                                                                                   'CdataEnd' => sub { "DUMMY" },
                                                                                                                                   'Start' => sub { "DUMMY" },
                                                                                                                                   'Entity' => sub { "DUMMY" },
                                                                                                                                   'ExternEntFin' => sub { "DUMMY" },
                                                                                                                                   'End' => sub { "DUMMY" },
                                                                                                                                   'Final' => 1,
                                                                                                                                   'Doctype' => sub { "DUMMY" },
                                                                                                                                   'Char' => sub { "DUMMY" },
                                                                                                                                   'Init' => 1,
                                                                                                                                   'XMLDecl' => sub { "DUMMY" },
                                                                                                                                   'Default' => sub { "DUMMY" },
                                                                                                                                   'CdataStart' => sub { "DUMMY" },
                                                                                                                                   'Comment' => sub { "DUMMY" },
                                                                                                                                   'Unparsed' => sub { "DUMMY" },
                                                                                                                                   'ExternEnt' => sub { "DUMMY" },
                                                                                                                                   'Element' => sub { "DUMMY" },
                                                                                                                                   'Attlist' => sub { "DUMMY" },
                                                                                                                                   'DoctypeFin' => sub { "DUMMY" },
                                                                                                                                   'Notation' => sub { "DUMMY" },
                                                                                                                                   'Proc' => sub { "DUMMY" }
                                                                                                                                 }
                                                                                                              }, 'XML::Parser' )
                                                                                        }, 'SOAP::Parser' )
                                                                  }, 'SOAP::Deserializer' ),
                                        '_autoresult' => 0,
                                        '_transport' => bless( {
                                                                 '_proxy' => bless( {
                                                                                      '_status' => '500 Internal Server Error',
                                                                                      '_message' => 'Internal Server Error',
                                                                                      'requests_redirectable' => [
                                                                                                                   'GET',
                                                                                                                   'HEAD'
                                                                                                                 ],
                                                                                      'timeout' => 30,
                                                                                      '_is_success' => '',
                                                                                      'max_redirect' => 7,
                                                                                      '_http_response' => bless( {
                                                                                                                   '_protocol' => 'HTTP/1.1',
                                                                                                                   '_content' => '<?xml version="1.0" encoding="utf-8"?><soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema"><soap:Body><soap:Fault><faultcode>soap:Server</faultcode><faultstring>System.Web.Services.Protocols.SoapException: Server was unable to process request. ---&gt; Bogus error injected into system to test error handling.</faultstring><detail /></soap:Fault></soap:Body></soap:Envelope>',
                                                                                                                   '_rc' => 500,
                                                                                                                   '_headers' => bless( {
                                                                                                                                          'x-powered-by' => 'ASP.NET',
                                                                                                                                          'client-response-num' => 1,
                                                                                                                                          'cache-control' => 'private',
                                                                                                                                          'date' => 'Wed, 03 Dec 2008 18:05:22 GMT',
                                                                                                                                          'client-peer' => '152.61.128.16:80',
                                                                                                                                          'content-length' => '1272',
                                                                                                                                          'x-aspnet-version' => '2.0.50727',
                                                                                                                                          'client-date' => 'Wed, 03 Dec 2008 18:05:21 GMT',
                                                                                                                                          'content-type' => 'text/xml; charset=utf-8',
                                                                                                                                          'server' => 'Microsoft-IIS/6.0'
                                                                                                                                        }, 'HTTP::Headers' ),
                                                                                                                   '_msg' => 'Internal Server Error',
                                                                                                                   'handlers' => {
                                                                                                                                   'response_data' => [
                                                                                                                                                        {
                                                                                                                                                          'callback' => sub { "DUMMY" }
                                                                                                                                                        }
                                                                                                                                                      ]
                                                                                                                                 },
                                                                                                                   '_request' => bless( {
                                                                                                                                          '_protocol' => 'HTTP/1.1',
                                                                                                                                          '_content' => '<?xml version="1.0" encoding="UTF-8"?><soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/" xmlns:xsd="http://www.w3.org/2001/XMLSchema" soap:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/"><soap:Body><getElevation xmlns="http://gisdata.usgs.gov/XMLWebServices2/"><X_Value xsi:type="xsd:string">-90</X_Value><Y_Value xsi:type="xsd:string">40</Y_Value><Source_Layer xsi:type="xsd:string">SRTM.C_US_1</Source_Layer><Elevation_Units xsi:type="xsd:string">FEET</Elevation_Units><Elevation_Only xsi:type="xsd:string">false</Elevation_Only></getElevation></soap:Body></soap:Envelope>',
                                                                                                                                          '_uri' => bless( do{\(my $o = 'http://gisdata.usgs.gov/xmlwebservices2/elevation_service.asmx')}, 'URI::http' ),
                                                                                                                                          '_headers' => bless( {
                                                                                                                                                                 'user-agent' => 'SOAP::Lite/Perl/0.710.08',
                                                                                                                                                                 'soapaction' => 'http://gisdata.usgs.gov/XMLWebServices2/getElevation',
                                                                                                                                                                 'content-type' => 'text/xml; charset=utf-8',
                                                                                                                                                                 'accept' => [
                                                                                                                                                                               'text/xml',
                                                                                                                                                                               'multipart/*',
                                                                                                                                                                               'application/soap'
                                                                                                                                                                             ],
                                                                                                                                                                 'content-length' => 715
                                                                                                                                                               }, 'HTTP::Headers' ),
                                                                                                                                          '_method' => 'POST',
                                                                                                                                          '_uri_canonical' => $VAR1->{'_context'}{'_transport'}{'_proxy'}{'_http_response'}{'_request'}{'_uri'}
                                                                                                                                        }, 'HTTP::Request' )
                                                                                                                 }, 'HTTP::Response' ),
                                                                                      '_endpoint' => 'http://gisdata.usgs.gov/xmlwebservices2/elevation_service.asmx',
                                                                                      'show_progress' => undef,
                                                                                      'protocols_forbidden' => undef,
                                                                                      'no_proxy' => [],
                                                                                      'handlers' => {
                                                                                                      'response_header' => bless( [
                                                                                                                                    {
                                                                                                                                      'owner' => 'LWP::UserAgent::parse_head',
                                                                                                                                      'callback' => sub { "DUMMY" },
                                                                                                                                      'm_media_type' => 'html',
                                                                                                                                      'line' => '/usr/local/lib/perl5/site_perl/5.10.0/LWP/UserAgent.pm:629'
                                                                                                                                    }
                                                                                                                                  ], 'HTTP::Config' )
                                                                                                    },
                                                                                      '_options' => {
                                                                                                      'is_compress' => ''
                                                                                                    },
                                                                                      'protocols_allowed' => undef,
                                                                                      'use_eval' => 1,
                                                                                      '_http_request' => bless( {
                                                                                                                  '_content' => '',
                                                                                                                  '_uri' => undef,
                                                                                                                  '_headers' => bless( {}, 'HTTP::Headers' ),
                                                                                                                  '_method' => undef
                                                                                                                }, 'HTTP::Request' ),
                                                                                      '_code' => '500',
                                                                                      'def_headers' => bless( {
                                                                                                                'user-agent' => 'SOAP::Lite/Perl/0.710.08'
                                                                                                              }, 'HTTP::Headers' ),
                                                                                      'proxy' => {},
                                                                                      'max_size' => undef
                                                                                    }, 'SOAP::Transport::HTTP::Client' )
                                                               }, 'SOAP::Transport' ),
                                        '_serializer' => bless( {
                                                                  '_typelookup' => {
                                                                                     'int' => [
                                                                                                20,
                                                                                                sub { "DUMMY" },
                                                                                                'as_int'
                                                                                              ],
                                                                                     'time' => [
                                                                                                 70,
                                                                                                 sub { "DUMMY" },
                                                                                                 'as_time'
                                                                                               ],
                                                                                     'date' => [
                                                                                                 60,
                                                                                                 sub { "DUMMY" },
                                                                                                 'as_date'
                                                                                               ],
                                                                                     'gYear' => [
                                                                                                  45,
                                                                                                  sub { "DUMMY" },
                                                                                                  'as_gYear'
                                                                                                ],
                                                                                     'string' => [
                                                                                                   100,
                                                                                                   sub { "DUMMY" },
                                                                                                   'as_string'
                                                                                                 ],
                                                                                     'dateTime' => [
                                                                                                     75,
                                                                                                     sub { "DUMMY" },
                                                                                                     'as_dateTime'
                                                                                                   ],
                                                                                     'boolean' => [
                                                                                                    90,
                                                                                                    sub { "DUMMY" },
                                                                                                    'as_boolean'
                                                                                                  ],
                                                                                     'float' => [
                                                                                                  30,
                                                                                                  sub { "DUMMY" },
                                                                                                  'as_float'
                                                                                                ],
                                                                                     'anyURI' => [
                                                                                                   95,
                                                                                                   sub { "DUMMY" },
                                                                                                   'as_anyURI'
                                                                                                 ],
                                                                                     'long' => [
                                                                                                 25,
                                                                                                 sub { "DUMMY" },
                                                                                                 'as_long'
                                                                                               ],
                                                                                     'gDay' => [
                                                                                                 40,
                                                                                                 sub { "DUMMY" },
                                                                                                 'as_gDay'
                                                                                               ],
                                                                                     'gMonthDay' => [
                                                                                                      50,
                                                                                                      sub { "DUMMY" },
                                                                                                      'as_gMonthDay'
                                                                                                    ],
                                                                                     'gYearMonth' => [
                                                                                                       55,
                                                                                                       sub { "DUMMY" },
                                                                                                       'as_gYearMonth'
                                                                                                     ],
                                                                                     'duration' => [
                                                                                                     80,
                                                                                                     sub { "DUMMY" },
                                                                                                     'as_duration'
                                                                                                   ],
                                                                                     'base64Binary' => [
                                                                                                         10,
                                                                                                         sub { "DUMMY" },
                                                                                                         'as_base64Binary'
                                                                                                       ],
                                                                                     'zerostring' => [
                                                                                                       12,
                                                                                                       sub { "DUMMY" },
                                                                                                       'as_string'
                                                                                                     ],
                                                                                     'gMonth' => [
                                                                                                   35,
                                                                                                   sub { "DUMMY" },
                                                                                                   'as_gMonth'
                                                                                                 ]
                                                                                   },
                                                                  '_encodingStyle' => 'http://schemas.xmlsoap.org/soap/encoding/',
                                                                  '_objectstack' => {},
                                                                  '_level' => 0,
                                                                  '_context' => $VAR1->{'_context'},
                                                                  '_signature' => [
                                                                                    'X_Valuestring',
                                                                                    'Y_Valuestring',
                                                                                    'Source_Layerstring',
                                                                                    'Elevation_Unitsstring',
                                                                                    'Elevation_Onlystring'
                                                                                  ],
                                                                  '_soapversion' => '1.1',
                                                                  '_maptype' => {},
                                                                  '_use_default_ns' => 1,
                                                                  '_namespaces' => {
                                                                                     'http://www.w3.org/2001/XMLSchema' => 'xsd',
                                                                                     'http://schemas.xmlsoap.org/soap/encoding/' => 'soapenc',
                                                                                     'http://www.w3.org/2001/XMLSchema-instance' => 'xsi',
                                                                                     'http://schemas.xmlsoap.org/soap/envelope/' => 'soap'
                                                                                   },
                                                                  '_seen' => {
                                                                               '9556512' => {
                                                                                              'recursive' => 0,
                                                                                              'count' => 1,
                                                                                              'value' => bless( {
                                                                                                                  '_name' => 'Y_Value',
                                                                                                                  '_type' => 'string',
                                                                                                                  '_signature' => [
                                                                                                                                    'Y_Valuestring'
                                                                                                                                  ],
                                                                                                                  '_value' => [
                                                                                                                                '40'
                                                                                                                              ],
                                                                                                                  '_attr' => {}
                                                                                                                }, 'SOAP::Data' ),
                                                                                              'multiref' => ''
                                                                                            },
                                                                               '9572032' => {
                                                                                              'recursive' => 0,
                                                                                              'count' => 1,
                                                                                              'value' => bless( {
                                                                                                                  '_name' => 'getElevation',
                                                                                                                  '_signature' => [
                                                                                                                                    'getElevation'
                                                                                                                                  ],
                                                                                                                  '_value' => [
                                                                                                                                \bless( {
                                                                                                                                           '_name' => undef,
                                                                                                                                           '_signature' => $VAR1->{'_context'}{'_serializer'}{'_signature'},
                                                                                                                                           '_value' => [
                                                                                                                                                         bless( {
                                                                                                                                                                  '_name' => 'X_Value',
                                                                                                                                                                  '_type' => 'string',
                                                                                                                                                                  '_signature' => [
                                                                                                                                                                                    'X_Valuestring'
                                                                                                                                                                                  ],
                                                                                                                                                                  '_value' => [
                                                                                                                                                                                '-90'
                                                                                                                                                                              ],
                                                                                                                                                                  '_attr' => {}
                                                                                                                                                                }, 'SOAP::Data' ),
                                                                                                                                                         $VAR1->{'_context'}{'_serializer'}{'_seen'}{'9556512'}{'value'},
                                                                                                                                                         bless( {
                                                                                                                                                                  '_name' => 'Source_Layer',
                                                                                                                                                                  '_type' => 'string',
                                                                                                                                                                  '_signature' => [
                                                                                                                                                                                    'Source_Layerstring'
                                                                                                                                                                                  ],
                                                                                                                                                                  '_value' => [
                                                                                                                                                                                'SRTM.C_US_1'
                                                                                                                                                                              ],
                                                                                                                                                                  '_attr' => {}
                                                                                                                                                                }, 'SOAP::Data' ),
                                                                                                                                                         bless( {
                                                                                                                                                                  '_name' => 'Elevation_Units',
                                                                                                                                                                  '_type' => 'string',
                                                                                                                                                                  '_signature' => [
                                                                                                                                                                                    'Elevation_Unitsstring'
                                                                                                                                                                                  ],
                                                                                                                                                                  '_value' => [
                                                                                                                                                                                'FEET'
                                                                                                                                                                              ],
                                                                                                                                                                  '_attr' => {}
                                                                                                                                                                }, 'SOAP::Data' ),
                                                                                                                                                         bless( {
                                                                                                                                                                  '_name' => 'Elevation_Only',
                                                                                                                                                                  '_type' => 'string',
                                                                                                                                                                  '_signature' => [
                                                                                                                                                                                    'Elevation_Onlystring'
                                                                                                                                                                                  ],
                                                                                                                                                                  '_value' => [
                                                                                                                                                                                'false'
                                                                                                                                                                              ],
                                                                                                                                                                  '_attr' => {}
                                                                                                                                                                }, 'SOAP::Data' )
                                                                                                                                                       ],
                                                                                                                                           '_attr' => {}
                                                                                                                                         }, 'SOAP::Data' )
                                                                                                                              ],
                                                                                                                  '_attr' => {
                                                                                                                               'xmlns' => 'http://gisdata.usgs.gov/XMLWebServices2/'
                                                                                                                             }
                                                                                                                }, 'SOAP::Data' ),
                                                                                              'multiref' => ''
                                                                                            },
                                                                               '2678224' => {
                                                                                              'recursive' => 0,
                                                                                              'count' => 1,
                                                                                              'value' => \$VAR1->{'_context'}{'_serializer'}{'_seen'}{'9572032'}{'value'},
                                                                                              'multiref' => ''
                                                                                            },
                                                                               '9572928' => {
                                                                                              'recursive' => 0,
                                                                                              'count' => 1,
                                                                                              'value' => ${$VAR1->{'_context'}{'_serializer'}{'_seen'}{'9572032'}{'value'}{'_value'}[0]}->{'_value'}->[0],
                                                                                              'multiref' => ''
                                                                                            },
                                                                               '2678256' => {
                                                                                              'recursive' => 0,
                                                                                              'count' => 1,
                                                                                              'value' => $VAR1->{'_context'}{'_serializer'}{'_seen'}{'9572032'}{'value'}{'_value'}[0],
                                                                                              'multiref' => ''
                                                                                            },
                                                                               '9556864' => {
                                                                                              'recursive' => 0,
                                                                                              'count' => 1,
                                                                                              'value' => ${$VAR1->{'_context'}{'_serializer'}{'_seen'}{'9572032'}{'value'}{'_value'}[0]}->{'_value'}->[3],
                                                                                              'multiref' => ''
                                                                                            },
                                                                               '9558128' => {
                                                                                              'recursive' => 0,
                                                                                              'count' => 1,
                                                                                              'value' => bless( {
                                                                                                                  '_name' => 'Envelope',
                                                                                                                  '_signature' => [
                                                                                                                                    'soap:Envelope'
                                                                                                                                  ],
                                                                                                                  '_value' => [
                                                                                                                                \bless( {
                                                                                                                                           '_name' => undef,
                                                                                                                                           '_signature' => [
                                                                                                                                                             'soap:Body'
                                                                                                                                                           ],
                                                                                                                                           '_value' => [
                                                                                                                                                         bless( {
                                                                                                                                                                  '_name' => 'Body',
                                                                                                                                                                  '_signature' => [
                                                                                                                                                                                    'soap:Body'
                                                                                                                                                                                  ],
                                                                                                                                                                  '_value' => [
                                                                                                                                                                                $VAR1->{'_context'}{'_serializer'}{'_seen'}{'2678224'}{'value'}
                                                                                                                                                                              ],
                                                                                                                                                                  '_prefix' => 'soap',
                                                                                                                                                                  '_attr' => {}
                                                                                                                                                                }, 'SOAP::Data' )
                                                                                                                                                       ],
                                                                                                                                           '_attr' => {}
                                                                                                                                         }, 'SOAP::Data' )
                                                                                                                              ],
                                                                                                                  '_prefix' => 'soap',
                                                                                                                  '_attr' => {
                                                                                                                               '{http://schemas.xmlsoap.org/soap/envelope/}encodingStyle' => 'http://schemas.xmlsoap.org/soap/encoding/'
                                                                                                                             }
                                                                                                                }, 'SOAP::Data' ),
                                                                                              'multiref' => ''
                                                                                            },
                                                                               '9557776' => {
                                                                                              'recursive' => 0,
                                                                                              'count' => 1,
                                                                                              'value' => ${$VAR1->{'_context'}{'_serializer'}{'_seen'}{'9558128'}{'value'}{'_value'}[0]}->{'_value'}->[0],
                                                                                              'multiref' => ''
                                                                                            },
                                                                               '9557952' => {
                                                                                              'recursive' => 0,
                                                                                              'count' => 1,
                                                                                              'value' => ${$VAR1->{'_context'}{'_serializer'}{'_seen'}{'9558128'}{'value'}{'_value'}[0]},
                                                                                              'multiref' => ''
                                                                                            },
                                                                               '9557040' => {
                                                                                              'recursive' => 0,
                                                                                              'count' => 1,
                                                                                              'value' => ${$VAR1->{'_context'}{'_serializer'}{'_seen'}{'9572032'}{'value'}{'_value'}[0]}->{'_value'}->[4],
                                                                                              'multiref' => ''
                                                                                            },
                                                                               '9557888' => {
                                                                                              'recursive' => 0,
                                                                                              'count' => 1,
                                                                                              'value' => $VAR1->{'_context'}{'_serializer'}{'_seen'}{'9558128'}{'value'}{'_value'}[0],
                                                                                              'multiref' => ''
                                                                                            },
                                                                               '9556688' => {
                                                                                              'recursive' => 0,
                                                                                              'count' => 1,
                                                                                              'value' => ${$VAR1->{'_context'}{'_serializer'}{'_seen'}{'9572032'}{'value'}{'_value'}[0]}->{'_value'}->[2],
                                                                                              'multiref' => ''
                                                                                            },
                                                                               '9557472' => {
                                                                                              'recursive' => 0,
                                                                                              'count' => 1,
                                                                                              'value' => ${$VAR1->{'_context'}{'_serializer'}{'_seen'}{'9572032'}{'value'}{'_value'}[0]},
                                                                                              'multiref' => ''
                                                                                            }
                                                                             },
                                                                  '_attr' => $VAR1->{'_context'}{'_serializer'}{'_seen'}{'9558128'}{'value'}{'_attr'},
                                                                  '_multirefinplace' => 0,
                                                                  '_on_nonserialized' => $VAR1->{'_context'}{'_on_nonserialized'},
                                                                  '_xmlschema' => 'http://www.w3.org/2001/XMLSchema',
                                                                  '_ns_prefix' => '',
                                                                  '_readable' => 0,
                                                                  '_ns_uri' => 'http://gisdata.usgs.gov/XMLWebServices2/',
                                                                  '_encoding' => 'UTF-8',
                                                                  '_autotype' => 1
                                                                }, 'SOAP::Serializer' ),
                                        '_schema' => undef,
                                        '_packager' => bless( {
                                                                '_env_location' => '/main_envelope',
                                                                '_content_encoding' => '8bit',
                                                                '_env_id' => '<main_envelope>',
                                                                '_parts' => [],
                                                                '_persist_parts' => 0,
                                                                '_parser' => undef
                                                              }, 'SOAP::Packager::MIME' ),
                                        '_on_action' => sub { "DUMMY" },
                                        '_on_fault' => sub { "DUMMY" }
                                      }, 'SOAP::Lite' ),
                 '_current' => [
                                 $VAR1->{'_content'}
                               ]
               }, 'SOAP::SOM' ));
}

1;
