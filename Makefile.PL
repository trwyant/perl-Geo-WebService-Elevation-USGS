use 5.008;	# because SOAP::Lite needs this

use strict;
use warnings;

use ExtUtils::MakeMaker;

(my $mmv = ExtUtils::MakeMaker->VERSION) =~ s/_//g;

my %args = (
    NAME => 'Geo::WebService::Elevation::USGS',
    VERSION_FROM => 'lib/Geo/WebService/Elevation/USGS.pm',
    PREREQ_PM => {
	'Params::Util'	=> 0.11,
	'Scalar::Util'	=> 1.10,
	'SOAP::Lite'	=> 0,
    },
    PL_FILES => {},	# Prevent old MakeMaker from running Build.PL
    'dist' => {COMPRESS => 'gzip', SUFFIX => 'gz'},
    AUTHOR => 'Tom Wyant (wyant@cpan.org)',
    ABSTRACT => 'Get elevation data from the USGS',
);

$mmv >= 6.31 and $args{LICENSE} = 'perl';

$mmv >= 6.4501 and $args{META_MERGE} = {
    build_requires => {
	'Test::More' => 0.40,
    },
    resources => {
	license => 'http://dev.perl.org/licenses/',
    },
};

WriteMakefile (%args);

