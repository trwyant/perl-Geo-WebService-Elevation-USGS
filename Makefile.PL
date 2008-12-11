use 5.008;	# because SOAP::Lite needs this

use strict;
use warnings;

use ExtUtils::MakeMaker;

(my $mmv = ExtUtils::MakeMaker->VERSION) =~ s/_//g;
WriteMakefile (
    NAME => 'Geo::WebService::Elevation::USGS',
    VERSION_FROM => 'lib/Geo/WebService/Elevation/USGS.pm',
    PREREQ_PM => {
	'SOAP::Lite'	=> 0,
    },
    PL_FILES => {},	# Prevent old MakeMaker from running Build.PL
    'dist' => {COMPRESS => 'gzip', SUFFIX => 'gz'},
    AUTHOR => 'Tom Wyant (wyant@cpan.org)',
    ABSTRACT => 'Get elevation data from the USGS',
    ($mmv >= 6.31 ?
	(LICENSE => 'perl') : ()),
    ($mmv >= 6.4501 ?
	(
	    META_MERGE => {
		build_requires => {
		    'Test::More' => 0.40,
		},
		resources => {
		    license => 'http://dev.perl.org/licenses/',
		},
	    }
	) : ())
);

