use 5.008;	# because SOAP::Lite needs this

use strict;
use warnings;

use lib qw{ inc };

use ExtUtils::MakeMaker;
use Geo::WebService::Elevation::USGS::Meta;

(my $mmv = ExtUtils::MakeMaker->VERSION) =~ s/_//g;

my $meta = Geo::WebService::Elevation::USGS::Meta->new();

my %args = (
    NAME => 'Geo::WebService::Elevation::USGS',
    VERSION_FROM => 'lib/Geo/WebService/Elevation/USGS.pm',
    PREREQ_PM => $meta->requires(),
    PL_FILES => {},	# Prevent old MakeMaker from running Build.PL
    'dist' => {COMPRESS => 'gzip', SUFFIX => 'gz'},
    AUTHOR => 'Tom Wyant (wyant@cpan.org)',
    ABSTRACT => 'Get elevation data from the USGS',
);

$mmv >= 6.31 and $args{LICENSE} = 'perl';

$mmv >= 6.4501 and $args{META_MERGE} = {
    build_requires => $meta->build_requires(),
    dynamic_config => 1,
    resources => {
	bugtracker => 'https://rt.cpan.org/Public/Dist/Display.html?Name=Geo-WebService-Elevation-USGS',
	license => 'http://dev.perl.org/licenses/',
    },
};

$mmv >= 6.4701 and $args{MIN_PERL_VERSION} = $meta->requires_perl();

WriteMakefile (%args);
