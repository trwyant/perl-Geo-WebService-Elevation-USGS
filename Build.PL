use 5.008;

use strict;
use warnings;

use lib qw{ inc };

use Geo::WebService::Elevation::USGS::Build;
use Module::Build;

(my $mbv = Module::Build->VERSION) =~ s/_//g;

my %args = (
    dist_author => 'Tom Wyant (wyant at cpan dot org)',
    dist_abstract => 'Get elevation data from the USGS',
    module_name => 'Geo::WebService::Elevation::USGS',
    build_requires => {
	'Test::More' => 0.40,
    },
    requires => {
	'Params::Util' => 0.11,
	'Scalar::Util' => 1.10,
	'SOAP::Lite' => 0,
    },
    add_to_cleanup => [ qw{ xt/optionals } ],
    license => 'perl',
);

$mbv >= 0.28 and $args{meta_merge} = {
    no_index => {
	directory => [qw{inc t}],
    },
    resources => {
	bugtracker => 'https://rt.cpan.org/Public/Dist/Display.html?Name=Geo-WebService-Elevation-USGS',
    },
};

$mbv >= 0.34
    and $args{auto_configure_requires} = 0;	# Don't require Module::Build

my $bldr = Geo::WebService::Elevation::USGS::Build->new (%args);

$bldr->create_build_script ();
