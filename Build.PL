use 5.008;

use strict;
use warnings;

use lib qw{ inc };

use Geo::WebService::Elevation::USGS::Build;
use Geo::WebService::Elevation::USGS::Meta;
use Module::Build;

(my $mbv = Module::Build->VERSION) =~ s/_//g;

my $meta = Geo::WebService::Elevation::USGS::Meta->new();

my %args = (
    dist_author => 'Tom Wyant (wyant at cpan dot org)',
    dist_abstract => 'Get elevation data from the USGS',
    module_name => 'Geo::WebService::Elevation::USGS',
    build_requires => {
	'Test::More' => 0.40,
    },
    requires => $meta->requires(
	perl	=> $meta->requires_perl(),
    ),
    add_to_cleanup => [ qw{ xt/optionals } ],
    dynamic_config => 1,
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

# Don't require Module::Build if we're making a distribution, since the
# user can use ExtUtils::MakeMaker.
$mbv >= 0.34
    and $args{auto_configure_requires} = !$meta->distribution();

my $bldr = Geo::WebService::Elevation::USGS::Build->new (%args);

$bldr->create_build_script ();
