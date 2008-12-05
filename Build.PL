use 5.008;

use strict;
use warnings;

use Module::Build;

my $bldr = Module::Build->new (
    dist_author => 'Tom Wyant (wyant at cpan dot org)',
    dist_abstract => 'Get elevation data from the USGS',
    module_name => 'Geo::WebService::Elevation::USGS',
    build_requires => {
	'Test::More' => 0,
    },
    requires => {
	'SOAP::Lite' => 0,
    },
    license => 'perl',
    meta_merge => {
	no_index => {
	    directory => [qw{inc t}],
	},
    },
);

$bldr->create_build_script ();
