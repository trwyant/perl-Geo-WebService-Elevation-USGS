package main;

use strict;
use warnings;

BEGIN {
    eval {
	require Test::Spelling;
	Test::Spelling->import();
    };
    $@ and do {
	print "1..0 # skip Test::Spelling not available.\n";
	exit;
    };
}

add_stopwords (<DATA>);

all_pod_files_spelling_ok ();

1;
__DATA__
CONUS
CUBITS
Dru
GEOID
Geoid
IDs
InvalidCastException
Milbert
NAD
NAVD
NED
Survey's
USGS
WGS
WSDL
Wyant
geo
getAllElevations
getElevation
targetNamespace
conterminous
geoid
gory
url
