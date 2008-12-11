use strict;
use warnings;

BEGIN {
    eval "use Test::Spelling";
    $@ and do {
	print "1..0 # skip Test::Spelling not available.\n";
	exit;
    };
}

add_stopwords (<DATA>);

all_pod_files_spelling_ok ();
__DATA__
CONUS
CUBITS
IDs
InvalidCastException
Survey's
USGS
WGS
WSDL
Wyant
geo
getAllElevations
getElevation
targetNamespace
url
