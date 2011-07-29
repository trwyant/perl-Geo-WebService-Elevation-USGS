package main;

use 5.006002;

use strict;
use warnings;

BEGIN {
    eval {
	require Test::More;
	Test::More->VERSION( 0.40 );
	Test::More->import();
	1;
    } or do {
	print "1..0 # skip Test::More 0.40 required\\n";
	exit;
    }
}

BEGIN {
    eval {
	require SOAP::Lite;
	1;
    } or do {
	plan skip_all => 'SOAP::Lite not available';
	exit;
    };
}

our $TEST_TRANSPORT = 'SOAP';

do 't/elev.t';


1;

# ex: set textwidth=72 :
