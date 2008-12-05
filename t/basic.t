use strict;
use warnings;

use Test::More tests => 37;

my $module = 'Geo::WebService::Elevation::USGS';

require_ok($module)
    or BAIL_OUT ("Can not continue without loading $module");

my $ele = new_ok('Geo::WebService::Elevation::USGS')
    or BAIL_OUT ("Can not continue without instantiating $module");

is($ele->get('units'), 'FEET', 'Units default to feet');
$ele->set(units => 'METERS');
is($ele->get('units'), 'METERS', 'Units can be set to meters');
$ele->set(units => 'FEET');
is($ele->get('units'), 'FEET', 'Units can be set back to feet');
ok($ele->get('croak'), 'Croak defaults to true');
$ele->set(croak => undef);
ok(!$ele->get('croak'), 'Croak can be set false');
is($ele->get('proxy'),
    'http://gisdata.usgs.gov/xmlwebservices2/elevation_service.asmx',
    'Proxy is as expected');
ok(!defined($ele->get('places')), 'Places defaults to undefined');
$ele->set(places => 2);	# USGS returns insane precision
is($ele->get('places'), 2, 'Places can be set to 2');
my $rslt = eval {$ele->attributes()};
ok($rslt, 'attributes() returned something');
is(ref $rslt, 'HASH', 'attributes() returned a hash reference');
is($rslt->{places}, 2, 'attributes() returned places => 2');
$rslt->{places} = undef;
is($ele->get('places'), 2,
    'Manipulating attributes() return does not affect attributes');
eval {$ele->set(places => 'fubar')};
like($@, qr{^Attribute places must be an unsigned integer},
    'Setting places to a non-integer should blow up,');
is($ele->get('places'), 2, 'and not change the value of places');
eval {$ele->set(places => undef)};
ok(!$@, 'Setting places to undef should work');
is($ele->get('places'), undef, 'and yield undef for places');
$ele->set(places => 2);	# For subsequent testing
eval {$ele->set(use_all_limit => -1)};
ok(!$@, 'Setting use_all_limit negative should work');
is($ele->get('use_all_limit'), '-1', 'and yield -1 for use_all_limit.');
eval {$ele->set(use_all_limit => 0)};
ok(!$@, 'Setting use_all_limit zero should work');
is($ele->get('use_all_limit'), '0', 'and yield 0 for use_all_limit.');
eval {$ele->set(use_all_limit => +1)};
ok(!$@, 'Setting use_all_limit positive should work');
is($ele->get('use_all_limit'), '1', 'and yield 1 for use_all_limit.');
eval {$ele->set(use_all_limit => 'fubar')};
ok($@, 'Setting use_all_limit to a string should not work');
is($ele->get('use_all_limit'), '1', 'and leave use_all_limit unchanged.');
eval {$ele->set(use_all_limit => undef)};
ok($@, 'Setting use_all_limit to undef should not work');
is($ele->get('use_all_limit'), '1', 'and leave use_all_limit unchanged.');
eval {$ele->set(use_all_limit => 5)};	# for subsequent testing

{
    my %rslt = eval {$ele->attributes()};
    ok(scalar %rslt, 'attributes() returned something in list context');
    is($rslt{places}, 2, 'attributes() returned places => 2');
    my $bogus = eval {Geo::WebService::Elevation::USGS::new()};
    my $msg = $@ || '';
    ok(!$bogus, 'Function call to new() returned nothing');
    like($msg, qr{^No class name specified},
	'Function call to new() threw an error');
    $rslt = eval{$ele->get('fubar')};
    like($@, qr{^No such attribute as 'fubar'}, "Can't get attribute 'fubar'");
    $rslt = eval{$ele->set(fubar => 'baz')};
    like($@, qr{^No such attribute as 'fubar'}, "Can't set 'fubar' either");
    $ele->{_bogus} = 'really';
    $rslt = eval{$ele->get('_bogus')};
    like($@, qr{^No such attribute as '_bogus'},
	"Can't get attribute '_bogus'");
    $rslt = eval{$ele->set(_bogus => 'baz')};
    like($@, qr{^No such attribute as '_bogus'}, "Can't set '_bogus' either");
    %rslt = eval {$ele->attributes()};
    ok(!exists $rslt{_bogus},
	"'_bogus' should not appear in attributes() output");
}
