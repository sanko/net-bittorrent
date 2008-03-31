# -*- perl -*-

# t/100_data_parsing/101_bdecode.t - test N::B::Util::bdecode()

use strict;
use warnings;

use Test::More tests => 23;

use lib q[../../lib/];

BEGIN { use_ok( q[Net::BitTorrent::Util], qw[:bencode] ); }

#tests taken from Convert::Bencode
my $test_string
    = q[d7:Integeri42e4:Listl6:item 1i2ei3ee6:String9:The Valuee];
my ($hashref) = bdecode($test_string);

ok defined $hashref, q[bdecode() returned something];
is ref $hashref, q[HASH], q[bdecode() returned a valid hash ref];
ok defined $hashref->{q[Integer]}, q[Integer key present];
is $hashref->{q[Integer]}, 42, q[  and its the correct value];
ok defined $hashref->{q[String]}, q[String key present];
is ${ $hashref; }{q[String]}, q[The Value],
    q[  and its the correct value];
ok defined $hashref->{q[List]}, q[List key present];
is @{ $hashref->{q[List]} }, 3, q[  list has 3 elements];
is ${ $hashref->{q[List]} }[0], q[item 1],
    q[    first element correct];
is ${ $hashref->{q[List]} }[1], 2, q[    second element correct];
is ${ $hashref->{q[List]} }[2], 3, q[    third element correct];

my ($encoded_string) = bencode($hashref);
ok defined $encoded_string, q[bencode() returned something];
is $encoded_string, $test_string,
    q[  and it appears to be the correct value];

is bdecode(q[i4e]), 4, q[integer];
is bdecode(q[i0e]), 0, q[zero];
is bdecode(q[i-0e]) + 0, 0, q[zero w/ sign];
is bdecode(q[i-10e]), -10,    q[negative integer];
is bdecode(q[0:]),    q[],    q[zero length string];
is bdecode(q[3:abc]), q[abc], q[string];
is bdecode(q[10:1234567890]), q[1234567890],
    q[integer cast as string];
is bdecode(q[02:xy]), q[xy], q[string with leading zero in length];
is bdecode(q[i03e]),  3,     q[integer with leading zero];
