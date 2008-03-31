# -*- perl -*-

# t/100_data_parsing/105_bencode.t - test N::B::Util::bencode()

use strict;
use warnings;

use Test::More tests => 19;

use lib q[../../lib/];

BEGIN { use_ok( q[Net::BitTorrent::Util], qw[bencode] ); }

is bencode(4),   q[i4e],   q[integer];
is bencode(0),   q[i0e],   q[zero];
is bencode(-0),  q[i0e],   q[zero w/ sign];
is bencode(-10), q[i-10e], q[negative integer];
is bencode(+10), q[i10e],  q[positive integer];
is bencode( time x 50 ), q[i] . ( time x 50 ) . q[e], q[large number];

is bencode(q[Perl]), q[4:Perl], q[string];
is bencode(q[]),     q[0:],     q[null string];
is bencode(q[0:0:]), q[4:0:0:],
    q[odd string (malformed bencoded int)];

is bencode( [ 1, 2, 3 ] ), q[li1ei2ei3ee], q[list (integers)];
is bencode( [ q[one], q[two], q[three] ] ), q[l3:one3:two5:threee],
    q[list (strings)];
is bencode( [ q[three], 1, 2, 3, q[one], q[two] ] ),
    q[l5:threei1ei2ei3e3:one3:twoe], q[list (mixed scalars)];
is bencode( [] ), q[le], q[empty list];
is bencode( [ [ q[Alice], q[Bob] ], [ 2, 3 ] ] ),
    q[ll5:Alice3:Bobeli2ei3eee], q[list^list];

is bencode( { date => { month => q[January], year => 2009 } } ), q[d4:dated5:month7:January4:yeari2009eee], q[dictionary];
is bencode( {} ), q[de], q[dictionary from empty hash];
is bencode( { age => 25, eyes => q[blue] } ),
    q[d3:agei25e4:eyes4:bluee], q[dictionary from anon hash];
is length bencode( { join( q[], map( chr($_), 0 .. 255 ) ) =>
                         join( q[], map( chr($_), 0 .. 255 ) )
                   }
    ),
    522, q[anon hash with long key/value pair];
