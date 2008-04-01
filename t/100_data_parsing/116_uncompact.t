# -*- perl -*-

# t/100_data_parsing/116_uncompact.t - test N::B::Util::uncompact()
# $Id$

use strict;
use warnings;

use Test::More tests => 5;

use lib q[../../lib/];

BEGIN { use_ok( q[Net::BitTorrent::Util], qw[:compact] ) }



#is( uncompact(qw[127.0.0.1:98]), qq[\x7F\0\0\1\0b], q[localhost] );

#is( uncompact(qw[127.0.0.1:0]),
#    qq[\x7F\0\0\1\0\0], q[port number of zero] );

#is( uncompact(qw[127.0.0.1:5000]),
#    qq[\x7F\0\0\1\23\x88], q[large port number] );

is( uncompact(qq[\x7F\0\0\1\xFF\xFF]),
    qw[127.0.0.1:65535], q[large port number] );

my @peers = sort( uncompact(qq[\x7F\0\0\1\f\xC1\xFF\31\25 \0\0]) );

is @peers, 2, q[short list of peers A];
is $peers[0], q[127.0.0.1:3265], q[short list of peers C];
is $peers[1], q[255.25.21.32:0], q[short list of peers B];
