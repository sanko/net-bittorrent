# -*- perl -*-

# t/100_data_parsing/111_compact.t - test N::B::Util::compact()
# $Id$

use strict;
use warnings;

use Test::More tests => 7;

use lib q[../../lib];

BEGIN { use_ok( q[Net::BitTorrent::Util], qw[compact] ) }

is( compact(qw[127.0.0.1:98]), qq[\x7F\0\0\1\0b], q[localhost] );

is( compact(qw[127.0.0.1:0]),
    qq[\x7F\0\0\1\0\0], q[port number of zero] );

is( compact(qw[127.0.0.1:5000]),
    qq[\x7F\0\0\1\23\x88], q[large port number] );

is( compact(qw[127.0.0.1:65535]),
    qq[\x7F\0\0\1\xFF\xFF], q[large port number] );

is( compact(qw[127.0.0.1:3265 255.25.21.32:0]),
    qq[\x7F\0\0\1\f\xC1\xFF\31\25 \0\0],
    q[short list of peers] );

is( compact(qw[127.0.0.1:3265 127.0.0.1:3265]),
    qq[\x7F\0\0\1\f\xC1], q[filter duplicates] );

# TODO
