# -*- perl -*-

# t/100_data_parsing/112_compact_bad.t - more N::B::Util::compact() tests
# $Id$

use strict;
use warnings;

use Test::More tests => 8;

use lib q[../../lib];

BEGIN { use_ok( q[Net::BitTorrent::Util], qw[:compact] ) }


is( compact(qw[127.0.0.1:000065]),
    qq[\x7F\0\0\1\0A], q[port with leading zeros] );

compact(qw[270.0.0.1:0]);
is( $@, q[Invalid IP address: 270.0.0.1:0], $@ );

compact(qw[127.0.0.1:5000000]);
ok( $@, $@ );

compact(qw[127.0.0.1:500:30]);
ok( $@, $@ );

compact(qw[127.0.0.1.3:50030]);
ok( $@, $@ );

is( compact(qw[127.0.0.1:3265 255.25.21.32:0:4554845]),
    qq[\x7F\0\0\1\f\xC1], q[invalid peer in list] );
ok( $@, $@ );

# TODO
#   IPv6 passing itself off as IPv4
