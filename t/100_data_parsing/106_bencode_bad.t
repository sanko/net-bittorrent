# -*- perl -*-

# t/100_data_parsing/106_bencode_bad.t - more N::B::Util::bencode() tests

use strict;
use warnings;
use Test::More tests => 2;

use lib q[../../lib/];

BEGIN { use_ok( q[Net::BitTorrent::Util], qw[bencode] ); }

bencode(
    { sub => sub { return my $brain; }
    }
);

ok $@, q[invalid format (coderef)];

# TODO
