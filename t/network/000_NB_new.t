#!/usr/bin/perl -w

# /t/000_load.t
#
# $Id$

use strict;
use warnings;

use Test::More tests => 2;

use lib q[../../lib];

BEGIN {
    use_ok(q[Net::BitTorrent]);
}

my $client;

ok($client = Net::BitTorrent->new(), q[Net::BitTorrent->new()]);

1;