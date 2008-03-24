#!/usr/bin/perl -w

# /t/000_load.t
#
# $Id$

use strict;
use warnings;

use Test::More tests => 9;

#use lib q[../lib];

BEGIN {
    use_ok(q[Net::BitTorrent]);
    use_ok(q[Net::BitTorrent::Session]);
    use_ok(q[Net::BitTorrent::Session::File]);
    use_ok(q[Net::BitTorrent::Session::Peer]);
    use_ok(q[Net::BitTorrent::Session::Peer::Request]);
    use_ok(q[Net::BitTorrent::Session::Piece]);
    use_ok(q[Net::BitTorrent::Session::Piece::Block]);
    use_ok(q[Net::BitTorrent::Session::Tracker]);
    use_ok(q[Net::BitTorrent::Util]);
}

1;
