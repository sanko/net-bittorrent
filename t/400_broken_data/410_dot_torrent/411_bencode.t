# -*- perl -*-

# t/400_broken_data/410_dot_torrent/411_bencode.t
# $Id$

use strict;
use warnings;

use Test::More tests => 1;

use lib q[../../../lib/];

BEGIN { use_ok(q[Net::BitTorrent]) }

