# -*- perl -*-

# t/100_data_parsing/117_uncompact_bad.t - more N::B::Util::uncompact() tests
# $Id$

use strict;
use warnings;

use Test::More tests => 1;

use lib q[../../lib/];

BEGIN{use_ok(q[Net::BitTorrent::Util], qw[:compact])}

