use strict;
use warnings;
use lib '../lib';
use Net::BitTorrent::Torrent::Generator;

#
my $torrent = Net::BitTorrent::Torrent::Generator->new(
                  path => '../t/90000_data/96000_data/96020_miniswarm_seed/');
$torrent->_set_comment('See credit.txt for attributions.');
print 'Saving ' . $torrent->info_hash->to_Hex . '...';
unlink 'test.torrent';
open my ($fh), '>', 'test.torrent' || die 'FAIL!';
syswrite $fh, $torrent->raw_data;
close $fh;

# $Id$
