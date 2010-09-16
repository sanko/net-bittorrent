use strict;
use warnings;
use lib '../lib';
use Net::BitTorrent::Torrent::Generator;
our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 12; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
$|++;

#
my $torrent = Net::BitTorrent::Torrent::Generator->new(
                 files => '../t/90000_data/96000_data/96020_miniswarm_seed/');
$torrent->_set_comment('See credit.txt for attributions.');
print 'Saving ' . $torrent->info_hash->to_Hex . '...';
unlink 'test.torrent';
open my ($fh), '>', 'test.torrent' || die 'FAIL!';
syswrite $fh, $torrent->raw_data;
close $fh;

# $Id$
