use strict;
use warnings;
use lib '../../lib';
use Test::More;
use File::Temp;
use Net::BitTorrent::Network::Utility qw[server];
use AnyEvent::Handle;
use AnyEvent;
use Net::BitTorrent;
use lib reverse 'lib', '../lib', '../../lib';
use lib reverse 't/80000_utility', '../t/80000_utility', '../../t/80000_utility';
use Net::BitTorrent::Tracker::HTTP;    # Utility
$|++;
my (@cli, @dir);

#
my $condvar = AnyEvent->condvar;
my $timeout = AnyEvent->timer(
    60, 0,
    sub {
        note 'Timeout!';
        @cli = ();    # Make sure the temp files can be removed at exit
        $condvar->broadcast;
    }
);

#
my $tracker = Net::BitTorrent::Tracker::HTTP->new(host => '127.0.0.1');
chdir '../..' if !-f 't/90000_data/95000_torrents/95003_miniswarm.torrent';
for my $seed (qw[1 0]) {
    push @cli, Net::BitTorrent->new(
        on_peer_id => sub {
            my ($s, $a) = @_;
            note $a->{'message'};
        },
        on_peer_packet => sub {
            my ($s, $a) = @_;
            note $a->{'message'};
        },
        on_peer_connect => sub {
            my ($s, $a) = @_;
            note 'Connect:    ' . $a->{'message'};
        },
        on_peer_disconnect => sub {
            my ($s, $a) = @_;
            note 'Disconnect: ' . $a->{'message'};
        },
        on_peer_packet_in => sub {
            my ($s, $a) = @_;
            note explain $a->{'message'}, $a->{'packet'};
        },
        on_piece_hash_pass =>  sub {   },
        on_piece_hash_fail =>  sub {   },
        on_peer_bitfield => sub {
            my ($s, $a) = @_;
            note 'Bitfield packet: ' . $a->{'message'};
    }
    );
    $cli[-1]->add_torrent(
                path => 't\90000_data\95000_torrents\95003_miniswarm.torrent')
        ->tracker->add_tier(
                   [sprintf 'http://%s:%d/announce.pl?%d^', $tracker->host,
                    $tracker->port,                         int rand time
                   ]
        );
        if ($seed) {
            $cli[-1]->torrent(0)->storage->_set_root('t\90000_data\96000_data\96020_miniswarm_seed');
        }
        else {
            push @dir, File::Temp->newdir( 'NBminiswarm_peer_'. scalar(@cli) .'_XXXX', TMPDIR=> 1, CLEANUP => 1 );
            $cli[-1]->torrent(0)->storage->_set_root($dir[-1]->dirname);


        }
        $cli[-1]->torrent(0)->hash_check;
        is $cli[-1]->torrent(0)->have->to_Enum, $seed ?  '0,1' : '',
        sprintf '[%02d] hashcheck %s', scalar(@cli),
        ($seed ?  'seed' : 'peer');
        #warn $cli[-1]->torrent(0)->have->to_Enum;
        #warn $cli[-1]->torrent(0)->wanted->to_Enum;
}
$tracker->wait;
