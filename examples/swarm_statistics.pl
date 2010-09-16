#!perl -I../lib -Ilib
use strict;
use warnings;
use AnyEvent;
use Net::BitTorrent;
use 5.010.000;
our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 12; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
$|++;
my %torrents;
{

    package Net::BitTorrent::Torrent::crypt_1;
    our $VERSION = '0.00000000000000000001';  # In other words, don't use this
    use Moose;
    extends 'Net::BitTorrent::Torrent';
    after 'start' => sub {
        my $s = shift;
        return if !$s;
        return if !$s->_has_client;
        $s->_del_quest('new_peer');
        $s->add_quest(
            'new_peer',
            AE::timer(
                30,
                15 * 60,
                sub {
                    for my $source ([$s->get_quest('dht_get_peers'), 'dht'],
                                    [$s->get_quest('tracker_announce'),
                                     'tracker'
                                    ]
                        )
                    {   next if !@{$source->[0][2]};
                        for my $addr (@{$source->[0][2]}) {
                            require Net::BitTorrent::Peer;
                            $s->client->add_peer(Net::BitTorrent::Peer->new(
                                                       torrent => $s,
                                                       connect => $addr,
                                                       source => $source->[1],
                                                       client => $s->client
                                                 )
                            );
                        }
                    }
                }
            )
        );
        }
}
my $client = Net::BitTorrent->new(
    peer_id            => 'X' x 20,
    port               => [1338 .. 1340, 0],
    on_peer_have       => \&gather_stats,
    on_peer_bitfield   => \&gather_stats,
    on_peer_disconnect => sub {
        warn $_[1]->{'message'};
    }
);

sub gather_stats {
    my (undef, $a) = @_;
    warn $a->{'message'};
    my $seed = $a->{'peer'}->torrent->piece_count;
    my $have = scalar grep {$_} split '', unpack 'b*', $a->{'peer'}->pieces;

    # Add/update this peer's seed status. This could go into a database.
    $torrents{$a->{'peer'}->torrent->info_hash->to_Hex}{$a->{'peer'}->peer_id}
        = $seed == $have ? 'seed' : 'leech';

    #$a->{'peer'}->disconnect('We are only gathering statistics');
}
for my $dot_torrent ('a.legal.torrent') {
    $client->add_torrent(Net::BitTorrent::Torrent::crypt_1->new(
                                                         path => $dot_torrent,
                                                         max_peers => 1000
                         )
    );
}
exit AnyEvent->condvar->wait;

=pod

=head1 NAME

swarm_statistics.pl - Very simple swarm watcher

=head1 Description

Don't use this. It's not finished.

=cut
