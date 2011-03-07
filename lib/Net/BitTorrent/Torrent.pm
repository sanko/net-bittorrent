package Net::BitTorrent::Torrent;
{
    use 5.010;
    use Moose;
    use Moose::Util::TypeConstraints;
    use AnyEvent;
    our $MAJOR = 0; our $MINOR = 74; our $DEV = 14; our $VERSION = sprintf('%0d.%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use lib '../../../lib';

    #use Net::BitTorrent::Protocol::BEP03::Types qw[:bencode :torrent];
    extends 'Net::BitTorrent::Protocol::BEP03::Metadata';

    #with 'Net::BitTorrent::Protocol::BEP12::Metadata'; # Private
    #use File::Spec::Functions qw[rel2abs];
    around BUILDARGS => sub { # Convert new('path.torrent') to => new( {...} )
        my $orig  = shift;
        my $class = shift;
        my $args
            = (@_ >= 1 && !(@_ % 2))
            ? {@_}
            : @_ == 1 ? ref $_[0]
                ? shift
                : {path => shift}
            : ();
        if ($args->{'path'} && -f $args->{'path'}) {
            open(my ($fh), '<', $args->{'path'}) || last;
            sysread $fh, my $data, -s $args->{'path'};
            require Net::BitTorrent::Protocol::BEP03::Bencode;
            my $torrent
                = Net::BitTorrent::Protocol::BEP03::Bencode::bdecode($data);
            $args = {%$args,
                     _prepared_metadata => $torrent,
                     %$torrent,
                     %{$torrent->{'info'}},
                     files => ($torrent->{'info'}{'files'} // [
                                    {path   => [$torrent->{'info'}{'name'}],
                                     length => $torrent->{'info'}{'length'}
                                    }
                               ]
                     ),
                     piece_length => $torrent->{'info'}{'piece length'}
            };
        }
        return $class->$orig($args);
    };

    #
    has '+key' => (    # BEP03::Storage
                    is       => 'ro',
                    lazy     => 1,
                    required => 1,
                    builder  => '_build_key'
    );
    sub _build_key { substr shift->info_hash->to_Hex, 0, 8 }
    for my $direction (qw[up down]) {
        has $direction
            . 'loaded' => (
                         is      => 'ro',
                         isa     => 'Int',
                         traits  => ['Counter'],
                         handles => {'inc_' . $direction . 'loaded' => 'inc'},
                         default => 0
            );
    }

    sub left {
        my ($self) = @_;
        require List::Util;
        return $self->piece_length
            * List::Util::sum(
                            split('', unpack('b*', ($self->wanted() || ''))));
    }

    #
    has client => (
        isa        => 'Maybe[Net::BitTorrent]',
        is         => 'ro',
        weak_ref   => 1,
        lazy_build => 1,
        writer     => '_set_client',
        predicate  => '_has_client',
        handles    => {

            #dht                     => 'dht',
            trigger_piece_hash_pass => 'trigger_piece_hash_pass',
            trigger_piece_hash_fail => 'trigger_piece_hash_fail',
            peers                   => sub {
                my $s = shift;
                return if !$s->has_client;
                return grep {
                    $_->has_torrent
                        && !$_->torrent->info_hash->Compare($s->info_hash)
                } $s->client->peers;
            },
            _count_peers => sub {
                my $s = shift;
                return if !$s->has_client;
                return scalar $s->peers;
                }
        },
        trigger => sub {
            my ($self, $client) = @_;

            # XXX - make sure the new client knows who I am
            #$self->queue;
            $self->start;    # ??? - Should this be automatic?
        }
    );

    sub _build_client {
        my $s = shift;
        require Net::BitTorrent;
        my $client = Net::BitTorrent->new();
        $client->add_torrent($s);
        $client;
    }

    #
    has quests => (is      => 'ro',
                   isa     => 'HashRef[Defined]',
                   traits  => ['Hash'],
                   handles => {add_quest    => 'set',
                               get_quest    => 'get',
                               has_quest    => 'defined',
                               _del_quest   => 'delete',
                               clear_quests => 'clear'
                   },
                   default => sub { {} }
    );

    # Actions
    sub start {
        my ($self) = @_;
        return if !$self->client;
        require Scalar::Util;
        Scalar::Util::weaken $self;

=todo
        $self->add_quest('tracker_announce',
                         $self->tracker->announce(
                                   'start',
                                   sub { $self->_dht_tracker_announce_cb(@_) }
                         )
        );
        $self->add_quest('dht_get_peers',
                         $self->dht->get_peers(
                                          $self->info_hash,
                                          sub { $self->_dht_get_peers_cb(@_) }
                         )
        );
        $self->add_quest('dht_announce_peer',
                         $self->dht->announce_peer(
                                      $self->info_hash,
                                      sub { $self->_dht_announce_peer_cb(@_) }
                         )
        );
        $self->add_quest(
            'new_peer_dht',
            AE::timer(
                3, 30,
                sub {
                    $self // return;
                    return if !$self->has_client;
                    return if $self->seed;
                    return if $self->_count_peers >= $self->max_peers;
                    my ($source) = $self->get_quest('dht_get_peers');
                    return if !@{$source->[2]};
                    require Net::BitTorrent::Protocol::BEP03::Peer::Outgoing;
                    my $x = 1;

                    for ($self->_count_peers .. $self->max_peers) {
                        $self->client->add_peer(
                              Net::BitTorrent::Protocol::BEP03::Peer::Outgoing
                                  ->new(
                                     torrent => $self,
                                     connect =>
                                         $source->[2]->[rand @{$source->[2]}],
                                     source => 'dht',
                                     client => $self->client
                                  )
                        );
                        last if $x == 3;
                    }
                }
            )
        );
=cut
        $self->add_quest(
            'new_peer_trackers',
            AE::timer(
                3, 30,
                sub {
                    $self // return;
                    return if !$self->has_client;
                    return if $self->seed;
                    return if $self->_count_peers >= $self->max_peers;
                    my ($source) = $self->get_quest('tracker_announce');
                    return if !@{$source->[2]};
                    require Net::BitTorrent::Protocol::BEP03::Peer::Outgoing;
                    my $x = 1;

                    for ($self->_count_peers .. $self->max_peers) {
                        $self->client->add_peer(
                              Net::BitTorrent::Protocol::BEP03::Peer::Outgoing
                                  ->new(
                                     torrent => $self,
                                     connect =>
                                         $source->[2]->[rand @{$source->[2]}],
                                     source => 'tracker',
                                     client => $self->client
                                  )
                        );
                        last if $x == 3;
                    }
                }
            )
        );
        $self->add_quest(
            'unchoke',
            AE::timer(
                15, 10,
                sub {
                    return if !$self;
                    return if !$self->has_client;
                    my @choked = sort {
                               $a->remote_choked <=> $b->remote_choked
                            || $a->total_download <=> $b->total_download
                        } grep { $_->remote_interested && $_->choked }
                        $self->peers;
                    return if !@choked;
                    for my $i (0 .. $self->max_upload_slots) {
                        last if !$choked[$i];
                        $choked[$i]->_unset_choked;
                    }
                }
            )
        );
        $self->add_quest(
            'optimistic_unchoke',
            AE::timer(
                120, 120,
                sub {
                    return if !$self;
                    return if !$self->has_client;
                    return if !scalar $self->peers;
                    my @choked = sort {
                        ($a->remote_choked <=> $b->remote_choked)
                            || (($a->total_download || 0)
                                <=> ($b->total_download || 0))
                        } grep {
                        $_->choked
                            && sub {
                            my $x = $self->have->Clone;
                            $x->Not($_->pieces);
                            $x->Norm;
                            }
                            ->()
                        } $self->peers;
                    for my $i (0 .. $self->max_upload_slots) {
                        last if !$choked[$i];
                        $choked[$i]->_unset_choked;
                    }
                }
            )
        );
        $self->add_quest(
            'choke',    # Cycle them until we get some good peers
            AE::timer(
                60, 60,
                sub {
                    return if !$self;
                    return if !$self->has_client;
                    return if !scalar $self->peers;
                    my @unchoked = grep { !$_->choked } $self->peers;

                    # XXX - choke the slow peers first?
                }
            )
        );
    }

    sub stop {
        my ($self) = @_;
        $self->clear_quests;

        #$self->clear_peers( );
    }

    #
    no Moose;
    no Moose::Util::TypeConstraints;
    __PACKAGE__->meta->make_immutable;
}
1;
__END__
package main;
use strictures;
use warnings;
use Data::Dump;
$|++;
my $torrent = Net::BitTorrent::Torrent->new(
    #'C:\Users\ProperNoun\Desktop\net-bittorrent-protocol-bep03\~dev\ws_thestones_thenights.wmv.torrent'
        path => 'D:\Storage\complete\dot-torrents\My.Darling.Is.a.Foreigner.2010.DVDRip.XviD.AC3-BAUM.torrent',
        root => 'K:\Storage\incomplete\data'
);
#die $torrent->root;
#ddx $torrent;
warn $torrent->key;
warn $torrent->info_hash->to_Hex;
ddx $torrent->count_pieces;
$torrent->hash_check();

die $torrent->bitfield->to_Bin;
