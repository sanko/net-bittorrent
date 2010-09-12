package Net::BitTorrent::Torrent;
{
    use Moose;
    use Moose::Util::TypeConstraints;
    extends 'Net::BitTorrent::Protocol::BEP03::Metadata';
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use lib '../../../lib';
    use Net::BitTorrent::Types qw[:torrent];
    use Fcntl ':flock';
    use 5.010.000;
    sub BUILD {1}
    has 'path' => (is          => 'ro',
                   isa         => 'Str',
                   required    => 1,
                   predicate   => '_has_path',
                   initializer => '_initializer_path'
    );

    sub _initializer_path {
        my ($s, $p, $set, $attr) = @_;
        $set->($p);
        open(my ($FH), '<', $p)
            || return !($_[0] = undef);    # exterminate! exterminate!
        flock $FH, LOCK_SH;
        sysread($FH, my ($METADATA), -s $FH) == -s $FH
            || return !($_[0] = undef);    # destroy!
        $s->_set_raw_data($METADATA);
        return close $FH;
    }
    has 'client' => (
        isa       => 'Maybe[Net::BitTorrent]',
        is        => 'rw',
        weak_ref  => 1,
        predicate => '_has_client',
        handles   => {
            dht                     => 'dht',
            trigger_piece_hash_pass => 'trigger_piece_hash_pass',
            trigger_piece_hash_fail => 'trigger_piece_hash_fail',
            peers                   => sub {
                my $s = shift;
                return if !$s->_has_client;
                return grep {
                    $_->_has_torrent
                        && !$_->torrent->info_hash->Compare($s->info_hash)
                } $s->client->peers;
                }
        },
        trigger => sub {
            my ($self, $client) = @_;

            # XXX - make sure the new client knows who I am
            #$self->queue;
            $self->start;    # ??? - Should this be automatic?
        }
    );
    has 'quests' => (is      => 'ro',
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
    has 'error' => (is       => 'rw',
                    isa      => 'Str',
                    init_arg => undef
    );
    has 'storage' => (is         => 'ro',
                      required   => 1,
                      isa        => 'Net::BitTorrent::Storage',
                      lazy_build => 1,
                      builder    => '_build_storage',
                      handles    => [qw[size read write wanted is_seed]]
    );

    sub _build_storage {
        require Net::BitTorrent::Storage;
        Net::BitTorrent::Storage->new(torrent => $_[0]);
    }
    override '_trigger_metadata' => sub {
        super;
        my ($self, $new_value, $old_value) = @_;
        if (@_ == 2) {    # parse files
            require Net::BitTorrent::Storage::File;

            #
            my @files;
            if (defined $new_value->{'info'}{'files'}) { # Multi-file .torrent
                my ($offset, $index) = (0, 0);
                $self->storage->_set_files(
                    [   map {
                            my $obj =
                                Net::BitTorrent::Storage::File->new(
                                          index  => $index++,
                                          length => $_->{'length'},
                                          offset => $offset,
                                          path => [grep {$_} @{$_->{'path'}}],
                                          storage => $self->storage
                                );
                            $offset += $_->{'length'};
                            $obj;
                            } @{$new_value->{'info'}{'files'}}
                    ]
                );
                $self->storage->_set_root($new_value->{'info'}{'name'});
            }
            else {    # single file torrent; use the name
                $self->storage->_set_files(
                             [Net::BitTorrent::Storage::File->new(
                                     index  => 0,
                                     length => $new_value->{'info'}{'length'},
                                     offset => 0,
                                     path   => [$new_value->{'info'}{'name'}],
                                     storage => $self->storage
                              )
                             ]
                );
            }
        }
    };
    has 'piece_selector' => (isa => 'Net::BitTorrent::Torrent::PieceSelector',
                             is  => 'ro',
                             builder => '_build_piece_selector',
                             handles => [qw[select_piece select_block]]
    );

    sub _build_piece_selector {
        require Net::BitTorrent::Torrent::PieceSelector;
        Net::BitTorrent::Torrent::PieceSelector->new(torrent => shift);
    }
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

    # Actions
    sub start {
        my ($self) = @_;
        return if !$self->client;
        require Scalar::Util;
        Scalar::Util::weaken $self;
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
            'new_peer',
            AE::timer(
                0, 30,
                sub {
                    return if !$self;
                    return if !$self->_has_client;
                    return if scalar($self->peers) >= $self->max_peers;
                    my ($source)
                        = [[$self->get_quest('dht_get_peers'),    'dht'],
                           [$self->get_quest('tracker_announce'), 'tracker']
                        ]->[int rand 2];
                    return if !@{$source->[0][2]};
                    my $addr = $source->[0][2]->[int rand @{$source->[0][2]}];
                    require Net::BitTorrent::Protocol::BEP03::Peer::Outgoing;
                    $self->client->add_peer(
                        Net::BitTorrent::Protocol::BEP03::Peer::Outgoing->new(
                                                       torrent => $self,
                                                       connect => $addr,
                                                       source => $source->[1],
                                                       client => $self->client
                        )
                    );
                }
            )
        );
        $self->add_quest(
            'unchoke',
            AE::timer(
                15, 10,
                sub {
                    return if !$self;
                    return if !$self->_has_client;
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
                    return if !$self->_has_client;
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
                    return if !$self->_has_client;
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
    sub _tracker_announce_cb  {1}
    sub _dht_announce_peer_cb {1}
    sub _dht_get_peers_cb     {1}

    # Quick methods
    my $pieces_per_hashcheck = 10;    # Max block of pieces in single call

    sub hash_check {    # Range is split up into $pieces_per_hashcheck blocks
                        # ??? - Disconnect peers if @$range > 1
        my ($self, $range) = @_;
        $range
            = defined $range
            ? ref $range
                ? $range
                : [$range]
            : [0 .. $self->piece_count - 1];
        if (scalar @$range <= $pieces_per_hashcheck) {
            $self->_clear_have() if !defined $_[0];  # retain current bitfield
            for my $index (@$range) {
                my $piece = $self->read($index);
                next if !$piece || !$$piece;
                require Digest::SHA;
                $self->_set_piece($index)
                    if Digest::SHA::sha1($$piece) eq
                        substr($self->pieces, ($index * 20), 20);
            }
        }
        else {
            my $cv = AnyEvent->condvar;
            $cv->begin;
            my (@watchers, @ranges, @this_range, $coderef);
            push @ranges, [splice(@$range, 0, $pieces_per_hashcheck, ())]
                while @$range;
            $coderef = sub {
                shift @watchers if @watchers;
                @this_range = shift @ranges;
                $self->hash_check(@this_range);
                push @watchers,
                    AE::idle(@ranges ? $coderef : sub { $cv->end });
            };
            push @watchers, AE::idle($coderef);
            $cv->recv;
            shift @watchers;
        }
        return 1;
    }
    has 'have' => (is         => 'ro',
                   isa        => 'NBTypes::Torrent::Bitfield',
                   lazy_build => 1,
                   coerce     => 1,
                   builder    => '_build_have',
                   init_arg   => undef,
                   writer     => '_have',
                   clearer    => '_clear_have',
                   handles    => {
                               _set_piece   => 'Bit_On',
                               _unset_piece => 'Bit_Off',
                               _has_piece   => 'bit_test',
                               seed         => 'is_full'
                   },
    );
    sub _build_have { '0' x $_[0]->piece_count }
    after '_set_piece' => sub {
        my ($s, $i) = @_;
        $s->trigger_piece_hash_pass($i);
        $_->_send_have($i) for $s->peers;
    };
    after '_unset_piece' => sub { $_[0]->trigger_piece_hash_fail($_[1]) };

    #{    ### Simple plugin system
    #    my @_plugins;
    #    sub _register_plugin {
    #        my $s = shift;
    #        return $s->meta->apply(@_) if blessed $s;
    #        my %seen = ();
    #        return @_plugins = grep { !$seen{$_}++ } @_plugins, @_;
    #    }
    #    after 'BUILD' => sub {
    #        return if !@_plugins;
    #        my ($s, $a) = @_;
    #        require Moose::Util;
    #        Moose::Util::apply_all_roles($s, @_plugins,
    #                                     {rebless_params => $a});
    #    };
    #}
    #
    has 'max_peers' => (isa     => subtype(as 'Int' => where { $_ >= 1 }),
                        is      => 'rw',
                        default => '200'
    );
    has 'max_upload_slots' => (isa => subtype(as 'Int' => where { $_ >= 1 }),
                               is  => 'rw',
                               default => '8'
    );

    #
    no Moose;
    __PACKAGE__->meta->make_immutable
}
1;

=pod

=head1 Author

Sanko Robinson <sanko@cpan.org> - http://sankorobinson.com/

CPAN ID: SANKO

=head1 License and Legal

Copyright (C) 2008-2010 by Sanko Robinson <sanko@cpan.org>

This program is free software; you can redistribute it and/or modify it under
the terms of
L<The Artistic License 2.0|http://www.perlfoundation.org/artistic_license_2_0>.
See the F<LICENSE> file included with this distribution or
L<notes on the Artistic License 2.0|http://www.perlfoundation.org/artistic_2_0_notes>
for clarification.

When separated from the distribution, all original POD documentation is
covered by the
L<Creative Commons Attribution-Share Alike 3.0 License|http://creativecommons.org/licenses/by-sa/3.0/us/legalcode>.
See the
L<clarification of the CCA-SA3.0|http://creativecommons.org/licenses/by-sa/3.0/us/>.

Neither this module nor the L<Author|/Author> is affiliated with BitTorrent,
Inc.

=for rcs $Id$

=cut
