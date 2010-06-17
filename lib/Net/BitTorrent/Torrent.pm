package Net::BitTorrent::Torrent;
{
    use Moose;
    use Moose::Util::TypeConstraints;
    our $MAJOR = 0.075; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use lib '../../../lib';
    use Net::BitTorrent::Types qw[:torrent];

    # Meat
    use lib '../../';
    has 'client' => (
        isa       => 'Maybe[Net::BitTorrent]',
        is        => 'rw',
        weak_ref  => 1,
        predicate => 'has_client',
        handles   => [qw[dht]],
        trigger   => sub {
            my ($self, $client) = @_;

            # XXX - make sure the new client knows who I am
            #$self->queue;
            warn 'TODO: Start trackers!';
            $self->start;    # ??? - Should this be automatic?
        }
    );
    has 'quests' => (is      => 'ro',
                     isa     => 'HashRef[ArrayRef]',
                     traits  => ['Hash'],
                     handles => {add_quest    => 'set',
                                 get_quest    => 'get',
                                 has_quest    => 'defined',
                                 delete_quest => 'delete',
                                 clear_quests => 'clear'
                     },
                     default => sub { {} }
    );
    has '_peers' => (
        is      => 'HashRef[Net::BitTorrent::Peer]',    # by creation id
        is      => 'ro',
        traits  => ['Hash'],
        handles => {
               peer        => 'get',
               add_peer    => 'set',
               del_peer    => 'delete',
               peer_ids    => 'keys',
               has_peer    => 'defined',
               peers       => 'values',
               clear_peers => 'clear',                     # removes all peers
               count_peers => 'count',
               no_peers    => 'is_empty'
        },
        default => sub { {} }
    );
    around [qw[peer add_peer del_peer has_peer]] => sub {
      my ($code, $self, $arg) = @_;
      blessed $arg ? $code->( $self, $arg->_id, $arg ) : $code->($self, $arg)
    };

    has 'error' => (is       => 'rw',
                    isa      => 'Str',
                    init_arg => undef
    );
    has 'storage' => (is         => 'ro',
                      required   => 1,
                      isa        => 'Net::BitTorrent::Storage',
                      lazy_build => 1,
                      builder    => '_build_storage',
                      handles    => {
                                  size => 'size',
                                  read => 'read'
                      }
    );

    sub _build_storage {
        require Net::BitTorrent::Storage;
        Net::BitTorrent::Storage->new(torrent => $_[0]);
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
                         $self->tracker->announce('start', sub {...}));
        $self->add_quest('dht_get_peers',
                         $self->dht->get_peers($self->info_hash, sub {...}));
        $self->add_quest('dht_announce_peer',
                         $self->dht->announce_peer($self->info_hash, sub {...}
                         )
        );
        $self->add_quest('new_peer', AE::timer(0, 3, sub {
            return if ! $self;
            $self->new_peer();
        }));
    }

    sub stop {
        my ($self) = @_;
        $self->clear_quests;
        $self->clear_peers;
    }

    #
    sub new_peer {
        my ($self, $peer) = @_;
        if ($peer) {...}
        else {
            my ($source) = [
                [$self->get_quest('dht_get_peers'), 'dht'],
                [$self->get_quest('tracker_announce'), 'tracker']
            ]->[int rand 2];
            use Data::Dump;
            ddx $source;
            return if !@{$source->[0][2]};
            my $addr = $source->[0][2]->[int rand @{$source->[0][2]}];
            require Net::BitTorrent::Peer;
            $peer = Net::BitTorrent::Peer->new(torrent => $self, connect => $addr, source => $source->[1]);
        }
        use Data::Dump;
        ddx $peer;
        $self->add_peer($peer);
    }

    # Quick methods
    my $pieces_per_hashcheck = 10;    # Max block of pieces in single call

    sub hashcheck {    # Range is split up into $pieces_per_hashcheck blocks
        my ($self, $range) = @_;
        $range
            = defined $range
            ? ref $range
                ? $range
                : [$range]
            : [0 .. $self->piece_count - 1];
        if (scalar @$range <= $pieces_per_hashcheck) {
            $self->_clear_have();
            for my $index (@$range) {
                my $piece = $self->read($index);
                next if !$piece || !$$piece;
                require Digest::SHA;
                $self->have->Bit_On($index)
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
                $self->hashcheck(@this_range);
                push @watchers,
                    AE::idle(@ranges ? $coderef : sub { $cv->end });
            };
            push @watchers, AE::idle($coderef);
            $cv->recv;
            shift @watchers;
        }
        return 1;
    }

    #
    with 'Net::BitTorrent::Protocol::BEP03::Metadata';
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
