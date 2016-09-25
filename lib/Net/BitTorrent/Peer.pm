package Net::BitTorrent::Peer;
use Moose;
use strictures 2;
use namespace::clean;
#
use IO::Async::Stream;
use IO::Async::Timer::Periodic;
#
use File::Spec;
use File::Path;
use Scalar::Util qw[/weak/];
use Digest::SHA qw[sha1];
use Types::Standard qw[ArrayRef CodeRef Enum HashRef Int Ref Str Bool];
#
use Net::BitTorrent::Protocol qw[:all];
#
our $VERSION = "0.75";
#
sub DEMOLISH {
    my ($s, $global_destruction) = @_;

if ($s->torrent) {
                    for my $req ($s->local_requests) {
                        $s->torrent->working_pieces->{$req->{index}}{$req->{offset}}{peer} = ()
                    }  }
    $s->stream->close if $s->stream;
}

sub BUILD {
    my ($s, $args) = @_;
    $s->stream->configure(
        on_read => sub {
            my ($stream, $buffref, $eof) = @_;
            return $_[0] = () if $eof;
            #warn length $$buffref;
            #use Data::Dump;
            #ddx length $$buffref < 40 ?
            #    $$buffref
            #    : (substr($$buffref, 0, 40) . '...');
            return $s = undef if !length $$buffref;
            while (length $$buffref) {
                my $packet = parse_packet($buffref);
                last if !$packet;

                #                use Data::Printer;
                #                p $packet;
                if (defined $packet->{error}) {
                    #warn $packet->{error};
                    return 0 if !$packet->{fatal};
                    $stream->close;
                    $s = undef;
                    return 1;
                }
                elsif ($packet->{type} eq $KEEPALIVE) {

                    #'noop'
                }
                elsif ($packet->{type} == $HANDSHAKE) {
                    $s->_set_reserved($packet->{payload}[0]);
                    $s->_set_peerid($packet->{payload}[2]);

                    # XXX - Disconnect! We already did this step
                }
                elsif ($packet->{type} == $CHOKE) {
                    $s->_set_choking(1);

                    for my $req ($s->local_requests) {
                        $s->torrent->working_pieces->{$req->{index}}{$req->{offset}}{peer} =
                        $s->torrent->working_pieces->{$req->{index}}{$req->{offset}}{timer}
                        = ()
                    }

                $s->_set_local_requests(
                    []
                );

                    $s->request_pieces_timer->stop;
                }
                elsif ($packet->{type} == $UNCHOKE) {
                    $s->_set_choking(0);
                    $s->request_pieces_timer->start;
                }
                elsif ($packet->{type} == $INTERESTED) {
                    $s->_set_interested(1);
                }
                elsif ($packet->{type} == $NOT_INTERESTED) {
                    $s->_set_interested(0);
                }
                elsif ($packet->{type} == $HAVE) {
                    warn 'Got have #' . $packet->{payload};
                    my $bitfield = $s->bitfield;
                    vec($bitfield, $packet->{payload}, 1) = 1;
                    $s->_set_bitfield($bitfield);
                    $s->_consider();
                    $s->_request_pieces($packet->{payload})
                        if vec($s->torrent->wanted, $packet->{payload}, 1);

                    #$s->maybe_invoke_event(on_have => $packet->{payload});
                }
                elsif ($packet->{type} == $BITFIELD) {
                    $s->_set_bitfield($packet->{payload});
                }
                elsif ($packet->{type} == $REQUEST) { }
                elsif ($packet->{type} == $PIECE) {

              # Delay removing peer due to lack of being nice to us
              #$s->peers->{$h}{timeout}
              #                = AE::timer(120, 0, sub { $s->_del_peer($h) });
                    my ($index, $offset, $data) = @{$packet->{payload}};
                    warn sprintf 'Got block %d, %d, %d',
                        $index, $offset, length $data;

                    # Make sure $index is a working piece
                    $s->torrent->working_pieces->{$index} // return;

                    #warn np $s->local_requests;
                    # Make sure we req from this peer
                    use Data::Printer;
                    if (!grep {
                                   $_->{index} == $index
                                && $_->{offset} == $offset
                                && $_->{length} == length $data
                        } @{$s->local_requests}
                        )
                    {
               # XXX - We were sent a piece we weren't expecting from ths peer
               #$s = undef;
                        return;
                    }

                    $s->_set_local_requests(
                        [   grep {
                                       ($_->{index} != $index)
                                    || ($_->{offset} != $offset)
                                    || ($_->{length} != length($data))
                            } @{$s->local_requests}
                        ]
                    );
                    $s->torrent->working_pieces->{$index}{$offset}{data}
                        = $data;
                    $s->torrent->working_pieces->{$index}{$offset}{timer}
                        ->stop;

               # TODO - Keep track of how much we've downloaded from this peer
               $s->_set_downloaded($s->downloaded + length $data);
                    use Data::Printer;

                #               warn np $s->torrent->working_pieces->{$index};
                    use Data::Dump;

     #               ddx $s->torrent->working_pieces->{$index};
     #                ddx grep { !(defined $_ && defined $_->{data}) }
     #                        values %{$s->torrent->working_pieces->{$index}};
                    my $piece = $s->torrent->working_pieces->{$index};
                    #ddx keys %$piece;
                    my @list
                        = grep { !defined $piece->{$_}{data} } keys %$piece;
                    warn scalar(@list) . ' missing blocks';
                    if (!scalar @list) {
                        my $piece = join '', map {
                            $s->torrent->working_pieces->{$index}{$_}{data}
                            }
                            sort { $a <=> $b }
                            keys %{$s->torrent->working_pieces->{$index}};
                        if ((substr($s->torrent->pieces, $index * 20, 20) eq
                             sha1($piece))
                            )
                        {   warn 'hash pass!';
                            for my $attempt (1 .. 5)
                            {    # XXX = 5 == failure callback
                                last
                                    unless $s->torrent->_write($index, 0,
                                                               $piece);
                            }
                            my $bitfield = $s->torrent->bitfield;
                            vec($bitfield, $index, 1) = 1;
                            $s->torrent->_set_bitfield($bitfield);
                            warn 'broadcasting...';
                            $s->torrent->client->_broadcast(
                                build_have($index),
                                sub {
                                    shift->torrent->infohash eq
                                        $s->torrent->infohash;
                                }
                            );
                            warn 'announce to trackers...';
                            $s->torrent->announce('complete')
                                if !scalar grep {$_} split '',
                                substr unpack('b*', ~$s->torrent->bitfield),
                                0,
                                $s->torrent->piece_count + 1;

                           #$_->_consider() for #grep { $_->local_interested }
                           #    values %{$s->torrent->peers};
                           # TODO: callback system
                           # $s->_trigger_hash_pass($index);
                        }
                        else {
                            # TODO: callback system
                            #$s->_trigger_hash_fail($index);
                            # XXX - Not sure what to do... I'd
                            #       ban the peers involved and
                            #       try the same piece again.
                        }
                        delete $s->torrent->working_pieces->{$index};
                        for my $peer (
                            $s->torrent->client->_peers(
                                sub {
                                    shift->torrent->infohash eq
                                        $s->torrent->infohash;
                                }
                            )
                            )
                        {   $peer->_consider();
                            $peer->_request_pieces();
                        }

                        #undef $$original_request;
                        #undef $original_request;
                    }
                    $s->_request_pieces();
                }
                elsif ($packet->{type} == $CANCEL) { ...; }
            }

            #warn substr($$buffref, 0, 1, '' )  if length $$buffref;
            #$$buffref = '' if length $$buffref;
            return 0;
        },
        on_outgoing_empty => sub { }
    );
    $s->_build_keepalive_timer->start;
}
# Optional: either use [host, port] or [stream]
has host => (is => 'bare', isa => Str, reader => '_host');
has port => (is => 'bare', isa => Int, reader => '_port');
has stream => (is       => 'ro',
               isa      => 'IO::Async::Stream',
               required => 1,
               weak_ref => 1,
               handles  => ['write']
);
has $_ => (is      => 'ro',
           isa     => Int,
           init_arg => undef,
           default => sub {0},
           writer  => '_set_' . $_
) for qw[uploaded downloaded];

# TODO: Keep track of this in the torrent as well!!!
#after uploaded => sub {my $s = shift; $s->torrent->uploaded($s->torrent->uploaded + shift)};
#after downloaded => sub {my $s = shift; $s->torrent->downloaded($s->torrent->downloaded + shift)};

#
has torrent => (is       => 'ro',
                isa      => 'Net::BitTorrent::Torrent',
                required => 1,
                weak_ref => 1,
                handles => [qw[client loop]]
);
#
has $_ => (is      => 'ro',
           isa     => Str,
           lazy    => 1,
           writer  => '_set_' . $_,
           builder => '_build_' . $_
) for qw[bitfield peerid reserved];

sub _build_bitfield {
    pack 'b*', "\0" x shift->torrent->piece_count;
}

sub _send_bitfield {
    # XXX - Decide if we sent our bitfield, just a few HAVE messages or nothing at all

    my $s = shift;
                $s->write(
    build_bitfield($s->bitfield)

            );


}

has $_ => (is       => 'ro',
           isa      => Bool,
           init_arg => undef,
           default  => sub {0},
           writer   => '_set_' . $_
) for qw[interested interesting];
has $_ => (is       => 'ro',
           isa      => Bool,
           init_arg => undef,
           default  => sub {1},
           writer   => '_set_' . $_
) for qw[choked choking];
has $_
    . '_timer' => (is      => 'ro',
                   isa     => 'IO::Async::Timer::Periodic',
                   builder => '_build_' . $_ . '_timer',
                   lazy    => 1
    ) for qw[keepalive request_pieces];

sub _build_keepalive_timer {
    my $s = shift;
    my $timer = IO::Async::Timer::Periodic->new(
        interval       => 30,
        first_interval => 10,
        on_tick        => sub {
            $s->write(build_keepalive);
        }
    );
    $s->stream->loop->add($timer);
    $timer;
}

sub _build_request_pieces_timer {
    my $s = shift;
    my $timer =
        IO::Async::Timer::Periodic->new(
                                     interval       => 15,
                                     first_interval => 10,
                                     on_tick => sub { $s->_request_pieces() },
        );
    $s->stream->loop->add($timer);
    $timer;
}
has $_
    . '_requests' => (is      => 'ro',
                      isa     => ArrayRef,
                      traits  => ['Array'],
                      lazy    => 1,
                      default => sub { [] },
                      writer  => '_set_' . $_ . '_requests',
                      handles => {'_add_' . $_ . '_request' => 'push'}
    ) for qw[local remote];

sub _consider {    # Figure out whether or not we find a peer interesting
    my ($s) = @_;
    my $relevence = $s->bitfield & $s->torrent->wanted;
    my $interesting = (index(substr(unpack('b*', $relevence),
                                    0,
                                    $s->torrent->piece_count + 1
                             ),
                             1, 0
                       ) != -1
    ) ? 1 : 0;
    if (   $interesting
        && $s->choking
        && !$s->interesting)
    {   $s->write(build_interested());
        $s->_set_interesting(1);
    }
    elsif ($interesting && !$s->interesting) {
        $s->write(build_not_interested());
        $s->_set_interesting(0);
    }
}

sub _request_pieces {
    use Carp;
    carp 'Requesting pieces...';
    my ($s, @indexes) = @_;
    return unless $s->torrent->is_started;

    return if $s->choking && !@indexes;    # TODO: Fast Ext.

    my $wanted = $s->torrent->wanted;

    if (@indexes) {
        warn 'Choose from these: ' . join ', ', @indexes;
    }
    elsif (scalar keys %{$s->torrent->working_pieces} < 10)
    {                                      # XXX - Max working pieces
        for my $findex (0 .. $#{$s->torrent->files}) {
            for my $index ($s->torrent->_file_to_range($findex)) {
                next
                    if !(   vec($s->bitfield, $index, 1)
                         && vec($wanted, $index, 1));
                push @indexes,
                    map {$index} 1 .. $s->torrent->files->[$findex]{priority};
            }
        }
    }
    else {
        @indexes = keys %{$s->torrent->working_pieces};
    }


    #use Data::Printer;
    #warn np @indexes;
    return if !@indexes;
    #
    #warn np @indexes;
    #
    my $index = $indexes[rand @indexes];  # XXX - Weighted random/Rarest first
    my $piece_size
        = $index == $s->torrent->piece_count
        ?
        $s->torrent->size % $s->torrent->piece_length
        : $s->torrent->piece_length;
    #
    my $block_count = $piece_size / $s->torrent->block_size;
    #
    my @offsets = map { $_ * $s->torrent->block_size }
        0 .. $block_count - ((int($block_count) == $block_count) ? 1 : 0);
    $s->torrent->working_pieces->{$index} //= {map { $_ => {} } @offsets};
    #warn np @offsets;
    #
    my @unrequested = sort { $a <=> $b }
        grep {    # XXX - If there are no unrequested blocks, pick a new index
        (!ref $s->torrent->working_pieces->{$index}{$_})
            || (!defined $s->torrent->working_pieces->{$index}{$_}{peer})
        } @offsets;
    #warn np @unrequested;
    my @unfilled_local_requests
        = grep { !defined $_->{data} } @{$s->local_requests};
    #warn np @unfilled_local_requests;
    for (scalar @unfilled_local_requests .. 12) {
        my $offset = shift @unrequested;
        $offset // return
            ; # $s->_request_pieces(grep {$_ != $index} @indexes);    # XXX - Start working on another piece
        my $_block_size
            = ($offset == $offsets[-1])
            ?
            ($piece_size % $s->torrent->block_size)
            || $s->torrent->block_size
            : $s->torrent->block_size;
        warn sprintf 'Piece: size: %d | Offset + Block size: %d + %d = %d',
            $piece_size, $offset, $_block_size, ($offset + $_block_size);

        # XXX - Limit to x req per peer (future: based on bandwidth)
        warn sprintf 'Requesting %d, %d, %d', $index, $offset, $_block_size;
        $s->write(build_request($index, $offset, $_block_size))
            ;    # XXX - len for last piece
        $s->torrent->working_pieces->{$index}{$offset} = {
                                                       index  => $index,
                                                       offset => $offset,
                                                       length => $_block_size,
                                                       timer => IO::Async::Timer::Countdown->new(
            delay     => 20,
            on_expire => sub {
                warn 'I wanna cancel!!!!!!!!!!!!!!!!';
                return if ! defined $s->torrent->working_pieces->{$index}{$offset}{peer};
                return if length $s->torrent->working_pieces->{$index}{$offset}{data};
                return if $s->torrent->working_pieces->{$index}{$offset}{peer}->peerid ne $s->peerid;
                my $timer = shift;
                warn sprintf 'Cancelling %d, %d, %d', $index, $offset,
                    $_block_size;
                $s->write(build_cancel($index, $offset, $_block_size));
                $s->torrent->working_pieces->{$index}{$offset}{peer} = ();
                $s->_set_local_requests(
                    [   grep {
                                   $_->{index} != $index
                                || $_->{offset} != $offset
                                || $_->{length} != $_block_size
                        } @{$s->local_requests}
                    ]
                );
            }
        ),
                                                       peer  => $s
        };

        $s->loop->add($s->torrent->working_pieces->{$index}{$offset}{timer});
        $s->torrent->working_pieces->{$index}{$offset}{timer}->start;

        weaken($s->torrent->working_pieces->{$index}{$offset}{peer})
            unless isweak(
                        $s->torrent->working_pieces->{$index}{$offset}{peer});
        $s->_add_local_request(
                              $s->torrent->working_pieces->{$index}{$offset});
    }
}
#
1;
__END__

=encoding utf-8

=head1 NAME

Net::BitTorrent::Peer -  BitTorrent peer-to-peer protocol class

=head1 SYNOPSIS

    use Net::BitTorrent;

    my $client = Net::BitTorrent->new();
    my $torrent = $client->add_torrent('totally.legal.torrent');
    $client->loop;

=head1 DESCRIPTION

Net::BitTorrent is ...

=head1 Methods



=head2 C<>



=head1 LICENSE

Copyright (C) Sanko Robinson.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=head1 AUTHOR

Sanko Robinson E<lt>sanko@cpan.orgE<gt>

=cut

