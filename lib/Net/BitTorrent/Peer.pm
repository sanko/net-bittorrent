package Net::BitTorrent::Peer;
{
    use strict;      # core as of perl 5
    use warnings;    # core as of perl 5.006

    #
    use Carp qw[confess];                   # core as of perl 5
    use Scalar::Util qw[blessed weaken];    # core as of perl 5.007003
    use List::Util qw[sum];                 # core as of perl 5.007003
    use Socket                              # core as of perl 5
        qw[/F_INET/ SOMAXCONN SOCK_STREAM
        /inet_/ /pack_sockaddr_in/
    ];
    use Fcntl qw[F_SETFL O_NONBLOCK];       # core as of perl 5

    #
    use version qw[qv];                     # core as of 5.009
    our $SVN = q[$Id$];
    our $VERSION = sprintf q[%.3f], version->new(qw$Rev: 24 $)->numify / 1000;

    #
    use lib q[../../../lib];
    use Net::BitTorrent::Protocol qw[:build parse_packet :types];
    use Net::BitTorrent::Util qw[:bencode];

    # debugging
    #use Data::Dump qw[pp];

    #
    my (%_client, %_socket, %_session);    # possible params to new()
    my (%_incoming);
    my (%_data_out, %_data_in, %peerid, %_bitfield);
    my (                                   # basic status
         %_am_choking,         # this client is choking the peer
         %_am_interested,      # this client is interested in the peer
         %_peer_choking,       # peer is choking this client
         %_peer_interested,    # peer is interested in this client
    );
    my (%requests_out, %requests_in);
    my (%_last_contact);

    # Constructor
    sub new {

        # Creates a new Peer object
        # Expects params in a hash ref:
        #   - Client      (blessed Net::BitTorrent object)
        # Optional parameters:
        #   - Incoming peers:
        #     + Socket    (GLOB)
        #   - Outgoing peers:
        #     + Address   (IPv4:port)
        #     + Session   (blessed Net::BitTorrent::Session object)
        # Returns:
        #   - Blessed Net::BitTorrent::Peer object on sucess
        #   - undef on failure
        my ($class, $args) = @_;
        my $self = undef;

        # Param validation... Ugh...
        if (not defined $args) {
            confess q[Net::BitTorrent::Peer->new({}) requires ]
                . q[parameters a set of parameters];
        }

        #
        if (    not defined $args->{q[Socket]}
            and not defined $args->{q[Address]})
        {   confess <<'END'; }
Net::BitTorrent::Peer->new({}) requires either...
  - an 'Address' (IPv4:port for new, outgoing connections)
    or
  - a 'Socket' (GLOB-type for newly accepted incoming connections)
END

        #
        if (defined $args->{q[Socket]}) {    # Untested
            if (ref($args->{q[Socket]}) ne q[GLOB]) {
                confess
                    q[Net::BitTorrent::Peer->new({}) requires a GLOB-type socket];
            }
            if (not defined $args->{q[Client]}) {
                confess q[Net::BitTorrent::Peer->new({}) requires a ]
                    . q['Client' parameter];
            }
            if (not blessed $args->{q[Client]}) {
                confess q[Net::BitTorrent::Peer->new({}) requires a ]
                    . q[blessed 'Client' object];
            }
            if (not $args->{q[Client]}->isa(q[Net::BitTorrent])) {
                confess q[Net::BitTorrent::Peer->new({}) requires a ]
                    . q[blessed Net::BitTorrent object in the 'Client' parameter];
            }
            my ($peerport, $packed_ip)
                = unpack_sockaddr_in(getpeername($args->{q[Socket]}));

            #
            my $ok =
                $args->{q[Client]}->_event(
                      q[ip_filter],
                      {Address =>
                           sprintf(q[%s:%d], inet_ntoa($packed_ip), $peerport)
                      }
                );
            if (defined $ok and $ok == 0) { return; }

            #
            $self
                = bless \sprintf(q[%s:%d], inet_ntoa($packed_ip), $peerport),
                $class;
            $_socket{$self} = $args->{q[Socket]};
            $_client{$self} = $args->{q[Client]};
            weaken $_client{$self};
            $_client{$self}->_add_connection($self, q[ro]) or return;

            #
            $_data_out{$self} = q[];
            $_data_in{$self}  = q[];

            #
            $_incoming{$self} = 1;
        }
        else {
            if ($args->{q[Address]}
                !~ m[^(?:(?:(?:25[0-5]|2[0-4][0-9]|[0-1]?[0-9]{1,2})[.]?){4}):\d+$]
                )
            {   confess
                    q[Net::BitTorrent::Peer->new({}) requires an IPv4:port 'Address'];
            }
            if (not defined $args->{q[Session]}) {
                confess
                    q[Net::BitTorrent::Peer->new({}) requires a 'Session'];
            }
            if (not $args->{q[Session]}->isa(q[Net::BitTorrent::Session])) {
                confess
                    q[Net::BitTorrent::Peer->new({}) requires a blessed 'Session'];
            }
            socket(my ($socket), PF_INET, SOCK_STREAM, getprotobyname(q[tcp]))
                or return;
            $self = bless \$args->{q[Address]}, $class;
            my ($host, $port) = split q[:], $args->{q[Address]}, 2;
            $_socket{$self} = $socket;

            # Set socket to non-blocking
            if (not($^O eq q[MSWin32]
                    ? ioctl($_socket{$self}, 0x8004667e, pack(q[I], 1))
                    : fcntl($_socket{$self}, F_SETFL, O_NONBLOCK)
                )
                )
            {    # !!! - impossible to test failure for coverage...
                $_client{$self}->on_event(
                    q[log],
                    sprintf(
                        q[There was a problem making an outgoing socket non-blocking: %s],
                        $^E)
                );
                return;
            }

            # Here, connect() is non-blocking so it doesn't return a valid
            # value to test agiainst.  We see how things are progressing by
            # checking $^E further down.
            connect($_socket{$self},
                    pack_sockaddr_in($port, inet_aton($host)));

            #
            $_client{$self} = $args->{q[Session]}->_client;
            weaken $_client{$self};
            $_session{$self} = $args->{q[Session]};
            weaken $_session{$self};
            $_bitfield{$self} = pack q[b] . $_session{$self}->_piece_count, 0;

            # Add initial handshake to outgoing queue
            $_data_out{$self} =
                build_handshake($_client{$self}->__build_reserved,
                                pack(q[H*], $_session{$self}->infohash),
                                $_client{$self}->peerid
                );
            $_data_in{$self} = q[];

            #
            $self->_send_bitfield;

            #
            $_client{$self}->_add_connection($self, q[rw]) or return;
            $_incoming{$self} = 0;
        }
        if ($self) {

            # Basic Status
            $_am_choking{$self}      = 1;
            $_am_interested{$self}   = 0;
            $_peer_choking{$self}    = 1;
            $_peer_interested{$self} = 0;
            $_last_contact{$self}    = time;    # lies

            #
            $requests_out{$self} = [];
            $requests_in{$self}  = [];

            # keepalive
            $_client{$self}->schedule({Time   => time + 120,
                                       Code   => \&_send_keepalive,
                                       Object => $self
                                      }
            );

            #
            $_client{$self}->schedule({Time   => time + 30,
                                       Code   => \&_cancel_old_requests,
                                       Object => $self
                                      }
            );

            #
            $_client{$self}->schedule({Time   => time + 90,
                                       Code   => \&_disconnect_useless_peer,
                                       Object => $self
                                      }
            );
        }

        #
        return $self;
    }

    # Accessors | Public
    sub as_string {
        my ($self) = @_;
        return $$self;

        #
        return $$self if not defined $_socket{$self};

        #
        my ($peer_name) = getpeername($_socket{$self});
        return $$self if not defined $peer_name;

        #
        my ($peer_port, $packed_peer_ip) = unpack_sockaddr_in($peer_name);

        #
        return sprintf(q[%s:%d], inet_ntoa($packed_peer_ip), $peer_port);
    }

    # Accessors | Private | General
    sub _socket   { return $_socket{+shift} }
    sub _session  { return $_session{+shift} }
    sub _bitfield { return $_bitfield{+shift} }

    # Accessors | Private | Status
    sub _peer_choking    { return $_peer_choking{+shift} }
    sub _am_choking      { return $_am_choking{+shift} }
    sub _peer_interested { return $_peer_interested{+shift} }
    sub _am_interested   { return $_am_interested{+shift} }
    sub _incoming        { return $_incoming{+shift} }

    # Methods | Private
    sub _rw {
        my ($self, $read, $write, $error) = @_;

        #
        if ($error) { weaken $self; $self->_disconnect($^E); return }

        #warn $read;
        #warn $write;
        #
        my ($actual_read, $actual_write) = (0, 0);

        #
        if ($write) {
            if (length $_data_out{$self}) {
                $actual_write
                    = syswrite($_socket{$self}, $_data_out{$self}, $write, 0);
                if (not $actual_write) {

                    #warn sprintf q[Failed to writing %d bytes to peer],
                    #    $write;
                    #use Devel::FindRef;
                    #warn Devel::FindRef::track \$self;
                    #use Devel::Peek;
                    #Dump(\$self);
                    weaken $self;
                    $self->_disconnect(
                             sprintf q[Failed to write %d bytes to peer (%s)],
                             $write, $^E);
                    return;
                }
                else {
                    $_client{$self}->_event(q[peer_write],
                                    {Peer => $self, Length => $actual_write});
                    substr($_data_out{$self}, 0, $actual_write, q[]);
                }
            }
        }

        #
        if ($read) {
            $actual_read = sysread($_socket{$self}, $_data_in{$self}, $read,
                                   length($_data_in{$self}));
            if (not $actual_read) {

                #use Devel::FindRef;
                #warn Devel::FindRef::track \$self;
                #use Devel::Peek;
                #Dump(\$self);
                weaken $self;
                $self->_disconnect(
                            sprintf q[Failed to read %d bytes from peer (%s)],
                            $read, $^E);
                return;
            }
            else {
                if (not defined $peerid{$self}) {
                    $_client{$self}->_event(q[peer_connect], {Peer => $self});
                }
                $_client{$self}->_event(q[peer_read],
                                     {Peer => $self, Length => $actual_read});

                #my $packet;
            PACKET: while ($_data_in{$self}) {
                    my $data_len = length $_data_in{$self};

                    #
                    my $packet = parse_packet(\$_data_in{$self});

                    #
                    if (not defined $packet) {
                        if (length($_data_in{$self}) != $data_len) {

                            #warn q[Bad packet from ]. $$self;
                            #warn  $_data_in{$self};
                            #warn $data_len;
                            weaken $self;
                            $self->_disconnect(q[...bad packet.]);
                            return;
                        }
                        last PACKET;
                    }

                    #
                    my %dispatch = (
                        &HANDSHAKE => sub {
                            my ($self, $payload) = @_;
                            $_last_contact{$self} = time;
                            $_client{$self}
                                ->_event(q[packet_incoming_handshake],
                                        {Peer => $self, Payload => $payload});
                            $peerid{$self} = $payload->[2];
                            if ($peerid{$self} eq $_client{$self}->peerid) {
                                weaken $self;
                                $self->disconnect(
                                           q[...we've connected to ourself.]);

                                #$_client{$self}->_del_connection($self);
                                return;
                            }

                            #
                            if (not defined $_session{$self})
                            {    # Incoming peer
                                $_session{$self}
                                    = $_client{$self}->_locate_session(
                                               unpack(q[H40], $payload->[1]));

                                #
                                if (not defined $_session{$self}) {
                                    weaken $self;
                                    $self->_disconnect(
                                        sprintf
                                            q[We aren't serving this torrent: %s],
                                        unpack(q[H40], $payload->[1])
                                    );
                                    $_client{$self}->_del_connection($self);
                                    return;
                                }

                                #
                                weaken $_session{$self};
                                $_bitfield{$self} = pack q[b]
                                    . $_session{$self}->_piece_count, 0;

                                #
                                $_data_out{$self} .=
                                    build_handshake(
                                     $_client{$self}->__build_reserved,
                                     pack(q[H40], $_session{$self}->infohash),
                                     $_client{$self}->peerid
                                    );
                                $self->_send_bitfield;

                              # XXX - add socket as rw
                              #$_client{$self}->_add_socket(
                              #           $_socket{$self},
                              #           ($_data_out{$self} ? q[rw] : q[ro]),
                              #           sub { $self->_rw(@_); }
                              #);
                            }
                        },
                        &KEEPALIVE => sub {
                            my ($self) = @_;

                            #
                            $_client{$self}
                                ->_event(q[packet_incoming_keepalive],
                                         {Peer => $self});

                            #
                            return 1;
                        },
                        &CHOKE => sub {
                            my ($self) = @_;

                            #
                            $_client{$self}->_event(q[packet_incoming_choke],
                                                    {Peer => $self});

                            #
                            $_peer_choking{$self}  = 1;
                            $_am_interested{$self} = 0;    # lies

                            #
                            for my $request (@{$requests_out{$self}}) {
                                my $piece = $_session{$self}
                                    ->_piece_by_index($request->{q[Index]});

                                #warn pp $piece;
                                #warn pp $request;
                                $piece->{q[Blocks_Requested]}
                                    ->[$request->{q[_vec_offset]}]--;

                                #exit die
                                #    if grep { $_ == -1 }
                                #        @{$piece->{q[Blocks_Requested]}};
                            }

                            #
                            return $requests_out{$self} = [];
                        },
                        &UNCHOKE => sub {
                            my ($self) = @_;
                            $_last_contact{$self} = time;
                            $_peer_choking{$self} = 0;

                            #$_am_interested{$self} = 1;    # lies
                            my $requests = 0;
                            $_client{$self}
                                ->_event(q[packet_incoming_unchoke],
                                         {Peer => $self});
                            for (1 .. int(
                                     (2**21) / $_session{$self}->_piece_length
                                 )
                                )
                            {    # ~2M/peer
                                last unless $self->_request_block;
                                $requests++;
                            }

                            #
                            #$_client{$self}->schedule(
                            #                    {Time   => time + 30,
                            #                     Code   => \&_request_block,
                            #                     Object => $self
                            #                    }
                            #   ) unless $request;
                            #
                            return 1;
                        },
                        &INTERESTED => sub {
                            my ($self) = @_;
                            $_last_contact{$self} = time;

                            #
                            $_peer_interested{$self} = 1;
                            $_client{$self}
                                ->_event(q[packet_incoming_interested],
                                         {Peer => $self});

                            # TODO: not this
                            $_data_out{$self} .= build_unchoke();
                            $_am_choking{$self} = 0;
                            $_client{$self}
                                ->_event(q[packet_outgoing_unchoke],
                                         {Peer => $self});

                            #
                            return 1;
                        },
                        &NOT_INTERESTED => sub {
                            my ($self) = @_;
                            $_last_contact{$self} = time;
                            $_client{$self}
                                ->_event(q[packet_incoming_not_interested],
                                         {Peer => $self});
                            $_peer_interested{$self} = 1;
                            $_am_choking{$self}      = 1;
                            return 1;
                        },
                        &HAVE => sub {
                            my ($self, $index) = @_;
                            $_client{$self}->_event(q[packet_incoming_have],
                                            {Peer => $self, Index => $index});
                            vec($_bitfield{$self}, $index, 1) = 1;
                            $self->_check_interest;
                            if ($_am_interested{$self}
                                and not $_peer_choking{$self})
                            {   $self->_request_block;
                            }
                        },
                        &BITFIELD => sub {
                            my ($self, $payload) = @_;

                            #
                            $_bitfield{$self} = $packet->{q[Payload]};

                            #
                            $_client{$self}
                                ->_event(q[packet_incoming_bitfield],
                                         {Peer => $self});

                            #
                            return $self->_check_interest;
                        },
                        &REQUEST => sub {
                            my ($self, $payload) = @_;

                            #
                            $_client{$self}->_event(
                                                   q[packet_incoming_request],
                                                   {Peer   => $self,
                                                    Index  => $payload->[0],
                                                    Offset => $payload->[1],
                                                    Length => $payload->[2]
                                                   }
                            );

                            #
                            if (not @{$requests_in{$self}}) {
                                $_client{$self}->schedule(
                                                 {Time   => time + 15,
                                                  Code   => \&_fill_requests,
                                                  Object => $self
                                                 }
                                );
                            }

                            #
                            return push @{$requests_in{$self}},
                                {Index  => $payload->[0],
                                 Offset => $payload->[1],
                                 Length => $payload->[2]
                                };
                        },
                        &PIECE => sub {
                            my ($self, $payload) = @_;
                            $_last_contact{$self} = time;

                            #
                            my ($index, $offset, $data) = @{$payload};
                            my $length = length($data);

                            #
                            my ($request) = grep {
                                        ($_->{q[Index]} == $index)
                                    and ($_->{q[Offset]} == $offset)
                                    and ($_->{q[Length]} == $length)
                            } @{$requests_out{$self}};

                            #
                            if (not defined $request) {
                                weaken $self;
                                $self->_disconnect(
                                       q[Handed a piece we never asked for.]);
                                return;
                            }

                            # Even if the block is junk...
                            $_session{$self}
                                ->_add_downloaded($request->{q[Length]});

                            #
                            @{$requests_out{$self}} = grep {
                                       ($_->{q[Index]} != $index)
                                    or ($_->{q[Offset]} != $offset)
                                    or ($_->{q[Length]} != $length)
                            } @{$requests_out{$self}};

                            # perlref
                            my $piece
                                = $_session{$self}->_piece_by_index($index);

                            #
                            if (not defined $piece) {
                                weaken $self;
                                $self->_disconnect(
                                     q[sent a block to a non-existant piece]);
                                return;
                            }

                            #
                            if (not $_session{$self}
                                ->_write_data($index, $offset, \$data))
                            {   warn $^E;

                               #$_client{$self}->_del_socket($_socket{$self});
                                return;
                            }

                            #
                            #warn q[Before change ] . pp $piece;
                            $piece->{q[Blocks_Recieved]}
                                ->[$request->{q[_vec_offset]}] = 1;

                            #
                            warn sprintf q[Waking sleeping piece up: %d],
                                $request->{q[Index]}
                                if $piece->{q[Slow]};
                            $piece->{q[Slow]}  = 0;
                            $piece->{q[Touch]} = time;

                            #warn q[After change  ] . pp $piece;
                            #
                            if (not grep { $_ == 0 }
                                @{$piece->{q[Blocks_Recieved]}})
                            {
                                if ($_session{$self}
                                    ->_check_piece_by_index($index))
                                {   for my $p (@{$_session{$self}->_peers}) {
                                        $_data_out{$p} .= build_have($index);
                                        $_client{$self}->_del_connection($p);
                                        $_client{$self}
                                            ->_add_connection($p, q[rw]);
                                    }
                                }
                            }

                            #
                            $_client{$self}->_event(q[packet_incoming_block],
                                                    {Index  => $index,
                                                     Offset => $offset,
                                                     Length => $length,
                                                     Peer   => $self
                                                    }
                            );

                            #
                            if ($piece->{q[Endgame]}) {

                                # cancel duplicate requests with other peers
                                for my $peer ($_session{$self}->_peers) {
                                    for my $x (
                                        reverse 0 .. $#{$requests_out{$peer}})
                                    {
                                        if (($requests_out{$peer}->[$x]
                                             ->{q[Index]} == $index
                                            )
                                            and ($requests_out{$peer}->[$x]
                                                 ->{q[Offset]} == $offset)
                                            and ($requests_out{$peer}->[$x]
                                                 ->{q[Length]} == $length)
                                            )
                                        {

                                            #
                                            $_data_out{$peer} .=
                                                build_cancel(
                                                        $request->{q[Index]},
                                                        $request->{q[Offset]},
                                                        $request->{q[Length]}
                                                );

                                            #
                                            $_client{$self}->_event(
                                                    q[packet_outgoing_cancel],
                                                    {Index  => $index,
                                                     Offset => $offset,
                                                     Length => $length,
                                                     Peer   => $peer
                                                    }
                                            );

                                            #
                                            $_client{$self}
                                                ->_del_connection($peer);
                                            $_client{$self}
                                                ->_add_connection($peer,
                                                                  q[rw]);

                                            #
                                            splice(@{$requests_out{$peer}},
                                                   $x, 1);

                                            #
                                            last;
                                        }
                                    }
                                }
                            }

                            #
                            if ($self->_check_interest()) {
                                $self->_request_block;
                            }

                            #
                            return 1;
                        },
                        &CANCEL => sub {
                            my ($self, $payload) = @_;
                            $_last_contact{$self} = time;
                            my ($index, $offset, $length) = @$payload;
                            $_client{$self}->_event(q[packet_incoming_cancel],
                                                    {Index  => $index,
                                                     Offset => $offset,
                                                     Length => $length,
                                                     Peer   => $self
                                                    }
                            );
                            for my $x (reverse 0 .. $#{$requests_in{$self}}) {
                                if (($requests_in{$self}->[$x]->{q[Index]}
                                     == $index
                                    )
                                    and
                                    ($requests_in{$self}->[$x]->{q[Offset]}
                                        == $offset)
                                    and
                                    ($requests_in{$self}->[$x]->{q[Length]}
                                        == $length)
                                    )
                                {   splice(@{$requests_in{$self}}, $x, 1);
                                }
                            }
                            #warn q[Remaining requests: ]
                            #        . pp $requests_in{$self};
                            return 1;
                        },
                        &EXTPROTOCOL => sub {
                            my ($self, $payload) = @_;
                            $_last_contact{$self} = time;
                            my ($id, $packet) = @{$payload};
                            $packet = bdecode $packet;
                            if ($packet) {
                                if ($id == 0) {
                                    if (defined $packet->{q[p]}) {
                                        my (undef, $packed_ip)
                                            = unpack_sockaddr_in(
                                                getpeername($_socket{$self}));
                                        my $node
                                            = sprintf(q[%s:%d],
                                                      inet_ntoa($packed_ip),
                                                      $packet->{q[p]});
                                        $_client{$self}
                                            ->_dht->_add_node($node);
                                    }
                                }
                                $_client{$self}->_event(
                                                  q[packet_incoming_extended],
                                                  {Payload => $packet,
                                                   ID      => $id,
                                                   Peer    => $self
                                                  }
                                );
                            }
                        }
                    );

                    #
                    if (defined $dispatch{$packet->{q[Type]}}) {
                        $dispatch{$packet->{q[Type]}}($self,
                                                      $packet->{q[Payload]});
                    }
                    else {
                        die q[Unknown packet! ] . pp $packet;
                    }
                }
            }
        }

        #
        $_client{$self}->_del_connection($self);
        $_client{$self}
            ->_add_connection($self, ($_data_out{$self} ? q[rw] : q[ro]))
            if $_socket{$self};

        #
        return ($actual_read, $actual_write);
    }

    sub _check_interest {
        my ($self) = @_;

        #
        if (not defined $_session{$self}) { return; }

        #
        my $interesting  = $_am_interested{$self};
        my $session_have = $_session{$self}->bitfield();
        my $session_want = $_session{$self}->_wanted();
        my $relevence    = $_bitfield{$self} & $session_want;

#warn sprintf
#    q[pieces:%d|phave:%d|ihave:%d|iwant:%d|rel:%d|int?:%d v. %d | 1st: %d | %s],
#    $_session{$self}->_piece_count,
#    sum(split(q[], unpack(q[b*], ($_bitfield{$self})))),
#    sum(split(q[], unpack(q[b*], ($session_have)))),
#    sum(split(q[], unpack(q[b*], ($session_want)))),
#    sum(split(q[], unpack(q[b*], $relevence))),
#    $_am_interested{$self},
#    ((index(unpack(q[b*], $relevence), 1, 0) != -1) ? 1 : 0),
#    index(unpack(q[b*], $relevence), 1, 0),
#    $$self;
        $interesting = (index(unpack(q[b*], $relevence), 1, 0) != -1) ? 1 : 0;

        #
        if ($interesting and not $_am_interested{$self}) {
            $_am_interested{$self} = 1;
            $_data_out{$self} .= build_interested;
            $_client{$self}
                ->_event(q[packet_outgoing_interested], {Peer => $self});
        }
        elsif (not $interesting and $_am_interested{$self}) {
            $_am_interested{$self} = 0;
            $_data_out{$self} .= build_not_interested;
            $_client{$self}
                ->_event(q[packet_outgoing_not_interested], {Peer => $self});
        }

        #
        return $_am_interested{$self};
    }

    sub _disconnect_useless_peer {
        my ($self) = @_;
        return if not defined $self;

        #warn sprintf q[Pre  I am %schoked and %sinterested | %s | %s],
        #    ($_peer_choking{$self}  ? q[] : q[not ]),
        #    ($_am_interested{$self} ? q[] : q[not ]),
        #    pp($requests_out{$self}), $$self;
        #
        #if (    (not $_peer_choking{$self})
        #    and ($#{$requests_out{$self}} < 16))
        #{   for (($#{$requests_out{$self}}) .. 20) {
        #        $self->_request_block();
        #    }
        #}
        if ($_last_contact{$self} < (time - (5 * 60))) {
            weaken $self;
            $self->_disconnect(q[No contact from peer in 5 min]);
            return;
        }
        if ($_peer_choking{$self} and $_am_interested{$self}) {

            # XXX - send uninterested?
            $self->_check_interest;
        }
        if (    ($_peer_choking{$self})
            and (not $_am_interested{$self})
            and (not $_peer_interested{$self}))
        {   weaken $self;
            $self->_disconnect(
                        q[Useless peer (Not interested and not interesting.)])

                # if $_am_interested{$self} and $_peer_interested{$self};
                ;
            return;
        }

        #
        $_client{$self}->schedule({Time   => time + 90,
                                   Code   => \&_disconnect_useless_peer,
                                   Object => $self
                                  }
        );

        #
        return 1;
    }

    sub _cancel_old_requests {
        my ($self) = @_;
        return if not defined $self;
        return if not defined $_socket{$self};

        #
        $_client{$self}->schedule({Time   => time + 60,
                                   Code   => \&_cancel_old_requests,
                                   Object => $self
                                  }
        );

        #
        my $canceled = 0;

        #
        if (@{$requests_out{$self}} == []) {
            return;
        }

        #
        #warn sprintf q[Post I am %schoked and %sinterested | %s],
        #    ($_peer_choking{$self}  ? q[] : q[not ]),
        #    ($_am_interested{$self} ? q[] : q[not ]),
        #    pp $requests_out{$self};
        #
        for my $i (reverse(0 .. $#{$requests_out{$self}})) {
            my $request = $requests_out{$self}->[$i];

            #
            if (time <= ($request->{q[Timestamp]} + 180)) {
                next;
            }

            #
            my $piece
                = $_session{$self}->_piece_by_index($request->{q[Index]});

            #
            $piece->{q[Blocks_Requested]}->[$request->{q[_vec_offset]}]--;

            #
            if ($piece->{q[Touch]} <= time - 20) {
                $piece->{q[Slow]} = 1;
                warn sprintf q[Putting slow piece to sleep: %d],
                    $request->{q[Index]}
                    if $piece->{q[Slow]};
            }

            #
            $_data_out{$self} .=
                build_cancel($request->{q[Index]}, $request->{q[Offset]},
                             $request->{q[Length]});

            #
            $_client{$self}->_event(q[packet_outgoing_cancel],
                                    {Index  => $request->{q[Index]},
                                     Offset => $request->{q[Offset]},
                                     Length => $request->{q[Length]},
                                     Peer   => $self
                                    }
            );

            #
            splice(@{$requests_out{$self}}, $i, 1);

            #
            $canceled++;
        }
        if ($canceled) {
            warn sprintf q[~~~~~~~~~~~~~~~~~~ Canceled %d pieces], $canceled;
            $_client{$self}->_del_connection($self);
            $_client{$self}->_add_connection($self, q[rw]);
        }

        #
        return $canceled;
    }

    sub _request_block {
        my ($self) = @_;

        #
        return if not defined $_socket{$self};

        #
        return if $_peer_choking{$self};

        #
        my $piece = $_session{$self}->_pick_piece($self);

        #
        if ($piece) {

            #warn q[$self == ] . $self->as_string;
            #warn pp \%requests_out;
            #warn q[Num requests: ] . scalar(@{$requests_out{$self}});
            #warn q[Before Request: ] . pp $piece;
            #warn q[Endgame? ] . $piece->{q[Endgame]};
            my $vec_offset;
            if ($piece->{q[Endgame]}) {

           # This next bit selects the least requested block (max 3 requests),
           #   makes sure this peer isn't already sitting on this request
           #   and... I just lost my train of thought; It's Friday afternoon.
           #   Regardless of how it looks, it does what I mean.
                my $tmp_index = -1;
                my %temp      = map {
                    $tmp_index++;
                    (    ($_ < 5)
                     and ($piece->{q[Blocks_Recieved]}->[$tmp_index] == 0)
                        )
                        ? ($tmp_index => $_)
                        : ()
                } @{$piece->{q[Blocks_Recieved]}};
                #warn q[%temp indexes: ] . pp \%temp;
                for my $index (sort { $temp{$a} <=> $temp{$b} }
                               sort { $a <=> $b } keys %temp)
                {   warn q[$index: ] . $index;
                    if (not grep {

                           #warn sprintf q[recieved: %d | matches[i:%d|o:%d]],
                           #    ($piece->{q[Blocks_Recieved]}->[$index]),
                           #    ($piece->{q[Index]} == $_->{q[Index]}),
                           #    ($index == $_->{q[_vec_offset]});
                            ($piece->{q[Blocks_Recieved]}->[$index])
                                or (    ($piece->{q[Index]} == $_->{q[Index]})
                                    and ($index == $_->{q[_vec_offset]}))
                        } @{$requests_out{$self}}
                        )
                    {   $vec_offset = $index;
                        last;
                    }
                }
            }
            else {
                for my $i (0 .. $#{$piece->{q[Blocks_Requested]}}) {
                    if ($piece->{q[Blocks_Requested]}->[$i] == 0) {
                        $vec_offset = $i;
                        last;
                    }
                }
            }

            #
            if (not defined $vec_offset or $vec_offset == -1) {

                #warn sprintf
                #    q[Piece has been fully requested: req:%s|rec:%s],
                #    join(q[], @{$piece->{q[Blocks_Requested]}}),
                #    join(q[], @{$piece->{q[Blocks_Recieved]}});
                # xxx - pick a different piece?
                # xxx - Honestly, this piece shouldn't have been returned
                #       from _pick_piece in the first place...
                return;
            }

            # Might already be 1 if in endgame mode...
            $piece->{q[Blocks_Requested]}->[$vec_offset]++;

            #warn q[After request: ] . pp $piece;
            #
            my $offset = $vec_offset * $piece->{q[Block_Length]};
            my $length = ((($vec_offset + 1) == $piece->{q[Block_Count]})
                          ? (($piece->{q[Length]} % $piece->{q[Block_Length]})
                             || $piece->{q[Block_Length]})
                          : ($piece->{q[Block_Length]})
            );
            my $request = {Index       => $piece->{q[Index]},
                           Offset      => $offset,
                           Length      => $length,
                           Timestamp   => time,
                           _vec_offset => $vec_offset,
            };

            #
            push @{$requests_out{$self}}, $request;

            #
            $_client{$self}->_event(q[packet_outgoing_request],
                                    {Index  => $piece->{q[Index]},
                                     Offset => $offset,
                                     Length => $length,
                                     Peer   => $self
                                    }
            );

            #
            $_client{$self}->_del_connection($self);
            $_client{$self}->_add_connection($self, q[rw]);
            return $_data_out{$self}
                .= build_request($piece->{q[Index]}, $offset, $length);
        }
        return;
    }

    sub _send_bitfield {    # XXX - or fast packet
        my ($self) = @_;

        #
        return if not defined $_socket{$self};

        #
        my @have = split q[], unpack q[b*], $_session{$self}->bitfield;

        #
        if (((sum(@have) * 8) < $#have)) {
            return $_data_out{$self}
                .= build_bitfield(pack q[B*], join q[], @have);
        }

        #
        for my $index (0 .. $#have) {
            $_data_out{$self} .= build_have($index) if $have[$index];
        }

        #
        $_client{$self}->_event(q[packet_outgoing_bitfield], {Peer => $self});

        #
        return 1;
    }

    sub _send_keepalive {
        my ($self) = @_;

        #
        return if not defined $self;
        return if not defined $_socket{$self};

        #
        $_client{$self}->schedule({Time   => time + 120,
                                   Code   => \&_send_keepalive,
                                   Object => $self
                                  }
        );

        #
        $_client{$self}
            ->_event(q[packet_outgoing_keepalive], {Peer => $self});

        #
        return $_data_out{$self} .= build_keepalive();
    }

    sub _fill_requests {
        my ($self) = @_;

        #
        return if not defined $self;
        return if not @{$requests_in{$self}};
        return if $_am_choking{$self};

        #
        $_client{$self}->schedule({Time   => time + 60,
                                   Code   => \&_fill_requests,
                                   Object => $self
                                  }
        );

        #
        while ((length($_data_out{$self}) < 2**15) and @{$requests_in{$self}})
        {   my $request = shift @{$requests_in{$self}};
            next
                unless $_session{$self}
                    ->_check_piece_by_index($request->{q[Index]});

            # Accounting...
            $_session{$self}->_add_uploaded($request->{q[Length]});

            #
            $_client{$self}->_event(q[packet_outgoing_block],
                                    {Index  => $request->{q[Index]},
                                     Offset => $request->{q[Offset]},
                                     Length => $request->{q[Length]},
                                     Peer   => $self
                                    }
            );
            $_data_out{$self} .=
                build_piece($request->{q[Index]},
                            $request->{q[Offset]},
                            $_session{$self}->_read_data(
                                                        $request->{q[Index]},
                                                        $request->{q[Offset]},
                                                        $request->{q[Length]}
                            )
                );

            #
            if (rand(3) >= 2) { $self->_send_choke; }
        }

        #
        $_client{$self}->_del_connection($self) or return;
        $_client{$self}->_add_connection($self, q[rw]) or return;

        #
        return 1;
    }

    sub _send_choke {
        my ($self) = @_;

        #
        return if $_am_choking{$self} == 1;

        #
        $requests_in{$self}      = [];
        $_am_choking{$self}      = 1;
        $_peer_interested{$self} = 0;

        #
        $_data_out{$self} .= build_choke();

        #
        $_client{$self}->_event(q[packet_outgoing_choke], {Peer => $self});

        #
        $_client{$self}->_del_connection($self) or return;
        $_client{$self}->_add_connection($self, q[rw]) or return;

        #
        return 1;
    }

    # Incomplete
    sub _disconnect {

        # Returns...
        #        1) ...when the socket is disconnected and (if applicable)
        #             removed from the client object.
        #    undef) ...whe any of the following cause an early return or
        #             the socket cannot be removed from the parent.
        # Expects a single parameter: a reference to a blessed N::B::S::Peer
        # TODO:
        #       - check if peer is in client's list of connections
        my ($self, $reason) = @_;

        #
        $reason ||= q[Connection closed by remote peer.];
        $_client{$self}->_del_connection($self);

        #
        #weaken $self;
        #use Devel::Peek qw[SvREFCNT Dump];
        #Dump($self);
        #use Devel::Refcount qw( refcount );
        #warn sprintf "SvREFCNT=%d, refcount=%d",
        #  SvREFCNT($self), refcount($self);
        #
        $_client{$self}
            ->_event(q[peer_disconnect], {Peer => $self, Reason => $reason});
        shutdown($_socket{$self}, 2)
            if defined $_socket{$self};    # safer than close()
                                           #
        delete $_socket{$self};

        #
        return 1;
    }

    #
    DESTROY {
        my ($self) = @_;

        #warn q[Goodbye, ] . $$self;
        if ($_session{$self}) {
            for my $request (@{$requests_out{$self}}) {
                my $piece
                    = $_session{$self}->_piece_by_index($request->{q[Index]});
                $piece->{q[Blocks_Requested]}->[$request->{q[_vec_offset]}]--;
                exit die
                    if grep { $_ == -1 } @{$piece->{q[Blocks_Requested]}};
            }
        }

        #
        #$_client{$self}->_del_socket($_socket{$self});
        #
        delete $_client{$self};
        delete $_socket{$self};
        delete $_session{$self};

        #
        delete $_incoming{$self};

        #
        delete $_data_out{$self};
        delete $_data_in{$self};
        delete $peerid{$self};
        delete $_bitfield{$self};

        #
        delete $_am_choking{$self};    # this client is choking the peer
        delete $_am_interested{$self}; # this client is interested in the peer
        delete $_peer_choking{$self};  # peer is choking this client
        delete $_peer_interested{$self};   # peer is interested in this client

        #
        delete $requests_out{$self};
        delete $requests_in{$self};

        #
        delete $_last_contact{$self};

        #
        return 1;
    }
    1;
}

=head1 Author

Sanko Robinson <sanko@cpan.org> - http://sankorobinson.com/

CPAN ID: SANKO

=head1 License and Legal

Copyright (C) 2008 by Sanko Robinson E<lt>sanko@cpan.orgE<gt>

This program is free software; you can redistribute it and/or modify
it under the terms of The Artistic License 2.0.  See the F<LICENSE>
file included with this distribution or
http://www.perlfoundation.org/artistic_license_2_0.  For
clarification, see http://www.perlfoundation.org/artistic_2_0_notes.

When separated from the distribution, all POD documentation is covered
by the Creative Commons Attribution-Share Alike 3.0 License.  See
http://creativecommons.org/licenses/by-sa/3.0/us/legalcode.  For
clarification, see http://creativecommons.org/licenses/by-sa/3.0/us/.

Neither this module nor the L<Author|/Author> is affiliated with
BitTorrent, Inc.

=for svn $Id$

=cut
