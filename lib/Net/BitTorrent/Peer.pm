#!C:\perl\bin\perl.exe 
package Net::BitTorrent::Peer;
{
    use strict;      # core as of perl 5
    use warnings;    # core as of perl 5.006

    #
    use Carp qw[carp];                              # core as of perl 5
    use Scalar::Util qw[blessed weaken refaddr];    # core as of perl 5.007003
    use List::Util qw[sum max];                     # core as of perl 5.007003
    use Socket                                      # core as of perl 5
        qw[/F_INET/ SOMAXCONN SOCK_STREAM
        /inet_/ /pack_sockaddr_in/
    ];
    use Fcntl qw[F_SETFL O_NONBLOCK];               # core as of perl 5

    #
    use version qw[qv];                             # core as of 5.009
    our $SVN = q[$Id$];
    our $UNSTABLE_RELEASE = 0; our $VERSION = sprintf(($UNSTABLE_RELEASE ? q[%.3f_%03d] : q[%.3f]), (version->new((qw$Rev$)[1])->numify / 1000), $UNSTABLE_RELEASE);

    #
    use lib q[../../../lib];
    use Net::BitTorrent::Protocol qw[:build parse_packet :types];
    use Net::BitTorrent::Util qw[:bencode];
    use Net::BitTorrent::Version;

    # debugging
    #use Data::Dump qw[pp];
    #
    my (@CONTENTS) = \my (
        %_client, %_socket, %_session,    # possible params to new()
        %_data_out, %_data_in, %peerid, %_bitfield,
        %_am_choking,         # this client is choking the peer
        %_am_interested,      # this client is interested in the peer
        %_peer_choking,       # peer is choking this client
        %_peer_interested,    # peer is interested in this client
        %_incoming,           # peer started conversation
        %requests_out, %requests_in, %_last_contact, %_clutter
    );
    my %REGISTRY;

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
            carp q[Net::BitTorrent::Peer->new({}) requires ]
                . q[parameters a set of parameters];
            return;
        }

        #
        if (    not defined $args->{q[Socket]}
            and not defined $args->{q[Address]})
        {   carp <<'END'; return; }
Net::BitTorrent::Peer->new({}) requires either...
  - an 'Address' (IPv4:port for new, outgoing connections)
    or
  - a 'Socket' (GLOB-type for newly accepted incoming connections)
END

        #
        if (defined $args->{q[Socket]}) {    # Untested
            if (ref($args->{q[Socket]}) ne q[GLOB]) {
                carp
                    q[Net::BitTorrent::Peer->new({}) requires a GLOB-type socket];
                return;
            }
            if (not defined $args->{q[Client]}) {
                carp q[Net::BitTorrent::Peer->new({}) requires a ]
                    . q['Client' parameter];
                return;
            }
            if (not blessed $args->{q[Client]}) {
                carp q[Net::BitTorrent::Peer->new({}) requires a ]
                    . q[blessed 'Client' object];
                return;
            }
            if (not $args->{q[Client]}->isa(q[Net::BitTorrent])) {
                carp q[Net::BitTorrent::Peer->new({}) requires a ]
                    . q[blessed Net::BitTorrent object in the 'Client' parameter];
                return;
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
            $_socket{refaddr $self} = $args->{q[Socket]};
            $_client{refaddr $self} = $args->{q[Client]};
            weaken $_client{refaddr $self};
            $_client{refaddr $self}->_add_connection($self, q[ro]) or return;

            #
            $_data_out{refaddr $self} = q[];
            $_data_in{refaddr $self}  = q[];

            #
            $_incoming{refaddr $self} = 1;
        }
        else {
            if ($args->{q[Address]}
                !~ m[^(?:(?:(?:25[0-5]|2[0-4][0-9]|[0-1]?[0-9]{1,2})[.]?){4}):\d+$]
                )
            {   carp
                    q[Net::BitTorrent::Peer->new({}) requires an IPv4:port 'Address'];
                return;
            }
            elsif (not defined $args->{q[Session]}) {
                carp q[Net::BitTorrent::Peer->new({}) requires a 'Session'];
                return;
            }
            elsif (not blessed $args->{q[Session]}) {
                carp
                    q[Net::BitTorrent::Peer->new({}) requires a blessed 'Session'];
                return;
            }
            elsif (not $args->{q[Session]}->isa(q[Net::BitTorrent::Session]))
            {   carp
                    q[Net::BitTorrent::Peer->new({}) requires a blessed 'Session'];
                return;
            }
            socket(my ($socket), PF_INET, SOCK_STREAM, getprotobyname(q[tcp]))
                or return;
            $self = bless \$args->{q[Address]}, $class;
            my ($host, $port) = split q[:], $args->{q[Address]}, 2;
            $_socket{refaddr $self} = $socket;

            # Set socket to non-blocking
            if (not($^O eq q[MSWin32]
                    ? ioctl($_socket{refaddr $self}, 0x8004667e,
                            pack(q[I], 1))
                    : fcntl($_socket{refaddr $self}, F_SETFL, O_NONBLOCK)
                )
                )
            {    # !!! - impossible to test failure for coverage...
                $_client{refaddr $self}->on_event(
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
            connect($_socket{refaddr $self},
                    pack_sockaddr_in($port, inet_aton($host)));

            #
            $_client{refaddr $self} = $args->{q[Session]}->_client;
            weaken $_client{refaddr $self};
            $_session{refaddr $self} = $args->{q[Session]};
            weaken $_session{refaddr $self};
            ${$_bitfield{refaddr $self}} = pack(q[b*],
                             qq[\0] x $_session{refaddr $self}->_piece_count);

            # Add initial handshake to outgoing queue
            $_data_out{refaddr $self}
                = build_handshake($_client{refaddr $self}->__build_reserved,
                                  pack(q[H*],
                                       $_session{refaddr $self}->infohash),
                                  $_client{refaddr $self}->peerid
                );
            $_data_in{refaddr $self} = q[];

            #
            $self->_send_bitfield;
            $self->_send_extended_handshake;

            #
            $_client{refaddr $self}->_add_connection($self, q[rw]) or return;
            $_incoming{refaddr $self} = 0;
        }
        if ($self) {

            # Basic Status
            ${$_am_choking{refaddr $self}}      = 1;
            ${$_am_interested{refaddr $self}}   = 0;
            ${$_peer_choking{refaddr $self}}    = 1;
            ${$_peer_interested{refaddr $self}} = 0;
            $_last_contact{refaddr $self} = time;    # lies

            #
            $requests_out{refaddr $self} = [];
            $requests_in{refaddr $self}  = [];

            # keepalive
            $_client{refaddr $self}->_schedule({Time   => time + 120,
                                                Code   => \&_send_keepalive,
                                                Object => $self
                                               }
            );

            #
            $_client{refaddr $self}->_schedule(
                                           {Time   => time + 30,
                                            Code   => \&_cancel_old_requests,
                                            Object => $self
                                           }
            );

            #
            $_client{refaddr $self}->_schedule(
                                       {Time   => time + 90,
                                        Code   => \&_disconnect_useless_peer,
                                        Object => $self
                                       }
            );

            # Shared stuff
            if ($threads::shared::threads_shared) {
                threads::shared::share($_bitfield{refaddr $self})
                    if defined $_bitfield{refaddr $self};
                threads::shared::share($_am_choking{refaddr $self});
                threads::shared::share($_am_interested{refaddr $self});
                threads::shared::share($_peer_choking{refaddr $self});
                threads::shared::share($_peer_interested{refaddr $self});
            }
            weaken($REGISTRY{refaddr $self} = $self);
        }

        #
        return $self;
    }

    # Accessors | Public | General
    sub peerid { return $peerid{refaddr +shift} }

    # Accessors | Private | General
    sub _socket   { return $_socket{refaddr +shift} }
    sub _session  { return $_session{refaddr +shift} }
    sub _bitfield { return ${$_bitfield{refaddr +shift}} }

    sub _port {
        return if defined $_[1];
        my ($self) = @_;
        return if not defined $_socket{refaddr $self};
        my ($port, undef)
            = unpack_sockaddr_in(getpeername($_socket{refaddr $self}));
        return $port;
    }

    sub _host {
        return if defined $_[1];
        my ($self) = @_;
        return if not defined $_socket{refaddr $self};
        my (undef, $packed_ip)
            = unpack_sockaddr_in(getpeername($_socket{refaddr $self}));
        return inet_ntoa($packed_ip);
    }

    # Accessors | Private | Status
    sub _peer_choking    { return ${$_peer_choking{refaddr +shift}} }
    sub _am_choking      { return ${$_am_choking{refaddr +shift}} }
    sub _peer_interested { return ${$_peer_interested{refaddr +shift}} }
    sub _am_interested   { return ${$_am_interested{refaddr +shift}} }
    sub _incoming        { return $_incoming{refaddr +shift} }

    # Methods | Private
    sub _rw {
        my ($self, $read, $write, $error) = @_;

        #
        if ($error) { weaken $self; $self->_disconnect($^E); return }
        if (defined $_session{refaddr $self}
            and $_session{refaddr $self}->status & 2)
        {   weaken $self;
            $self->_disconnect(q[Hashchecking]);
            return;
        }

        #warn $read;
        #warn $write;
        #
        my ($actual_read, $actual_write) = (0, 0);

        #
        if ($write) {
            if (length $_data_out{refaddr $self}) {
                $actual_write =
                    syswrite($_socket{refaddr $self},
                             $_data_out{refaddr $self},
                             $write, 0);
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
                    $_client{refaddr $self}->_event(q[peer_write],
                                    {Peer => $self, Length => $actual_write});
                    substr($_data_out{refaddr $self}, 0, $actual_write, q[]);
                }
            }
        }

        #
        if ($read) {
            $actual_read = sysread($_socket{refaddr $self},
                                   $_data_in{refaddr $self},
                                   $read,
                                   length($_data_in{refaddr $self})
            );
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
                if (not defined $peerid{refaddr $self}) {
                    $_client{refaddr $self}
                        ->_event(q[peer_connect], {Peer => $self});
                }
                $_client{refaddr $self}->_event(q[peer_read],
                                     {Peer => $self, Length => $actual_read});

                #my $packet;
            PACKET: while ($_data_in{refaddr $self}) {
                    my $data_len = length $_data_in{refaddr $self};

                    #
                    my $packet = parse_packet(\$_data_in{refaddr $self});

                    #
                    if (not defined $packet) {
                        if (length($_data_in{refaddr $self}) != $data_len) {

                            #warn q[Bad packet from ]. $$self;
                            #warn  $_data_in{refaddr $self};
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
                            $_last_contact{refaddr $self} = time;
                            ($_clutter{refaddr $self}{q[reserved]},
                             undef, $peerid{refaddr $self}
                            ) = @{$payload};
                            $_clutter{refaddr $self}{q[support]}
                                {q[extended protocol]} = (
                                ord(substr(
                                        $_clutter{refaddr $self}{q[reserved]},
                                        5,
                                        1
                                    )
                                    ) & 0x10
                                ) ? 1 : 0;
                            $_client{refaddr $self}
                                ->_event(q[packet_incoming_handshake],
                                        {Peer => $self, Payload => $payload});
                            if ($peerid{refaddr $self} eq
                                $_client{refaddr $self}->peerid)
                            {   weaken $self;
                                $self->_disconnect(
                                           q[...we've connected to ourself.]);
                                return;
                            }

                            #
                            if (not defined $_session{refaddr $self})
                            {    # Incoming peer
                                $_session{refaddr $self}
                                    = $_client{refaddr $self}
                                    ->_locate_session(
                                               unpack(q[H40], $payload->[1]));

                                #
                                if (not defined $_session{refaddr $self}) {
                                    weaken $self;
                                    $self->_disconnect(
                                        sprintf
                                            q[We aren't serving this torrent: %s],
                                        unpack(q[H40], $payload->[1])
                                    );
                                    return;
                                }
                                if (!$_session{refaddr $self}->status() & 1) {

=for status
        # (201=Started, 961=Force Download, 1000=finished)
        #     1 = Started
        #     2 = Checking
        #     4 = Start after check
        #     8 = Checked
        #    16 = Error
        #    32 = Paused
        #    64 = Queued
        #   128 = Loaded
        #   256 =
        #   512 = Force
        #  1000 = Complete
=cut
                                    weaken $self;
                                    $self->_disconnect(
                                        sprintf
                                            q[This torrent is loaded but stopped: %s],
                                        unpack(q[H40], $payload->[1])
                                    );
                                    return;
                                }
                                if (defined $_session{refaddr $self}
                                    and $_session{refaddr $self}->status & 2)
                                {   weaken $self;
                                    $self->_disconnect(q[Hashchecking]);
                                    return;
                                }

                                #
                                weaken $_session{refaddr $self};
                                ${$_bitfield{refaddr $self}}
                                    = pack(q[b*],
                                           qq[\0] x $_session{refaddr $self}
                                               ->_piece_count);
                                if ($threads::shared::threads_shared) {
                                    threads::shared::share(
                                                   $_bitfield{refaddr $self});
                                }

                                #
                                $_data_out{refaddr $self} .=
                                    build_handshake(
                                    $_client{refaddr $self}->__build_reserved,
                                    pack(q[H40],
                                         $_session{refaddr $self}->infohash),
                                    $_client{refaddr $self}->peerid
                                    );
                                $self->_send_bitfield;
                                $self->_send_extended_handshake;

                      # XXX - add socket as rw
                      #$_client{refaddr $self}->_add_socket(
                      #           $_socket{refaddr $self},
                      #           ($_data_out{refaddr $self} ? q[rw] : q[ro]),
                      #           sub { $self->_rw(@_); }
                      #);
                            }
                        },
                        &KEEPALIVE => sub {
                            my ($self) = @_;
                            return if !defined $_session{refaddr $self};
                            if (defined $_session{refaddr $self}
                                and $_session{refaddr $self}->status & 2)
                            {   weaken $self;
                                $self->_disconnect(q[Hashchecking]);
                                return;
                            }

                            #
                            $_client{refaddr $self}
                                ->_event(q[packet_incoming_keepalive],
                                         {Peer => $self});

                            #
                            return 1;
                        },
                        &CHOKE => sub {
                            my ($self) = @_;
                            return if !defined $_session{refaddr $self};
                            if (defined $_session{refaddr $self}
                                and $_session{refaddr $self}->status & 2)
                            {   weaken $self;
                                $self->_disconnect(q[Hashchecking]);
                                return;
                            }

                            #
                            $_client{refaddr $self}
                                ->_event(q[packet_incoming_choke],
                                         {Peer => $self});

                            #
                            ${$_peer_choking{refaddr $self}}  = 1;
                            ${$_am_interested{refaddr $self}} = 0;    # lies

                            #
                            for my $request (@{$requests_out{refaddr $self}})
                            {   my $piece = $_session{refaddr $self}
                                    ->_piece_by_index($request->{q[Index]});

                                #warn pp $piece;
                                #warn pp $request;
                                delete $piece->{q[Blocks_Requested]}
                                    ->[$request->{q[_vec_offset]}]
                                    ->{refaddr $self};
                            }

                            #
                            return $requests_out{refaddr $self} = [];
                        },
                        &UNCHOKE => sub {
                            my ($self) = @_;
                            return if !defined $_session{refaddr $self};
                            if (defined $_session{refaddr $self}
                                and $_session{refaddr $self}->status & 2)
                            {   weaken $self;
                                $self->_disconnect(q[Hashchecking]);
                                return;
                            }
                            $_last_contact{refaddr $self} = time;
                            ${$_peer_choking{refaddr $self}} = 0;
                            $_client{refaddr $self}
                                ->_event(q[packet_incoming_unchoke],
                                         {Peer => $self});
                            $self->_request_block(2);

                            #
                            #$_client{refaddr $self}->_schedule(
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
                            return if !defined $_session{refaddr $self};
                            if (defined $_session{refaddr $self}
                                and $_session{refaddr $self}->status & 2)
                            {   weaken $self;
                                $self->_disconnect(q[Hashchecking]);
                                return;
                            }
                            $_last_contact{refaddr $self} = time;

                            #
                            ${$_peer_interested{refaddr $self}} = 1;
                            $_client{refaddr $self}
                                ->_event(q[packet_incoming_interested],
                                         {Peer => $self});

                            # TODO: not this
                            $_data_out{refaddr $self} .= build_unchoke();
                            ${$_am_choking{refaddr $self}} = 0;
                            $_client{refaddr $self}
                                ->_event(q[packet_outgoing_unchoke],
                                         {Peer => $self});

                            #
                            return 1;
                        },
                        &NOT_INTERESTED => sub {
                            my ($self) = @_;
                            return if !defined $_session{refaddr $self};
                            if (defined $_session{refaddr $self}
                                and $_session{refaddr $self}->status & 2)
                            {   weaken $self;
                                $self->_disconnect(q[Hashchecking]);
                                return;
                            }
                            $_last_contact{refaddr $self} = time;
                            $_client{refaddr $self}
                                ->_event(q[packet_incoming_not_interested],
                                         {Peer => $self});
                            ${$_peer_interested{refaddr $self}} = 1;
                            ${$_am_choking{refaddr $self}}      = 1;
                            return 1;
                        },
                        &HAVE => sub {
                            my ($self, $index) = @_;
                            return if !defined $_session{refaddr $self};
                            if (defined $_session{refaddr $self}
                                and $_session{refaddr $self}->status & 2)
                            {   weaken $self;
                                $self->_disconnect(q[Hashchecking]);
                                return;
                            }
                            $_client{refaddr $self}
                                ->_event(q[packet_incoming_have],
                                         {Peer => $self, Index => $index});
                            vec(${$_bitfield{refaddr $self}}, $index, 1) = 1;
                            $self->_check_interest;
                            if (${$_am_interested{refaddr $self}}
                                and not ${$_peer_choking{refaddr $self}})
                            {   $self->_request_block;
                            }
                        },
                        &BITFIELD => sub {
                            my ($self, $payload) = @_;
                            return if !defined $_session{refaddr $self};
                            if (defined $_session{refaddr $self}
                                and $_session{refaddr $self}->status & 2)
                            {   weaken $self;
                                $self->_disconnect(q[Hashchecking]);
                                return;
                            }

                            #
                            ${$_bitfield{refaddr $self}}
                                = $packet->{q[Payload]};

                            #
                            $_client{refaddr $self}
                                ->_event(q[packet_incoming_bitfield],
                                         {Peer => $self});

                            #
                            return $self->_check_interest;
                        },
                        &REQUEST => sub {
                            my ($self, $payload) = @_;
                            return if !defined $_session{refaddr $self};
                            if (defined $_session{refaddr $self}
                                and $_session{refaddr $self}->status & 2)
                            {   weaken $self;
                                $self->_disconnect(q[Hashchecking]);
                                return;
                            }

                            #
                            $_client{refaddr $self}->_event(
                                                   q[packet_incoming_request],
                                                   {Peer   => $self,
                                                    Index  => $payload->[0],
                                                    Offset => $payload->[1],
                                                    Length => $payload->[2]
                                                   }
                            );

                            #
                            if (not @{$requests_in{refaddr $self}}) {
                                $_client{refaddr $self}->_schedule(
                                                 {Time   => time + 15,
                                                  Code   => \&_fill_requests,
                                                  Object => $self
                                                 }
                                );
                            }

                            #
                            return push @{$requests_in{refaddr $self}},
                                {Index  => $payload->[0],
                                 Offset => $payload->[1],
                                 Length => $payload->[2]
                                };
                        },
                        &PIECE  => \&__handle_piece,
                        &CANCEL => sub {
                            my ($self, $payload) = @_;
                            return if !defined $_session{refaddr $self};
                            if (defined $_session{refaddr $self}
                                and $_session{refaddr $self}->status & 2)
                            {   weaken $self;
                                $self->_disconnect(q[Hashchecking]);
                                return;
                            }
                            $_last_contact{refaddr $self} = time;
                            my ($index, $offset, $length) = @$payload;
                            $_client{refaddr $self}->_event(
                                                    q[packet_incoming_cancel],
                                                    {Index  => $index,
                                                     Offset => $offset,
                                                     Length => $length,
                                                     Peer   => $self
                                                    }
                            );
                            for my $x (
                                 reverse 0 .. $#{$requests_in{refaddr $self}})
                            {
                                if (($requests_in{refaddr $self}->[$x]
                                     ->{q[Index]} == $index
                                    )
                                    and ($requests_in{refaddr $self}->[$x]
                                         ->{q[Offset]} == $offset)
                                    and ($requests_in{refaddr $self}->[$x]
                                         ->{q[Length]} == $length)
                                    )
                                {   splice(@{$requests_in{refaddr $self}},
                                           $x, 1);
                                }
                            }

                            #warn q[Remaining requests: ]
                            #        . pp $requests_in{refaddr $self};
                            return 1;
                        },
                        &EXTPROTOCOL => sub {
                            my ($self, $payload) = @_;
                            return if !defined $_session{refaddr $self};
                            if (defined $_session{refaddr $self}
                                and $_session{refaddr $self}->status & 2)
                            {   weaken $self;
                                $self->_disconnect(q[Hashchecking]);
                                return;
                            }
                            $_last_contact{refaddr $self} = time;
                            my ($id, $packet) = @{$payload};
                            if ($packet) {
                                if ($id == 0) {
                                    if (defined $_client{refaddr $self}->_dht
                                        and defined $packet->{q[p]})
                                    {   my (undef, $packed_ip)
                                            = unpack_sockaddr_in(
                                                   getpeername(
                                                       $_socket{refaddr $self}
                                                   )
                                            );
                                        my $node
                                            = sprintf(q[%s:%d],
                                                      inet_ntoa($packed_ip),
                                                      $packet->{q[p]});
                                        $_client{refaddr $self}
                                            ->_dht->_add_node($node);
                                    }
                                }
                                $_client{refaddr $self}->_event(
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
        $_client{refaddr $self}->_add_connection($self,
                                  ($_data_out{refaddr $self} ? q[rw] : q[ro]))
            if $_socket{refaddr $self};

        #
        return ($actual_read, $actual_write);
    }

    sub __handle_piece {
        my ($self, $payload) = @_;
        if (defined $_session{refaddr $self}
            and $_session{refaddr $self}->status & 2)
        {   weaken $self;
            $self->_disconnect(q[Hashchecking]);
            return;
        }

        #return if $_session{refaddr $self}->status () & 64;
        return if !defined $_session{refaddr $self};
        $_last_contact{refaddr $self} = time;

        #
        my ($index, $offset, $data) = @{$payload};
        my $length = length($data);

        #
        my ($request) = grep {
                    ($_->{q[Index]} == $index)
                and ($_->{q[Offset]} == $offset)
                and ($_->{q[Length]} == $length)
        } @{$requests_out{refaddr $self}};

        #
        if (not defined $request) {
            weaken $self;
            $self->_disconnect(q[Handed a piece we never asked for.]);
            return;
        }

        # Even if the block is junk...
        $_session{refaddr $self}->_add_downloaded($request->{q[Length]});

        #
        @{$requests_out{refaddr $self}} = grep {
                   ($_->{q[Index]} != $index)
                or ($_->{q[Offset]} != $offset)
                or ($_->{q[Length]} != $length)
        } @{$requests_out{refaddr $self}};

        #
        $_client{refaddr $self}->_event(q[packet_incoming_block],
                                        {Index  => $index,
                                         Offset => $offset,
                                         Length => $length,
                                         Peer   => $self
                                        }
        );

        # perlref
        my $piece = $_session{refaddr $self}->_piece_by_index($index);

        #
        if (not defined $piece) {
            weaken $self;
            $self->_disconnect(q[sent a block to a non-existant piece]);
            return;
        }

        #
        if (not $_session{refaddr $self}->_write_data($index, $offset, \$data)
            )
        {    #warn $^E;

            #$_client{refaddr $self}->_del_socket($_socket{refaddr $self});
            return;
        }

        #
        #warn q[Before change ] . pp $piece;
        $piece->{q[Blocks_Recieved]}->[$request->{q[_vec_offset]}] = 1;

        #
        #carp sprintf q[Waking sleeping piece up: %d],
        #    $request->{q[Index]}
        #    if $piece->{q[Slow]};
        $piece->{q[Slow]}  = 0;
        $piece->{q[Touch]} = time;

        #
        # cancel duplicate requests with other peers | Endgame stuff
        for my $peer (@{$_session{refaddr $self}->_peers}) {
            for my $x (reverse 0 .. $#{$requests_out{refaddr $peer}}) {
                if (    (defined $requests_out{refaddr $peer}->[$x])
                    and
                    ($requests_out{refaddr $peer}->[$x]->{q[Index]} == $index)
                    and ($requests_out{refaddr $peer}->[$x]->{q[Offset]}
                         == $offset)
                    and ($requests_out{refaddr $peer}->[$x]->{q[Length]}
                         == $length)
                    )
                {   $_data_out{refaddr $peer } .=
                        build_cancel($request->{q[Index]},
                                     $request->{q[Offset]},
                                     $request->{q[Length]}
                        );

                    #
                    $_client{refaddr $self}->_event(q[packet_outgoing_cancel],
                                                    {Index  => $index,
                                                     Offset => $offset,
                                                     Length => $length,
                                                     Peer   => $peer
                                                    }
                    );

                    #
                    $_client{refaddr $self}->_add_connection($peer, q[rw]);

                    #
                    splice(@{$requests_out{refaddr $peer }}, $x, 1);

                    #
                    last;
                }
            }
        }

        #warn q[After change  ] . pp $piece;
        #
        if (not grep { $_ == 0 } @{$piece->{q[Blocks_Recieved]}}) {
            if ($_session{refaddr $self}->_check_piece_by_index($index)
                and defined $_session{refaddr $self})
            {   my @_peers = @{$_session{refaddr $self}->_peers};
                for my $p (@_peers) {
                    $_data_out{$p} .= build_have($index);
                    $_client{refaddr $self}->_add_connection($p, q[rw]);
                }
            }
        }

        #
        if ($self->_check_interest) {
            if (not $self->_request_block) {    # Try again later...
                $_client{refaddr $self}->_schedule(
                    {   Time => time + 30,
                        Code => sub {
                            my $s = shift;
                            return if not defined $s;
                            if ($s->_check_interest) {
                                if (not $self->_request_block) {
                                    $_client{refaddr $self}->_schedule(
                                        {   Time => time + 30,
                                            Code => sub {
                                                my $s = shift;
                                                return
                                                    if not defined $s;
                                                if ($s->_check_interest) {
                                                    $s->_request_block;
                                                }
                                            },
                                            Object => $self
                                        }
                                    );
                                }
                            }
                        },
                        Object => $self
                    }
                );
            }
        }

        #
        return 1;
    }

    sub _check_interest {
        my ($self) = @_;

        #
        if (not defined $_session{refaddr $self}) { return; }
        if (defined $_session{refaddr $self}
            and $_session{refaddr $self}->status & 2)
        {   weaken $self;
            $self->_disconnect(q[Hashchecking]);
            return;
        }

        #
        my $interesting  = ${$_am_interested{refaddr $self}};
        my $session_have = $_session{refaddr $self}->bitfield();
        my $session_want = $_session{refaddr $self}->_wanted();
        my $relevence    = ${$_bitfield{refaddr $self}} & $session_want;

#warn sprintf
#    q[pieces:%d|phave:%d|ihave:%d|iwant:%d|rel:%d|int?:%d v. %d | 1st: %d | %s],
#    $_session{refaddr $self}->_piece_count,
#    sum(split(q[], unpack(q[b*], (${$_bitfield{refaddr $self}})))),
#    sum(split(q[], unpack(q[b*], ($session_have)))),
#    sum(split(q[], unpack(q[b*], ($session_want)))),
#    sum(split(q[], unpack(q[b*], $relevence))),
#    ${$_am_interested{refaddr $self}},
#    ((index(unpack(q[b*], $relevence), 1, 0) != -1) ? 1 : 0),
#    index(unpack(q[b*], $relevence), 1, 0),
#    $$self;
        $interesting = (index(unpack(q[b*], $relevence), 1, 0) != -1) ? 1 : 0;

        #
        if ($interesting and not ${$_am_interested{refaddr $self}}) {
            ${$_am_interested{refaddr $self}} = 1;
            $_data_out{refaddr $self} .= build_interested;
            $_client{refaddr $self}
                ->_event(q[packet_outgoing_interested], {Peer => $self});
        }
        elsif (not $interesting and ${$_am_interested{refaddr $self}}) {
            ${$_am_interested{refaddr $self}} = 0;
            $_data_out{refaddr $self} .= build_not_interested;
            $_client{refaddr $self}
                ->_event(q[packet_outgoing_not_interested], {Peer => $self});
        }

        #
        return ${$_am_interested{refaddr $self}};
    }

    sub _disconnect_useless_peer {
        my ($self) = @_;
        return if not defined $self;
        if (defined $_session{refaddr $self}
            and $_session{refaddr $self}->status & 2)
        {   weaken $self;
            $self->_disconnect(q[Hashchecking]);
            return;
        }

        #warn sprintf q[Pre  I am %schoked and %sinterested | %s | %s],
        #    (${$_peer_choking{refaddr $self}}  ? q[] : q[not ]),
        #    (${$_am_interested{refaddr $self}} ? q[] : q[not ]),
        #    pp($requests_out{refaddr $self}), $$self;
        #
        #if (    (not ${$_peer_choking{refaddr $self}})
        #    and  ($#{$requests_out{refaddr $self}} < 16))
        #{   for (($#{$requests_out{refaddr $self}}) .. 20) {
        #        $self->_request_block();
        #    }
        #}
        if ($_last_contact{refaddr $self} < (time - (5 * 60))) {
            weaken $self;
            $self->_disconnect(q[No contact from peer in 5 min]);
            return;
        }
        if (    ${$_peer_choking{refaddr $self}}
            and ${$_am_interested{refaddr $self}})
        {    # XXX - send uninterested?
            $self->_check_interest;
        }
        if (    (${$_peer_choking{refaddr $self}})
            and (not ${$_am_interested{refaddr $self}})
            and (not ${$_peer_interested{refaddr $self}}))
        {   weaken $self;
            $self->_disconnect(
                        q[Useless peer (Not interested and not interesting.)])

# if ${$_am_interested{refaddr $self} and ${$_peer_interested{refaddr $self}}};
                ;
            return;
        }

        #
        $_client{refaddr $self}->_schedule(
                                       {Time   => time + 90,
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
        return if not defined $_socket{refaddr $self};
        if (defined $_session{refaddr $self}
            and $_session{refaddr $self}->status & 2)
        {   weaken $self;
            $self->_disconnect(q[Hashchecking]);
            return;
        }

        #
        $_client{refaddr $self}->_schedule({Time   => time + 60,
                                            Code   => \&_cancel_old_requests,
                                            Object => $self
                                           }
        );

        #
        my $canceled = 0;

        #
        if (@{$requests_out{refaddr $self}} == []) {
            return;
        }

        #
        #warn sprintf q[Post I am %schoked and %sinterested | %s],
        #    (${$_peer_choking{refaddr $self}}  ? q[] : q[not ]),
        #    (${$_am_interested{refaddr $self}} ? q[] : q[not ]),
        #    pp $requests_out{refaddr $self};
        #
        for my $i (reverse(0 .. $#{$requests_out{refaddr $self}})) {
            my $request = $requests_out{refaddr $self}->[$i];

            #
            if (time <= ($request->{q[Timestamp]} + 35)) {
                next;
            }

            #
            my $piece = $_session{refaddr $self}
                ->_piece_by_index($request->{q[Index]});

            #
            delete $piece->{q[Blocks_Requested]}->[$request->{q[_vec_offset]}]
                ->{refaddr $self};

            #
            if ($piece->{q[Touch]} <= time - 60) {
                $piece->{q[Slow]} = 1;

                #warn sprintf q[Putting slow piece to sleep: %d],
                #    $request->{q[Index]}
                #    if $piece->{q[Slow]};
            }

            #
            $_data_out{refaddr $self} .=
                build_cancel($request->{q[Index]}, $request->{q[Offset]},
                             $request->{q[Length]});

            #
            $_client{refaddr $self}->_event(q[packet_outgoing_cancel],
                                            {Index  => $request->{q[Index]},
                                             Offset => $request->{q[Offset]},
                                             Length => $request->{q[Length]},
                                             Peer   => $self
                                            }
            );

            #
            splice(@{$requests_out{refaddr $self}}, $i, 1);

            #
            $canceled++;
        }
        if ($canceled) {

            #warn sprintf q[~~~~~~~~~~~~~~~~~~ Canceled %d pieces], $canceled;
            $_client{refaddr $self}->_add_connection($self, q[rw]);
        }

        #
        return $canceled;
    }

    sub _request_block {
        my ($self, $_range) = @_;
        if (defined $_session{refaddr $self}
            and $_session{refaddr $self}->status & 2)
        {   weaken $self;
            $self->_disconnect(q[Hashchecking]);
            return;
        }

        #
        return if not defined $_socket{refaddr $self};

        #
        return if ${$_peer_choking{refaddr $self}};

        #return if $_session{refaddr $self}->status() ^ 1; # Stopped
        return if $_session{refaddr $self}->status() & 32;    # Paused
        return if $_session{refaddr $self}->status() & 2;     # Hash checking

=for status
        # (201=Started, 961=Force Download, 1000=finished)
        #     1 = Started
        #     2 = Checking
        #     4 = Start after check
        #     8 = Checked
        #    16 = Error
        #    32 = Paused
        #    64 = Queued
        #   128 = Loaded
        #   256 =
        #   512 = Force
        #  1000 = Complete
=cut

        #
        my $return = 0;

        #warn sprintf q[for (%d .. %d) { [...] }],
        #    max(1, scalar(@{$requests_out{refaddr $self}})),
        #    max(3, int((2**21) / $_session{refaddr $self}->_piece_length));
        # ~2M/peer or at least three requests
        my $range
            = defined $_range
            ? [1 .. $_range]
            : [max(1, scalar(@{$requests_out{refaddr $self}})) .. max(
                    12, int((2**21) / $_session{refaddr $self}->_piece_length)
               )
            ];

        #warn sprintf q[$range == [ %s ]], join q[, ], @$range;
    REQUEST:
        for (@$range) {

            #
            my $piece = $_session{refaddr $self}->_pick_piece($self);

            #
            if ($piece) {

            #warn q[$self == ] . $self->_as_string;
            #warn pp \%requests_out;
            #warn q[Num requests: ] . scalar(@{$requests_out{refaddr $self}});
            #warn q[Before Request: ] . pp $piece;
            #warn q[Endgame? ] . $piece->{q[Endgame]};
                my $vec_offset;
                if ($piece->{q[Endgame]}) {

           #warn q[Endgame!];
           # This next bit selects the least requested block (max 3 requests),
           #   makes sure this peer isn't already sitting on this request
           #   and... I just lost my train of thought; It's Friday afternoon.
           #   Regardless of how it looks, it does what I mean.
                    my $tmp_index = -1;
                    my %temp      = map {
                        $tmp_index++;
                        (        ($_ < 5)
                             and
                             ($piece->{q[Blocks_Recieved]}->[$tmp_index] == 0)
                            )
                            ? ($tmp_index => $_)
                            : ()
                    } @{$piece->{q[Blocks_Recieved]}};

                    #use Data::Dump qw[pp];
                    #warn q[%temp indexes: ] . pp \%temp;
                INDEX:
                    for my $index (sort { $temp{$a} <=> $temp{$b} }
                                   sort { $a <=> $b } keys %temp)
                    {    #warn q[$index: ] . $index;
                        if (not grep {

                           #warn sprintf q[recieved: %d | matches[i:%d|o:%d]],
                           #    ($piece->{q[Blocks_Recieved]}->[$index]),
                           #    ($piece->{q[Index]} == $_->{q[Index]}),
                           #    ($index == $_->{q[_vec_offset]});
                                (defined $piece->{q[Blocks_Requested]}
                                     ->[$index]->{refaddr $self})
                                    and (
                                    ($piece->{q[Blocks_Recieved]}->[$index])
                                    or (($piece->{q[Index]} == $_->{q[Index]})
                                        and ($index == $_->{q[_vec_offset]}))
                                    )
                            } @{$requests_out{refaddr $self}}
                            )
                        {   $vec_offset = $index;
                            last INDEX;
                        }
                    }
                }
                else {

                    #warn pp $piece;
                BLOCK:
                    for my $i (0 .. $#{$piece->{q[Blocks_Requested]}}) {
                        if (not(keys %{$piece->{q[Blocks_Requested]}->[$i]}))
                        {   $vec_offset = $i;

                            #use Data::Dump qw[pp];
                            #warn pp $piece->{q[Blocks_Requested]}->[$i];
                            last BLOCK;
                        }
                    }
                }

                #
                if (not defined $vec_offset or $vec_offset == -1) {

                    #warn sprintf
                    #    q[Piece has been fully requested: req:%s|rec:%s],
                    #    join(q[],
                    #         map { scalar keys %$_ }
                    #             @{$piece->{q[Blocks_Requested]}}),
                    #    join(q[], @{$piece->{q[Blocks_Recieved]}});
                    # xxx - pick a different piece?
                    # xxx - Honestly, this piece shouldn't have been returned
                    #       from _pick_piece in the first place...
                    last REQUEST;
                }

                # May already be requested if in endgame mode...
                $piece->{q[Blocks_Requested]}->[$vec_offset]->{refaddr $self}
                    = $self;
                weaken $piece->{q[Blocks_Requested]}->[$vec_offset]
                    ->{refaddr $self};

      #use Data::Dump qw[pp];
      #warn q[After request: ] . pp $_session{refaddr $self}->_piece_by_index(
      #                $piece->{q[Index]}
      #);
      #
                my $offset = $vec_offset * $piece->{q[Block_Length]};
                my $length = (
                          (($vec_offset + 1) == $piece->{q[Block_Count]})
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
                push @{$requests_out{refaddr $self}}, $request;

                #
                $_client{refaddr $self}->_event(q[packet_outgoing_request],
                                                {Index  => $piece->{q[Index]},
                                                 Offset => $offset,
                                                 Length => $length,
                                                 Peer   => $self
                                                }
                );

                #
                $_client{refaddr $self}->_add_connection($self, q[rw]);
                $_data_out{refaddr $self}
                    .= build_request($piece->{q[Index]}, $offset, $length);

                #
                $return++;
            }
            else {
                last REQUEST;
            }
        }
        return $return;
    }

    sub _send_bitfield {    # XXX - or fast packet
        my ($self) = @_;
        if (defined $_session{refaddr $self}
            and $_session{refaddr $self}->status & 2)
        {   weaken $self;
            $self->_disconnect(q[Hashchecking]);
            return;
        }

        #
        return if not defined $_socket{refaddr $self};

        #
        my @have = split q[], unpack q[b*],
            $_session{refaddr $self}->bitfield;

        #
        if (((sum(@have) * 8) < $#have)) {
            $_data_out{refaddr $self}
                .= build_bitfield(pack q[B*], join q[], @have);
            $_client{refaddr $self}
                ->_event(q[packet_outgoing_bitfield], {Peer => $self});
        }
        else {
            for my $index (0 .. $#have) {
                if ($have[$index]) {
                    $_data_out{refaddr $self} .= build_have($index);
                    $_client{refaddr $self}->_event(q[packet_outgoing_have],
                                            {Peer => $self, Index => $index});
                }
            }
        }

        #
        return 1;
    }

    sub _send_extended_handshake {
        my ($self) = @_;
        if (defined $_session{refaddr $self}
            and $_session{refaddr $self}->status & 2)
        {   weaken $self;
            $self->_disconnect(q[Hashchecking]);
            return;
        }

        #
        return if not defined $_socket{refaddr $self};
        return
            if not $_clutter{refaddr $self}{q[support]}{q[extended protocol]};

        #
        my ($_peerport, $_packed_ip)
            = unpack_sockaddr_in(getpeername($_socket{refaddr $self}));

        #
        return if not $_packed_ip;
        return if not $_client{refaddr $self}->_dht;
        return if not $_client{refaddr $self}->_dht->_port;
        return $_data_out{refaddr $self} .= build_extended(
            0,    # handshake
            {m => {ut_pex => 1, q[µT_PEX] => 2},
             (    # is incoming ? ():
                (p => $_client{refaddr $self}->_dht->_port)    # dht port
             ),
             v      => $Net::BitTorrent::Version::PRODUCT_TOKEN,
             yourip => $_packed_ip,
             reqq => 30    # XXX - Lies.  It's on my todo list...
                   # reqq == An integer, the number of outstanding request messages
                   # this client supports without dropping any.  The default in in
                   # libtorrent is 2050.
            }
        );
    }

    sub _send_keepalive {
        my ($self) = @_;
        if (defined $_session{refaddr $self}
            and $_session{refaddr $self}->status & 2)
        {   weaken $self;
            $self->_disconnect(q[Hashchecking]);
            return;
        }

        #
        return if not defined $self;
        return if not defined $_socket{refaddr $self};

        #
        $_client{refaddr $self}->_schedule({Time   => time + 120,
                                            Code   => \&_send_keepalive,
                                            Object => $self
                                           }
        );

        #
        $_client{refaddr $self}
            ->_event(q[packet_outgoing_keepalive], {Peer => $self});

        #
        return $_data_out{refaddr $self} .= build_keepalive();
    }

    sub _fill_requests {
        my ($self) = @_;

        #
        return if not defined $self;
        return if not @{$requests_in{refaddr $self}};
        return if ${$_am_choking{refaddr $self}};

        #
        $_client{refaddr $self}->_schedule({Time   => time + 60,
                                            Code   => \&_fill_requests,
                                            Object => $self
                                           }
        );

        #
        while ((length($_data_out{refaddr $self}) < 2**15)
               and @{$requests_in{refaddr $self}})
        {   my $request = shift @{$requests_in{refaddr $self}};
            next
                unless $_session{refaddr $self}
                    ->_check_piece_by_index($request->{q[Index]});

            # Accounting...
            $_session{refaddr $self}->_add_uploaded($request->{q[Length]});

            #
            $_client{refaddr $self}->_event(q[packet_outgoing_block],
                                            {Index  => $request->{q[Index]},
                                             Offset => $request->{q[Offset]},
                                             Length => $request->{q[Length]},
                                             Peer   => $self
                                            }
            );
            $_data_out{refaddr $self} .=
                build_piece($request->{q[Index]},
                            $request->{q[Offset]},
                            $_session{refaddr $self}->_read_data(
                                                        $request->{q[Index]},
                                                        $request->{q[Offset]},
                                                        $request->{q[Length]}
                            )
                );

            #
            if (rand(3) >= 2) { $self->_send_choke; }
        }

        #
        $_client{refaddr $self}->_add_connection($self, q[rw]) or return;

        #
        return 1;
    }

    sub _send_choke {
        my ($self) = @_;
        if (defined $_session{refaddr $self}
            and $_session{refaddr $self}->status & 2)
        {   weaken $self;
            $self->_disconnect(q[Hashchecking]);
            return;
        }

        #
        return if ${$_am_choking{refaddr $self}} == 1;

        #
        $requests_in{refaddr $self} = [];
        ${$_am_choking{refaddr $self}}      = 1;
        ${$_peer_interested{refaddr $self}} = 0;

        #
        $_data_out{refaddr $self} .= build_choke();

        #
        $_client{refaddr $self}
            ->_event(q[packet_outgoing_choke], {Peer => $self});

        #
        $_client{refaddr $self}->_add_connection($self, q[rw]) or return;

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
        $_client{refaddr $self}->_remove_connection($self);

        #
        #weaken $self;
        #use Devel::Peek qw[SvREFCNT Dump];
        #Dump($self);
        #use Devel::Refcount qw( refcount );
        #warn sprintf "SvREFCNT=%d, refcount=%d",
        #  SvREFCNT($self), refcount($self);
        #
        $_client{refaddr $self}
            ->_event(q[peer_disconnect], {Peer => $self, Reason => $reason});
        shutdown($_socket{refaddr $self}, 2)
            if defined $_socket{refaddr $self};    # safer than close()
                                                   #
        delete $_socket{refaddr $self};

        #
        return 1;
    }

    sub _as_string {
        my ($self, $advanced) = @_;
        my $dump
            = !$advanced
            ? $$self
            : q[TODO];
        return print STDERR qq[$dump\n] unless defined wantarray;
        return $dump;
    }

    sub CLONE {
        for my $_oID (keys %REGISTRY) {

            #  look under oID to find new, cloned reference
            my $_obj = $REGISTRY{$_oID};
            my $_nID = refaddr $_obj;

            #  relocate data
            for (@CONTENTS) {
                $_->{$_nID} = $_->{$_oID};
                delete $_->{$_oID};
            }
            weaken $_client{$_nID};
            weaken $_session{$_nID};

            #  update he weak refernce to the new, cloned object
            weaken($REGISTRY{$_nID} = $_obj);
            delete $REGISTRY{$_oID};
        }
        return 1;
    }

    # Destructor
    DESTROY {
        my ($self) = @_;

        #warn q[Goodbye, ] . $$self;
        # remove incomplete requests
        if ($_session{refaddr $self}) {
            for my $request (@{$requests_out{refaddr $self}}) {
                my $piece = $_session{refaddr $self}
                    ->_piece_by_index($request->{q[Index]});
                delete $piece->{q[Blocks_Requested]}
                    ->[$request->{q[_vec_offset]}]->{refaddr $self};
            }
        }

        # Clean all data
        for (@CONTENTS) {
            delete $_->{refaddr $self};
        }
        delete $REGISTRY{refaddr $self};

        #
        return 1;
    }
    1;
}

=pod

=head1 NAME

Net::BitTorrent::Peer - Remote BitTorrent Peer

=head1 Description

L<Net::BitTorrent::Peer|Net::BitTorrent::Peer> represents a single peer
connection.

=head1 Constructor

=over

=item C<new ( { [ARGS] } )>

Creates a L<Net::BitTorrent::Peer|Net::BitTorrent::Peer> object.  This
constructor should not be used directly.

=back

=head1 Methods

I<This list only contains the most common, public accessors.>

=over

=item C<peerid>

Returns the Peer ID used to identify this peer.

See also: theory.org (http://tinyurl.com/4a9cuv)

=back

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
