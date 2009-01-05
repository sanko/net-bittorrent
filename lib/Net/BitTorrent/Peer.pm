#!/usr/bin/perl -w
package Net::BitTorrent::Peer;
{
    use strict;
    use warnings;
    use Carp qw[carp];
    use Scalar::Util qw[blessed weaken refaddr];
    use List::Util qw[sum max];
    use Socket qw[/F_INET/ SOMAXCONN SOCK_STREAM /inet_/ /pack_sockaddr_in/];
    use Fcntl qw[F_SETFL O_NONBLOCK];
    use version qw[qv];
    our $SVN = q[$Id$];
    our $UNSTABLE_RELEASE = 3; our $VERSION = sprintf(($UNSTABLE_RELEASE ? q[%.3f_%03d] : q[%.3f]), (version->new((qw$Rev$)[1])->numify / 1000), $UNSTABLE_RELEASE);
    use lib q[../../../lib];
    use Net::BitTorrent::Protocol qw[:build parse_packet :types];
    use Net::BitTorrent::Util qw[:bencode];
    use Net::BitTorrent::Version;
    my (@CONTENTS)
        = \my (%_client,           %_socket,          %_torrent,
               %_data_out,         %_data_in,         %peerid,
               %_bitfield,         %_am_choking,      %_am_interested,
               %_peer_choking,     %_peer_interested, %_incoming,
               %requests_out,      %requests_in,      %_last_contact,
               %_incoming_fastset, %_reserved_bytes,  %_source
        );
    my %REGISTRY;

    sub new {
        my ($class, $args) = @_;
        my $self = undef;
        if (not defined $args) {
            carp q[Net::BitTorrent::Peer->new({}) requires ]
                . q[parameters a hashref];
            return;
        }
        if (    !$args->{q[Socket]}
            and !$args->{q[Address]})
        {   carp <<'END'; return; }
Net::BitTorrent::Peer->new({}) requires either...
  - an 'Address' (IPv4:port for new, outgoing connections)
    or
  - a 'Socket' (GLOB-type for newly accepted incoming connections)
END
        if ($args->{q[Socket]}) {
            if (ref($args->{q[Socket]}) ne q[GLOB]) {
                carp
                    q[Net::BitTorrent::Peer->new({}) requires a GLOB-type socket];
                return;
            }
            if (   (!$args->{q[Client]})
                || (!blessed $args->{q[Client]})
                || (!$args->{q[Client]}->isa(q[Net::BitTorrent])))
            {   carp
                    q[Net::BitTorrent::Peer->new({}) requires a blessed Net::BitTorrent object in the 'Client' parameter];
                return;
            }
            my ($port, $packed_ip)
                = unpack_sockaddr_in(getpeername($args->{q[Socket]}));
            my $ok = $args->{q[Client]}->_event(q[ip_filter],
                {Address => sprintf(q[%s:%d], inet_ntoa($packed_ip), $port)});
            if (defined $ok and $ok == 0) { return; }
            my $ip = inet_ntoa($packed_ip);
            if (scalar(
                    grep {
                               $_->{q[Object]}->isa(q[Net::BitTorrent::Peer])
                            && $_->{q[Object]}->_host
                            && $_->{q[Object]}->_host eq $ip
                            && $_->{q[Object]}->_port
                            && $_->{q[Object]}->_port eq $port
                        } values %{$args->{q[Client]}->_connections}
                ) >= $args->{q[Client]}->_connections_per_host
                )
            {   shutdown($args->{q[Socket]}, 2);
                close($args->{q[Socket]});
                return;
            }
            $self
                = bless \sprintf(q[%s:%d], $ip, $port),
                $class;
            $_socket{refaddr $self} = $args->{q[Socket]};
            $_client{refaddr $self} = $args->{q[Client]};
            weaken $_client{refaddr $self};
            $_client{refaddr $self}->_add_connection($self, q[ro]) or return;
            $_data_out{refaddr $self} = q[];
            $_data_in{refaddr $self}  = q[];
            $_incoming{refaddr $self} = 1;
            $_source{refaddr $self}   = q[Incoming];
        }
        else {
            if ($args->{q[Address]}
                !~ m[^(?:(?:(?:25[0-5]|2[0-4][0-9]|[0-1]?[0-9]{1,2})[.]?){4}):\d+$]
                )
            {   carp
                    q[Net::BitTorrent::Peer->new({}) requires an IPv4:port 'Address'];
                return;
            }
            if (   (!$args->{q[Torrent]})
                || (!blessed $args->{q[Torrent]})
                || (!$args->{q[Torrent]}->isa(q[Net::BitTorrent::Torrent])))
            {   carp
                    q[Net::BitTorrent::Peer->new({}) requires a blessed 'Torrent'];
                return;
            }
            if (!$args->{q[Source]}) {
                carp
                    q[Net::BitTorrent::Peer->new({}) would like to know where this peer info is from];
                return;
            }
            my $half_open = grep {
                $_->{q[Object]}->isa(q[Net::BitTorrent::Peer])
                    && !$_->{q[Object]}->_torrent
            } values %{$args->{q[Torrent]}->_client->_connections};
            if ($half_open >= $args->{q[Torrent]}->_client->_half_open) {

                #warn sprintf q[%d half open sockets!], $half_open;
                return;
            }
            if (scalar($args->{q[Torrent]}->peers)
                >= $args->{q[Torrent]}->_client->_peers_per_torrent)
            {   return;
            }
            my ($host, $port) = split q[:], $args->{q[Address]}, 2;
            if (scalar(
                    grep {
                               $_->{q[Object]}->isa(q[Net::BitTorrent::Peer])
                            && $_->{q[Object]}->_host
                            && $_->{q[Object]}->_host eq $host
                            && $_->{q[Object]}->_port
                            && $_->{q[Object]}->_port eq $port
                        } values %{$args->{q[Torrent]}->_client->_connections}
                ) >= $args->{q[Torrent]}->_client->_connections_per_host
                )
            {   return;
            }
            socket(my ($socket), PF_INET, SOCK_STREAM, getprotobyname(q[tcp]))
                or return;
            $self = bless \$args->{q[Address]}, $class;
            $_socket{refaddr $self} = $socket;
            if (not($^O eq q[MSWin32]    # set non blocking
                    ? ioctl($_socket{refaddr $self}, 0x8004667e,
                            pack(q[I], 1))
                    : fcntl($_socket{refaddr $self}, F_SETFL, O_NONBLOCK)
                )
                )
            {   return;
            }
            connect($_socket{refaddr $self},
                    pack_sockaddr_in($port, inet_aton($host)));
            $_client{refaddr $self} = $args->{q[Torrent]}->_client;
            weaken $_client{refaddr $self};
            $_torrent{refaddr $self} = $args->{q[Torrent]};
            weaken $_torrent{refaddr $self};
            ${$_bitfield{refaddr $self}}
                = pack(q[b*], qq[\0] x $_torrent{refaddr $self}->piece_count);
            my %_payload = (
                 Reserved => $_client{refaddr $self}->_build_reserved,
                 Infohash => pack(q[H40], $_torrent{refaddr $self}->infohash),
                 PeerID   => $_client{refaddr $self}->peerid
            );
            $_data_out{refaddr $self} .=
                build_handshake($_payload{q[Reserved]},
                                $_payload{q[Infohash]},
                                $_payload{q[PeerID]}
                );
            $_client{refaddr $self}->_event(q[outgoing_packet],
                                            {Peer    => $self,
                                             Payload => \%_payload,
                                             Type    => HANDSHAKE
                                            }
            );
            $_data_in{refaddr $self} = q[];
            $_client{refaddr $self}->_add_connection($self, q[rw]) or return;
            $_incoming{refaddr $self} = 0;
            $_source{refaddr $self}   = $args->{q[Source]};
        }
        if ($self) {
            ${$_am_choking{refaddr $self}}      = 1;
            ${$_am_interested{refaddr $self}}   = 0;
            ${$_peer_choking{refaddr $self}}    = 1;
            ${$_peer_interested{refaddr $self}} = 0;
            $_last_contact{refaddr $self} = time;    # lies
            $requests_out{refaddr $self}  = [];
            $requests_in{refaddr $self}   = [];
            $_client{refaddr $self}->_schedule({Time   => time + 120,
                                                Code   => \&_send_keepalive,
                                                Object => $self
                                               }
            );
            $_client{refaddr $self}->_schedule(
                                           {Time   => time + 30,
                                            Code   => \&_cancel_old_requests,
                                            Object => $self
                                           }
            );
            $_client{refaddr $self}->_schedule(
                                       {Time   => time + 90,
                                        Code   => \&_disconnect_useless_peer,
                                        Object => $self
                                       }
            );
            $_client{refaddr $self}->_schedule(
                {   Time => time + 30,
                    Code => sub {
                        my $s = shift;
                        if (!$peerid{refaddr $s}) {
                            weaken $s;
                            $s->_disconnect(
                                  q[Failed to complete handshake within 30s]);
                        }
                        return 1;
                    },
                    Object => $self
                }
            );
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
        return $self;
    }

    # Accessors | Public | General
    sub peerid { return $peerid{refaddr +shift} }

    # Accessors | Private | General
    sub _socket         { return $_socket{refaddr +shift} }
    sub _torrent        { return $_torrent{refaddr +shift} }
    sub _reserved_bytes { return $_reserved_bytes{refaddr +shift} }
    sub _bitfield       { return ${$_bitfield{refaddr +shift}} }

    sub _port {
        return if defined $_[1];
        my ($self) = @_;
        return if not defined $_socket{refaddr $self};
        my $peername = getpeername($_socket{refaddr $self});
        return if not defined $peername;
        my ($port, undef) = unpack_sockaddr_in($peername);
        return $port;
    }

    sub _host {
        return if defined $_[1];
        my ($self) = @_;
        return if not defined $_socket{refaddr $self};
        my $peername = getpeername($_socket{refaddr $self});
        return if not defined $peername;
        my (undef, $packed_ip) = unpack_sockaddr_in($peername);
        return inet_ntoa($packed_ip);
    }

    # Accessors | Private | Status
    sub _peer_choking    { return ${$_peer_choking{refaddr +shift}} }
    sub _am_choking      { return ${$_am_choking{refaddr +shift}} }
    sub _peer_interested { return ${$_peer_interested{refaddr +shift}} }
    sub _am_interested   { return ${$_am_interested{refaddr +shift}} }
    sub _incoming        { return $_incoming{refaddr +shift} }
    sub _source          { return $_source{refaddr +shift} }

    # Methods | Private
    sub _rw {
        my ($self, $read, $write, $error) = @_;

#Carp::cluck sprintf q[%s->_rw(%d, %d, %d)], __PACKAGE__, $read, $write, $error;
        if (defined $_torrent{refaddr $self}
            and !($_torrent{refaddr $self}->status & 1))
        {   weaken $self;
            $self->_disconnect(q[We aren't serving this torrent]);
            return;
        }
        if ($error) {
            weaken $self;
            $self->_disconnect($^E);
            return;
        }
        if (defined $_torrent{refaddr $self}
            and $_torrent{refaddr $self}->status & 2)
        {   weaken $self;
            $self->_disconnect(q[Hashchecking]);
            return;
        }
        my ($actual_read, $actual_write) = (0, 0);
        if ($write) {
            if (length $_data_out{refaddr $self}) {
                $actual_write =
                    syswrite($_socket{refaddr $self},
                             $_data_out{refaddr $self},
                             $write, 0);
                if (not $actual_write) {
                    weaken $self;
                    $self->_disconnect(sprintf q[[%d] %s], $^E, $^E);
                    return;
                }
                else {
                    $_client{refaddr $self}->_event(q[peer_write],
                                    {Peer => $self, Length => $actual_write});
                    substr($_data_out{refaddr $self}, 0, $actual_write, q[]);
                }
            }
        }
        if ($read) {
            $actual_read = sysread($_socket{refaddr $self},
                                   $_data_in{refaddr $self},
                                   $read,
                                   length($_data_in{refaddr $self})
            );
            if (!$actual_read) {
                weaken $self;
                $self->_disconnect(sprintf q[[%d] %s], $^E, $^E);
                return;
            }
            else {
                if (!$peerid{refaddr $self}) {
                    $_client{refaddr $self}
                        ->_event(q[peer_connect], {Peer => $self});
                }
                $_client{refaddr $self}->_event(q[peer_read],
                                     {Peer => $self, Length => $actual_read});
            PACKET: while ($_data_in{refaddr $self}) {
                    my $data_len = length $_data_in{refaddr $self};
                    my $packet   = parse_packet(\$_data_in{refaddr $self});
                    if (not defined $packet) {
                        if (length($_data_in{refaddr $self}) != $data_len) {
                            weaken $self;
                            $self->_disconnect(q[...bad packet.]);
                            return;
                        }
                        last PACKET;
                    }
                    my %dispatch = (
                                 &HANDSHAKE      => \&__handle_handshake,
                                 &KEEPALIVE      => \&__handle_keepalive,
                                 &CHOKE          => \&__handle_choke,
                                 &UNCHOKE        => \&__handle_unchoke,
                                 &INTERESTED     => \&__handle_interested,
                                 &NOT_INTERESTED => \&__handle_not_interested,
                                 &HAVE           => \&__handle_have,
                                 &BITFIELD       => \&__handle_bitfield,
                                 &REQUEST        => \&__handle_request,
                                 &PIECE          => \&__handle_piece,
                                 &CANCEL         => \&__handle_cancel,
                                 &HAVE_ALL       => \&__handle_have_all,
                                 &HAVE_NONE      => \&__handle_have_none,
                                 &ALLOWED_FAST   => \&__handle_allowed_fast,
                                 &REJECT         => \&__handle_reject,
                                 &EXTPROTOCOL    => \&__handle_ext_protocol
                    );
                    if (defined $dispatch{$packet->{q[Type]}}) {
                        $dispatch{$packet->{q[Type]}}($self,
                                                      $packet->{q[Payload]});
                    }
                    elsif (eval require Data::Dump) {
                        Carp::carp q[Unknown packet! ]
                            . Data::Dump::pp($packet);
                    }
                }
            }
        }
        $_client{refaddr $self}->_add_connection($self,
                         (length($_data_out{refaddr $self}) ? q[rw] : q[ro]));
        return ($actual_read, $actual_write);
    }

    sub __handle_handshake {
        my ($self, $payload) = @_;
        return if !defined $_socket{refaddr $self};
        return if !defined $payload;
        $_last_contact{refaddr $self} = time;
        ($_reserved_bytes{refaddr $self}, undef, $peerid{refaddr $self})
            = @{$payload};
        $_client{refaddr $self}->_event(q[incoming_packet],
                                        {Peer    => $self,
                                         Payload => {
                                                    Reserved => $payload->[0],
                                                    Infohash => $payload->[1],
                                                    PeerID   => $payload->[2]
                                         },
                                         Type => HANDSHAKE
                                        }
        );
        if ($peerid{refaddr $self} eq $_client{refaddr $self}->peerid) {
            weaken $self;
            $self->_disconnect(q[...we've connected to ourself.]);
            return;
        }
        if ($_incoming{refaddr $self} && !$_torrent{refaddr $self}) {
            $_torrent{refaddr $self} = $_client{refaddr $self}
                ->_locate_torrent(unpack(q[H40], $payload->[1]));
            if (!defined $_torrent{refaddr $self}) {
                weaken $self;
                $self->_disconnect(
                                sprintf q[We aren't serving this torrent: %s],
                                unpack(q[H40], $payload->[1]));
                return;
            }
            if (scalar(
                    grep {
                               $_->{q[Object]}->isa(q[Net::BitTorrent::Peer])
                            && $_->{q[Object]}->peerid
                            && $_->{q[Object]}->peerid eq
                            $peerid{refaddr $self}
                        } values %{$_client{refaddr $self}->_connections}
                ) >= $_client{refaddr $self}->_connections_per_host
                )
            {   $self->_disconnect(
                       sprintf(
                           q[Already connected to this peer (by peerid '%s')],
                           $peerid{refaddr $self})
                );
                return;
            }
            if (scalar($_torrent{refaddr $self}->peers)
                >= $_client{refaddr $self}->_peers_per_torrent)
            {   $self->_disconnect(q[Enough peers already!]);
                return;
            }
            weaken $_torrent{refaddr $self};
            ${$_bitfield{refaddr $self}}
                = pack(q[b*], qq[\0] x $_torrent{refaddr $self}->piece_count);
            if ($threads::shared::threads_shared) {
                threads::shared::share($_bitfield{refaddr $self});
            }
            my %_payload = (
                 Reserved => $_client{refaddr $self}->_build_reserved,
                 Infohash => pack(q[H40], $_torrent{refaddr $self}->infohash),
                 PeerID   => $_client{refaddr $self}->peerid
            );
            $_data_out{refaddr $self} .=
                build_handshake($_payload{q[Reserved]},
                                $_payload{q[Infohash]},
                                $_payload{q[PeerID]}
                );
            $_client{refaddr $self}->_event(q[outgoing_packet],
                                            {Peer    => $self,
                                             Payload => \%_payload,
                                             Type    => HANDSHAKE
                                            }
            );
        }
        $self->_send_bitfield;
        $self->_send_extended_handshake;
    }

    sub __handle_keepalive {
        my ($self) = @_;
        return if !defined $_torrent{refaddr $self};
        return if !defined $_socket{refaddr $self};
        if (defined $_torrent{refaddr $self}
            and !($_torrent{refaddr $self}->status & 1))
        {   weaken $self;
            $self->_disconnect(q[We aren't serving this torrent]);
            return;
        }
        $_client{refaddr $self}->_event(q[incoming_packet],
                           {Peer => $self, Payload => {}, Type => KEEPALIVE});
        return 1;
    }

    sub __handle_choke {
        my ($self) = @_;
        return if !defined $_torrent{refaddr $self};
        return if !defined $_socket{refaddr $self};
        if (defined $_torrent{refaddr $self}
            and !($_torrent{refaddr $self}->status & 1))
        {   weaken $self;
            $self->_disconnect(q[We aren't serving this torrent]);
            return;
        }
        $_client{refaddr $self}->_event(q[incoming_packet],
                               {Peer => $self, Payload => {}, Type => CHOKE});
        ${$_peer_choking{refaddr $self}}  = 1;
        ${$_am_interested{refaddr $self}} = 0;
        for my $request (@{$requests_out{refaddr $self}}) {
            my $piece = $_torrent{refaddr $self}
                ->_piece_by_index($request->{q[Index]});
            delete $piece->{q[Blocks_Requested]}->[$request->{q[_vec_offset]}]
                ->{refaddr $self};
        }
        return $requests_out{refaddr $self} = [];
    }

    sub __handle_unchoke {
        my ($self) = @_;
        return if !defined $_torrent{refaddr $self};
        return if !defined $_socket{refaddr $self};
        if (defined $_torrent{refaddr $self}
            and !($_torrent{refaddr $self}->status & 1))
        {   weaken $self;
            $self->_disconnect(q[We aren't serving this torrent]);
            return;
        }
        $_last_contact{refaddr $self} = time;
        ${$_peer_choking{refaddr $self}} = 0;
        $_client{refaddr $self}->_event(q[incoming_packet],
                             {Peer => $self, Payload => {}, Type => UNCHOKE});
        $self->_request_block(2);
        return 1;
    }

    sub __handle_interested {
        my ($self) = @_;
        return if !defined $_torrent{refaddr $self};
        return if !defined $_socket{refaddr $self};
        if (defined $_torrent{refaddr $self}
            and !($_torrent{refaddr $self}->status & 1))
        {   weaken $self;
            $self->_disconnect(q[We aren't serving this torrent]);
            return;
        }
        $_last_contact{refaddr $self} = time;
        ${$_peer_interested{refaddr $self}} = 1;
        $_client{refaddr $self}->_event(q[incoming_packet],
                          {Peer => $self, Payload => {}, Type => INTERESTED});
        $self->_send_unchoke();
        return 1;
    }

    sub __handle_not_interested {
        my ($self) = @_;
        return if !defined $_torrent{refaddr $self};
        return if !defined $_socket{refaddr $self};
        if (defined $_torrent{refaddr $self}
            and !($_torrent{refaddr $self}->status & 1))
        {   weaken $self;
            $self->_disconnect(q[We aren't serving this torrent]);
            return;
        }
        $_last_contact{refaddr $self} = time;
        $_client{refaddr $self}->_event(q[incoming_packet],
                                        {Peer    => $self,
                                         Payload => {},
                                         Type    => NOT_INTERESTED
                                        }
        );
        ${$_peer_interested{refaddr $self}} = 1;
        ${$_am_choking{refaddr $self}}      = 1;
        return 1;
    }

    sub __handle_have {
        my ($self, $index) = @_;
        return if !defined $_torrent{refaddr $self};
        return if !defined $_socket{refaddr $self};
        return if !defined $index;
        if (defined $_torrent{refaddr $self}
            and !($_torrent{refaddr $self}->status & 1))
        {   weaken $self;
            $self->_disconnect(q[We aren't serving this torrent]);
            return;
        }
        $_client{refaddr $self}->_event(q[incoming_packet],
                                        {Peer    => $self,
                                         Payload => {Index => $index},
                                         Type    => HAVE
                                        }
        );
        vec(${$_bitfield{refaddr $self}}, $index, 1) = 1;
        if ((unpack(q[b*], ${$_bitfield{refaddr $self}}) !~ m[1])
            && $_torrent{refaddr $self}->is_complete)
        {   weaken $self;
            $self->_disconnect(q[Disconnect seed]);
            return;
        }
        $self->_check_interest;
        if (${$_am_interested{refaddr $self}}
            and not ${$_peer_choking{refaddr $self}})
        {   $self->_request_block;
        }
    }

    sub __handle_bitfield {
        my ($self, $payload) = @_;
        return if !defined $_torrent{refaddr $self};
        return if !defined $_socket{refaddr $self};
        return if !defined $payload;
        if (defined $_torrent{refaddr $self}
            and !($_torrent{refaddr $self}->status & 1))
        {   weaken $self;
            $self->_disconnect(q[We aren't serving this torrent]);
            return;
        }
        ${$_bitfield{refaddr $self}} = $payload;
        $_client{refaddr $self}->_event(q[incoming_packet],
                                        {Peer    => $self,
                                         Payload => {Bitfield => $payload},
                                         Type    => BITFIELD
                                        }
        );
        if ((unpack(q[b*], ${$_bitfield{refaddr $self}}) !~ m[1])
            && $_torrent{refaddr $self}->is_complete)
        {   weaken $self;
            $self->_disconnect(q[Disconnect seed]);
            return;
        }
        return $self->_check_interest;
    }

    sub __handle_request {
        my ($self, $payload) = @_;
        return if !defined $_torrent{refaddr $self};
        return if !defined $_socket{refaddr $self};
        return if !defined $payload;
        if (defined $_torrent{refaddr $self}
            and !($_torrent{refaddr $self}->status & 1))
        {   weaken $self;
            $self->_disconnect(q[We aren't serving this torrent]);
            return;
        }
        $_client{refaddr $self}->_event(q[incoming_packet],
                                        {Peer    => $self,
                                         Payload => {Index  => $payload->[0],
                                                     Offset => $payload->[1],
                                                     Length => $payload->[2]
                                         },
                                         Type => REQUEST
                                        }
        );
        if (not @{$requests_in{refaddr $self}}) {
            $_client{refaddr $self}->_schedule({Time   => time + 15,
                                                Code   => \&_fill_requests,
                                                Object => $self
                                               }
            );
        }
        return push @{$requests_in{refaddr $self}},
            {Index  => $payload->[0],
             Offset => $payload->[1],
             Length => $payload->[2]
            };
    }

    sub __handle_piece {
        my ($self, $payload) = @_;
        return if !defined $payload;
        if (defined $_torrent{refaddr $self}
            and !($_torrent{refaddr $self}->status & 1))
        {   weaken $self;
            $self->_disconnect(q[We aren't serving this torrent]);
            return;
        }
        return if !defined $_torrent{refaddr $self};
        return if !defined $_socket{refaddr $self};
        $_last_contact{refaddr $self} = time;
        my ($index, $offset, $data) = @{$payload};
        my $length = length($data);
        my ($request) = grep {
                    ($_->{q[Index]} == $index)
                and ($_->{q[Offset]} == $offset)
                and ($_->{q[Length]} == $length)
        } @{$requests_out{refaddr $self}};

        if (not defined $request) {
            weaken $self;
            $self->_disconnect(
                sprintf
                    q[Handed a piece we never asked for (I: %d | O: %d | L: %d).],
                $index, $offset, $length);
            return;
        }
        $_torrent{refaddr $self}->_add_downloaded($request->{q[Length]});
        @{$requests_out{refaddr $self}} = grep {
                   ($_->{q[Index]} != $index)
                or ($_->{q[Offset]} != $offset)
                or ($_->{q[Length]} != $length)
        } @{$requests_out{refaddr $self}};
        $_client{refaddr $self}->_event(q[incoming_packet],
                                        {Payload => {Index  => $index,
                                                     Offset => $offset,
                                                     Length => $length
                                         },
                                         Peer => $self,
                                         Type => PIECE
                                        }
        );
        my $piece = $_torrent{refaddr $self}->_piece_by_index($index);
        my $okay  = 0;
        for my $_retry (1 .. 3) {

            if ($_torrent{refaddr $self}->_write_data($index, $offset, \$data)
                )
            {   $okay++;
                last;
            }
        }
        return if !$okay;
        $piece->{q[Blocks_Received]}->[$request->{q[_vec_offset]}] = 1;
        $piece->{q[Slow]}                                          = 0;
        $piece->{q[Touch]}                                         = time;
        for my $peer ($_torrent{refaddr $self}->peers) {
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
                    $_client{refaddr $self}->_event(q[outgoing_packet],
                                                    {Payload => {
                                                            Index  => $index,
                                                            Offset => $offset,
                                                            Length => $length
                                                     },
                                                     Peer => $peer,
                                                     Type => CANCEL
                                                    }
                    );
                    $_client{refaddr $self}->_add_connection($peer, q[rw]);
                    splice(@{$requests_out{refaddr $peer }}, $x, 1);
                    last;
                }
            }
        }
        if (not grep { !$_ } @{$piece->{q[Blocks_Received]}}) {
            if ($_torrent{refaddr $self}->_check_piece_by_index($index)
                and defined $_torrent{refaddr $self})
            {   for my $p ($_torrent{refaddr $self}->peers) {
                    $_data_out{$p} .= build_have($index);
                    $_client{refaddr $self}->_add_connection($p, q[rw]);
                }
            }
        }
        $self->_request_block if $self->_check_interest;
        return 1;
    }

    sub __handle_cancel {
        my ($self, $payload) = @_;
        return if !defined $_torrent{refaddr $self};
        return if !defined $_socket{refaddr $self};
        return if !defined $payload;
        if (defined $_torrent{refaddr $self}
            and !($_torrent{refaddr $self}->status & 1))
        {   weaken $self;
            $self->_disconnect(q[We aren't serving this torrent]);
            return;
        }
        $_last_contact{refaddr $self} = time;
        my ($index, $offset, $length) = @$payload;
        $_client{refaddr $self}->_event(q[incoming_packet],
                                        {Payload => {Index  => $index,
                                                     Offset => $offset,
                                                     Length => $length
                                         },
                                         Peer => $self,
                                         Type => CANCEL
                                        }
        );
        for my $x (reverse 0 .. $#{$requests_in{refaddr $self}}) {
            if (    ($requests_in{refaddr $self}->[$x]->{q[Index]} == $index)
                and
                ($requests_in{refaddr $self}->[$x]->{q[Offset]} == $offset)
                and
                ($requests_in{refaddr $self}->[$x]->{q[Length]} == $length))
            {   splice(@{$requests_in{refaddr $self}}, $x, 1);
            }
        }
        return 1;
    }

    sub __handle_have_all {
        my ($self) = @_;
        return if !defined $_torrent{refaddr $self};
        return if !defined $_socket{refaddr $self};
        if (!$_torrent{refaddr $self}->status & 1) {
            weaken $self;
            $self->_disconnect(q[We aren't serving this torrent]);
            return;
        }
        ${$_bitfield{refaddr $self}}
            = pack(q[b*], qq[\1] x $_torrent{refaddr $self}->piece_count);
        $_client{refaddr $self}->_event(q[incoming_packet],
                                        {Peer    => $self,
                                         Payload => {},
                                         Type    => HAVE_ALL
                                        }
        );
        if ($_torrent{refaddr $self}->is_complete) {
            weaken $self;
            $self->_disconnect(q[Disconnect seed]);
            return;
        }
        $self->_check_interest;
        if (${$_am_interested{refaddr $self}}
            and not ${$_peer_choking{refaddr $self}})
        {   $self->_request_block;
        }
        return 1;
    }

    sub __handle_have_none {
        my ($self) = @_;
        return if !defined $_torrent{refaddr $self};
        return if !defined $_socket{refaddr $self};
        if (defined $_torrent{refaddr $self}
            and !($_torrent{refaddr $self}->status & 1))
        {   weaken $self;
            $self->_disconnect(q[We aren't serving this torrent]);
            return;
        }
        ${$_bitfield{refaddr $self}}
            = pack(q[b*], qq[\0] x $_torrent{refaddr $self}->piece_count);
        $_client{refaddr $self}->_event(q[incoming_packet],
                                        {Peer    => $self,
                                         Payload => {},
                                         Type    => HAVE_NONE
                                        }
        );
        return $self->_check_interest;
    }

    sub __handle_allowed_fast {
        my ($self, $payload) = @_;
        return if !defined $_torrent{refaddr $self};
        return if !defined $_socket{refaddr $self};
        return if !defined $payload;
        $_client{refaddr $self}->_event(q[incoming_packet],
                                        {Payload => {Index => $payload},
                                         Peer    => $self,
                                         Type    => ALLOWED_FAST
                                        }
        );
        push(@{$_incoming_fastset{refaddr $self}}, $payload);
        return 1;
    }

    sub __handle_reject {
        my ($self, $payload) = @_;
        return if !defined $payload;
        return if !defined $_torrent{refaddr $self};
        return if !defined $_socket{refaddr $self};
        my ($index, $offset, $length) = @{$payload};
        my ($request) = grep {
                    ($_->{q[Index]} == $index)
                and ($_->{q[Offset]} == $offset)
                and ($_->{q[Length]} == $length)
        } @{$requests_out{refaddr $self}};
        if (not defined $request) {
            weaken $self;
            $self->_disconnect(q[Rejected a request we never made.]);
            return;
        }
        @{$requests_out{refaddr $self}} = grep {
                   ($_->{q[Index]} != $index)
                or ($_->{q[Offset]} != $offset)
                or ($_->{q[Length]} != $length)
        } @{$requests_out{refaddr $self}};
        my $piece = $_torrent{refaddr $self}->_piece_by_index($index);
        if (not defined $piece) {
            weaken $self;
            $self->_disconnect(q[Sent a reject to a non-existant piece]);
            return;
        }
        delete $piece->{q[Blocks_Requested]}->[$request->{q[_vec_offset]}]
            ->{refaddr $self};
        $_client{refaddr $self}->_event(q[incoming_packet],
                                        {Payload => {Index  => $index,
                                                     Offset => $offset,
                                                     Length => $length
                                         },
                                         Peer => $self,
                                         Type => REJECT
                                        }
        );
        return 1;
    }

    sub __handle_ext_protocol {
        my ($self, $payload) = @_;
        return if !defined $_torrent{refaddr $self};
        return if !defined $_socket{refaddr $self};
        return if !defined $payload;
        if (defined $_torrent{refaddr $self}
            and !($_torrent{refaddr $self}->status & 1))
        {   weaken $self;
            $self->_disconnect(q[We aren't serving this torrent]);
            return;
        }
        return if $_torrent{refaddr $self}->status & 32;
        $_last_contact{refaddr $self} = time;
        my ($id, $packet) = @{$payload};
        if ($packet) {
            if ($id == 0) {
                if (    defined $_client{refaddr $self}->_dht
                    and defined $packet->{q[p]})
                {   my (undef, $packed_ip)
                        = unpack_sockaddr_in(
                                        getpeername($_socket{refaddr $self}));
                    $_client{refaddr $self}->_dht->add_node(
                                              {ip   => inet_ntoa($packed_ip),
                                               port => $packet->{q[p]}
                                              }
                    );
                }
            }
            $packet->{q[ID]} = $id;
            $_client{refaddr $self}->_event(q[incoming_packet],
                                            {Payload => $packet,
                                             Peer    => $self,
                                             Type    => EXTPROTOCOL
                                            }
            );
        }
    }

    sub _check_interest {
        my ($self) = @_;
        if (not defined $_torrent{refaddr $self}) { return; }
        if (defined $_torrent{refaddr $self}
            and !($_torrent{refaddr $self}->status & 1))
        {   weaken $self;
            $self->_disconnect(q[We aren't serving this torrent]);
            return;
        }
        return if $_torrent{refaddr $self}->status & 32;
        my $interesting  = ${$_am_interested{refaddr $self}};
        my $torrent_have = $_torrent{refaddr $self}->bitfield();
        my $torrent_want = $_torrent{refaddr $self}->_wanted();
        my $relevence    = ${$_bitfield{refaddr $self}} & $torrent_want;
        $interesting = (index(unpack(q[b*], $relevence), 1, 0) != -1) ? 1 : 0;
        if ($interesting and not ${$_am_interested{refaddr $self}}) {
            ${$_am_interested{refaddr $self}} = 1;
            $_data_out{refaddr $self} .= build_interested;
            $_client{refaddr $self}->_event(q[outgoing_packet],
                          {Peer => $self, Payload => {}, Type => INTERESTED});
        }
        elsif (not $interesting and ${$_am_interested{refaddr $self}}) {
            ${$_am_interested{refaddr $self}} = 0;
            $_data_out{refaddr $self} .= build_not_interested;
            $_client{refaddr $self}->_event(q[outgoing_packet],
                      {Peer => $self, Payload => {}, Type => NOT_INTERESTED});
        }
        return ${$_am_interested{refaddr $self}};
    }

    sub _disconnect_useless_peer {
        my ($self) = @_;
        return if not defined $self;
        if ($_last_contact{refaddr $self} < (time - 180)) {
            weaken $self;
            $self->_disconnect(q[Peer is idle]);
            return;
        }
        if (    ${$_peer_choking{refaddr $self}}
            and ${$_am_interested{refaddr $self}})
        {    # XXX - send uninterested?
            $self->_check_interest;
        }
        if (    (${$_peer_choking{refaddr $self}})
            and (!${$_am_interested{refaddr $self}})
            and (!${$_peer_interested{refaddr $self}}))
        {   weaken $self;
            $self->_disconnect(
                       q[Useless peer (Not interested and not interesting.)]);
            return;
        }
        $_client{refaddr $self}->_schedule(
                            {Time   => (180 + $_last_contact{refaddr $self}),
                             Code   => \&_disconnect_useless_peer,
                             Object => $self
                            }
        );
        return 1;
    }

    sub _cancel_old_requests {
        my ($self) = @_;
        return if not defined $self;
        return if not defined $_socket{refaddr $self};
        $_client{refaddr $self}->_schedule({Time   => time + 15,
                                            Code   => \&_cancel_old_requests,
                                            Object => $self
                                           }
        );
        my $canceled = 0;
        if (@{$requests_out{refaddr $self}} == []) {
            return;
        }
        for my $i (reverse(0 .. $#{$requests_out{refaddr $self}})) {
            my $request = $requests_out{refaddr $self}->[$i];
            if (time <= ($request->{q[Timestamp]} + 60)) {
                next;
            }
            my $piece = $_torrent{refaddr $self}
                ->_piece_by_index($request->{q[Index]});
            delete $piece->{q[Blocks_Requested]}->[$request->{q[_vec_offset]}]
                ->{refaddr $self};
            if (!$piece->{q[Touch]} || $piece->{q[Touch]} <= time - 180) {
                $piece->{q[Slow]} = 1;
            }
            $_data_out{refaddr $self} .=
                build_cancel($request->{q[Index]}, $request->{q[Offset]},
                             $request->{q[Length]});
            $_client{refaddr $self}->_event(
                                         q[outgoing_packet],
                                         {Payload => {
                                              Index  => $request->{q[Index]},
                                              Offset => $request->{q[Offset]},
                                              Length => $request->{q[Length]}
                                          },
                                          Peer => $self,
                                          Type => CANCEL
                                         }
            );
            splice(@{$requests_out{refaddr $self}}, $i, 1);
            $canceled++;
        }
        $_client{refaddr $self}->_add_connection($self, q[rw])
            if $canceled;
        return $canceled;
    }

    sub _request_block {
        my ($self, $_range) = @_;
        return if not defined $_socket{refaddr $self};
        return if ${$_peer_choking{refaddr $self}};
        if (!($_torrent{refaddr $self}->status & 1)) {
            weaken $self;
            $self->_disconnect(q[We aren't serving this torrent]);
            return;
        }
        return if $_torrent{refaddr $self}->status & 32;
        my $return = 0;
        my $range
            = defined $_range
            ? [1 .. $_range]
            : [max(1, scalar(@{$requests_out{refaddr $self}})) .. max(
                                   25,
                                   int((2**21)
                                       / $_torrent{refaddr $self}->raw_data(1)
                                           ->{q[info]}{q[piece length]}
                                   )
               )
            ];
    REQUEST:
        for (@$range) {
            my $piece = $_torrent{refaddr $self}->_pick_piece($self);
            if ($piece) {
                my $vec_offset;
                if ($piece->{q[Endgame]}) {

           # This next bit selects the least requested block (max 5 requests),
           # makes sure this peer isn't already sitting on this request
           # and... I just lost my train of thought; It's Friday afternoon.
           # Regardless of how it looks, it does what I mean.
                    my $tmp_index = -1;
                    my %temp      = map {
                        $tmp_index++;
                        (        ($_ < 5)
                             and
                             ($piece->{q[Blocks_Received]}->[$tmp_index] == 0)
                            )
                            ? ($tmp_index => $_)
                            : ()
                    } @{$piece->{q[Blocks_Received]}};
                INDEX:
                    for my $index (sort { $temp{$a} <=> $temp{$b} }
                                   sort { $a <=> $b } keys %temp)
                    {
                        if (not grep {
                                (defined $piece->{q[Blocks_Requested]}
                                     ->[$index]->{refaddr $self})
                                    and (
                                    ($piece->{q[Blocks_Received]}->[$index])
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
                BLOCK:
                    for my $i (0 .. $#{$piece->{q[Blocks_Requested]}}) {
                        if (not(keys %{$piece->{q[Blocks_Requested]}->[$i]}))
                        {   $vec_offset = $i;
                            last BLOCK;
                        }
                    }
                }
                if (not defined $vec_offset or $vec_offset == -1) {

                    # xxx - pick a different piece?
                    # xxx - Honestly, this piece shouldn't have been returned
                    #       from _pick_piece in the first place...
                    last REQUEST;
                }
                $piece->{q[Blocks_Requested]}->[$vec_offset]->{refaddr $self}
                    = $self;
                weaken $piece->{q[Blocks_Requested]}->[$vec_offset]
                    ->{refaddr $self};
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
                push @{$requests_out{refaddr $self}}, $request;
                $_client{refaddr $self}->_event(
                                             q[outgoing_packet],
                                             {Payload => {
                                                  Index => $piece->{q[Index]},
                                                  Offset => $offset,
                                                  Length => $length
                                              },
                                              Peer => $self,
                                              Type => REQUEST
                                             }
                );
                $_client{refaddr $self}->_add_connection($self, q[rw]);
                $_data_out{refaddr $self}
                    .= build_request($piece->{q[Index]}, $offset, $length);
                $return++;
            }
            else {
                last REQUEST;
            }
        }
        return $return;
    }

    sub _send_bitfield {
        my ($self) = @_;
        if (defined $_torrent{refaddr $self}
            and !($_torrent{refaddr $self}->status & 1))
        {   weaken $self;
            $self->_disconnect(q[We aren't serving this torrent]);
            return;
        }
        return if !defined $_torrent{refaddr $self};
        return if !defined $_socket{refaddr $self};
        return if not defined $_reserved_bytes{refaddr $self};
        my ($_i, @have) = (0);
        for my $x (split q[], unpack q[b*],
                   $_torrent{refaddr $self}->bitfield)
        {   push @have, $_i if $x;
            $_i++;
        }
        if (   (scalar(@have) == 0)
            && (ord(substr($_reserved_bytes{refaddr $self}, 7, 1)) & 0x04))
        {   $_data_out{refaddr $self} .= build_have_none();
            $_client{refaddr $self}->_event(q[outgoing_packet],
                           {Peer => $self, Payload => {}, Type => HAVE_NONE});
        }
        elsif (   (scalar(@have) == $self->_torrent->piece_count)
               && (ord(substr($_reserved_bytes{refaddr $self}, 7, 1)) & 0x04))
        {   $_data_out{refaddr $self} .= build_have_all();
            $_client{refaddr $self}->_event(q[outgoing_packet],
                            {Peer => $self, Payload => {}, Type => HAVE_ALL});
        }
        elsif (scalar(@have) > 12) {
            $_data_out{refaddr $self} .=
                build_bitfield(pack q[B*], unpack q[b*],
                               $_torrent{refaddr $self}->bitfield);
            $_client{refaddr $self}->_event(q[outgoing_packet],
                            {Peer => $self, Payload => {}, Type => BITFIELD});
        }
        else {
            for my $index (@have) {
                $_data_out{refaddr $self} .= build_have($index);
                $_client{refaddr $self}->_event(q[outgoing_packet],
                                                {Peer    => $self,
                                                 Payload => {Index => $index},
                                                 Type    => HAVE
                                                }
                );
            }
        }
        return 1;
    }

    sub _send_extended_handshake {
        my ($self) = @_;
        if (defined $_torrent{refaddr $self}
            and !($_torrent{refaddr $self}->status & 1))
        {   weaken $self;
            $self->_disconnect(q[We aren't serving this torrent]);
            return;
        }
        return if !defined $_torrent{refaddr $self};
        return if !defined $_socket{refaddr $self};
        return if not defined $_reserved_bytes{refaddr $self};
        return
            if not ord(substr($_reserved_bytes{refaddr $self}, 5, 1)) & 0x10;
        my ($_peerport, $_packed_ip)
            = unpack_sockaddr_in(getpeername($_socket{refaddr $self}));
        my $_id      = 0;
        my $_payload = {

            #m => {$_private{refaddr $self} ? () : (ut_pex => 1)}, # TODO: PEX
            ($_client{refaddr $self}->_use_dht
             ? (p => $_client{refaddr $self}->_udp_port)
             : ()
            ),
            v => $Net::BitTorrent::Version::PRODUCT_TOKEN,
            ($_packed_ip ? (yourip => $_packed_ip) : ()),
            reqq => 30    # XXX - Lies
        };
        $_client{refaddr $self}->_event(q[outgoing_packet],
                                        {Payload => $_payload,
                                         ID      => $_id,
                                         Peer    => $self,
                                         Type    => EXTPROTOCOL
                                        }
        );
        return $_data_out{refaddr $self} .= build_extended($_id, $_payload);
    }

    sub _send_keepalive {
        my ($self) = @_;
        return if not defined $self;
        return if not defined $_socket{refaddr $self};
        return if not defined $_torrent{refaddr $self};
        if (defined $_torrent{refaddr $self}
            and !($_torrent{refaddr $self}->status & 1))
        {   weaken $self;
            $self->_disconnect(q[We aren't serving this torrent]);
            return;
        }
        $_client{refaddr $self}->_schedule({Time   => time + 120,
                                            Code   => \&_send_keepalive,
                                            Object => $self
                                           }
        );
        return if $_torrent{refaddr $self}->status & 32;
        $_client{refaddr $self}->_event(q[outgoing_packet],
                                        {Peer    => $self,
                                         Payload => {},
                                         Type    => KEEPALIVE
                                        }
        );
        $_data_out{refaddr $self} .= build_keepalive();
        $self->_check_interest;
        return 1;
    }

    sub _fill_requests {
        my ($self) = @_;
        return if !defined $_torrent{refaddr $self};
        return if !defined $_socket{refaddr $self};
        return if $_torrent{refaddr $self}->status & 32;
        return if not @{$requests_in{refaddr $self}};
        return if ${$_am_choking{refaddr $self}};
        if (defined $_torrent{refaddr $self}
            and !($_torrent{refaddr $self}->status & 1))
        {   weaken $self;
            $self->_disconnect(q[We aren't serving this torrent]);
            return;
        }
        if (defined $_torrent{refaddr $self}
            and ($_torrent{refaddr $self}->status & 32))
        {   return;
        }
        $_client{refaddr $self}->_schedule({Time   => time + 60,
                                            Code   => \&_fill_requests,
                                            Object => $self
                                           }
        );
        while ((length($_data_out{refaddr $self}) < 2**15)
               and @{$requests_in{refaddr $self}})
        {   my $request = shift @{$requests_in{refaddr $self}};
            next
                unless $_torrent{refaddr $self}
                    ->_check_piece_by_index($request->{q[Index]});
            next unless $request->{q[Length]};
            $_torrent{refaddr $self}->_add_uploaded($request->{q[Length]});
            $_client{refaddr $self}->_event(
                                         q[outgoing_packet],
                                         {Payload => {
                                              Index  => $request->{q[Index]},
                                              Offset => $request->{q[Offset]},
                                              Length => $request->{q[Length]}
                                          },
                                          Peer => $self,
                                          Type => PIECE
                                         }
            );
            $_data_out{refaddr $self} .=
                build_piece($request->{q[Index]},
                            $request->{q[Offset]},
                            $_torrent{refaddr $self}->_read_data(
                                                        $request->{q[Index]},
                                                        $request->{q[Offset]},
                                                        $request->{q[Length]}
                            )
                );

            #if (rand(20) >= 20) { $self->_send_choke; }
        }
        $_client{refaddr $self}->_add_connection($self, q[rw]) or return;
        return 1;
    }

    sub _send_choke {
        my ($self) = @_;
        return if !defined $_torrent{refaddr $self};
        return if !defined $_socket{refaddr $self};
        if (defined $_torrent{refaddr $self}
            and $_torrent{refaddr $self}->status & 2)
        {   weaken $self;
            $self->_disconnect(q[Hashchecking]);
            return;
        }
        return if ${$_am_choking{refaddr $self}} == 1;
        $requests_in{refaddr $self} = [];
        ${$_am_choking{refaddr $self}}      = 1;
        ${$_peer_interested{refaddr $self}} = 0;
        $_data_out{refaddr $self} .= build_choke();
        $_client{refaddr $self}->_event(q[outgoing_packet],
                                        {Peer    => $self,
                                         Payload => {},
                                         Type    => CHOKE
                                        }
        );
        $_client{refaddr $self}->_add_connection($self, q[rw]) or return;
        return 1;
    }

    sub _send_unchoke {
        my ($self) = @_;
        return if !defined $_torrent{refaddr $self};
        return if !defined $_socket{refaddr $self};
        if (defined $_torrent{refaddr $self}
            and $_torrent{refaddr $self}->status & 2)
        {   weaken $self;
            $self->_disconnect(q[Hashchecking]);
            return;
        }
        return if $_torrent{refaddr $self}->status & 32;
        return if ${$_am_choking{refaddr $self}} == 0;
        if (scalar(
                 grep { $_->_am_choking == 0 } $_torrent{refaddr $self}->peers
            ) <= 8
            )
        {   ${$_am_choking{refaddr $self}} = 0;
            $_data_out{refaddr $self} .= build_unchoke();
            $_client{refaddr $self}->_event(q[outgoing_packet],
                             {Peer => $self, Payload => {}, Type => UNCHOKE});
            $_client{refaddr $self}->_add_connection($self, q[rw])
                or return;
        }
        else {
            $_client{refaddr $self}->_schedule({Time   => time + 30,
                                                Code   => \&_send_unchoke,
                                                Object => $self
                                               }
            );
        }
        return 1;
    }

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
        $reason ||= q[Connection closed by remote peer.];
        $_client{refaddr $self}->_remove_connection($self);
        $_client{refaddr $self}
            ->_event(q[peer_disconnect], {Peer => $self, Reason => $reason});
        if (defined $_socket{refaddr $self}) {
            shutdown($_socket{refaddr $self}, 2);
            close($_socket{refaddr $self});
        }
        return delete $_socket{refaddr $self};
    }

    sub as_string {
        my ($self, $advanced) = @_;
        my $dump = sprintf(
            (!$advanced ? q[%s:%s (%s)] : <<'ADVANCED'),
Net::BitTorrent::Peer

Address:     %s:%s
Peer ID:     %s
Torrent:     %s
Direction:   %s

Interested:  %s
Interesting: %s
Choked:      %s
Choking:     %s

Progress:
[%s]
ADVANCED
            ($self->_host || q[]),
            ($self->_port || q[]),
            ($peerid{refaddr $self} ? $peerid{refaddr $self} : q[Unknown]),
            (  $_torrent{refaddr $self}
             ? $_torrent{refaddr $self}->infohash
             : q[Unknown]
            ),
            ($_incoming{refaddr $self} ? q[Incoming] : q[Outgoing]),
            (map { $_ ? q[Yes] : q[No] } (${$_peer_interested{refaddr $self}},
                                          ${$_am_interested{refaddr $self}},
                                          ${$_am_choking{refaddr $self}},
                                          ${$_peer_choking{refaddr $self}}
             )
            ),
            ($_torrent{refaddr $self}
             ? (sprintf q[%s],
                join q[],
                map { vec(${$_bitfield{refaddr $self}}, $_, 1) ? q[|] : q[ ] }
                    0 .. $_torrent{refaddr $self}->piece_count - 1
                 )
             : q[NA]
            )
        );
        return defined wantarray ? $dump : print STDERR qq[$dump\n];
    }

    sub CLONE {
        for my $_oID (keys %REGISTRY) {
            my $_obj = $REGISTRY{$_oID};
            my $_nID = refaddr $_obj;
            for (@CONTENTS) {
                $_->{$_nID} = $_->{$_oID};
                delete $_->{$_oID};
            }
            weaken $_client{$_nID};
            weaken $_torrent{$_nID};
            weaken($REGISTRY{$_nID} = $_obj);
            delete $REGISTRY{$_oID};
        }
        return 1;
    }
    DESTROY {
        my ($self) = @_;
        if ($_torrent{refaddr $self}) {
            for my $request (@{$requests_out{refaddr $self}}) {
                my $piece = $_torrent{refaddr $self}
                    ->_piece_by_index($request->{q[Index]});
                delete $piece->{q[Blocks_Requested]}
                    ->[$request->{q[_vec_offset]}]->{refaddr $self};
            }
        }
        for (@CONTENTS) { delete $_->{refaddr $self}; }
        return delete $REGISTRY{refaddr $self};
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

=over

=item C<peerid>

Returns the Peer ID used to identify this peer.

See also: theory.org (http://tinyurl.com/4a9cuv)

=item C<as_string ( [ VERBOSE ] )>

Returns a 'ready to print' dump of the  object's data structure.  If
called in void context, the structure is printed to C<STDERR>.
C<VERBOSE> is a boolean value.

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
