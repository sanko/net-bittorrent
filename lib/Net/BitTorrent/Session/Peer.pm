package Net::BitTorrent::Session::Peer;
use strict;
use warnings;
{

    BEGIN {
        use version qw[qv];
        our $SVN = q[$Id$];
        our $VERSION = sprintf q[%.3f], version->new(qw$Rev$)->numify / 1000;
    }
    use Socket
        qw[SOL_SOCKET SO_SNDTIMEO SO_RCVTIMEO PF_INET AF_INET SOCK_STREAM];
    use Fcntl qw[F_SETFL O_NONBLOCK];
    use Digest::SHA qw[sha1_hex];
    use lib q[../../../../lib/];
    use Net::BitTorrent::Session::Peer::Request;
    use Net::BitTorrent::Util qw[min :bencode :log compact];

    # identification
    my (%client,   %fileno,   %socket,   %peer_id, %session,
        %bitfield, %peerhost, %peerport, %reserved);

    # status
    my (%is_choked,                   %is_choking,
        %is_interested,               %is_interesting,
        %queue_incoming,              %queue_outgoing,
        %incoming_requests,           %outgoing_requests,
        %incoming_connection,         %connection_timestamp,
        %previous_incoming_block,     %previous_outgoing_request,
        %previous_outgoing_keepalive, %connected,
        %listening_port,
    );

    # statistics
    my (%uploaded, %downloaded, %previous_incoming_data);

    # ext - support
    my (%_supports_DHT,         %_supports_FastPeers,
        %_supports_ExtProtocol, %_supports_Encryption,
        %_supports_BitComet
    );

    # ext - data
    my (%_fastset_out, %_fastset_in);

    sub new {
        my ($class, $args) = @_;
        my $self = undef;
        if (    defined $args->{q[socket]}
            and defined $args->{q[client]})
        {   my (undef, $peerport, @address)
                = unpack(q[SnC4x8], getpeername($args->{q[socket]}));
            $self
                = bless \sprintf(q[%d.%d.%d.%d:%d], @address, $peerport),
                $class;
            $client{$self}              = $args->{q[client]};
            $incoming_connection{$self} = 1;
            $self->_init($args->{q[socket]});
        }
        elsif (    defined $args->{q[address]}
               and $args->{q[address]} =~ m[^(?:\d+\.?){4}:(?:\d+)$]
               and defined $args->{q[session]}
               and Scalar::Util::blessed($args->{q[session]})
               and $args->{q[session]}->isa(q[Net::BitTorrent::Session])
               and not scalar grep { $_ eq $args->{q[address]} }
               $args->{q[session]}->get_peers)
        {

            # perldoc perlipc
            socket(my ($socket), PF_INET, SOCK_STREAM, getprotobyname(q[tcp]))
                or next PORT;
            if ($^O eq q[MSWin32]) {
                ioctl($socket, 0x8004667e, pack(q[I], 1));
            }
            else { fcntl($socket, F_SETFL, O_NONBLOCK) }
            my ($ip, $peerport) = split q[:], $args->{q[address]}, 2;
            connect($socket,
                    pack(q[Sna4x8],
                         AF_INET, $peerport,
                         join(q[], map { chr $_ } ($ip =~ m[(\d+)]g)))
            );

            # TODO: check value of err to verify non-blocking connect
            $self = bless \$args->{q[address]}, $class;
            $socket{$self}              = $socket;
            $fileno{$self}              = fileno($socket{$self});
            $connected{$self}           = 0;
            $session{$self}             = $args->{q[session]};
            $incoming_requests{$self}   = [];
            $client{$self}              = $args->{q[session]}->get_client;
            $incoming_connection{$self} = 0;
            $self->_init($socket);
            $self->_build_packet_handshake;
            $peerhost{$self}       = $ip;
            $peerport{$self}       = $peerport;
            $listening_port{$self} = $peerport;
        }
        return $self;
    }

    sub _init {
        my ($self, $socket) = @_;
        $is_interesting{$self}          = 0;
        $is_interested{$self}           = 0;
        $is_choking{$self}              = 1;
        $is_choked{$self}               = 1;
        $downloaded{$self}              = 0;
        $uploaded{$self}                = 0;
        $connection_timestamp{$self}    = time;
        $previous_incoming_block{$self} = time;                     # lies
        $previous_incoming_data{$self}  = time;                     # lies
        $outgoing_requests{$self}       = [];
        $incoming_requests{$self}       = [];
        $queue_outgoing{$self}          = q[];
        $queue_incoming{$self}          = q[];
        $bitfield{$self}                = q[];
        $socket{$self}                  = $socket;
        $fileno{$self}                  = fileno($socket{$self});
        $client{$self}->_set_pulse($self, time + 5);
        return 1;
    }
    sub _get_fileno    { return $fileno{$_[0]}; }
    sub get_session    { return $session{$_[0]}; }
    sub _get_connected { return $connected{$_[0]}; }

    sub _get_connection_timestamp {
        return $connection_timestamp{$_[0]};
    }
    sub _get_queue_outgoing { return $queue_outgoing{$_[0]}; }
    sub _get_socket         { return $socket{$_[0]}; }
    sub get_peer_id         { return $peer_id{$_[0]}; }
    sub get_client          { return $client{$_[0]}; }
    sub get_bitfield        { return $bitfield{$_[0]}; }
    sub get_is_choking      { return $is_choking{$_[0]}; }

    sub get_outgoing_requests {
        return $outgoing_requests{$_[0]};
    }

    sub get_peerhost {    # cache it
        my ($self) = @_;
        if (not defined $peerhost{$self}
            and $connected{$self})
        {   my (undef, undef, @address)
                = unpack(q[SnC4x8], getpeername($socket{$self}));
            $peerhost{$self} = join q[.], @address;
        }
        return $peerhost{$self};
    }

    sub get_peerport {    # cache it
        my ($self) = @_;
        if (not defined $peerport{$self}
            and $connected{$self})
        {   (undef, $peerport{$self}, undef)
                = unpack(q[SnC4x8], getpeername($socket{$self}));
        }
        return $peerport{$self};
    }

    sub _pulse {
        my ($self) = @_;

        #         if (    (time - $connection_timestamp{$self} >= 10 * 60)
        #~             and (time - $previous_incoming_block{$self} >= 5 * 60))
        #~         {    # TODO: make this timeout a variable
        #~             $self->_disconnect(q[peer must be useless]);
        #~             return 0;
        #~         }
        #~         if (    (time - $connection_timestamp{$self} >= 60)
        #~             and (time - $previous_incoming_data{$self} >= 130))
        #~         {    # TODO: make this timeout a variable
        #~             $self->_disconnect(q[Peer must be dead]);
        #~             return 0;
        #~         }
        #
        if ($connected{$self}) {

            #if (not defined $previous_outgoing_keepalive{$self}
            #    or $previous_outgoing_keepalive{$self} + 90 < time)
            #{   $self->_build_packet_keepalive;
            #}
            $self->_action_check_interesting if not $is_interesting{$self};
            $self->_action_unchoke
                if $is_interested{$self}
                    and $is_choked{$self
                    }; # XXX - and we have less than... eight(?) unchoked peers for this session
            $self->_action_cancel_old_requests();
            $self->_action_request_block();
            if (@{$incoming_requests{$self}}

                #and length($queue_outgoing{$self})
                #< $self->get_client->get_max_buffer_per_conn
                )
            {   my $request = shift @{$incoming_requests{$self}};
                if ($request and $request->get_piece->get_verified_integrity)
                {   $self->_build_packet_piece($request);
                    $uploaded{$self} += $request->get_length;
                    $session{$self}->_inc_uploaded($request->get_length);
                }
            }
        }
        return
            $client{$self}->_set_pulse($self,
                                       min(time + 10,
                                           $client{$self}->_get_pulse($self)
                                       )
            )

            #return $next_pulse{$self}
            #    = min(time + 10, $next_pulse{$self});
    }

    sub _process_one {
        my ($self, $read, $write) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        return if not defined $socket{$self};
        my $actual_read = $read ? $self->_read($read) : 0;
        return if not defined $socket{$self};
        my $actual_write = $write ? $self->_write($write) : 0;
        return ($actual_read, $actual_write);
    }

    sub _read {
        my ($self, $length) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        if (not $socket{$self} || not defined $socket{$self}) {
            $self->_disconnect;
            return;
        }
        my $actual_read =
            sysread($socket{$self},
                    $queue_incoming{$self},
                    $length,
                    (defined $queue_incoming{$self}
                     ? length($queue_incoming{$self})
                     : 0
                    )
            );
        if ($actual_read) {
            $previous_incoming_data{$self} = time;
            if (not $connected{$self}
                and defined $session{$self})
            {   $self->_action_send_bitfield;
            }
            $client{$self}
                ->_do_callback(q[peer_incoming_data], $self, $actual_read);
            while ($self->_parse_packet) {;}
        }
        else {
            $self->_disconnect($^E);
        }
        return $actual_read;
    }

    sub _write {
        my ($self, $length) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        if (not $socket{$self} || not defined $socket{$self}) {
            $self->_disconnect;
            return;
        }
        my $actual_write =
            syswrite($socket{$self},
                     substr($queue_outgoing{$self}, 0, $length, q[]),
                     $length);
        if ($actual_write) {
            $client{$self}
                ->_do_callback(q[peer_outgoing_data], $self, $actual_write);
        }
        else { $self->_disconnect($^E); }
        return $actual_write;
    }

    sub _disconnect {
        my ($self, $reason) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        close $socket{$self} if $socket{$self};
        $client{$self}->_remove_connection($self);
        $reason ||= q[Connection closed by remote peer.];
        $client{$self}->_do_callback(q[peer_disconnect], $self, $reason);
        delete $socket{$self};
        return 1;
    }

    # Packet stuff - Build
    sub _build_packet_handshake {
        my ($self) = @_;
        $queue_outgoing{$self} .=
            pack(q[c/a* a8 a20 a20],
                 q[BitTorrent protocol],
                 $client{$self}->_build_reserved,
                 pack(q[H40], $session{$self}->get_infohash),
                 $client{$self}->get_peer_id
            );
        $client{$self}->_do_callback(q[peer_outgoing_handshake], $self);
        return 1;
    }

    sub _build_packet_bitfield {
        my ($self) = @_;
        $queue_outgoing{$self} .= pack(q[Nca*],
                                    length($session{$self}->get_bitfield) + 1,
                                    5, $session{$self}->get_bitfield);
        $client{$self}->_do_callback(q[peer_outgoing_bitfield], $self);
        return 1;
    }

    sub _build_packet_have_all {    # FastExt
        my ($self) = @_;
        return
            if not $_supports_FastPeers{$self};
        $queue_outgoing{$self} .= pack(q[Nc], 1, 14);
        $client{$self}->_do_callback(q[peer_outgoing_have_all], $self);
        return 1;
    }

    sub _build_packet_have_none {    # FastExt
        my ($self) = @_;
        return
            if not $_supports_FastPeers{$self};
        $queue_outgoing{$self} .= pack(q[Nc], 1, 15);
        $client{$self}->_do_callback(q[peer_outgoing_have_none], $self);
        return 1;
    }

    sub _build_packet_ExtProtocol {
        my ($self, $messageID, $data) = @_;
        return
            if not $_supports_ExtProtocol{$self};
        my $packet = pack(q[ca*], $messageID, bencode $data);
        $queue_outgoing{$self}
            .= pack(q[Nca*], length($packet) + 1, 20, $packet);
        $client{$self}->_do_callback(q[peer_outgoing_extended], $self);
        return 1;
    }

    sub _build_packet_interested {
        my ($self) = @_;
        $queue_outgoing{$self} .= pack(q[Nc], 1, 2);
        $client{$self}->_do_callback(q[peer_outgoing_interested], $self);
        return 1;
    }

    sub _build_packet_not_interested {
        my ($self) = @_;
        $queue_outgoing{$self} .= pack(q[Nc], 1, 3);
        $client{$self}->_do_callback(q[peer_outgoing_disinterested], $self);
        return 1;
    }

    sub _build_packet_keepalive {
        my ($self) = @_;
        $queue_outgoing{$self} .= pack(q[N], 0);
        $client{$self}->_do_callback(q[peer_outgoing_keepalive], $self);
        $previous_outgoing_keepalive{$self} = time;
        return 1;
    }

    sub _build_packet_choke {
        my ($self) = @_;
        $queue_outgoing{$self} .= pack(q[Nc], 1, 0);
        $client{$self}->_do_callback(q[peer_outgoing_choke], $self);
        return 1;
    }

    sub _build_packet_unchoke {
        my ($self) = @_;
        $queue_outgoing{$self} .= pack(q[Nc], 1, 1);
        $client{$self}->_do_callback(q[peer_outgoing_unchoke], $self);
        return 1;
    }

    sub _build_packet_have {
        my ($self, $index) = @_;
        $queue_outgoing{$self} .= pack(q[NcN], 5, 4, $index);
        $client{$self}->_do_callback(q[peer_outgoing_have], $self, $index);
        return 1;
    }

    sub _build_packet_request {
        my ($self, $block) = @_;
        my $packed = pack(q[NNN],
                          $block->get_index, $block->get_offset,
                          $block->get_length);
        $queue_outgoing{$self}
            .= pack(q[Nca*], length($packed) + 1, 6, $packed);
        $client{$self}->_do_callback(q[peer_outgoing_request], $self, $block);
        return 1;
    }

    sub _build_packet_piece {
        my ($self, $request) = @_;
        my $packed = pack(q[N2a*],
                          $request->get_index, $request->get_offset,
                          $request->_read);
        $queue_outgoing{$self}
            .= pack(q[Nca*], length($packed) + 1, 7, $packed);
        $client{$self}->_do_callback(q[peer_outgoing_block], $self, $request);
        return 1;
    }

    sub _build_packet_cancel {
        my ($self, $block) = @_;
        my $packed = pack(q[NNN],
                          $block->get_index, $block->get_offset,
                          $block->get_length);
        $queue_outgoing{$self}
            .= pack(q[Nca*], length($packed) + 1, 8, $packed);
        $client{$self}->_do_callback(q[peer_outgoing_cancel], $self, $block);
        return 1;
    }

    # Packet stuff - Parse
    sub _parse_packet {    # TODO: refactor
        my ($self) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my $packet_len = unpack q[N], $queue_incoming{$self};
        return
            if not defined $packet_len
                or length $queue_incoming{$self} == 0;
        my (%ref, $type);
        if (unpack(q[c], $queue_incoming{$self}) == 0x13) {
            %ref = $self->_parse_packet_handshake($queue_incoming{$self});

            #$self->_build_packet_port; # No one ever sends these either...
        }
        else {
            return
                if $packet_len > length $queue_incoming{$self};
            (undef, my $packet_data, $queue_incoming{$self})
                = unpack q[Na] . ($packet_len) . q[ a*],
                $queue_incoming{$self};
            ($type, my $packet) = unpack(q[ca*], $packet_data);
            my %dispatch = (q[] => \&_parse_packet_keepalive,
                            0   => \&_parse_packet_choke,
                            1   => \&_parse_packet_unchoke,
                            2   => \&_parse_packet_interested,
                            3   => \&_parse_packet_disinterested,
                            4   => \&_parse_packet_have,
                            5   => \&_parse_packet_bitfield,
                            6   => \&_parse_packet_request,
                            7   => \&_parse_packet_piece,
                            8   => \&_parse_packet_cancel,
                            9   => \&_parse_packet_port,
                            14  => \&_parse_packet_have_all,
                            15  => \&_parse_packet_have_none,
                            16  => \&_parse_packet_reject,
                            17  => \&_parse_packet_allowed_fast,
                            20  => \&_parse_packet_extended
            );
            if (defined $dispatch{$type}) {
                %ref = $dispatch{$type}($self, $packet_len, $packet);
            }
            else {
                if (require Data::Dumper) {
                    warn $type;
                    warn q[Unhandled BitTorrent packet: ]
                        . Data::Dumper->Dump([$type, $packet],
                                             [qw[type infohash]]);
                }

                # XXX - think about banning this guy
                $self->_disconnect(q[Unknown or malformed packet]);
            }
        }
        if (%ref) {
            $client{$self}->_do_callback(q[peer_incoming_packet],
                                         $self,
                                         {length => $packet_len,
                                          type   => $type,
                                          data   => \%ref
                                         }
            );
            return 1;
        }
        return;
    }

    sub _parse_packet_handshake {
        my ($self, $packet) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        if (defined $peer_id{$self}) {
            $self->_disconnect(q[Second handshake packet]);
            return;
        }
        if (length $packet < 68) {
            $self->_disconnect(q[Not enough data for handshake packet]);
            return;
        }
        (my $protocol_name,
         my $reserved, my $info_hash,
         my $peer_id, $queue_incoming{$self}
        ) = unpack(q[c/a a8 H40 a20 a*], $packet);
        if ($protocol_name ne q[BitTorrent protocol]) {
            $self->_disconnect(
                        sprintf(q[Improper handshake; Bad protocol name (%s)],
                                $protocol_name)
            );
            return;
        }
        if ($peer_id eq $self->get_client->get_peer_id) {
            warn q[Connected to ourselves];
            $self->_disconnect(q[We connected to ourselves.]);
            return;
        }
        $reserved{$self} = $reserved;
        $self->_parse_reserved();
        if ($incoming_connection{$self}) {
            my $session = $client{$self}->_locate_session($info_hash);
            if (defined $session
                and $session->isa(q[Net::BitTorrent::Session]))
            {
                if ($session->get_peers
                    > $self->get_client->get_conns_per_session)
                {   $self->_disconnect(q[We have enough peers]);
                    return;
                }
                $session{$self} = $session;
                if (scalar grep {
                        defined $_->get_peer_id
                            and $_->get_peer_id eq $peer_id
                    } $session{$self}->get_peers
                    )
                {   $self->_disconnect(
                                    q[We've already connected to this peer.]);
                    return;
                }
                $incoming_requests{$self} = [];
                $peer_id{$self}           = $peer_id;
                $bitfield{$self}
                    = pack(q[b*], q[0] x $session{$self}->get_piece_count);
                $self->_build_packet_handshake;
            }
            else {
                $self->_disconnect(
                               sprintf(q[We aren't serving this torrent (%s)],
                                       $info_hash)
                );
                return;
            }
        }
        if (not $connected{$self}) {
            $connected{$self} = 1;
            $client{$self}->_do_callback(q[peer_connect], $self);
            $self->_action_send_bitfield;
            $self->_action_send_ExtProtocol;

# $self->_build_packet_port; # ...no one seems to send these to me. Obsoleted by extProt?
            $self->_action_send_fastset;
        }
        $client{$self}->_do_callback(q[peer_incoming_handshake], $self);

        # Do stuff here.
        return (protocol  => $protocol_name,
                reserved  => $reserved,
                info_hash => $info_hash,
                peer_id   => $peer_id
        );
    }

    sub _parse_packet_bitfield {    # ID: 5
        my ($self, $packet_len, $packet) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my %ref = (load => q[]);
        if (length($packet) < 1) {
            $self->_disconnect(q[Incorrect packet length for BITFIELD]);
            return;
        }
        $bitfield{$self} = pack q[b*], unpack q[B*], $packet;   # vec friendly
        $self->_action_check_interesting;
        %ref = (bitfield => $packet);
        $client{$self}->_do_callback(q[peer_incoming_bitfield], $self);
        return %ref;
    }

    sub _parse_packet_keepalive {
        my ($self, $packet_len, $packet) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my %ref = (load => q[]);
        if (defined $packet) {
            $self->_disconnect(
                        sprintf(q[Incorrect packet length for keepalive (%d)],
                                length($packet))
            );
            return;
        }
        $client{$self}->_do_callback(q[peer_incoming_keepalive], $self);
        return %ref;
    }

    sub _parse_packet_unchoke {
        my ($self, $packet_len, $packet) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my %ref = (load => q[]);
        if ($packet_len != 1) {
            $self->_disconnect(q[Incorrect packet length for UNCHOKE]);
            return;
        }
        $is_choking{$self} = 0;
        $client{$self}->_set_pulse($self,
                                   min(time + 2,
                                       $client{$self}->_get_pulse($self)
                                   )
        );
        $client{$self}->_do_callback(q[peer_incoming_unchoke], $self);

        # Do stuff here.
        $self->_action_request_block();
        return %ref;
    }

    sub _parse_packet_choke {
        my ($self, $packet_len, $packet) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my %ref = (load => q[]);
        if ($packet_len != 1) {
            $self->_disconnect(q[Incorrect packet length for _choke]);
            return;
        }
        $is_choking{$self}     = 1;
        $is_interesting{$self} = 1;
        grep { $_->_remove_peer($self) } @{$outgoing_requests{$self}};
        $outgoing_requests{$self} = [];
        $client{$self}->_do_callback(q[peer_incoming_choke], $self);

        # Do stuff here.
        return %ref;
    }

    sub _parse_packet_have {    # ID: 4
        my ($self, $packet_len, $packet) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my %ref = (load => q[]);
        if ($packet_len != 5) {
            $self->_disconnect(q[Incorrect packet length for HAVE]);
            return;
        }
        my ($index) = unpack(q[N], $packet);
        if (not defined $index) {
            $self->_disconnect(q[Malformed HAVE packet]);
            return;
        }
        vec($bitfield{$self}, $index, 1) = 1;
        $client{$self}->_do_callback(q[peer_incoming_have], $self, $index);
        %ref = (index => $index);

        # Do stuff here.
        $self->_action_check_interesting;
        return %ref;
    }

    sub _parse_packet_disinterested {
        my ($self, $packet_len, $packet) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my %ref = (load => q[]);
        if ($packet_len != 1) {
            $self->_disconnect(q[Incorrect packet length for DISINTERESTED]);
            return;
        }
        $is_interested{$self} = 0;
        $client{$self}->_do_callback(q[peer_incoming_disinterested], $self);

        # Do stuff here.
        return %ref;
    }

    sub _parse_packet_interested {
        my ($self, $packet_len, $packet) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my %ref = (load => q[]);
        if ($packet_len != 1) {
            $self->_disconnect(q[Incorrect packet length for INTERESTED]);
            return;
        }
        $is_interested{$self} = 1;
        $client{$self}->_do_callback(q[peer_incoming_interested], $self);

        # Do stuff here.
        return %ref;
    }

    sub _parse_packet_cancel {
        my ($self, $packet_len, $packet) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my %ref = (load => q[]);
        if ($packet_len != 13) {
            $self->_disconnect(q[Incorrect packet length for cancel]);
            return;
        }
        my ($index, $offset, $length) = unpack(q[N3], $packet);
        %ref = (index  => $index,
                offset => $offset,
                length => $length
        );
        my ($request) = grep {
                    $_->get_index == $index
                and $_->get_offset == $offset
                and $_->get_length == $length
        } @{$incoming_requests{$self}};
        if (defined $request) {
            $client{$self}
                ->_do_callback(q[peer_incoming_cancel], $self, $request);
        }
        else {
            $self->_disconnect(
                q[Peer has canceled a request they never made or has already been filled.]
            );
        }

        # Do stuff here.
        return %ref;
    }

    sub _parse_packet_have_all {
        my ($self, $packet_len, $packet) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my %ref = (load => q[]);
        if (!$_supports_FastPeers{$self}) {
            $self->_disconnect(
                q[Invalid packet: Peer does not claim to support Fast Extension but has sent us a HAVE ALL message]
            );
            return;
        }
        if ($packet_len != 1) {
            $self->_disconnect(q[Incorrect packet length for HAVE ALL]);
            return;
        }
        $bitfield{$self} = pack(q[b*],
                                (q[1] x ($session{$self}->get_piece_count + 1)
                                )
        );
        $client{$self}->_do_callback(q[peer_incoming_have_all], $self);
        return %ref;
    }

    sub _parse_packet_have_none {
        my ($self, $packet_len, $packet) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my %ref = (load => q[]);
        if (!$_supports_FastPeers{$self}) {
            $self->_disconnect(
                q[Invalid packet: Peer does not claim to support Fast Extension but has sent us a HAVE NONE message]
            );
            return;
        }
        if ($packet_len != 1) {
            $self->_disconnect(q[Incorrect packet length for HAVE NONE]);
            return;
        }
        $bitfield{$self}
            = pack(q[b*], q[0] x ($session{$self}->get_piece_count + 1));
        $client{$self}->_do_callback(q[peer_incoming_have_none], $self);

        # Do stuff here.
        return %ref;
    }

    sub _parse_packet_reject {
        my ($self, $packet_len, $packet) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my %ref = (load => q[]);
        if (!$_supports_FastPeers{$self}) {
            $self->_disconnect(
                q[Invalid packet: Peer does not claim to support Fast Extension but has sent us a REJECT message]
            );
            return;
        }
        if ($packet_len != 13) {
            $self->_disconnect(q[Incorrect packet length for REJECT]);
            return;
        }
        my ($index, $offset, $length) = unpack(q[N3], $packet);
        %ref = (index  => $index,
                offset => $offset,
                length => $length
        );

        # Do stuff here.
        return %ref;
    }

    sub _parse_packet_allowed_fast {
        my ($self, $packet_len, $packet) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my %ref = (load => q[]);
        if (!$_supports_FastPeers{$self}) {
            $self->_disconnect(
                q[Invalid packet: Peer does not claim to support Fast Extension but has sent us an ALLOWED FAST message]
            );
            return;
        }
        if ($packet_len != 5) {
            $self->_disconnect(q[Incorrect packet length for ALLOWED FAST]);
            return;
        }
        my ($index) = unpack(q[N], $packet);
        %ref = (index => $index);

        # Do stuff here.
        return %ref;
    }

    sub _parse_packet_extended {
        my ($self, $packet_len, $packet) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my %ref = (load => q[]);
        if ($_supports_ExtProtocol{$self}) {
            $self->_disconnect(
                q[User does not support the Ext. Protocol but has sent an Ext. Protocol packet]
            );
            return;
        }
        if (length($packet) < 3) {
            $self->_disconnect(q[Incorrect packet length for Ext. Protocol]);
            return;
        }
        my ($messageid, $data) = unpack(q[ca*], $packet);
        my $_content = bdecode($data);
        %ref = (messageid => $messageid,
                packet    => $_content);

        #use Data::Dump qw[pp];
        #warn pp \%ref;
        #warn pp $_content;
        #warn pp $data;
        # messageid:
        #  0 = handshake
        # >0 = extended message as specified by the handshake
        # Do stuff here.
        return %ref;
    }

    sub _parse_packet_piece {
        my ($self, $packet_len, $packet) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my %ref = (load => q[]);
        if (length($packet) < 9) {
            $self->_disconnect(
                   sprintf(
                       q[Incorrect packet length for PIECE (%d requires >=9)],
                       length($packet))
            );
            return;
        }
        my ($index, $offset, $data) = unpack(q[N2a*], $packet);
        %ref = (block  => $data,
                index  => $index,
                offset => $offset,
                length => length($data)
        );
        if (not $session{$self}->get_pieces->[$index]->get_working) {
            $self->_disconnect(q[Malformed PIECE packet. (1)]);
        }
        else {
            my $block = $session{$self}->get_pieces->[$index]
                ->get_blocks->{$offset};
            if (   (not defined $block)
                or (length($data) != $block->get_length))
            {   $self->_disconnect(q[Malformed PIECE packet. (2)]);

                #
                #}
                #elsif (not scalar $block->get_peers
                #    or not $block->get_request_timestamp($self))
                #{
                # TODO: ...should we accept the block anyway if we need it?
                $self->_disconnect(
                    q[Peer sent us a piece we've already canceled or we never asked for.]
                );
            }
            else {
                $client{$self}
                    ->_do_callback(q[peer_incoming_block], $self, $block);
                if ($block->_write($data)) {
                    delete $session{$self}->get_pieces->[$index]
                        ->get_blocks->{$offset};
                    @{$outgoing_requests{$self}}
                        = grep { $_ ne $block } @{$outgoing_requests{$self}};
                    $client{$self}->_set_pulse($self,
                            min(time + 5, $client{$self}->_get_pulse($self)));
                    $downloaded{$self} += length $data;
                    $session{$self}->_inc_downloaded(length $data);
                    $previous_incoming_block{$self} = time;
                    $block->get_piece->_set_previous_incoming_block(time);

                  # TODO: if endgame, cancel all other requests for this block
                    if (scalar($block->get_peers) > 1) {
                        for my $peer ($block->get_peers) {
                            $peer->_action_cancel_block($block)
                                unless $peer == $self;
                        }
                    }
                    if (not scalar values %{$block->get_piece->get_blocks}) {
                        if ($block->get_piece->get_verified_integrity) {
                            grep {
                                $_->_build_packet_have(
                                                 $block->get_piece->get_index)
                            } $session{$self}->get_peers;
                        }
                        else {
                            die
                                q[BAD PIECE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!];

                            # TODO: penalize all peers related to piece
                            # See N::B::P::verify()
                        }
                    }
                    else {

                        #die q[Gah];
                        # .:shrugs:.
                    }
                }
            }
        }
        return %ref;
    }

    sub _parse_packet_request {
        my ($self, $packet_len, $packet) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my %ref = (load => q[]);
        if ($packet_len != 13) {
            $self->_disconnect(q[Incorrect packet length for REQUEST]);
            return;
        }
        my ($index, $offset, $length) = unpack(q[N3], $packet);
        %ref = (index  => $index,
                offset => $offset,
                length => $length
        );
        my $request =
            Net::BitTorrent::Session::Peer::Request->new(
                                                        {index  => $index,
                                                         offset => $offset,
                                                         length => $length,
                                                         peer   => $self
                                                        }
            );
        push @{$incoming_requests{$self}}, $request;
        $client{$self}
            ->_do_callback(q[peer_incoming_request], $self, $request);
        return %ref;
    }

    sub _parse_packet_port {    # DHT
        my ($self, $packet_len, $packet) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        if ($packet_len != 3) {
            $self->_disconnect(q[Incorrect packet length for PORT message]);
            return;
        }
        $listening_port{$self} = unpack(q[n], $packet);
        $self->get_session->append_nodes(
                                compact(sprintf q[%s:%d], $self->get_peerhost,
                                        $listening_port{$self}
                                )
        );
        return;
    }

    sub _parse_reserved {    # ...sub-packet, actually.
        my $self = shift;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my ($reserved) = [map {ord} split(q[], $reserved{$self})];
        $_supports_DHT{$self}         = ($reserved->[7] &= 0x01 ? 1 : 0);
        $_supports_FastPeers{$self}   = ($reserved->[7] &= 0x04 ? 1 : 0);
        $_supports_ExtProtocol{$self} = ($reserved->[5] &= 0x10 ? 1 : 0);
        $_supports_Encryption{$self} = 0;    # ...todo
        $_supports_BitComet{$self}
            = (($reserved->[1] . $reserved->[1] eq q[ex]) ? 1 : 0);
        $_fastset_out{$self} = [] if $_supports_FastPeers{$self};
        $_fastset_in{$self}  = [] if $_supports_FastPeers{$self};
        $client{$self}->get_dht->add_node($$self) if $_supports_DHT{$self};
        return 1;
    }

    # Actions
    sub _action_send_bitfield {
        my ($self) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        if ($_supports_FastPeers{$self}) {
            my $good = 0;
            if ($session{$self}->get_complete) {
                $self->_build_packet_have_all;
                $good++;
            }
            elsif (not(scalar(grep { $_->get_cached_integrity }
                                  @{$session{$self}->get_pieces})
                   )
                )
            {   $self->_build_packet_have_none();
                $good++;
            }
            return if $good;
        }
        if ((scalar $session{$self}->get_pieces > 100)
            and ((scalar(grep { not $_->get_cached_integrity }
                             @{$session{$self}->get_pieces})
                 ) <= 12
            )
            )
        {    # lazy bitfield
                # TODO: send a series of HAVE packets to save bandwidth
                # return
        }
        return $self->_build_packet_bitfield();
    }

    sub _action_send_ExtProtocol {
        my ($self) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        return if not $_supports_ExtProtocol{$self};
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        $self->_build_packet_ExtProtocol(
            0,
            {m => {ut_pex => 1, q[µT_PEX] => 2},
             (    # is incoming ? ():
                (p => $client{$self}->get_sockport)
             ),
             v      => q[Net::BitTorrent r] . $Net::BitTorrent::VERSION,
             yourip => pack(q[C4], ($self->get_peerhost =~ m[(\d+)]g)),
             reqq => 30    # XXX - Lies.  It's on my todo list...
                   # reqq == An integer, the number of outstanding request messages
                   # this client supports without dropping any.  The default in in
                   # libtorrent is 250.
            }
        );
        return;

# {
#  messageid => 0,
#  packet    => { e => 0, "m" => { ut_pex => 1 }, p => 21901, v => "\xC2\xB5Torrent 1.7.7" },
#}
    }

    sub _action_send_fastset {
        my ($self) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        return if not $_supports_FastPeers{$self};
        $self->_generate_fast_set;
        return 1;

        # XXX - uTorrent doesn't advertise a fast set... and neither will I.
        for my $index (@{$_fastset_out{$self}}) {
            $self->_build_packet_allowed_fast($index)

           #    if $session{$self}->get_pieces->[$index]->get_cached_integrity
           #        #and $peer->does not have
        }
        return;
    }

    sub _action_check_interesting {
        my ($self) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        return if not defined $session{$self};
        return if not defined $bitfield{$self} or not length $bitfield{$self};
        my $interesting = 0;
        if (not $session{$self}->get_complete
            and defined $bitfield{$self})
        {   for my $piece (
                grep {
                            vec($bitfield{$self}, $_->get_index, 1)
                        and !$_->get_cached_integrity
                        and $_->get_priority
                }

                #sort { $a->get_priority <=> $b->get_priority }
                @{$session{$self}->get_pieces}
                )
            {   $interesting = 1;
                last;
            }
        }
        if ($interesting and not $is_interesting{$self}) {
            $is_interesting{$self} = 1;
            $self->_build_packet_interested;
        }
        elsif (not $interesting and $is_interesting{$self}) {
            $is_interesting{$self} = 0;
            $self->_build_packet_not_interested;
        }
        return $interesting;
    }

    sub _action_cancel_block {
        my ($self, $block) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        $block->_remove_peer($self);
        @{$outgoing_requests{$self}}
            = grep { $_ ne $block } @{$outgoing_requests{$self}};
        $client{$self}->_set_pulse($self,
                                   min(time + 5,
                                       $client{$self}->_get_pulse($self)
                                   )
        );

        #$next_pulse{$self}= min((time + 5), $next_pulse{$self});
        return $self->_build_packet_cancel($block);
    }

    sub _action_choke {
        my ($self) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        return if $is_choked{$self};
        $is_choked{$self} = 1;
        return $self->_build_packet_choke();
    }

    sub _action_request_block {
        my ($self) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        if (($is_choking{$self} == 0)
            and (
                scalar @{$outgoing_requests{$self}}
                < 8    # XXX - This value MAY be set via DHT/ext.prot
            )
            )
        {   my $piece = $session{$self}->_pick_piece($self);
            if ($piece) {
            REQUEST:
                for (
                    scalar @{$outgoing_requests{$self}} ..
                    8    # XXX - This value MAY be set via DHT/ext.prot
                    )
                {   my $block = $piece->_locate_unrequested_block;
                    if ($block
                        and not grep { $$_ eq $$self } $block->get_peers)
                    {   $self->_build_packet_request($block);
                        $block->_add_peer($self);
                        push @{$outgoing_requests{$self}}, $block;
                    }
                    else {
                        last REQUEST;
                    }
                }
            }
        }
        else {

            #die;
        }
        return;
    }

    sub _action_unchoke {
        my ($self) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        return if not $is_choked{$self};
        $is_choked{$self} = 0;
        return $self->_build_packet_unchoke();
    }

    sub _action_cancel_old_requests {
        my ($self) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );

        # TODO: make this timeout variable
        my @remove = grep { $_->_request_timestamp($self) < (time - 300) }
            @{$outgoing_requests{$self}};
        for my $block (@remove) {
            $self->_action_cancel_block($block);
        }
        return;
    }

    sub _generate_fast_set {    # http://www.bittorrent.org/beps/bep_0006.html
        my ($self, $k)
            = @_;    # $k (optional) is the number of pieces in fast set
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my @a;
        $k ||= 9;

        # convert host to byte order, ie localhost is 0x7f000001
        my ($ip, undef) = split q[:], $$self;
        my $x = sprintf(q[%X],
                        (0xFFFFFF00 & (hex unpack q[H*], pack q[C*],
                                       $ip =~ m[(\d+)]g)
                        )
        );
        $x .= $session{$self}->get_infohash;
        my $piece_count = $session{$self}->get_piece_count;
        return if $piece_count <= $k;
        while (scalar @a < $k) {
            $x = sha1_hex(pack(q[H*], $x));
            for (my $i = 0; $i < 5 && scalar @a < $k; $i++) {
                my $j     = $i * 8;
                my $y     = hex(substr($x, $j, 8));
                my $index = $y % $piece_count;
                push(@a, $index)
                    unless grep { $_ == $index } @a;
            }
        }
        return $_fastset_out{$self} = \@a;
    }
    sub get_downloaded { return $downloaded{$_[0]}; }
    sub get_is_choked  { return $is_choked{$_[0]}; }

    sub get_incoming_connection {
        return $incoming_connection{$_[0]};
    }
    sub get_is_interested   { return $is_interested{$_[0]}; }
    sub get_is_interesting  { return $is_interesting{$_[0]}; }
    sub get_reserved        { return $reserved{$_[0]}; }
    sub get_uploaded        { return $uploaded{$_[0]}; }
    sub _get_queue_incoming { return $queue_incoming{$_[0]}; }

#     sub _build_packet_port {
#~         my ($self) = @_;
#~         return
#~             if not $_supports_DHT{$self};
#~         return if not $client{$self}->_ext_DHT;
#~
#~         #warn q[OUTGOING PORT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!];
#~         my $packed = pack(q[n], $self->get_client->get_dht->get_sockport);
#~         $queue_outgoing{$self}
#~             .= pack(q[Nca*], length($packed) + 1, 9, $packed);
#~         $client{$self}->_do_callback(q[peer_outgoing_port], $self);
#~         return 1;
#~     }
#~
#~     sub _build_packet_allowed_fast {
#~         my ($self, $index) = @_;
#~         $queue_outgoing{$self} .= pack(q[NcN], 5, 11, $index);
#~         $client{$self}->_do_callback(q[peer_outgoing_have], $self, $index);
#~         return 1;
#~     }
#
    sub as_string {
        my ($self, $advanced) = @_;
        my $dump = $self . q[ [TODO]];
        return print STDERR qq[$dump\n]
            unless defined wantarray;
        return $dump;
    }
    DESTROY {
        my ($self) = @_;
        $client{$self}->_del_pulse($self)
            if defined $client{$self};
        delete $client{$self};
        delete $peer_id{$self};
        delete $bitfield{$self};
        delete $incoming_requests{$session{$self}}{$self}
            if $session{$self};
        delete $outgoing_requests{$self};
        delete $session{$self} if $session{$self};
        delete $queue_incoming{$self};
        delete $queue_outgoing{$self};
        delete $is_interested{$self};
        delete $is_choked{$self};
        delete $is_interesting{$self};
        delete $is_choking{$self};
        delete $reserved{$self};
        delete $incoming_connection{$self};
        delete $connection_timestamp{$self};
        delete $connected{$self};
        close $socket{$self} if defined $socket{$self};
        delete $socket{$self};
        delete $previous_outgoing_keepalive{$self};
        delete $previous_incoming_data{$self};
        delete $previous_outgoing_request{$self};
        delete $previous_incoming_block{$self};
        delete $downloaded{$self};
        delete $uploaded{$self};
        delete $fileno{$self};
        delete $peerhost{$self};
        delete $peerport{$self};
        delete $listening_port{$self};
        delete $_supports_DHT{$self};
        delete $_supports_FastPeers{$self};
        delete $_supports_ExtProtocol{$self};
        delete $_supports_Encryption{$self};
        delete $_supports_BitComet{$self};
        delete $_fastset_out{$self};
        delete $_fastset_in{$self};
        return 1;
    }
}
1;
__END__

=pod

=head1 NAME

Net::BitTorrent::Session::Peer - Remote BitTorrent Peer

=head1 Constructor

=over 4

=item C<new ( { [ARGS] } )>

Creates a C<Net::BitTorrent::Session::Peer> object.  This constructor
should not be used directly.

=back

=head1 Methods

=over 4

=item C<as_string ( [ VERBOSE ] )>

Returns a 'ready to print' dump of the
C<Net::BitTorrent::Session::Peer> object's data structure.  If called
in void context, the structure is printed to C<STDERR>.

See also: L<Net::BitTorrent|Net::BitTorrent/"as_string ( [ VERBOSE ] )">

=item C<get_bitfield ( )>

Returns the bitfield representing the pieces this peer claims to have
successfully downloaded.

=item C<get_client ( )>

Returns the L<Net::BitTorrent|Net::BitTorrent> object related to this
peer.

=item C<get_downloaded ( )>

Returns the total amount of data downloaded from this peer.

See also: L<get_uploaded ( )|/"get_uploaded ( )">

=item C<get_incoming_connection ( )>

Returns a boolean indicating who initiated this connection.

=item C<get_is_choked ( )>

Returns a boolean indicating whether or not we are choking this peer.

=item C<get_is_choking ( )>

Returns a boolean indicating whether or not we are being choked by
this peer.

=item C<get_is_interested ( )>

Returns a boolean indicating whether or not this peer is interested in
downloading pieces from us.

=item C<get_is_interesting ( )>

Returns a boolean indicating whether or not we are interested in this
peer.

=item C<get_outgoing_requests ( )>

Returns a list of
L<Net::BitTorrent::Session::Peer::Request|Net::BitTorrent::Session::Peer::Request>
objects representing blocks this peer has asked us for.

See Also: L<Net::BitTorrent|Net::BitTorrent/"get_ul_slots_per_conn ( )">
L<Net::BitTorrent|Net::BitTorrent/"set_ul_slots_per_conn ( NEWVAL )">

=item C<get_peer_id ( )>

Returns the Peer ID used to identify this peer.

See also: theory.org (http://tinyurl.com/4a9cuv)

=item C<get_peerhost ( )>

Return the address part of the sockaddr structure for the socket on
the peer host in a text form xx.xx.xx.xx

=item C<get_peerport ( )>

Return the port number for the socket on the peer host.

=item C<get_reserved ( )>

Returns the eight (C<8>) byte string the peer sent us as part of the
BitTorrent handshake.

See also: theory.org (http://tinyurl.com/3lo5oj)

=item C<get_session ( )>

Returns the L<Net::BitTorrent::Session|Net::BitTorrent::Session>
object related to this peer.

=item C<get_uploaded ( )>

Returns the total amount of data uploaded to this peer.

See also: L<get_downloaded ( )|/"get_downloaded ( )">

=back

=head1 Author

Sanko Robinson <sanko@cpan.org> - http://sankorobinson.com/

CPAN ID: SANKO

=head1 License and Legal

Copyright 2008 by Sanko Robinson E<lt>sanko@cpan.orgE<gt>

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl 5.10 (or higher).  See
http://www.perl.com/perl/misc/Artistic.html or the F<LICENSE> file
included with this distribution.

All POD documentation is covered by the Creative Commons Attribution-
Noncommercial-Share Alike 3.0 License
(http://creativecommons.org/licenses/by-nc-sa/3.0/us/).

Neither this module nor the L<Author|/Author> is affiliated with
BitTorrent, Inc.

=for svn $Id$

=cut
