package Net::BitTorrent::Session::Peer;
use strict;
use warnings;
{

    BEGIN {
        use vars qw[$VERSION];
        use version qw[qv];
        our $SVN
            = q[$Id$];
        our $VERSION = sprintf q[%.3f], version->new(qw$Rev$)->numify / 1000;
    }
    use Socket
        qw[SOL_SOCKET SO_SNDTIMEO SO_RCVTIMEO PF_INET AF_INET SOCK_STREAM];
    use Fcntl qw[F_SETFL O_NONBLOCK];
    use Carp qw[carp];
    use Digest::SHA qw[sha1_hex];
    use lib q[../../../../lib];
    use Net::BitTorrent::Session::Peer::Request;
    use Net::BitTorrent::Util qw[min :bencode :log];
    {

        # identification
        my (%client,   %fileno,   %socket,   %peer_id, %session,
            %bitfield, %peerhost, %peerport, %reserved);

        # status
        my (%is_choked,
            %is_choking,
            %is_interested,
            %is_interesting,
            %queue_incoming,
            %queue_outgoing,
            %incoming_requests,
            %outgoing_requests,
            %incoming_connection,
            %connection_timestamp,
            %previous_incoming_block,
            %previous_outgoing_request,
            %previous_outgoing_keepalive,
            %connected,
            %extentions
        );

        # statistics
        my (%uploaded, %downloaded, %next_pulse,
            %previous_incoming_data);

        sub new {
            my ($class, $args) = @_;
            my $self = undef;
            if (    defined $args->{q[socket]}
                and defined $args->{q[client]})
            {   my (undef, $peerport, @address)
                    = unpack(q[SnC4x8],
                             getpeername($args->{q[socket]}));
                $self
                    = bless \
                    sprintf(q[%d.%d.%d.%d:%d], @address, $peerport),
                    $class;
                $socket{$self}              = $args->{q[socket]};
                $fileno{$self}              = fileno($socket{$self});
                $client{$self}              = $args->{q[client]};
                $incoming_connection{$self} = 1;
                $self->_set_defaults;
            }
            elsif (  defined $args->{q[address]}
                 and $args->{q[address]} =~ m[^(?:\d+\.?){4}:(?:\d+)$]
                 and defined $args->{q[session]}
                 and Scalar::Util::blessed($args->{q[session]})
                 and
                 $args->{q[session]}->isa(q[Net::BitTorrent::Session])
                 and not scalar grep { $_ eq $args->{q[address]} }
                 $args->{q[session]}->peers)
            {

                # perldoc perlipc
                socket(my ($socket),
                       &PF_INET, &SOCK_STREAM, getprotobyname(q[tcp]))
                    or next PORT;
                if ($^O eq q[MSWin32]) {
                    ioctl($socket, 0x8004667e, pack(q[I], 1));
                }
                else { fcntl($socket, F_SETFL, O_NONBLOCK) }
                my ($ip, $peerport) = split q[:],
                    $args->{q[address]}, 2;
                connect($socket,
                        pack(q[Sna4x8],
                             &AF_INET,
                             $peerport,
                             join(q[],
                                  map { chr $_ } ($ip =~ m[(\d+)]g))
                        )
                );

             # TODO: check value of err to verify non-blocking connect
                $self = bless \$args->{q[address]}, $class;
                $socket{$self}            = $socket;
                $fileno{$self}            = fileno($socket{$self});
                $connected{$self}         = 0;
                $session{$self}           = $args->{q[session]};
                $incoming_requests{$self} = [];
                $client{$self} = $args->{q[session]}->client;
                $incoming_connection{$self} = 0;
                $self->_set_defaults;
                $self->_build_packet_handshake();
                $peerhost{$self} = $ip;
                $peerport{$self} = $peerport;
            }
            return $self;
        }

        sub _set_defaults {
            my $self = shift;
            $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
            );
            $is_interesting{$self}          = 0;
            $is_interested{$self}           = 0;
            $is_choking{$self}              = 1;
            $is_choked{$self}               = 1;
            $downloaded{$self}              = 0;
            $uploaded{$self}                = 0;
            $connection_timestamp{$self}    = time;
            $previous_incoming_block{$self} = time;    # lies
            $previous_incoming_data{$self}  = time;    # lies
            $outgoing_requests{$self}       = [];

            #$incoming_requests{$self}       = {};
            $next_pulse{$self}     = time + 5;
            $queue_outgoing{$self} = q[];
            $queue_incoming{$self} = q[];
            $bitfield{$self}       = q[];
            $client{$self}->_do_callback(
                 q[log], TRACE,
                 sprintf(q[Exiting %s for %s], [caller 0]->[3], $$self
                 )
            );
            return 1;
        }
        {    # Accessors
            {    # Public

                sub bitfield {
                    my ($self) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    return $bitfield{$self};
                }

                sub client {
                    my ($self) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    return $client{$self};
                }

                sub downloaded {
                    my ($self) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    return $downloaded{$self};
                }

                sub is_choked {
                    my ($self) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    return $is_choked{$self};
                }

                sub is_choking {
                    my ($self) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    return $is_choking{$self};
                }

                sub incoming_connection {
                    my ($self) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    return $incoming_connection{$self};
                }

                sub is_interested {
                    my ($self) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    return $is_interested{$self};
                }

                sub is_interesting {
                    my ($self) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    return $is_interesting{$self};
                }

                sub outgoing_requests {
                    my ($self) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    return $outgoing_requests{$self};
                }

                sub peer_id {
                    my ($self) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    return $peer_id{$self};
                }

                sub peerhost {
                    my ($self) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    if (not defined $peerhost{$self}
                        and $connected{$self})
                    {   my (undef, undef, @address)
                            = unpack(q[SnC4x8],
                                     getpeername($socket{$self}));
                        $peerhost{$self} = join q[.], @address;
                    }
                    return $peerhost{$self};
                }

                sub peerport {
                    my ($self) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    if (not defined $peerport{$self}
                        and $connected{$self})
                    {   (undef, $peerport{$self}, undef)
                            = unpack(q[SnC4x8],
                                     getpeername($socket{$self}));
                    }
                    return $peerport{$self};
                }

                sub reserved {
                    my ($self) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    return $reserved{$self};
                }

                sub session {
                    my ($self) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    return $session{$self};
                }

                sub uploaded {
                    my ($self) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    return $uploaded{$self};
                }
            }
            {    # Private

                sub _connected {
                    my ($self) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    return $connected{$self};
                }

                sub _fileno {
                    my ($self) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    return $fileno{$self};
                }

                sub _socket {
                    my ($self) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    return $socket{$self};
                }

                sub _next_pulse {
                    my ($self) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    return $next_pulse{$self};
                }

                sub _connection_timestamp {
                    my ($self) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    return $connection_timestamp{$self};
                }

                sub _queue_outgoing {
                    my ($self) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    return $queue_outgoing{$self};
                }

                sub _queue_incoming {
                    my ($self) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    return $queue_incoming{$self};
                }
            }
        }
        {    # Debug

            sub as_string {
                my ($self, $advanced) = @_;
                $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                );
                my $dump = $self . q[ [TODO]];
                return print STDERR qq[$dump\n]
                    unless defined wantarray;
                return $dump;
            }
        }
        {    # Private Methods

            sub _process_one {
                my $self = shift;
                my $read = shift
                    ; # length (>= 0) we should read from this peer...
                my $write = shift
                    ;  # ...or write. In the future, this is how we'll
                       # limit bandwidth.
                $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                );
                my ($actual_read, $actual_write) = (0, 0);
                if ($write and defined $socket{$self}) {
                    $actual_write
                        = syswrite($socket{$self},
                                   substr($queue_outgoing{$self},
                                          0, $write, q[]
                                   ),
                                   $write
                        );
                    if ($actual_write) {
                        $client{$self}
                            ->_do_callback(q[peer_outgoing_data],
                                           $self, $actual_write);
                    }
                    else { $self->_disconnect($^E); goto RETURN; }
                }
                if ($read) {
                    $actual_read =
                        sysread($socket{$self},
                                $queue_incoming{$self},
                                $read,
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
                            ->_do_callback(q[peer_incoming_data],
                                           $self, $actual_read);
                        while ($self->_parse_packet) {;}
                    }
                    else {
                        $self->_disconnect($^E);
                    }
                }
            RETURN:
                return ($actual_read, $actual_write);
            }
        }
        {    # Parse
            {    # Parse Packet

                sub _parse_packet {    # TODO: refactor
                    my $self = shift;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    my $packet_len = unpack q[N],
                        $queue_incoming{$self};
                    return
                        if not defined $packet_len
                            or length $queue_incoming{$self} == 0;
                    my (%ref, $type);
                    if (unpack(q[c], $queue_incoming{$self}) == 0x13)
                    {   %ref = $self->_parse_packet_handshake( $queue_incoming{$self});
                    }
                    else {
                        return
                            if $packet_len
                                > length $queue_incoming{$self};
                        (undef, my $packet_data,
                         $queue_incoming{$self}
                            )
                            = unpack q[Na] . ($packet_len) . q[ a*],
                            $queue_incoming{$self};
                        ($type, my $packet)
                            = unpack(q[ca*], $packet_data);
                        my %dispatch = (
                            q[] => \&_parse_packet_keepalive,
                            0   => \&_parse_packet_choke,
                            1   => \&_parse_packet_unchoke,
                            2   => \&_parse_packet_interested,
                            3   => \&_parse_packet_disinterested,
                            4   => \&_parse_packet_have,
                            5   => \&_parse_packet_bitfield,
                            6   => \&_parse_packet_request,
                            7   => \&_parse_packet_piece,
                            8   => \&_parse_packet_cancel,

                            #9 => \&_parse_packet_port,
                            14 => \&_parse_packet_have_all,
                            15 => \&_parse_packet_have_none,

                            #16 => \&_parse_packet_reject,
                            #17 => \&_parse_packet_allowed_fast,
                            20 => \&_parse_packet_extended
                        );
                        if (defined $dispatch{$type}) {
                            %ref
                                = $dispatch{$type}($self, $packet_len,
                                                   $packet);
                        }
                        else {
                            $self->_disconnect(
                                      q[Unknown or malformed packet]);
                        }
                    }
                    if (%ref) {
                        $client{$self}->_do_callback(
                                              q[peer_incoming_packet],
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
                {

                    sub _parse_packet_bitfield {    # ID: 5
                        my ($self, $packet_len, $packet) = @_;
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        my %ref;
                        if (length($packet) < 1) {
                            $self->_disconnect(
                                q[Incorrect packet length for BITFIELD]
                            );
                            return;
                        }

                        # Make it vec friendly.
                        $bitfield{$self} = pack q[b*], unpack q[B*],
                            $packet;
                        $self->_action_check_interesting;
                        %ref = (bitfield => $packet);
                        $client{$self}
                            ->_do_callback(q[peer_incoming_bitfield],
                                           $self);

                        # Do stuff here.
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Exiting %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        return %ref;
                    }

                    sub _parse_packet_have {    # ID: 4
                        my ($self, $packet_len, $packet) = @_;
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        my %ref;
                        if ($packet_len != 5) {
                            $self->_disconnect(
                                 q[Incorrect packet length for HAVE]);
                            return;
                        }
                        my ($index) = unpack(q[N], $packet);
                        if (not defined $index) {
                            $self->_disconnect(
                                            q[Malformed HAVE packet]);
                            return;
                        }
                        vec($bitfield{$self}, $index, 1) = 1;
                        if (    $is_choking{$self}
                            and not $is_interesting{$self}
                            and not $session{$self}->pieces->[$index]
                            ->check)
                        {   $is_interesting{$self} = 1;
                            $self->_build_packet_interested;
                        }
                        %ref = (index => $index);
                        $client{$self}
                            ->_do_callback(q[peer_incoming_have],
                                           $self, $index);

                        # Do stuff here.
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Exiting %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        return %ref;
                    }

                    sub _parse_packet_disinterested {
                        my ($self, $packet_len, $packet) = @_;
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        my %ref;
                        if ($packet_len != 1) {
                            $self->_disconnect(
                                q[Incorrect packet length for DISINTERESTED]
                            );
                            return;
                        }
                        $is_interested{$self} = 0;
                        $client{$self}->_do_callback(
                                       q[peer_incoming_disinterested],
                                       $self);

                        # Do stuff here.
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Exiting %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        return %ref;
                    }

                    sub _parse_packet_interested {
                        my ($self, $packet_len, $packet) = @_;
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        my %ref;
                        if ($packet_len != 1) {
                            $self->_disconnect(
                                q[Incorrect packet length for INTERESTED]
                            );
                            return;
                        }
                        $is_interested{$self} = 1;
                        $client{$self}->_do_callback(
                                          q[peer_incoming_interested],
                                          $self);

                        # Do stuff here.
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Exiting %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        return %ref;
                    }

                    sub _parse_packet_unchoke {
                        my ($self, $packet_len, $packet) = @_;
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        my %ref;
                        if ($packet_len != 1) {
                            die 'bad length!';
                            $self->_disconnect(
                                q[Incorrect packet length for UNCHOKE]
                            );
                            return;
                        }
                        $is_choking{$self} = 0;
                        $next_pulse{$self}
                            = min($next_pulse{$self}, time + 2);
                        $client{$self}
                            ->_do_callback(q[peer_incoming_unchoke],
                                           $self);

                        # Do stuff here.
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Exiting %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        return %ref;
                    }

                    sub _parse_packet_choke {
                        my ($self, $packet_len, $packet) = @_;
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        my %ref;
                        if ($packet_len != 1) {
                            $self->_disconnect(
                                 q[Incorrect packet length for _choke]
                            );
                            return;
                        }
                        $is_choking{$self}     = 1;
                        $is_interesting{$self} = 1;
                        grep { $_->_remove_peer($self) }
                            @{$outgoing_requests{$self}};
                        $outgoing_requests{$self} = [];
                        $client{$self}
                            ->_do_callback(q[peer_incoming_choke],
                                           $self);

                        # Do stuff here.
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Exiting %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        return %ref;
                    }

                    sub _parse_packet_keepalive {
                        my ($self, $packet_len, $packet) = @_;
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        my %ref;
                        if (defined $packet) {
                            $self->_disconnect(
                                sprintf(
                                    q[Incorrect packet length for keepalive (%d)],
                                    length($packet))
                            );
                            return;
                        }
                        $client{$self}
                            ->_do_callback(q[peer_incoming_keepalive],
                                           $self);

                        # Do stuff here.
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Exiting %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        return %ref;
                    }

                    sub _parse_packet_cancel {
                        my ($self, $packet_len, $packet) = @_;
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        my %ref;
                        if ($packet_len != 13) {
                            $self->_disconnect(
                                 q[Incorrect packet length for cancel]
                            );
                            return;
                        }
                        my ($index, $offset, $length)
                            = unpack(q[N3], $packet);
                        %ref = (index  => $index,
                                offset => $offset,
                                length => $length
                        );
                        my ($request) = grep {
                                    $_->index == $index
                                and $_->offset == $offset
                                and $_->length == $length
                        } @{$incoming_requests{$self}};
                        if (defined $request) {
                            $client{$self}->_do_callback(
                                       q[peer_incoming_cancel], $self,
                                       $request);
                        }
                        else {
                            $self->_disconnect(
                                q[Peer has canceled a request they never made or has already been filled.]
                            );
                        }

                        # Do stuff here.
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Exiting %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        return %ref;
                    }

                    sub _parse_packet_port {
                        my ($self, $packet_len, $packet) = @_;
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        my %ref;
                        if (q[TODO: I'll get to this one day...]) {
                            $self->_disconnect(
                                q[We don't support PORT messages. Yet.]
                            );
                            return;
                        }
                        if ($packet_len != 3) {
                            $self->_disconnect(
                                q[Incorrect packet length for PORT message]
                            );
                            return;
                        }
                        my ($listen_port) = unpack(q[N], $packet);
                        %ref = (listen_port => $listen_port);

                        # Do stuff here.
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Exiting %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        return %ref;
                    }

                    sub _parse_packet_have_all {
                        my ($self, $packet_len, $packet) = @_;
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        my %ref;
                        if (!$extentions{$self}{q[supported]}
                            {q[FastPeers]})
                        {   $self->_disconnect(
                                q[Invalid packet: Peer does not claim to support Fast Extension but has sent us a HAVE ALL message]
                            );
                            return;
                        }
                        if ($packet_len != 1) {
                            $self->_disconnect(
                                q[Incorrect packet length for HAVE ALL]
                            );
                            return;
                        }
                        $bitfield{$self}
                            = pack(q[b*],
                                  q[1]
                                      x $session{$self}->piece_count);
                        $client{$self}
                            ->_do_callback(q[peer_incoming_have_all],
                                           $self);

                        # Do stuff here.
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Exiting %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        return %ref;
                    }

                    sub _parse_packet_have_none {
                        my ($self, $packet_len, $packet) = @_;
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        my %ref;
                        if (!$extentions{$self}{q[supported]}
                            {q[FastPeers]})
                        {   $self->_disconnect(
                                q[Invalid packet: Peer does not claim to support Fast Extension but has sent us a HAVE NONE message]
                            );
                            return;
                        }
                        if ($packet_len != 1) {
                            $self->_disconnect(
                                q[Incorrect packet length for HAVE NONE]
                            );
                            return;
                        }
                        $bitfield{$self}
                            = pack(q[b*],
                                  q[0]
                                      x $session{$self}->piece_count);
                        $client{$self}
                            ->_do_callback(q[peer_incoming_have_none],
                                           $self);

                        # Do stuff here.
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Exiting %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        return %ref;
                    }

                    sub _parse_packet_reject {
                        my ($self, $packet_len, $packet) = @_;
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        my %ref;
                        if (!$extentions{$self}{q[supported]}
                            {q[FastPeers]})
                        {   $self->_disconnect(
                                q[Invalid packet: Peer does not claim to support Fast Extension but has sent us a REJECT message]
                            );
                            return;
                        }
                        if ($packet_len != 13) {
                            $self->_disconnect(
                                 q[Incorrect packet length for REJECT]
                            );
                            return;
                        }
                        my ($index, $offset, $length)
                            = unpack(q[N3], $packet);
                        %ref = (index  => $index,
                                offset => $offset,
                                length => $length
                        );

                        # Do stuff here.
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Exiting %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        return %ref;
                    }

                    sub _parse_packet_allowed_fast {
                        my ($self, $packet_len, $packet) = @_;
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        my %ref;
                        if (!$extentions{$self}{q[supported]}
                            {q[FastPeers]})
                        {   $self->_disconnect(
                                q[Invalid packet: Peer does not claim to support Fast Extension but has sent us an ALLOWED FAST message]
                            );
                            return;
                        }
                        if ($packet_len != 5) {
                            $self->_disconnect(
                                q[Incorrect packet length for ALLOWED FAST]
                            );
                            return;
                        }
                        my ($index) = unpack(q[N], $packet);
                        %ref = (index => $index);

                        # Do stuff here.
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Exiting %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        return %ref;
                    }

                    sub _parse_packet_extended {
                        my ($self, $packet_len, $packet) = @_;
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        my %ref;
                        if ($extentions{$self}{q[supported]}
                            {q[ExtProtocol]})
                        {   $self->_disconnect(
                                q[User does not support the Ext. Protocol but has sent an Ext. Protocol packet]
                            );
                            return;
                        }
                        if (length($packet) < 3) {
                            $self->_disconnect(
                                q[Incorrect packet length for Ext. Protocol]
                            );
                            return;
                        }
                        my ($messageid, $data)
                            = unpack(q[ca*], $packet);
                        my ($_content) = bdecode($data);
                        %ref = (messageid => $messageid,
                                packet    => $_content);

                 # messageid:
                 #  0 = handshake
                 # >0 = extended message as specified by the handshake
                 # Do stuff here.
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Exiting %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        return %ref;
                    }

                    sub _parse_packet_handshake {
                        my ($self, $packet) = @_;
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        my %ref;

                        if(defined $peer_id{$self}) {
 $self->_disconnect(                                           q[Second handshake packet]);
return;
                            }

                        if (length $packet < 68) {
                            $self->_disconnect(
                                q[Not enough data for handshake packet]
                            );
                            return;
                        }
                        (my $protocol_name,
                         my $reserved, my $info_hash,
                         my $peer_id, $queue_incoming{$self}
                        ) = unpack(q[c/a a8 H40 a20 a*], $packet);
                        if ($protocol_name ne q[BitTorrent protocol])
                        {   $self->_disconnect(
                                sprintf(
                                    q[Improper handshake; Bad protocol name (%s)],
                                    $protocol_name)
                            );
                            return;
                        }
                        $reserved{$self} = $reserved;
                        $self->_parse_reserved();
                        if ($incoming_connection{$self}) {
                            my $session = $client{$self}
                                ->_locate_session($info_hash);
                            if (defined $session
                                and $session->isa(
                                          q[Net::BitTorrent::Session])
                                )
                            {
                                if ($session->peers > $self->client
                                    ->maximum_peers_per_session)
                                {  $self->_disconnect(
                                             q[We have enough peers]);
                                             return;
                                }
                                $session{$self}           = $session;
                                $incoming_requests{$self} = [];
                                if (scalar grep {
                                        defined $_->peer_id
                                            and $_->peer_id eq
                                            $peer_id
                                    } $session{$self}->peers
                                    )
                                {    $self->_disconnect(
                                        q[We've already connected to this peer.]
                                    );return
                                }
                                elsif ($peer_id eq
                                       $self->client->peer_id)
                                {   $self->_disconnect(
                                         q[We connected to ourselves.]
                                    );
                                    return;
                                }
                                $peer_id{$self} = $peer_id;
                                $bitfield{$self}
                                    = pack(q[b*],
                                           q[0] x $session{$self}
                                               ->piece_count);
                                $self->_build_packet_handshake;
                            }
                            else {
                                $self->_disconnect(
                                    sprintf(
                                        q[We aren't serving this torrent (%s)],
                                        $info_hash)
                                );
                                return;
                            }
                        }
                        if (not $connected{$self}) {
                            $connected{$self} = 1;
                            $client{$self}
                                ->_do_callback(q[peer_connect],
                                               $self);
                            $self->_action_send_bitfield;
                            $self->_action_send_ExtProtocol;
                            $self->_action_send_fastset;
                        }
                        %ref = (protocol  => $protocol_name,
                                reserved  => $reserved,
                                info_hash => $info_hash,
                                peer_id   => $peer_id,
                        );
                        $client{$self}
                            ->_do_callback(q[peer_incoming_handshake],
                                           $self);

                        # Do stuff here.
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Exiting %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        return %ref;
                    }

                    sub _parse_packet_piece {
                        my ($self, $packet_len, $packet) = @_;
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        my %ref;
                        if (length($packet) < 9) {
                            $self->_disconnect(
                                sprintf(
                                    q[Incorrect packet length for PIECE (%d requires >=9)],
                                    length($packet))
                            );
                            return;
                        }
                        my ($index, $offset, $data)
                            = unpack(q[N2a*], $packet);
                        %ref = (block  => $data,
                                index  => $index,
                                offset => $offset,
                                length => length($data),
                        );
                        if (not $session{$self}->pieces->[$index]
                            ->working)
                        {   $self->_disconnect(
                                      q[Malformed PIECE packet. (1)]);
                        }
                        else {
                            my $block
                                = $session{$self}->pieces->[$index]
                                ->blocks->{$offset};
                            if (   (not defined $block)
                                or (length($data) != $block->length))
                            {   $self->_disconnect(
                                      q[Malformed PIECE packet. (2)]);

           #
           #}
           #elsif (not scalar $block->peers
           #    or not $block->request_timestamp($self))
           #{
           # TODO: ...should we accept the block anyway if we need it?
                                $self->_disconnect(
                                    q[Peer sent us a piece we've already canceled or we never asked for.]
                                );
                            }
                            elsif ($block->_write($data)) {
                                delete $session{$self}
                                    ->pieces->[$index]
                                    ->blocks->{$offset};
                                @{$outgoing_requests{$self}}
                                    = grep { $_ ne $block }
                                    @{$outgoing_requests{$self}};
                                $next_pulse{$self}
                                    = min((time + 5),
                                          $next_pulse{$self});
                                $downloaded{$self} += length $data;
                                $session{$self}
                                    ->_inc_downloaded(length $data);
                                $previous_incoming_block{$self}
                                    = time;
                                $block->piece
                                    ->_previous_incoming_block(time);
                                $client{$self}->_do_callback(
                                    q[peer_incoming_block], $self,
                                    $block);

          # TODO: if endgame, cancel all other requests for this block
                                if (scalar($block->peers) > 1) {
                                    for my $peer ($block->peers) {
                                        $peer->_cancel_block($block)
                                            unless $peer == $self;
                                    }
                                }
                                if (not scalar
                                    values %{$block->piece->blocks})
                                {   if ($block->piece->verify) {
                                        grep {
                                            $_->_build_packet_have(
                                                 $block->piece->index)
                                        } $session{$self}->peers;
                                    }
                                    else {

                 #die q[BAD PIECE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!];
                 # TODO: penalize all peers related to piece
                 # See N::B::P::verify()
                                    }
                                }
                                else {

                                    #die q[Gah];
                                    # .:shrugs:.
                                }
                            }
                            else {

                                # .:shrugs:.
                            }
                        }
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Exiting %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        return %ref;
                    }

                    sub _parse_packet_request {
                        my ($self, $packet_len, $packet) = @_;
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        my %ref;
                        if ($packet_len != 13) {
                            $self->_disconnect(
                                q[Incorrect packet length for REQUEST]
                            );
                            return;
                        }
                        my ($index, $offset, $length)
                            = unpack(q[N3], $packet);
                        %ref = (index  => $index,
                                offset => $offset,
                                length => $length,
                        );
                        my $request
                            = Net::BitTorrent::Session::Peer::Request
                            ->new({index  => $index,
                                   offset => $offset,
                                   length => $length,
                                   peer   => $self
                                  }
                            );
                        push @{$incoming_requests{$self}}, $request;
                        $client{$self}
                            ->_do_callback(q[peer_incoming_request],
                                           $self, $request);
                        $client{$self}->_do_callback(
                                           q[log], TRACE,
                                           sprintf(
                                               q[Exiting %s for %s],
                                               [caller 0]->[3], $$self
                                           )
                        );
                        return %ref;
                    }
                }
            }
            {    # Parse Various

                sub _parse_reserved {
                    my $self = shift;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    my ($reserved)
                        = [map {ord} split(q[], $reserved{$self})];
                    $extentions{$self} = {
                            data => {FastPeers => {
                                               outgoing_fastset => [],
                                               incoming_fastset => []
                                     }
                            },
                            supported => {
                                BitComet => (
                                               $reserved->[1]
                                             . $reserved->[1] eq q[ex]
                                    )
                                ? 1
                                : 0,
                                DHT => $reserved->[7] &= 0x01 ? 1 : 0,
                                Encryption  => 0,
                                ExtProtocol => $reserved->[5]
                                    &= 0x10 ? 1 : 0,
                                FastPeers => $reserved->[7]
                                    &= 0x04 ? 1 : 0,
                                PEX => 0
                            }
                    };
                    return 1;
                }
            }
        }
        {    # Build
            {    # Build Packet

                sub _build_packet {    # refactor out of existance
                    my ($self, $packet) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    my $packet_data = q[];
                    my $data;
                    if ($packet->{q[type]} == 17) {
                        $packet_data
                            = pack(q[NcN],
                                   2, $packet->{q[type]},
                                   $data->{q[index]});
                    }
                    else {
                        require Data::Dumper;
                        carp(
                            sprintf(
                                q[Unknown packet! Please, include this in your bug report: %s],
                                Data::Dumper::Dump($packet))
                        );
                    }
                    return $queue_outgoing{$self} .= $packet_data;
                }

                sub _build_packet_keepalive {
                    my ($self) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    $queue_outgoing{$self} .= pack(q[N], 0);
                    $client{$self}
                        ->_do_callback(q[peer_outgoing_keepalive],
                                       $self);
                    $previous_outgoing_keepalive{$self} = time;
                    return 1;
                }

                sub _build_packet_choke {
                    my ($self) = @_;
                    $queue_outgoing{$self} .= pack(q[Nc], 1, 0);
                    $client{$self}
                        ->_do_callback(q[peer_outgoing_choke], $self);
                    return 1;
                }

                sub _build_packet_unchoke {
                    my ($self) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    $queue_outgoing{$self} .= pack(q[Nc], 1, 1);
                    $client{$self}
                        ->_do_callback(q[peer_outgoing_unchoke],
                                       $self);
                    return 1;
                }

                sub _build_packet_interested {
                    my ($self) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    $queue_outgoing{$self} .= pack(q[Nc], 1, 2);
                    $client{$self}
                        ->_do_callback(q[peer_outgoing_interested],
                                       $self);
                    return 1;
                }

                sub _build_packet_not_interested {
                    my ($self) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    $queue_outgoing{$self} .= pack(q[Nc], 1, 3);
                    $client{$self}
                        ->_do_callback(q[peer_outgoing_disinterested],
                                       $self);
                    return 1;
                }

                sub _build_packet_have {
                    my ($self, $index) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    $queue_outgoing{$self}
                        .= pack(q[NcN], 5, 4, $index);
                    $client{$self}
                        ->_do_callback(q[peer_outgoing_have], $self,
                                       $index);
                    return 1;
                }

                sub _build_packet_bitfield {
                    my ($self, $bitfield) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    $queue_outgoing{$self} .=
                        pack(q[Nca*],
                             length($bitfield) + 1,
                             5, $bitfield);
                    $client{$self}
                        ->_do_callback(q[peer_outgoing_bitfield],
                                       $self);
                    return 1;
                }

                sub _build_packet_request {
                    my ($self, $block) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    my $packed =
                        pack(q[NNN],
                             $block->index, $block->offset,
                             $block->length);
                    $queue_outgoing{$self}
                        .= pack(q[Nca*], length($packed) + 1, 6,
                                $packed);
                    $client{$self}
                        ->_do_callback(q[peer_outgoing_request],
                                       $self, $block);
                    return 1;
                }

                sub _build_packet_piece {
                    my ($self, $request) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    my $packed =
                        pack(q[N2a*],
                             $request->index, $request->offset,
                             $request->_read);
                    $queue_outgoing{$self}
                        .= pack(q[Nca*], length($packed) + 1, 7,
                                $packed);
                    $client{$self}
                        ->_do_callback(q[peer_outgoing_block], $self,
                                       $request);
                    return 1;
                }

                sub _build_packet_cancel {
                    my ($self, $block) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    my $packed =
                        pack(q[NNN],
                             $block->index, $block->offset,
                             $block->length);
                    $queue_outgoing{$self}
                        .= pack(q[Nca*], length($packed) + 1, 8,
                                $packed);
                    $client{$self}
                        ->_do_callback(q[peer_outgoing_cancel], $self,
                                       $block);
                    return 1;
                }

                sub _build_packet_have_all {
                    my ($self) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                     return
                if
                not $extentions{$self}{q[supported]}{q[FastPeers]};
            return if not $client{$self}->_ext_FastPeers;
                    $queue_outgoing{$self} .= pack(q[Nc], 5, 14);
                    $client{$self}
                        ->_do_callback(q[peer_outgoing_have_all],
                                       $self);
                    return 1;
                }

                sub _build_packet_have_none {
                    my ($self) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );

 return
                if
                not $extentions{$self}{q[supported]}{q[FastPeers]};
            return if not $client{$self}->_ext_FastPeers;

                    $queue_outgoing{$self} .= pack(q[Nc], 5, 15);
                    $client{$self}
                        ->_do_callback(q[peer_outgoing_have_none],
                                       $self);
                    return 1;
                }

                sub _build_packet_ExtProtocol {
                    my ($self, $messageID, $data) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );


 return
                if
                not $extentions{$self}{q[supported]}{q[ExtProtocol]};
            return if not $client{$self}->_ext_ExtProtocol;

                    my $packet
                        = pack(q[ca*], $messageID, bencode $data);

# {
#  messageid => 0,
#  packet    => { e => 0, "m" => { ut_pex => 1 }, p => 21901, v => "\xC2\xB5Torrent 1.7.7" },
#}
#{ v => "Net::BitTorrent r0.010" }
                    $queue_outgoing{$self} .=
                        pack(q[Nca*],
                             length($packet) + 1,
                             20, $packet);
                    $client{$self}
                        ->_do_callback(q[peer_outgoing_extended],
                                       $self);
                    return 1;
                }

                sub _build_packet_allowed_fast {
                    my ($self, $index) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    $queue_outgoing{$self}
                        .= pack(q[NcN], 5, 11, $index);
                    $client{$self}
                        ->_do_callback(q[peer_outgoing_have], $self,
                                       $index);
                    return 1;
                }

                sub _build_packet_handshake {
                    my ($self) = @_;
                    $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                    );
                    $queue_outgoing{$self} .=
                        pack(q[c/a* a8 a20 a20],
                             q[BitTorrent protocol],
                             $client{$self}->_build_reserved,
                             pack(q[H40], $session{$self}->infohash),
                             $client{$self}->peer_id
                        );
                    $client{$self}
                        ->_do_callback(q[peer_outgoing_handshake],
                                       $self);
                    return 1;
                }
            }
        }

        sub _pulse {
            my ($self) = @_;
            $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
            );
            if (    (time - $connection_timestamp{$self} >= 10 * 60)
                and (time - $previous_incoming_block{$self} >= 5 * 60)
                )
            {    # TODO: make this timeout a variable
                $self->_disconnect(q[peer must be useless]);
                return 0;
            }
            if (    (time - $connection_timestamp{$self} >= 60)
                and (time - $previous_incoming_data{$self} >= 130))
            {    # TODO: make this timeout a variable
                $self->_disconnect(q[Peer must be dead]);
                return 0;
            }
            if (not defined $previous_outgoing_keepalive{$self}
                or $previous_outgoing_keepalive{$self} + 90 < time)
            {   $self->_build_packet_keepalive;
            }
            $self->_action_check_interesting;
            $self->_action_request_block;
            $self->_action_cancel_old_requests;
            $self->_action_unchoke
                if $is_interested{$self} and $is_choked{$self};
            if (@{$incoming_requests{$self}}
                and length($queue_outgoing{$self})
                < $self->client->maximum_buffer_size)
            {   my $request = shift @{$incoming_requests{$self}};

                # TODO: verify piece before handing over data
                $self->_build_packet_piece($request);
                $uploaded{$self} += $request->length;
                $session{$self}->_inc_uploaded($request->length);
            }
            return $next_pulse{$self}
                = min(time + 10, $next_pulse{$self});
        }
        {    # Actions

            sub _action_cancel_block {
                my ($self, $block) = @_;
                $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                );
                $block->_remove_peer($self);
                @{$outgoing_requests{$self}} = grep { $_ ne $block }
                    @{$outgoing_requests{$self}};
                $next_pulse{$self}
                    = min((time + 5), $next_pulse{$self});
                return $self->_build_packet_cancel($block);
            }

            sub _action_choke {
                my ($self) = @_;
                $client{$self}->_do_callback(
                                       q[log], TRACE,
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
                $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
                );
                if (($is_choking{$self} == 0)
                    and (scalar @{$outgoing_requests{$self}}
                         < $client{$self}->maximum_requests_per_peer)
                    )
                {   my $piece = $session{$self}->_pick_piece($self);
                    if ($piece) {
                    REQUEST:
                        for (
                            scalar @{$outgoing_requests{$self}} ..
                            $client{$self}->maximum_requests_per_peer)
                        {   my $block = $piece->_unrequested_block;
if ($block
                                and not grep { $$_ eq $$self }
                                $block->peers)
                            {   $self->_build_packet_request($block);
                                $block->_add_peer($self);
                                push @{$outgoing_requests{$self}},
                                    $block;
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
        }

        sub _action_unchoke {
            my ($self) = @_;
            $client{$self}->_do_callback(
                                       q[log], TRACE,
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
            $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
            );

            # TODO: make this timeout variable
            my @remove
                = grep { $_->_request_timestamp($self) < (time - 300) }
                @{$outgoing_requests{$self}};
            for my $block (@remove) {
                $self->_action_cancel_block($block);
            }
            return;
        }

        sub _action_check_interesting {
            my ($self) = @_;
            $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
            );
            return if not defined $session{$self};
            my $interesting = 0;
            if (not $session{$self}->complete
                and defined $bitfield{$self})
            {   for my $index (0 .. $session{$self}->piece_count) {
                    if (vec($bitfield{$self}, $index, 1)
                        and
                        $session{$self}->pieces->[$index]->priority
                        and
                        not $session{$self}->pieces->[$index]->check)
                    {   $interesting = 1;
                        last;
                    }
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

        sub _action_send_bitfield {
            my ($self) = @_;
            $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
            );
            if (    $extentions{$self}{q[supported]}{q[FastPeers]}
                and $client{$self}->_ext_FastPeers)
            {   my $good = 0;
                if ($session{$self}->complete) {
                    $self->_build_packetha_have_all;
                    $good++;
                }
                elsif (not(scalar(grep { $_->check }
                                      @{$session{$self}->pieces})
                       )
                    )
                {   $self->_build_packet_have_none();
                    $good++;
                }
                return if $good;
            }
            if ((scalar $session{$self}->pieces > 100)
                and ((scalar(grep { not $_->check }
                                 @{$session{$self}->pieces})
                     ) <= 12
                )
                )
            {

               # TODO: send a series of HAVE packets to save bandwidth
               # return
            }
            return $self->_build_packet_bitfield(
                                           $session{$self}->bitfield);
        }

        sub _action_send_fastset {
            my ($self) = @_;
            $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
            );
            return
                if not $extentions{$self}{q[supported]}{q[FastPeers]};
            return if not $client{$self}->_ext_FastPeers;
            $self->_generate_fast_set;
            $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
            );
            for my $index (
                         @{  $extentions{$self}{q[data]}{q[FastPeers]}
                                 {q[outgoing_fastset]}
                         }
                )
            {    #$self->_build_packet_allowed_fast($index)
                    #    if $session{$self}->pieces->[$index]->check
                    #        #and $peer->does not have
            }
            return;
        }

        sub _action_send_ExtProtocol {
            my ($self) = @_;
            $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
            );
            return
                if
                not $extentions{$self}{q[supported]}{q[ExtProtocol]};
            return if not $client{$self}->_ext_ExtProtocol;
            $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
            );
            $self->_build_packet_ExtProtocol(
                0,
                {v => q[Net::BitTorrent r] . $Net::BitTorrent::VERSION
                }
            );
            return;

# {
#  messageid => 0,
#  packet    => { e => 0, "m" => { ut_pex => 1 }, p => 21901, v => "\xC2\xB5Torrent 1.7.7" },
#}
        }

        sub _disconnect {
            my ($self, $reason) = @_;
            $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
            );
            close $socket{$self};
            $connected{$self} = 0;
            $client{$self}->_remove_connection($self);
            $client{$self}->_do_callback(q[peer_disconnect], $self,
                                         ($reason || $^E))
                if $connected{$self};
            return 1;
        }

        sub _generate_fast_set {

            # http://www.bittorrent.org/beps/bep_0006.html
            my ($self, $k) = @_;
            $client{$self}->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
            );
            my @a;
            $k ||= 9;

            # convert host to byte order, ie localhost is 0x7f000001
            my ($ip, undef) = split q[:], $$self;
            my $x = sprintf(q[%X],
                            (0xFFFFFF00 & (hex unpack q[H*],
                                           pack q[C*],
                                           $ip =~ m[(\d+)]g
                             )
                            )
            );
            $x .= $session{$self}->infohash;
            my $piece_count = $session{$self}->piece_count;
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
            return $extentions{$self}{q[data]}{q[FastPeers]}
                {q[outgoing_fastset]} = \@a;
        }
        DESTROY {
            my ($self) = @_;
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
            delete $extentions{$self};
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
            delete $next_pulse{$self};
            delete $fileno{$self};
            delete $peerhost{$self};
            delete $peerport{$self};
            return 1;
        }
    }
}
1;
__END__

=pod

=head1 NAME

Net::BitTorrent::Session::Peer - Remote peer

=head1 CONSTRUCTOR

=over 4

=item C<new ( { [ARGS] } )>

Creates a C<Net::BitTorrent::Session::Peer> object.  This constructor
should not be used directly.

=back

=head1 METHODS

=over 4

=item C<as_string ( [ VERBOSE ] )>

Returns a 'ready to print' dump of the
C<Net::BitTorrent::Session::Peer> object's data structure.  If called
in void context, the structure is printed to C<STDERR>.

See also: [id://317520],
L<Net::BitTorrent::as_string()|Net::BitTorrent/as_string ( [ VERBOSE ] )>

=item C<bitfield ( )>

Returns a bitfield representing the pieces this peer claims to have
successfully downloaded.

=item C<client ( )>

Returns the L<Net::BitTorrent|Net::BitTorrent> object related to this
peer.

=item C<downloaded ( )>

Returns the total amount of data downloaded from this peer.

See also: L<uploaded ( )|/uploaded ( )>

=item C<incoming_connection ( )>

Returns a boolean indicating who initiated this connection.

=item C<is_choked ( )>

Returns a boolean indicating whether or not we are choking this peer.

=item C<is_choking ( )>

Returns a boolean indicating whether or not we are being choked by
this peer.

=item C<is_interested ( )>

Returns a boolean indicating whether or not this peer is interested in
downloading pieces from us.

=item C<is_interesting ( )>

Returns a boolean indicating whether or not we are interested in this
peer.

=item C<outgoing_requests ( )>

Returns a list of
L<Net::BitTorrent::Peer::Request|Net::BitTorrent::Peer::Request>
objects representing blocks this peer has asked us for.

=item C<peer_id ( )>

Returns the Peer ID used to identify this peer.

See also: theory.org (L<http://tinyurl.com/4a9cuv>)

=item C<peerhost ( )>

Return the address part of the sockaddr structure for the socket on
the peer host in a text form xx.xx.xx.xx

=item C<peerport ( )>

Return the port number for the socket on the peer host.

=item C<reserved ( )>

Returns the eight (C<8>) byte string the peer sent us as part of the
BitTorrent handshake.

See also: theory.org (L<http://tinyurl.com/3lo5oj>)

=item C<session ( )>

Returns the L<Net::BitTorrent::Session|Net::BitTorrent::Session>
object related to this peer.

=item C<uploaded ( )>

Returns the total amount of data uploaded to this peer.

See also: L<downloaded ( )|/downloaded ( )>

=back

=head1 AUTHOR

Sanko Robinson <sanko@cpan.org> - L<http://sankorobinson.com/>

CPAN ID: SANKO

=head1 LICENSE AND LEGAL

Copyright 2008 by Sanko Robinson E<lt>sanko@cpan.orgE<gt>

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.  See
L<http://www.perl.com/perl/misc/Artistic.html> or the F<LICENSE> file
included with this module.

All POD documentation is covered by the Creative Commons
Attribution-Noncommercial-Share Alike 3.0 License
(L<http://creativecommons.org/licenses/by-nc-sa/3.0/us/>).

Neither this module nor the L<AUTHOR|/AUTHOR> is affiliated with
BitTorrent, Inc.

=for svn $Id$

=cut
