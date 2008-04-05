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
    use Net::BitTorrent::Session::Peer::Request;
    use Net::BitTorrent::Util qw[min bdecode];
    {
        my ( %client,
             %fileno,
             %socket,
             %peer_id,
             %quality,
             %session,
             %bitfield,
             %peerhost,
             %peerport,
             %reserved,
             %uploaded,
             %connected,
             %is_choked,
             %downloaded,
             %is_choking,
             %next_pulse,
             %is_interested,
             %extentions_DHT,
             %extentions_PEX,
             %is_interesting,
             %queue_incoming,
             %queue_outgoing,
             %incoming_requests,
             %outgoing_requests,
             %extentions_BitComet,
             %extentions_protocol,
             %incoming_connection,
             %connection_timestamp,
             %extentions_FastPeers,
             %extentions_encryption,
             %previous_incoming_data,
             %previous_incoming_block,
             %previous_outgoing_request,
             %previous_outgoing_keepalive,
             %outgoing_fastset
        );

        # constructor
        sub new {
            my ( $class, $args ) = @_;
            my $self = undef;
            if (     defined $args->{q[socket]}
                 and defined $args->{q[client]} )
            {
                my ( undef, $peerport, @address )
                    = unpack( q[SnC4x8],
                              getpeername( $args->{q[socket]} ) );

                # Constructor.
                $self
                    = bless \
                    sprintf( q[%d.%d.%d.%d:%d], @address, $peerport ),
                    $class;
                $socket{$self}    = $args->{q[socket]};
                $fileno{$self}    = fileno( $socket{$self} );
                $client{$self}    = $args->{q[client]};
                $connected{$self} = 1;
                $incoming_connection{$self} = 1;
                $self->_set_defaults;
            }
            elsif (  defined $args->{q[address]}
                 and $args->{q[address]} =~ m[^(?:\d+\.?){4}:(?:\d+)$]
                 and defined $args->{q[session]}
                 and Scalar::Util::blessed( $args->{q[session]} )
                 and
                 $args->{q[session]}->isa(q[Net::BitTorrent::Session])
                 and not scalar grep { $_ eq $args->{q[address]} }
                 $args->{q[session]}->peers )
            {

                # [perldoc://perlipc]
                socket( my ($socket),
                        &PF_INET, &SOCK_STREAM,
                        getprotobyname(q[tcp]) )
                    or next PORT;
                if ( $^O eq q[MSWin32] ) {
                    ioctl( $socket, 0x8004667e, pack( q[I], 1 ) );
                }
                else { fcntl( $socket, F_SETFL, O_NONBLOCK ) }

                #if(
                #	(
                #		not
                #			(
                #				$^O eq q[Win32]
                #				? not ioctl($socket, 0x8004667e, pack(q[I], 1))
                #				: fcntl(fd, F_SETFL, ~O_NONBLOCK)
                #			)

#	)
#)
#{carp(q[Failed to set socket blocking status]);}
#if (not setsockopt($socket, SOL_SOCKET, SO_SNDTIMEO, pack(q[LL], 30, 0))
# ){carp(q[Failed to set socket timeout]);}
#if not( setsockopt($socket, SOL_SOCKET, SO_RCVTIMEO, pack(q[LL], 30, 0))
# ){or carp(q[Failed to set socket timeout]);}
#setsockopt($socket, SOL_SOCKET, SO_SNDBUF, 1024 * 256);
#setsockopt($socket, SOL_SOCKET, SO_RCVBUF, 1024 * 256);
                my ( $ip, $peerport ) = split q[:],
                    $args->{q[address]}, 2;
                connect( $socket,
                         pack( q[Sna4x8],
                               &AF_INET,
                               $peerport,
                               join( q[],
                                     map { chr $_ }
                                         ( $ip =~ m[(\d+)]g ) )
                         )
                );

             # TODO: check value of err to verify non-blocking connect
                $self = bless \$args->{q[address]}, $class;
                $socket{$self}            = $socket;
                $fileno{$self}            = fileno( $socket{$self} );
                $connected{$self}         = 0;
                $session{$self}           = $args->{q[session]};
                $incoming_requests{$self} = [];
                $client{$self} = $args->{q[session]}->client;
                $incoming_connection{$self} = 0;
                $self->_set_defaults;
                $self->_build_packet( {} );    # handshake
                $peerhost{$self} = $ip;
                $peerport{$self} = $peerport;
            }
            return $self;
        }

        sub _set_defaults {
            my $self = shift;
            $is_interesting{$self}          = 0;
            $is_interested{$self}           = 0;
            $is_choking{$self}              = 1;
            $is_choked{$self}               = 1;
            $quality{$self}                 = 0;
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
            return 1;
        }

        # static
        sub peer_id { my ($self) = @_; return $peer_id{$self}; }
        sub _socket { my ($self) = @_; return $socket{$self}; }

        sub peerport {
            my ($self) = @_;
            if ( not defined $peerport{$self}
                 and $connected{$self} )
            {
                ( undef, $peerport{$self}, undef )
                    = unpack( q[SnC4x8],
                              getpeername( $socket{$self} ) );
            }
            return $peerport{$self};
        }

        sub peerhost {
            my ($self) = @_;
            if ( not defined $peerhost{$self}
                 and $connected{$self} )
            {
                my ( undef, undef, @address )
                    = unpack( q[SnC4x8],
                              getpeername( $socket{$self} ) );
                $peerhost{$self} = join q[.], @address;
            }
            return $peerhost{$self};
        }
        sub _fileno    { my ($self) = @_; return $fileno{$self}; }
        sub _connected { my ($self) = @_; return $connected{$self}; }
        sub session    { my ($self) = @_; return $session{$self}; }
        sub bitfield   { my ($self) = @_; return $bitfield{$self}; }
        sub client     { my ($self) = @_; return $client{$self}; }

        sub downloaded {
            my ($self) = @_;
            return $downloaded{$self};
        }
        sub uploaded  { my ($self) = @_; return $uploaded{$self}; }
        sub is_choked { my ($self) = @_; return $is_choked{$self}; }

        sub is_choking {
            my ($self) = @_;
            return $is_choking{$self};
        }

        sub is_interested {
            my ($self) = @_;
            return $is_interested{$self};
        }

        sub is_interesting {
            my ($self) = @_;
            return $is_interesting{$self};
        }

        sub _next_pulse {
            my ($self) = @_;
            return $next_pulse{$self};
        }
        sub reserved { my ($self) = @_; return $reserved{$self}; }

        sub _connection_timestamp {
            my ($self) = @_;
            return $connection_timestamp{$self};
        }

        sub incoming_connection {
            my ($self) = @_;
            return $incoming_connection{$self};
        }

        sub _queue_outgoing {
            my ($self) = @_;
            return $queue_outgoing{$self};
        }

        sub _queue_incoming {
            my ($self) = @_;
            return $queue_incoming{$self};
        }

       #sub _add_outgoing_request {
       #    my ( $self, $request ) = @_;
       #    return
       #        unless $request->isa(
       #                          q[Net::BitTorrent::Session::Block]);
       #    return push @{ $outgoing_requests{$self} }, $request;
       #}

        sub outgoing_requests {
            my ($self) = @_;
            return $outgoing_requests{$self};
        }

        # Private Methods
        sub _process_one {
            my $self = shift;
            my $read = shift
                ;    # length (>= 0) we should read from this peer...
            my $write = shift
                ;    # ...or write. In the future, this is how we'll
                     # limit bandwidth.
            my ( $actual_read, $actual_write ) = ( 0, 0 );
            if ( $write and defined $socket{$self} ) {
                $actual_write =
                    syswrite( $socket{$self},
                              substr( $queue_outgoing{$self},
                                      0, $write, q[]
                              ),
                              $write
                    );
                if ($actual_write) {
                    $client{$self}
                        ->_do_callback( q[peer_outgoing_data], $self,
                                        $actual_write );
                }
                else { $self->_disconnect($^E); goto RETURN; }
            }
            if ($read) {
                $actual_read =
                    sysread( $socket{$self},
                             $queue_incoming{$self},
                             $read,
                             (  defined $queue_incoming{$self}
                                ? length( $queue_incoming{$self} )
                                : 0
                             )
                    );
                if ($actual_read) {
                    $previous_incoming_data{$self} = time;
                    if ( not $connected{$self} ) {
                        $connected{$self} = 1;
                        $client{$self}
                            ->_do_callback( q[peer_connect], $self );
                    }
                    $client{$self}
                        ->_do_callback( q[peer_incoming_data], $self,
                                        $actual_read );
                    while ( $self->_parse_packet ) {;}
                }
                else {
                    $self->_disconnect($^E);
                }
            }
        RETURN:
            return ( $actual_read, $actual_write );
        }

        sub _parse_reserved {
            my $self = shift;
            my ($reserved)
                = [ map {ord} split( q[], $reserved{$self} ) ];

            # official extentions
            $extentions_FastPeers{$self}
                = ( ( $reserved->[7] &= 0x04 ) ? 1 : 0 );
            $extentions_DHT{$self}
                = ( ( $reserved->[7] &= 0x01 ) ? 1 : 0 );

            # unofficial extentions
            $extentions_encryption{$self} = 0;    # TODO
            $extentions_PEX{$self}        = 0;    # TODO
            $extentions_protocol{$self}
                = ( ( $reserved->[5] &= 0x10 ) ? 1 : 0 );
            $extentions_BitComet{$self} = (
                          ( $reserved->[1] . $reserved->[1] eq q[ex] )
                          ? 1
                          : 0
            );
            return 1;
        }

        sub _parse_packet {                       # TODO: refactor
            my $self = shift;
            my $packet_len = unpack q[N], $queue_incoming{$self};
            return
                if not defined $packet_len
                    or length $queue_incoming{$self} == 0;
            my ( %ref, $type );
            if ( unpack( q[c], $queue_incoming{$self} ) == 0x13 ) {
                return $self->_disconnect(q[Second handshake packet])
                    if defined $peer_id{$self};
                $packet_len = 68;
                return
                    if $packet_len > length $queue_incoming{$self};
                (  my $protocol_name,
                   my $reserved, my $info_hash,
                   my $peer_id, $queue_incoming{$self}
                    )
                    = unpack( q[c/a a8 H40 a20 a*],
                              $queue_incoming{$self} );
                if ( $protocol_name ne q[BitTorrent protocol] ) {
                    $self->_disconnect(
                        qq[Improper handshake; Bad protocol name ($protocol_name)]
                    );
                    return;
                }
                $reserved{$self} = $reserved;
                $self->_parse_reserved();
                if ( $incoming_connection{$self} ) {
                    my $session
                        = $client{$self}->_locate_session($info_hash);
                    if ( defined $session
                        and $session->isa(q[Net::BitTorrent::Session])
                        )
                    {
                        if ( $session->peers
                            > $self->client->maximum_peers_per_session
                            )
                        {
                            return $self->_disconnect(
                                             q[We have enough peers]);
                        }
                        $session{$self}           = $session;
                        $incoming_requests{$self} = [];
                        if (scalar grep {
                                defined $_->peer_id
                                    and $_->peer_id eq $peer_id
                            } $session{$self}->peers
                            )
                        {
                            return $self->_disconnect(
                                q[We've already connected to this peer.]
                            );
                        }
                        elsif ( $peer_id eq $self->client->peer_id ) {
                            return $self->_disconnect(
                                       q[We connected to ourselves.]);
                        }
                        $peer_id{$self} = $peer_id;
                        $bitfield{$self}
                            = pack( q[b*],
                                    q[0] x $session{$self}
                                        ->piece_count );
                        $self->_build_packet( {} );    # handshake
                    }
                    else {
                        $self->_disconnect(
                            sprintf(
                                q[We aren't serving this torrent (%s)],
                                $info_hash )
                        );
                        return;
                    }
                }
                elsif ( not $connected{$self} ) {
                    $connected{$self} = 1;
                    $client{$self}
                        ->_do_callback( q[peer_connect], $self );
                }
                %ref = ( protocol  => $protocol_name,
                         reserved  => $reserved,
                         info_hash => $info_hash,
                         peer_id   => $peer_id,
                );
                $client{$self}
                    ->_do_callback( q[peer_incoming_handshake],
                                    $self );
                $self->_send_bitfield if defined $session{$self};
            }
            else {
                return
                    if $packet_len > length $queue_incoming{$self};
                ( undef, my $packet_data, $queue_incoming{$self} )
                    = unpack q[Na] . ($packet_len) . q[ a*],
                    $queue_incoming{$self};
                ( $type, my $packet )
                    = unpack( q[ca*], $packet_data );
                if ( $type eq q[] ) {
                    if ($packet_len) {
                        $self->_disconnect(
                            sprintf(
                                q[Incorrect packet length for keepalive (%d)],
                                $packet_len )
                        );
                        return;
                    }
                    $type = -1;
                    $client{$self}
                        ->_do_callback( q[peer_incoming_keepalive],
                                        $self );
                }
                elsif ( $type == 0 ) {
                    if ( $packet_len != 1 ) {
                        $self->_disconnect(
                               q[Incorrect packet length for _choke]);
                        return;
                    }
                    $is_choking{$self}     = 1;
                    $is_interesting{$self} = 1;
                    grep { $_->_remove_peer($self) }
                        @{ $outgoing_requests{$self} };
                    $outgoing_requests{$self} = [];
                    $client{$self}
                        ->_do_callback( q[peer_incoming_choke],
                                        $self );
                }
                elsif ( $type == 1 ) {
                    if ( $packet_len != 1 ) {
                        $self->_disconnect(
                              q[Incorrect packet length for UNCHOKE]);
                        return;
                    }
                    $is_choking{$self} = 0;
                    $next_pulse{$self}
                        = min( $next_pulse{$self}, time + 2 );
                    $client{$self}
                        ->_do_callback( q[peer_incoming_unchoke],
                                        $self );
                }
                elsif ( $type == 2 ) {
                    if ( $packet_len != 1 ) {
                        $self->_disconnect(
                             q[Incorrect packet length for INTERESTED]
                        );
                        return;
                    }
                    $is_interested{$self} = 1;
                    $client{$self}
                        ->_do_callback( q[peer_incoming_interested],
                                        $self );
                }
                elsif ( $type == 3 ) {
                    if ( $packet_len != 1 ) {
                        $self->_disconnect(
                            q[Incorrect packet length for not interested]
                        );
                        return;
                    }
                    $is_interested{$self} = 0;
                    $client{$self}->_do_callback(
                                       q[peer_incoming_disinterested],
                                       $self );
                }
                elsif ( $type == 4 ) {
                    if ( $packet_len != 5 ) {
                        $self->_disconnect(
                                 q[Incorrect packet length for HAVE]);
                        return;
                    }
                    my ($index) = unpack( q[N], $packet );
                    if ( not defined $index ) {
                        $self->_disconnect(q[Malformed HAVE packet]);
                        return;
                    }
                    vec( $bitfield{$self}, $index, 1 ) = 1;
                    if (    $is_choking{$self}
                        and not $is_interesting{$self}
                        and
                        not $session{$self}->pieces->[$index]->check )
                    {
                        $is_interesting{$self} = 1;
                        $self->_build_packet( { type => 2 } );
                    }
                    %ref = ( index => $index );
                    $client{$self}
                        ->_do_callback( q[peer_incoming_have], $self,
                                        $index );
                }
                elsif ( $type == 5 ) {
                    if ( $packet_len < 1 ) {
                        $self->_disconnect(
                             q[Incorrect packet length for BITFIELD]);
                        return;
                    }

                    # Make it vec friendly.
                    $bitfield{$self} = pack q[b*], unpack q[B*],
                        $packet;
                    $self->_check_interesting;
                    %ref = ( bitfield => $packet );
                    $client{$self}
                        ->_do_callback( q[peer_incoming_bitfield],
                                        $self );
                }
                elsif ( $type == 6 ) {
                    if ( $packet_len != 13 ) {
                        $self->_disconnect(
                              q[Incorrect packet length for REQUEST]);
                        return;
                    }
                    my ( $index, $offset, $length )
                        = unpack( q[N3], $packet );
                    %ref = ( index  => $index,
                             offset => $offset,
                             length => $length,
                    );
                    my $request =
                        Net::BitTorrent::Session::Peer::Request->new(
                                                 { index  => $index,
                                                   offset => $offset,
                                                   length => $length,
                                                   peer   => $self
                                                 }
                        );
                    push @{ $incoming_requests{$self} }, $request;
                    $client{$self}
                        ->_do_callback( q[peer_incoming_request],
                                        $self, $request );
                }
                elsif ( $type == 7 ) {
                    if ( $packet_len < 9 ) {
                        $self->_disconnect(
                            sprintf(
                                q[Incorrect packet length for PIECE (%d requires >=9)],
                                $packet_len )
                        );
                        return;
                    }

                    my ( $index, $offset, $data )
                        = unpack( q[N2a*], $packet );
                    %ref = ( block  => $data,
                             index  => $index,
                             offset => $offset,
                             length => length($data),
                    );
                    if ( not $session{$self}->pieces->[$index]
                         ->working )
                    {
                        $self->_disconnect(
                                      q[Malformed PIECE packet. (1)]);
                    }
                    else {

                        my $block = $session{$self}->pieces->[$index]
                            ->blocks->{$offset};
                        if (    ( not defined $block )
                             or ( length($data) != $block->length ) )
                        {
                            $self->_disconnect(
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
                        elsif ( $block->_write($data) ) {
                            delete $session{$self}->pieces->[$index]
                                ->blocks->{$offset};
                            @{ $outgoing_requests{$self} }
                                = grep { $_ ne $block }
                                @{ $outgoing_requests{$self} };
                            $next_pulse{$self}
                                = min( ( time + 5 ),
                                       $next_pulse{$self} );
                            $downloaded{$self} += length $data;
                            $session{$self}
                                ->_inc_downloaded( length $data );
                            $previous_incoming_block{$self} = time;
                            $block->piece->_previous_incoming_block(
                                                                time);
                            $client{$self}->_do_callback(
                                q[peer_incoming_block], $self,
                                $block );

          # TODO: if endgame, cancel all other requests for this block

                            if ( scalar( $block->peers ) > 1 ) {
                                for my $peer ( $block->peers ) {
                                    $peer->_cancel_block($block)
                                        unless $peer == $self;
                                }
                            }

                            if ( not scalar
                                 values %{ $block->piece->blocks } )
                            {
                                if ( $block->piece->verify ) {
                                    grep {
                                        $_->_build_packet(
                                              { type => 4,
                                                data => {
                                                     index =>
                                                         $block->piece
                                                         ->index
                                                }
                                              }
                                            )
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
                }
                elsif ( $type == 8 ) {
                    if ( $packet_len != 13 ) {
                        $self->_disconnect(
                               q[Incorrect packet length for cancel]);
                        return;
                    }
                    my ( $index, $offset, $length )
                        = unpack( q[N3], $packet );
                    %ref = ( index  => $index,
                             offset => $offset,
                             length => $length
                    );

                    my ($request) = grep {
                                $_->index == $index
                            and $_->offset == $offset
                            and $_->length == $length
                    } @{ $incoming_requests{$self} };
                    if ( defined $request ) {
                        $client{$self}
                            ->_do_callback( q[peer_incoming_cancel],
                                            $self, $request );
                    }
                    else {
                        $self->_disconnect(
                            q[Peer has canceled a request they never made or has already been filled.]
                        );
                    }
                }
                elsif ( $type == 9 ) {
                    if (q[TODO: I'll get to this one day...]) {
                        $self->_disconnect(
                             q[We don't support PORT messages. Yet.]);
                        return;
                    }
                    if ( $packet_len != 3 ) {
                        $self->_disconnect(
                            q[Incorrect packet length for PORT message]
                        );
                        return;
                    }
                    my ($listen_port) = unpack( q[N], $packet );
                    %ref = ( listen_port => $listen_port );
                }
                elsif ( $type == 14 ) {
                    if ( $packet_len != 1 ) {
                        $self->_disconnect(
                             q[Incorrect packet length for HAVE ALL]);
                        return;
                    }
                }
                elsif ( $type == 15 ) {
                    if ( $packet_len != 1 ) {
                        $self->_disconnect(
                            q[Incorrect packet length for HAVE NONE]);
                        return;
                    }
                }
                elsif ( $type == 16 ) {
                    if ( $packet_len != 13 ) {
                        $self->_disconnect(
                               q[Incorrect packet length for REJECT]);
                        return;
                    }
                    my ( $index, $offset, $length )
                        = unpack( q[N3], $packet );
                    %ref = ( index  => $index,
                             offset => $offset,
                             length => $length
                    );
                }
                elsif ( $type == 17 ) {
                    if ( $packet_len != 5 ) {
                        $self->_disconnect(
                            q[Incorrect packet length for ALLOWED FAST]
                        );
                        return;
                    }
                    my ($index) = unpack( q[N], $packet );
                    %ref = ( index => $index );
                }
                elsif ( $type == 20 ) {
                    if (q[TODO: Soon, soon...]) {
                        $self->_disconnect(
                             q[Support for utx messages is incomplete]
                        );
                        return;
                    }
                    if ( $packet_len < 3 ) {
                        $self->_disconnect(
                                  q[Incorrect packet length for utx]);
                        return;
                    }
                    my ( $messageid, $data )
                        = unpack( q[ca*], $packet );
                    %ref = ( packet    => bdecode($data),
                             messageid => $messageid );
                }
                else {
                    $self->_disconnect(
                                      q[Unknown or malformed packet]);
                }
            }
            $client{$self}->_do_callback( q[peer_incoming_packet],
                                          $self,
                                          { length => $packet_len,
                                            type   => $type,
                                            data   => \%ref
                                          }
            );
            return 1;
        }

        sub _build_reserved {

            # TODO: As we add support for more ext,
            # this will be updated. For now, ...
            return q[0] x 8;
        }

        sub _build_packet {
            my ( $self, $packet ) = @_;
            my $packet_data = q[];
            if ( not defined $packet->{q[type]} ) {    # handshake
                $packet_data = pack(
                            q[c/a* a8 a20 a20],
                            q[BitTorrent protocol],
                            pack( q[C8],  $self->_build_reserved ),
                            pack( q[H40], $session{$self}->infohash ),
                            $client{$self}->peer_id
                );
                $client{$self}
                    ->_do_callback( q[peer_outgoing_handshake],
                                    $self );
            }
            elsif ( $packet->{q[type]} == -1 )
            {    # keepalive; no payload
                $packet_data = ( pack( q[N], 0 ) );
                $client{$self}
                    ->_do_callback( q[peer_outgoing_keepalive],
                                    $self );
            }
            elsif ( ( $packet->{q[type]} == 0 )       # choke
                    or ( $packet->{q[type]} == 1 )    # unchoke
                    or ( $packet->{q[type]} == 2 )    # interested
                    or ( $packet->{q[type]} == 3 )    # not interested
                )
            {
                $packet_data = pack( q[NC], 1, $packet->{q[type]} );
                $client{$self}->_do_callback(
                                   ( $packet->{q[type]} == 0
                                     ? q[peer_outgoing_choke]
                                     : $packet->{q[type]} == 1
                                     ? q[peer_outgoing_unchoke]
                                     : $packet->{q[type]} == 2
                                     ? q[peer_outgoing_interested]
                                     : q[peer_outgoing_disinterested]
                                   ),
                                   $self
                );
            }
            elsif ( $packet->{q[type]} == 4 ) {    # have
                $packet_data = pack( q[NCN],
                                     5, $packet->{q[type]},
                                     $packet->{q[data]}{q[index]} );
                $client{$self}
                    ->_do_callback( q[peer_outgoing_have], $self,
                                    $packet->{q[data]}{q[index]} );
            }
            elsif ( $packet->{q[type]} == 5 ) {    # bitfield
                $packet_data = pack( q[NCa*],
                        length( $packet->{q[data]}{q[bitfield]} ) + 1,
                        $packet->{q[type]},
                        $packet->{q[data]}{q[bitfield]} );
                $client{$self}
                    ->_do_callback( q[peer_outgoing_bitfield],
                                    $self );
            }
            elsif ( ( $packet->{q[type]} == 6 )        # request
                    or ( $packet->{q[type]} == 8 )     # cancel
                )
            {
                my $packed = pack( q[N3],
                             $packet->{q[data]}{q[request]}->index,
                             $packet->{q[data]}{q[request]}->offset,
                             $packet->{q[data]}{q[request]}->length );
                $packet_data = pack( q[NCa*],
                                     length($packed) + 1,
                                     $packet->{q[type]}, $packed );
                $client{$self}->_do_callback(
                                        ( $packet->{q[type]} == 6
                                          ? q[peer_outgoing_request]
                                          : q[peer_outgoing_cancel]
                                        ),
                                        $self,
                                        $packet->{q[data]}{q[request]}
                );
            }
            elsif ( $packet->{q[type]} == 7 ) {    # piece
                my $packed = pack( q[N2a*],
                              $packet->{q[data]}{q[request]}->index,
                              $packet->{q[data]}{q[request]}->offset,
                              $packet->{q[data]}{q[request]}->_read );
                $packet_data = pack( q[NCa*],
                                     length($packed) + 1,
                                     $packet->{q[type]}, $packed );
                $client{$self}
                    ->_do_callback( q[peer_outgoing_block], $self,
                                    $packet->{q[data]}{q[request]} );
            }

            #elsif ($packet->{q[type]} == 9)  { carp pp \@_; }# port
            #elsif ($packet->{q[type]} == 10) { carp pp \@_; } # ???
            elsif (    ( $packet->{q[type]} == 14 )
                    or ( $packet->{q[type]} == 15 ) )
            {
                $packet_data = pack( q[NC], 1, $packet->{q[type]} );
            }
            elsif ( $packet->{q[type]} == 17 ) {
                $packet_data = pack( q[NCN],
                                     2, $packet->{q[type]},
                                     $packet->{q[data]}{q[index]} );
            }

            #elsif ($packet->{q[type]} == 20) { carp pp \@_; }
            else {
                require Data::Dumper;
                carp(
                    sprintf(
                        q[Unknown packet! Please, include this in your bug report: %s],
                        Data::Dumper::Dump($packet) )
                );
            }
            $client{$self}
                ->_do_callback( q[peer_outgoing_packet], $self,
                                $packet, $packet_data );
            return $queue_outgoing{$self} .= $packet_data;
        }

        sub _pulse {
            my ($self) = @_;
            if (    ( time - $connection_timestamp{$self} >= 10 * 60 )
                and
                ( time - $previous_incoming_block{$self} >= 5 * 60 ) )
            {    # TODO: make this timeout a variable
                $self->_disconnect(q[peer must be useless]);
                return 0;
            }
            if (    ( time - $connection_timestamp{$self} >= 60 )
                and ( time - $previous_incoming_data{$self} >= 130 ) )
            {    # TODO: make this timeout a variable
                $self->_disconnect(q[Peer must be dead]);
                return 0;
            }
            if ( not defined $previous_outgoing_keepalive{$self}
                 or $previous_outgoing_keepalive{$self} + 90 < time )
            {
                $self->_build_packet( { type => -1 } );
                $previous_outgoing_keepalive{$self} = time;
            }
            $self->_check_interesting;
            $self->_request_block;
            $self->_cancel_old_requests;
            $self->_unchoke
                if $is_interested{$self} and $is_choked{$self};

            if ( @{ $incoming_requests{$self} }
                 and length( $queue_outgoing{$self} )
                 < $self->client->maximum_buffer_size )
            {
                my $request = shift @{ $incoming_requests{$self} };

                # TODO: verify piece before handing over data
                $self->_build_packet(
                                   { data => { request => $request },
                                     type => 7
                                   }
                );
                $uploaded{$self} += $request->length;
                $session{$self}->_inc_uploaded( $request->length );
            }
            return $next_pulse{$self}
                = min( time + 10, $next_pulse{$self} );
        }

        sub _request_block {
            my ($self) = @_;
            if ( not $is_choking{$self}
                 and ( scalar @{ $outgoing_requests{$self} }
                       < $client{$self}->maximum_requests_per_peer )
                )
            {
                my $piece = $session{$self}->_pick_piece($self);
                if ($piece) {
                REQUEST:
                    for ( scalar @{ $outgoing_requests{$self} } ..
                          $client{$self}->maximum_requests_per_peer )
                    {
                        my $block = $piece->_unrequested_block;

                        #warn pp $block->peers;
                        #warn pp $$self;
                        if ( $block
                             and not grep { $$_ eq $$self }
                             $block->peers )
                        {
                            $self->_build_packet(
                                     { data => { request => $block },
                                       type => 6
                                     }
                            );
                            $block->_add_peer($self);
                            push @{ $outgoing_requests{$self} },
                                $block;
                        }
                        else {
                            last REQUEST;
                        }
                    }
                }
            }
            return;
        }

        sub _cancel_block {
            my ( $self, $block ) = @_;
            $block->_remove_peer($self);
            @{ $outgoing_requests{$self} } = grep { $_ ne $block }
                @{ $outgoing_requests{$self} };
            $next_pulse{$self}
                = min( ( time + 5 ), $next_pulse{$self} );
            return
                $self->_build_packet({ data => { request => $block },
                                       type => 8
                                     }
                );
        }

        sub _choke {
            my ($self) = @_;
            return if $is_choked{$self};
            $is_choked{$self} = 1;
            return $self->_build_packet( { type => 0 } );
        }

        sub _unchoke {
            my ($self) = @_;
            return if not $is_choked{$self};
            $is_choked{$self} = 0;
            return $self->_build_packet( { type => 1 } );
        }

        sub _cancel_old_requests {
            my ($self) = @_;

            # TODO: make this timeout variable
            my @remove = grep {
                $_->_request_timestamp($self)
                    < ( time - 300 )
            } @{ $outgoing_requests{$self} };
            for my $block (@remove) { $self->_cancel_block($block); }
        }

        sub _check_interesting {
            my ($self) = @_;
            return if not defined $session{$self};
            my $interesting = 0;
            if ( not $session{$self}->complete
                 and defined $bitfield{$self} )
            {
                for my $index ( 0 .. $session{$self}->piece_count ) {
                    if ( vec( $bitfield{$self}, $index, 1 )
                        and
                        $session{$self}->pieces->[$index]->priority
                        and
                        not $session{$self}->pieces->[$index]->check )
                    {
                        $interesting = 1;
                        last;
                    }
                }
            }
            if ( $interesting and not $is_interesting{$self} ) {
                $is_interesting{$self} = 1;
                $self->_build_packet( { type => 2 } );
            }
            elsif ( not $interesting and $is_interesting{$self} ) {
                $is_interesting{$self} = 0;
                $self->_build_packet( { type => 3 } );
            }
            return $interesting;
        }

        sub _send_bitfield {
            my ($self) = @_;
            return
                $self->_build_packet(
                  { type => 5,
                    data => { bitfield => $session{$self}->bitfield }
                  }
                );
        }

        sub _disconnect {
            my ( $self, $reason ) = @_;
            close $socket{$self};
            $connected{$self} = 0;
            $client{$self}->_remove_connection($self);
            $client{$self}->_do_callback( q[peer_disconnect], $self,
                                          ( $reason || $^E ) )
                if $connected{$self};
            return 1;
        }

        sub _generate_fast_set {

            # http://www.bittorrent.org/beps/bep_0006.html
            my ( $self, $k ) = @_;
            my @a;
            $k ||= 9;

            # convert host to byte order, ie localhost is 0x7f000001
            my ( $ip, undef ) = split q[:], $$self;
            my $x = sprintf( q[%X],
                             (  0xFFFFFF00 & ( hex unpack q[H*],
                                               pack q[C*],
                                               $ip =~ m[(\d+)]g
                                )
                             )
            );
            $x .= $session{$self}->infohash;
            my $piece_count = $session{$self}->piece_count;
            while ( scalar @a < $k ) {
                $x = sha1_hex( pack( q[H*], $x ) );
                for ( my $i = 0; $i < 5 && scalar @a < $k; $i++ ) {
                    my $j     = $i * 8;
                    my $y     = hex( substr( $x, $j, 8 ) );
                    my $index = $y % $piece_count;
                    push( @a, $index )
                        unless grep { $_ == $index } @a;
                }
            }
            return $outgoing_fastset{$self} = \@a;
        }

        sub as_string {
            my ( $self, $advanced ) = @_;
            my $dump = $self . q[ [TODO]];
            return print STDERR qq[$dump\n] unless defined wantarray;
            return $dump;
        }
        DESTROY {
            my ($self) = @_;
            delete $client{$self};
            delete $peer_id{$self};
            delete $bitfield{$self};
            delete $incoming_requests{ $session{$self} }{$self}
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
            delete $extentions_FastPeers{$self};
            delete $outgoing_fastset{$self};
            delete $extentions_PEX{$self};
            delete $extentions_DHT{$self};
            delete $extentions_encryption{$self};
            delete $extentions_protocol{$self};
            delete $extentions_BitComet{$self};
            delete $incoming_connection{$self};
            delete $connection_timestamp{$self};
            delete $connected{$self};
            close $socket{$self} if defined $socket{$self};
            delete $socket{$self};
            delete $previous_outgoing_keepalive{$self};
            delete $previous_incoming_data{$self};
            delete $previous_outgoing_request{$self};
            delete $previous_incoming_block{$self};
            delete $quality{$self};
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

Net::BitTorrent::Session::Peer - A remote peer

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

ProperNoun on Freenode

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
