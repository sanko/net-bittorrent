{

    package Net::BitTorrent;

    BEGIN {
        use vars qw[$VERSION];
        use version qw[qv];
        our $SVN = q[$Id$];
        our $VERSION = sprintf q[%.3f], version->new(qw$Rev$)->numify / 1000;
        our $DEBUG = 0;    # Set to true to get loads of useless messages
    }

    use strict;
    use warnings 'all';
    use Socket qw[PF_INET AF_INET SOCK_STREAM SOMAXCONN sockaddr_in INADDR_ANY];
    use Carp qw[carp croak];
    use List::Util qw[shuffle];

    use lib q[../];
    use Net::BitTorrent::Session;
    use Net::BitTorrent::Peer;
    {
        my (
            %peer_id,                   %socket,
            %fileno,                    %timeout,
            %maximum_requests_per_peer, %maximum_requests_size,
            %BufferSize,                %maximum_peers_half_open,
            %maximum_peers_per_session, %maximum_peers_per_client,
            %connections,               %callbacks,
            %sessions,                  %use_unicode
        );

        sub new {
            my ($class, $args) = @_;
            my $self = undef;

            $args->{q[LocalAddr]} = $args->{q[LocalHost]}
              if exists $args->{q[LocalHost]} && !exists $args->{q[LocalAddr]};

            {
                my @portrange =
                    defined $args->{q[LocalPort]}
                  ? ref $args->{q[LocalPort]} eq q[ARRAY]
                      ? @{$args->{q[LocalPort]}}
                      : $args->{q[LocalPort]}
                  : undef;
              PORT: for my $port (@portrange) {

                    # [perldoc://perlipc]
                    socket(my ($socket),
                        &PF_INET, &SOCK_STREAM, getprotobyname(q[tcp]))
                      or next PORT;

                    # [http://www.unixguide.net/network/socketfaq/4.11.shtml]
                    # [id://63280]
                    #setsockopt($socket, &SOL_SOCKET, &SO_REUSEADDR,
                    #  pack(q[l], 1))
                    #or next PORT;

                    bind(
                        $socket,
                        pack(
                            q[Sna4x8],
                            &AF_INET,
                            (defined $port and $port =~ m[^(\d+)$] ? $1 : 0),
                            (
                                defined $args->{q[LocalAddr]}
                                  and $args->{q[LocalAddr]} =~
                                  m[^(?:\d+\.?){4}$]
                                ? (
                                    join q[],
                                    map { chr $_ }
                                      ($args->{q[LocalAddr]} =~ m[(\d+)]g)
                                  )
                                : &INADDR_ANY
                            )
                        )
                    ) or next PORT;

                    #ioctl($socket, 0x8004667e, pack(q[I], 1))
                    #  or die qq[nonblocking: $^E];
                    listen($socket, 5) or next PORT;

                    my (undef, $port, @address) =
                      unpack(q[SnC4x8], getsockname($socket));

                    defined $port or next PORT;

                    # Constructor.
                    $self = bless \sprintf(q[%d.%d.%d.%d:%d], @address, $port),
                      $class;

                    {

                        # Load values user has no control over.
                        $socket{$self}  = $socket;
                        $fileno{$self}  = CORE::fileno($socket);
                        $peer_id{$self} = pack(
                            q[a20],
                            (
                                sprintf(
                                    q[NB%03dS-%8s%5s],
                                    (q[$Rev$] =~ m[(\d+)]g),
                                    (
                                        join q[],
                                        map {
                                            [   q[A] .. q[Z],
                                                q[a] .. q[z],
                                                0 .. 9,
                                                qw[- . _ ~]
                                            ]->[rand(66)]
                                          } 1 .. 8
                                    ),
                                    q[--SVN],
                                )
                            )
                        );
                        $sessions{$self} = [];
                        $self->add_connection($self);
                    }
                    {
                        $BufferSize{$self} = (
                            defined $args->{q[BufferSize]}
                            ? $args->{q[BufferSize]}
                            : 98304
                        );
                        $maximum_peers_per_client{$self} = (
                            defined $args->{q[maximum_peers_per_client]}
                            ? $args->{q[maximum_peers_per_client]}
                            : 300
                        );
                        $maximum_peers_per_session{$self} = (
                            defined $args->{q[maximum_peers_per_session]}
                            ? $args->{q[maximum_peers_per_session]}
                            : 30
                        );
                        $maximum_peers_half_open{$self} = (
                            defined $args->{q[maximum_peers_half_open]}
                            ? $args->{q[maximum_peers_half_open]}
                            : 8
                        );
                        $maximum_requests_size{$self} = (
                            defined $args->{q[maximum_requests_size]}
                            ? $args->{q[maximum_requests_size]}
                            : 32768
                        );
                        $maximum_requests_per_peer{$self} = (
                            defined $args->{q[maximum_requests_per_peer]}
                            ? $args->{q[maximum_requests_per_peer]}
                            : 10
                        );
                        $timeout{$self} = (
                            defined $args->{q[Timeout]}
                            ? $args->{q[Timeout]}
                            : 5
                        );
                        $use_unicode{$self} = 0;
                    }
                    last PORT;
                }
            }
            return $self;
        }

        # static
        sub peer_id { return $peer_id{+shift}; }
        sub socket  { return $socket{+shift}; }
        sub fileno  { return $fileno{+shift}; }

        sub use_unicode {
            my ($self, $value) = @_;
            #carp(q[use_unicode is only supported on Win32]) and return
            #  unless $^O eq q[MSWin32];
            return (
                defined $value
                ? do {
                    carp(q[use_unicode is malformed]) and return
                      unless $value =~ m[^[01]$];
                    $use_unicode{$self} = $value;
                  }
                : $use_unicode{$self}
            );
        }

        sub sockport {
            my (undef, $port, undef) =
              unpack(q[SnC4x8], getsockname(shift->socket));
            return $port;
        }

        sub sockaddr {
            my (undef, undef, @address) =
              unpack(q[SnC4x8], getsockname(shift->socket));
            return join q[.], @address;
        }

        sub maximum_peers_per_client {
            my ($self, $value) = @_;
            return (
                defined $value
                ? do {
                    croak(q[maximum_peers_per_client is malformed])
                      and return
                      unless $value =~ m[^\d+$];
                    $maximum_peers_per_client{$self} = $value;
                  }
                : $maximum_peers_per_client{$self}
            );
        }

        sub maximum_peers_per_session {
            my ($self, $value) = @_;
            return (
                defined $value
                ? do {
                    croak(q[maximum_peers_per_session is malformed])
                      and return
                      unless $value =~ m[^\d+$];
                    $maximum_peers_per_session{$self} = $value;
                  }
                : $maximum_peers_per_session{$self}
            );
        }

        sub maximum_peers_half_open {
            my ($self, $value) = @_;
            return (
                defined $value
                ? do {
                    croak(q[maximum_peers_half_open is malformed]) and return
                      unless $value =~ m[^\d+$];
                    $maximum_peers_half_open{$self} = $value;
                  }
                : $maximum_peers_half_open{$self}
            );
        }

        sub BufferSize {
            my ($self, $value) = @_;
            return (
                defined $value
                ? do {
                    croak(q[BufferSize is malformed]) and return
                      unless $value =~ m[^\d+$];
                    $BufferSize{$self} = $value;
                  }
                : $BufferSize{$self}
            );
        }

        sub maximum_requests_size {
            my ($self, $value) = @_;
            return (
                defined $value
                ? do {
                    croak(q[maximum_requests_size is malformed]) and return
                      unless $value =~ m[^\d+$];
                    $maximum_requests_size{$self} = $value;
                  }
                : $maximum_requests_size{$self}
            );
        }

        sub maximum_requests_per_peer {
            my ($self, $value) = @_;
            return (
                defined $value
                ? do {
                    croak(q[maximum_requests_per_peer is malformed])
                      and return
                      unless $value =~ m[^\d+$];
                    $maximum_requests_per_peer{$self} = $value;
                  }
                : $maximum_requests_per_peer{$self}
            );
        }

        sub timeout {
            my ($self, $value) = @_;
            return (
                defined $value
                ? do {
                    carp(q[Timeout is malformed; requires float])
                      and return
                      unless $value =~
                          m[^([+]?)(?=\d|\.\d)\d*(\.\d*)?([Ee]([+]?\d+))?$];
                    $timeout{$self} = $value;
                  }
                : $timeout{$self}
            );
        }

        sub add_connection {
            my ($self, $connection) = @_;
            return $connections{$self}{$connection->fileno} = $connection;
        }

        sub remove_connection {
            my ($self, $connection) = @_;
            return
              if not defined $connections{$self}{$connection->fileno};
            return delete $connections{$self}{$connection->fileno};
        }

        sub connections {
            croak q[ARG! ...s. Too many of them.] if @_ > 1;
            return values %{$connections{+shift}};
        }

        sub do_one_loop {
            my ($self) = @_;

            for my $session (shuffle @{$sessions{$self}}) {
                $session->pulse if $session->next_pulse < time;
            }

            grep {
                $_->disconnect(
                    q[Connection timed out before established connection])
                  if $_ ne $self
                      and (not $_->connected)
                      and ($_->connection_timestamp < (time - 60))
            } values %{$connections{$self}};

            my $timeout =
                $timeout{$self}
              ? $timeout{$self} == -1
                  ? undef
                  : $timeout{$self}
              : undef;

            # [id://371720]
            my ($rin, $win, $ein) = (q[], q[], q[]);
          PUSH_SOCKET: foreach my $fileno (keys %{$connections{$self}}) {
                vec($ein, $fileno, 1) = 1;
                vec($rin, $fileno, 1) = 1;
                vec($win, $fileno, 1) = 1
                  if $fileno ne $fileno{$self}
                      and $connections{$self}{$fileno}->queue_outgoing;
            }

            my ($nfound, $timeleft) = select($rin, $win, $ein, $timeout);
            if ($nfound and $nfound != -1) {
              POP_SOCKET: foreach my $fileno (keys %{$connections{$self}}) {
                    if (vec($ein, $fileno, 1)
                        or not $connections{$self}{$fileno}->socket)
                    {
                        if ($^E and (($^E != 10036) and ($^E != 10035))) {
                            $connections{$self}{$fileno}->disconnect($^E);
                        }
                        next POP_SOCKET;
                    }
                    elsif ($fileno eq $fileno{$self}) {
                        if (vec($rin, $fileno, 1)) {
                            accept(my ($new_socket), $socket{$self})
                              or $self->do_callback(q[log],
                                q[Failed to accept new connection])
                              and return;
                            if (
                                scalar(
                                    grep { $_->isa(q[Net::BitTorrent::Peer]) }
                                      values %{$connections{$self}}
                                ) >= $maximum_peers_per_client{$self}
                              )
                            {
                                close $new_socket;
                            }
                            else {
                                my $new_peer = Net::BitTorrent::Peer->new(
                                    {   socket => $new_socket,
                                        client => $self
                                    }
                                );
                                $self->add_connection($new_peer) if $new_peer;
                            }
                        }
                    }
                    else {
                        my $read  = vec($rin, $fileno, 1);
                        my $write = vec($win, $fileno, 1);
                        if ($read or $write) {
                            $connections{$self}{$fileno}
                              ->process_one(((2**15) * $read),
                                ((2**15) * $write));
                        }
                    }
                }
            }
            select(undef, undef, undef, $timeleft) if $timeleft;  # save the CPU
            return 1;
        }

        sub sessions {
            my ($self, $value) = @_;
            return ($sessions{$self} ? $sessions{$self} : []);
        }

        sub add_session {
            my ($self, $args) = @_;
            $args->{q[client]} = $self;
            my $session = Net::BitTorrent::Session->new($args);
            if ($session) {
                push @{$sessions{$self}}, $session;
                $session->hash_check unless $args->{q[skip_hashcheck]};
            }
            return $session;
        }

        sub remove_session {
            my ($self, $session) = @_;
            $session->trackers->[0]->announce(q[stopped])
              if scalar @{$session->trackers};
            $session->close_files;
            return $sessions{$self} =
              [grep { $session ne $_ } @{$sessions{$self}}];
        }

        sub locate_session {
            my ($self, $infohash) = @_;
            for my $session (@{$sessions{$self}}) {
                return $session if $session->infohash eq $infohash;
            }
            return;
        }

        sub as_string {
            my ($self, $advanced) = @_;

=pod

=begin blarg

            my %_data = (

                socket                    => $socket{$self},
                use_unicode               => $use_unicode{$self},
                Timeout            => $Timeout{$self},
                q[objects with sockets]   => $connections{$self},
                callbacks                 => $callbacks{$self}
            );

=end blarg

=cut

            my @values = (
                $peer_id{$self},
                $self->sockaddr,
                $self->sockport,
                $maximum_peers_per_client{$self},
                $maximum_peers_per_session{$self},
                $maximum_peers_half_open{$self},
                $BufferSize{$self},
                $maximum_requests_size{$self},
                $maximum_requests_per_peer{$self},
            );
            s/(^[-+]?\d+?(?=(?>(?:\d{3})+)(?!\d))|\G\d{3}(?=\d))/$1,/g
              for @values[3 .. 8];
            my $dump = sprintf(<<'END', @values);
Net::BitTorrent (%20s)
======================================
Basic Information
  Bind address:                  %s:%d
  Limits:
    Number of peers:             %s
    Number of peers per session: %s
    Number of half-open peers:   %s
    Amount of unparsed data:     %s bytes
    Size of incoming requests:   %s bytes
    Number of requests per peer: %s

END
            if ($advanced) {
                my @adv_values = (scalar(@{$sessions{$self}}));
                $dump .= sprintf(<<'END', @adv_values);
Advanced Information
  Loaded sessions: (%d torrents)
END
                $dump .= join qq[\n], map {
                    my $session = $_->as_string($advanced);
                    $session =~ s|\n|\n    |g;
                    q[ ] x 4 . $session
                } @{$sessions{$self}};
            }
            return print STDERR qq[$dump\n] unless defined wantarray;
            return $dump;
        }

        for my $callback (
            qw[
            log

            peer_connect                 peer_disconnect
            peer_incoming_keepalive      peer_outgoing_keepalive
            peer_incoming_data           peer_outgoing_data
            peer_outgoing_packet         peer_incoming_packet
            peer_incoming_handshake      peer_outgoing_handshake
            peer_incoming_choke          peer_outgoing_choke
            peer_incoming_unchoke        peer_outgoing_unchoke
            peer_incoming_interested     peer_outgoing_interested
            peer_incoming_disinterested  peer_outgoing_disinterested
            peer_incoming_have           peer_outgoing_have
            peer_incoming_bitfield       peer_outgoing_bitfield
            peer_incoming_request        peer_outgoing_request
            peer_incoming_block          peer_outgoing_block
            peer_incoming_cancel         peer_outgoing_cancel

            file_read  file_write
            file_open  file_close
            file_error

            piece_hash_pass piece_hash_fail

            block_write

            tracker_connect        tracker_disconnect
            tracker_scrape         tracker_announce
            tracker_scrape_okay    tracker_announce_okay
            tracker_incoming_data  tracker_outgoing_data
            tracker_error
            ]
          )
        {
            no strict q[refs];
            *{"set_callback_on_$callback"} = sub {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                croak(q[callback is malformed])
                  unless ref $coderef eq q[CODE];
                $callbacks{$self}{$callback} = $coderef;
            };
        }

        sub do_callback {
            my ($self, $callback, @params) = @_;
            if (not defined $callbacks{$self}{$callback}) {
                carp sprintf(q[Unhandled callback '%s'], $callback)
                  if $Net::BitTorrent::DEBUG;
                return;
            }
            return &{$callbacks{$self}{$callback}}($self, @params);
        }

        DESTROY {
            my $self = shift;

            delete $peer_id{$self};
            delete $socket{$self};

            delete $use_unicode{$self};
            delete $maximum_peers_per_client{$self};
            delete $maximum_peers_per_session{$self};
            delete $maximum_peers_half_open{$self};
            delete $BufferSize{$self};

            delete $maximum_requests_size{$self};

            delete $maximum_requests_per_peer{$self};
            delete $timeout{$self};
            delete $connections{$self};
            delete $callbacks{$self};

            #grep { $self->remove_session($_) } @{$sessions{$self}};
            delete $sessions{$self};
            delete $fileno{$self};
            return 1

        }
    }
    1;
}
__END__
=pod

=head1 NAME

Net::BitTorrent - BitTorrent client class

=head1 SYNOPSIS

  use Net::BitTorrent;

  sub hash_pass {
      my ($self, $piece) = @_;
      printf(qq[on_hash_pass: piece number %04d of %s\n], $piece->index,
          $piece->session);
  }

  my $client = Net::BitTorrent->new();

  $client->set_callback_on_piece_hash_pass(\&hash_pass);

  # ...
  # set various callbacks if you so desire
  # ...

  my $torrent = $client->add_session({path => q[a.legal.torrent]})
    or die q[Cannot load .torrent];

  while (1) {
      $client->do_one_loop();
      # Etc.
  }

=head1 DESCRIPTION

C<Net::BitTorrent> is a class based implementation of a simple
BitTorrent client in Perl as described in the latest BitTorrent
Protocol Specification.  Each C<Net::BitTorrent> object is capable of
handling several concurrent torrent sessions.

=head1 OVERVIEW

BitTorrent is a free speech tool.

=head1 CONSTRUCTOR

=over 4

=item C<new ( [PARAMETERS] )>

This is the constructor for a new C<Net::BitTorrent> object.

C<PARAMETERS> are passed as a hash, using key and value pairs, all
of which are optional. Possible options are:

B<LocalAddr> - Local host bind address.  The value must be a IPv4
("dotted quad") IP-address of the C<xx.xx.xx.xx> form.  This
parameter is only useful on multihomed hosts.

I<Note: this differs from the C<LocalAddr> key used by
C<IO::Socket::INET>>

B<LocalPort> - TCP port (or range if passed in array context) opened
to remote peers for incoming connections.  If handed a range of
numbers, C<Net::BitTorrent> will traverse the list, attempting to
open on each of the ports until we succeed.  If this value is
C<undef> or C<0>, we allow the OS to choose an open port.

Note: BitTorrent has not been assigned a port number or range by the
IANA nor is such a standard needed.  Though, the default in most
clients is a random port in the 6881-6889 range.

=begin future

B<BufferSize> - Amount of data, in bytes, we store from a peer before
dropping their connection.  Setting this too high leaves you open to
DDos-like attacks.  Malicious or not. (Defaults to 98304)

maximum_peers_per_client - Max number of peers per client.

Default: 300

See also: [theory://Algorithms:_Queuing>]

maximum_peers_per_session - Max number of peers per session.

Default: 30

maximum_peers_half_open - Max number of sockets we have yet to
reciece a handshake from.

NOTE: On some OS (WinXP, et. al.), setting this too high can cause
problems with the TCP stack.

Default: 8

maximum_requests_size - Maximum size, in bytes, a peer is allowed to
request from us as a single block.

Default: 32768

See also: [talk://Messages:_request]

maximum_requests_per_peer - Maximum number of blocks we have in queue
from each peer.

Default: 10

=end future

B<Timeout> - The maximum amount of time C<select()> is allowed to wait
before returning, in seconds, possibly fractional. (Defaults to 5.0)

If the constructor fails, C<undef> will be returned and the value of
C<$!> and C<$^E> shoulc be checked.

=back

=head1 METHODS

Unless otherwise stated, all methods return either a C<true> or
C<false> value, with C<true> meaning that the operation was a
success.  When a method states that it returns a value, failure will
be returned as C<undef> or an empty list.

=over 4

=item C<do_one_loop ( )>

Processes the various socket-containing objects held by this
C<Net::BitTorrent> object.  This method should be called frequently.

See Also: L</timeout( )> method to set the timeout interval used by
this method's C<select> call.

=item C<timeout ( [TIMEOUT] )>

Gets or sets the timeout value used by the L<do_one_loop( )> method.
The default timeout is 5 seconds.

See Also: L</do_one_loop( )>, L</new( )>

=item C<sessions ( )>

Returns a list of all (if any) loaded
L<Net::BitTorrent::Session|Net::BitTorrent::Session> objects.

See Also: L</add_session( )>, L</remove_session( )>,
L<Net::BitTorrent::Session|Net::BitTorrent::Session>

=item C<add_session ( { ... } )>

Loads a .torrent file and starts a new BitTorrent session.

Parameters passed to this method are handed directly to
C<Net::BitTorrent::Session::new()>, so see the
L<Net::BitTorrent::Session|Net::BitTorrent::Session> documentation
for a list of required and optional parameters.

This method returns C<undef> on failure or a new
L<Net::BitTorrent::Session|Net::BitTorrent::Session> object on
success.

See also: L</sessions( )>, L</remove_session( )>,
L<Net::BitTorrent::Session|Net::BitTorrent::Session>

=item C<remove_session ( SESSION )>

Removes a C<Net::BitTorrent::Session> object from the client.

Before the torrent session is closed, we announce to the tracker
that we have 'stopped' downloading and the callback to store the
current state is called.

See also: L</sessions( )>, L</add_session( )>,
L<Net::BitTorrent::Session|Net::BitTorrent::Session>

=item C<peer_id ( )>

Retrieve the peer_id genreated for this C<Net::BitTorrent> object.

See also [theory://peer_id].

=item C<as_string ( [ VERBOSE ] )>

Returns a 'ready to print' dump of the C<Net::BitTorrent> object's
data structure.  If called in void context, the structure is printed
to C<STDERR>.

Note: The stringified version returned by this method is not
a full, accurate representation of the object and cannot be C<eval>ed
into a new C<Net::BitTorrent> object or used as resume data.

The layout of and the data included in this dump is subject to change
in future versions.

This is a debugging method, not to be used under normal
circumstances.

See also: [id://317520]

=item C<use_unicode ( [VALUE] )>

Win32 perl does not handle filenames with extended characters
properly.

I<This is an experimental workaround that may or may not be
removed or improved in the future.>

See also [id://538097], [id://229642], [id://445883],
[L<http://groups.google.com/group/perl.unicode/msg/86ab5af239975df7>].

=back

=head1 CALLBACKS

C<Net::BitTorrent> provides a convienient callback system for client
developers.  To set a callback, use the equivilent
C<set_callback_on_[action]> method.  For example, to catch all attempts
to read from a file, use
C<$client-E<gt>set_callback_on_file_read(\&on_read)>.

Here is the current list of events fired by C<Net::BitTorrent> and
its related objects as well as a brief description (soon) of them:

=head2 Peer level

Peer level events are triggered by
L<Net::BitTorrent::Peer|Net::BitTorrent::Peer> objects.

=begin future

This list will be moved to N::B::P's POD.  Same goes for all the
other callbacks.

=end future

=over

=item peer_connect ( CLIENT, PEER )

=item peer_disconnect ( CLIENT, PEER, REASON )

=item peer_incoming_keepalive ( CLIENT, PEER )

=item peer_outgoing_keepalive ( CLIENT, PEER )

=item peer_incoming_data ( CLIENT, PEER, LENGTH )

=item peer_outgoing_data ( CLIENT, PEER, LENGTH )

=item peer_outgoing_packet ( CLIENT, PEER, PACKET )

=item peer_incoming_packet ( CLIENT, PEER, PACKET )

=item peer_incoming_handshake ( CLIENT, PEER )

=item peer_outgoing_handshake ( CLIENT, PEER )

=item peer_incoming_choke ( CLIENT, PEER )

=item peer_outgoing_choke ( CLIENT, PEER )

=item peer_incoming_unchoke ( CLIENT, PEER )

=item peer_outgoing_unchoke ( CLIENT, PEER )

=item peer_incoming_interested ( CLIENT, PEER )

=item peer_outgoing_interested ( CLIENT, PEER )

=item peer_incoming_disinterested ( CLIENT, PEER )

=item peer_outgoing_disinterested ( CLIENT, PEER )

=item peer_incoming_have ( CLIENT, PEER, INDEX )

=item peer_outgoing_have ( CLIENT, PEER, INDEX )

=item peer_incoming_bitfield ( CLIENT, PEER )

=item peer_outgoing_bitfield ( CLIENT, PEER )

=item peer_incoming_request ( CLIENT, PEER, REQUEST )

=item peer_outgoing_request ( CLIENT, PEER, BLOCK )

=item peer_incoming_block ( CLIENT, PEER, BLOCK )

=item peer_outgoing_block ( CLIENT, PEER, REQUEST )

=item peer_incoming_cancel ( CLIENT, PEER, REQUEST )

=item peer_outgoing_cancel ( CLIENT, PEER, BLOCK )

=back

=head2 Tracker level

Peer level events are triggered by
L<Net::BitTorrent::Tracker|Net::BitTorrent::Tracker> objects.

=over

=item tracker_connect ( CLIENT, TRACKER )

=item tracker_disconnect ( CLIENT, TRACKER )

=item tracker_scrape ( CLIENT, TRACKER )

=item tracker_announce ( CLIENT, TRACKER )

=item tracker_scrape_okay ( CLIENT, TRACKER )

=item tracker_announce_okay ( CLIENT, TRACKER )

=item tracker_incoming_data ( CLIENT, TRACKER, LENGTH )

=item tracker_outgoing_data ( CLIENT, TRACKER, LENGTH )

=item tracker_error ( CLIENT, TRACKER, MESSAGE )

=back

=head2 File level

File level events are triggered by
L<Net::BitTorrent::Session::File|Net::BitTorrent::Session::File>
objects.

=over

=item file_read ( CLIENT, FILE, LENGTH )

=item file_write ( CLIENT, FILE, LENGTH )

=item file_open ( CLIENT, FILE )

=item file_close ( CLIENT, FILE )

=item file_error ( CLIENT, FILE, MESSAGE )

=back

=head2 Piece level

Peer level events are triggered by
L<Net::BitTorrent::Session::Piece|Net::BitTorrent::Session::Piece>
objects.

=over

=item piece_hash_pass ( CLIENT, PIECE )

=item piece_hash_fail ( CLIENT, PIECE )

=back

=head2 Block level

Block level events are triggered by
L<Net::BitTorrent::Session::Piece::Block|Net::BitTorrent::Session::Piece::Block>
objects.

=over

=item block_write ( CLIENT, BLOCK )

=back

=head2 Debug level

Debug level callbacks can be from anywhere and are not object
specific.

=over

=item log ( CLIENT, STRING )

=back


=head1 UNIMPLEMENTED EXTENTIONS

The following BitTorrent extentions have not been implemented:

=over 4

=item B<Metadata Extension>

The purpose of this extension is to allow clients to join a swarm and
complete a download without the need of downloading a .torrent file
first.  This extension instead allows clients to download the
metadata from peers.  It makes it possible to support magnet links, a
link on a web page only containing enough information to join the
swarm (the info hash).

See also: [bep://9]

=item B<DHT Protocol>

BitTorrent uses a "distributed sloppy hash table" (DHT) for storing
peer contact information for "trackerless" torrents.  In effect, each
peer becomes a tracker.  The protocol is based on Kademila and is
implemented over UDP.

See also: [bep://5],
[L<http://www.cs.rice.edu/Conferences/IPTPS02/109.pdf>]

=item B<Fast Extension>

The Fast Extension packages several extensions to the base BitTorrent
Protocol the least of which being the ability to request and recieve
certain pieces (known as a peer's "Allowed Fast Set") regardless of
choke status.

This extention was present in early, pre-CPAN releases of this module
and will return soon.  This is a high priority.

See also: [bep://6]

=item B<IPv6 Tracker Extension>

This extension extends the tracker response to better support IPv6
peers as well as defines a way for multihomed machines to announce
multiple addresses at the same time. This proposal addresses the use
case where peers are either on an IPv4 network running Teredo or
peers are on an IPv6 network with an IPv4 tunnel interface.

When will [cpan://L<IO::Socket::INET6|IO::Socket::INET6>] or
(better yet) [cpan://L<Socket6|Socket6>] be CORE?

See also: [bep://7],
[L<https://www.microsoft.com/technet/network/ipv6/teredo.mspx>]

=item B<Tracker Peer Obfuscation>

This extends the tracker protocol to support simple obfuscation of
the peers it returns, using the infohash as a shared secret between
the peer and the tracker.  The obfuscation does not provide any
security against eavesdroppers that know the infohash of the
torrent.  The goal is to prevent internet service providers and other
network administrators from blocking or disrupting bittorrent traffic
connections that span between the receiver of a tracker response and
any peer IP-port appearing in that tracker response.

See also: [bep://8]

=item B<Extension Protocol>

The intention of this protocol is to provide a simple and thin
transport for extensions to the bittorrent protocol.  Supporting this
protocol makes it easy to add new extensions without interfering with
the standard bittorrent protocol or clients that don't support this
extension or the one you want to add.

See also: [bep://10]

=item B<HTTP Seeding>

Very low priority.

See [bep://17]

=back

=head1 CAVEATS

=head2 BUGS/TODO

Numerous. If you find one, please report it.

List of know bugs:

=over

=item *

Socket handling is most likely wonky.

=item *

Large files are probably not well managed.  If someone has the time,
try dl'ing something huge (Fedora's DVD iso?) and let me know how it
goes.

=item *

Callback system is incomplete

=item *

Unicode filenames are un(der)tested and may not work properly.  See
[perldoc://L<perlunifaq>].  Don't blame me.

Okay, blame me...

=item *

Documentation is incomplete.

=item *

A more complete test suite needs to be written to test the small
things just in case.

=back

=head1 NOTES

=head2 INSTALLATION

The current distribution uses the CORE ExtUtils::MakeMaker module, so
the standard procedure will suffice:

 perl Makefile.PL
 make
 make test
 make install

If you would like to contribute automated test reports (and I hope
you do), first install C<CPAN::Reporter> from the CPAN shell and then
install C<Net::BitTorrent>:

 $ cpan
 cpan> install CPAN::Reporter
 cpan> reload cpan
 cpan> o conf init test_report
   [...follow the CPAN::Reporter setup prompts...]
 cpan> o conf commit
 cpan> install Net::BitTorrent

For more on becoming a CPAN tester and why this is useful, please see
the [cpan://L<CPAN::Reporter|CPAN::Reporter/"DESCRIPTION">]
documentation, [L<http://cpantesters.perl.org/>], and the CPAN
Testers Wiki ([L<http://cpantest.grango.org/>]).

=head2 DEPENDENCIES

C<Net::BitTorrent> requires [cpan://L<version|version>], and
[cpan://L<Digest::SHA|Digest::SHA>].  As of perl 5.10, these are CORE
modules; they come bundled with the distribution.

=head2 INTERNALS vs. DOCUMENTATION

B<All undocumented functionality is subject to change without notice.>

If you sift through the source and find something nifty that isn't
described I<in full> in POD, don't expect your code to work with
future releases. Again, B<all undocumented functionality is subject
to change without notice.>

Changes to documented or well established parts will be clearly
listed and archived in the F<CHANGES> file bundled with this
software package.

=head2 TAGS

Throughout the source (in POD and inline comments), I have used
bracketed tags when linking to reference material.  The basis for
these tags is the list from PerlMonks ([id://43037]) with the
addition of the following:

=over

=item [theory://]

These are links to [L<http://wiki.theory.org/>] documentation.  The
base URL for these is
[L<http://wiki.theory.org/BitTorrentSpecification>] where the tag's
value is either a named anchor or easily noted section name.

=item [talk://]

These are links to [L<http://wiki.theory.org/>] discussions of
disputed parts of the protocol's implementation.  The base URL for
these is [L<http://wiki.theory.org/Talk:BitTorrentSpecification>]
where the tag's value is a named anchor.

=item [bep://]

BEP stands for BitTorrent Enhancement Proposal.  A BEP is a design
document providing information to the BitTorrent community, or
describing a new feature for the BitTorrent protocols.  See
[L<http://bittorrent.org/beps/bep_0000.html>] for the current list of
BEPs and [L<http://bittorrent.org/beps/bep_0001.html>] for more on
BEPs.

=back

=head2 EXAMPLES

For a demonstration of C<Net::BitTorrent>, see F</scripts/client.pl>.

=head1 AVAILABILITY AND SUPPORT

See [L<http://net-bittorrent.googlecode.com/>] for support and SVN
repository access.

For now, please use
[L<http://code.google.com/p/net-bittorrent/issues/list>] for bug
tracking.  When reporting bugs/problems please include as much
information as possible.  It may be difficult for me to reproduce the
problem as almost every setup is different.

=head1 SEE ALSO

BitTorrent Protocol Specification - [bep://3]

=head1 CREDITS

Bram Cohen ([wikipedia://Bram_Cohen]), for designing the base
protocol and letting the community decide what to do with it.

L Rotger

#bittorrent on Freenode for letting me idle.

=head1 AUTHOR

Sanko Robinson <sanko@cpan.org> - [http://sankorobinson.com/]

=head1 LICENSE AND LEGAL

Copyright 2008 by Sanko Robinson E<lt>sanko@cpan.orgE<gt>

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

See [http://www.perl.com/perl/misc/Artistic.html] or the LICENSE file
included with this module.

Neither this module nor the L<AUTHOR|/AUTHOR> is affiliated with
BitTorrent, Inc.

=for svn $Id$

=cut
