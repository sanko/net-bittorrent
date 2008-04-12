package Net::BitTorrent;
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
    use Socket qw[PF_INET AF_INET SOCK_STREAM INADDR_ANY];
    use Time::HiRes qw[sleep];
    use Net::BitTorrent::Session;
    use Net::BitTorrent::Session::Peer;
    use Net::BitTorrent::Util qw[shuffle :log];
    {
        my (%peer_id,                   %socket,
            %fileno,                    %timeout,
            %maximum_requests_per_peer, %maximum_requests_size,
            %maximum_buffer_size,       %maximum_peers_half_open,
            %maximum_peers_per_session, %maximum_peers_per_client,
            %connections,               %callbacks,
            %sessions,                  %use_unicode,
            %debug_level
        );

        sub new {
            my ($class, $args) = @_;
            my $self = undef;
            $args->{q[LocalAddr]} = $args->{q[LocalHost]}
                if exists $args->{q[LocalHost]}
                    && !exists $args->{q[LocalAddr]};
            {
                my @portrange
                    = defined $args->{q[LocalPort]}
                    ? ref $args->{q[LocalPort]} eq q[ARRAY]
                        ? @{$args->{q[LocalPort]}}
                        : $args->{q[LocalPort]}
                    : undef;
            PORT: for my $port (@portrange) {

                    # [perldoc://perlipc]
                    socket(my ($socket),
                           PF_INET, SOCK_STREAM,
                           getprotobyname(q[tcp]))
                        or next PORT;

             # [http://www.unixguide.net/network/socketfaq/4.11.shtml]
             # [id://63280]
             #setsockopt($socket, SOL_SOCKET, SO_REUSEADDR,
             #   pack(q[l], 1)) or next PORT;
                    bind(
                        $socket,
                        pack(
                            q[Sna4x8],
                            AF_INET,
                            (defined $port
                                 and $port =~ m[^(\d+)$] ? $1 : 0
                            ),
                            (defined $args->{q[LocalAddr]}
                                 and $args->{q[LocalAddr]}
                                 =~ m[^(?:\d+\.?){4}$]
                             ? (join q[],
                                map { chr $_ } (
                                    $args->{q[LocalAddr]} =~ m[(\d+)]g
                                )
                                 )
                             : INADDR_ANY
                            )
                        )
                    ) or next PORT;
                    listen($socket, 5) or next PORT;
                    my (undef, $port, @address)
                        = unpack(q[SnC4x8], getsockname($socket));
                    defined $port or next PORT;
                    $self
                        = bless \
                        sprintf(q[%d.%d.%d.%d:%d], @address, $port),
                        $class;
                    {    # Load values user has no control over.
                        $maximum_buffer_size{$self} = (
                               defined $args->{q[maximum_buffer_size]}
                               ? $args->{q[maximum_buffer_size]}
                               : 131072
                        );
                        $maximum_peers_per_client{$self} = (
                                defined $args->{
                                    q[maximum_peers_per_client]}
                                ? $args->{q[maximum_peers_per_client]}
                                : 300
                        );
                        $maximum_peers_per_session{$self} = (
                               defined $args->{
                                   q[maximum_peers_per_session]}
                               ? $args->{q[maximum_peers_per_session]}
                               : 100
                        );
                        $maximum_peers_half_open{$self} = (
                                 defined $args->{
                                     q[maximum_peers_half_open]}
                                 ? $args->{q[maximum_peers_half_open]}
                                 : 8
                        );
                        $maximum_requests_size{$self} = (
                             defined $args->{q[maximum_requests_size]}
                             ? $args->{q[maximum_requests_size]}
                             : 32768
                        );
                        $maximum_requests_per_peer{$self} = (
                               defined $args->{
                                   q[maximum_requests_per_peer]}
                               ? $args->{q[maximum_requests_per_peer]}
                               : 10
                        );
                        $timeout{$self} = (defined $args->{q[Timeout]}
                                           ? $args->{q[Timeout]}
                                           : 5
                        );
                        $use_unicode{$self} = 0;
                        $debug_level{$self} = ERROR;
                        $socket{$self}      = $socket;
                        $fileno{$self}      = fileno($socket);
                        $peer_id{$self} = pack(
                            q[a20],
                            (sprintf(
                                 q[NB%03dC-%8s%5s],
                                 (q[$Rev$] =~ m[(\d+)]g),
                                 (  join q[],
                                    map {
                                        [q[A] .. q[Z],
                                         q[a] .. q[z],
                                         0 .. 9,
                                         qw[- . _ ~]
                                        ]->[rand(66)]
                                        } 1 .. 8
                                 ),
                                 q[Oops.],
                             )
                            )
                        );
                        $sessions{$self} = [];
                        $self->_add_connection($self);
                    }
                    last PORT;
                }
            }
            return $self;
        }

        # static
        sub peer_id {
            my ($self) = @_;
            $self->_do_callback(q[log], TRACE,
                                sprintf(q[Entering %s for %s],
                                        [caller 0]->[3], $$self
                                )
            );
            return $peer_id{$self};
        }

        sub _socket {
            my ($self) = @_;
            $self->_do_callback(q[log], TRACE,
                                sprintf(q[Entering %s for %s],
                                        [caller 0]->[3], $$self
                                )
            );
            return $socket{$self};
        }

        sub _fileno {
            my ($self) = @_;
            $self->_do_callback(q[log], TRACE,
                                sprintf(q[Entering %s for %s],
                                        [caller 0]->[3], $$self
                                )
            );
            return $fileno{$self};
        }

        sub use_unicode {
            my ($self, $value) = @_;
            $self->_do_callback(q[log], TRACE,
                                sprintf(q[Entering %s for %s],
                                        [caller 0]->[3], $$self
                                )
            );
            $self->_do_callback(q[log], WARN,
                            q[use_unicode is only supported on Win32])
                and return
                unless $^O eq q[MSWin32];
            return (
                defined $value
                ? do {
                    $self->_do_callback(q[log], WARN,
                                        q[use_unicode is malformed])
                        and return
                        unless $value =~ m[^[01]$];
                    $use_unicode{$self} = $value;
                    }
                : $use_unicode{$self}
            );
        }

        sub sockport {
            my ($self) = @_;
            $self->_do_callback(q[log], TRACE,
                                sprintf(q[Entering %s for %s],
                                        [caller 0]->[3], $$self
                                )
            );
            my (undef, $port, undef)
                = unpack(q[SnC4x8], getsockname($self->_socket));
            return $port;
        }

        sub sockaddr {
            my ($self) = @_;
            $self->_do_callback(q[log], TRACE,
                                sprintf(q[Entering %s for %s],
                                        [caller 0]->[3], $$self
                                )
            );
            my (undef, undef, @address)
                = unpack(q[SnC4x8], getsockname($self->_socket));
            return join q[.], @address;
        }

        sub maximum_peers_per_client {
            my ($self, $value) = @_;
            $self->_do_callback(q[log], TRACE,
                                sprintf(q[Entering %s for %s],
                                        [caller 0]->[3], $$self
                                )
            );
            return (
                defined $value
                ? do {
                    $self->_do_callback(q[log], WARN,
                             q[maximum_peers_per_client is malformed])
                        and return
                        unless $value =~ m[^\d+$];
                    $maximum_peers_per_client{$self} = $value;
                    }
                : $maximum_peers_per_client{$self}
            );
        }

        sub maximum_peers_per_session {
            my ($self, $value) = @_;
            $self->_do_callback(q[log], TRACE,
                                sprintf(q[Entering %s for %s],
                                        [caller 0]->[3], $$self
                                )
            );
            return (
                defined $value
                ? do {
                    $self->_do_callback(q[log], WARN,
                            q[maximum_peers_per_session is malformed])
                        and return
                        unless $value =~ m[^\d+$];
                    $maximum_peers_per_session{$self} = $value;
                    }
                : $maximum_peers_per_session{$self}
            );
        }

        sub maximum_peers_half_open {
            my ($self, $value) = @_;
            $self->_do_callback(q[log], TRACE,
                                sprintf(q[Entering %s for %s],
                                        [caller 0]->[3], $$self
                                )
            );
            return (
                defined $value
                ? do {
                    $self->_do_callback(q[log], WARN,
                              q[maximum_peers_half_open is malformed])
                        and return
                        unless $value =~ m[^\d+$];
                    $maximum_peers_half_open{$self} = $value;
                    }
                : $maximum_peers_half_open{$self}
            );
        }

        sub maximum_buffer_size {
            my ($self, $value) = @_;
            $self->_do_callback(q[log], TRACE,
                                sprintf(q[Entering %s for %s],
                                        [caller 0]->[3], $$self
                                )
            );
            return (
                defined $value
                ? do {
                    $self->_do_callback(q[log], WARN,
                                  q[maximum_buffer_size is malformed])
                        and return
                        unless $value =~ m[^\d+$];
                    $maximum_buffer_size{$self} = $value;
                    }
                : $maximum_buffer_size{$self}
            );
        }

        sub maximum_requests_size {
            my ($self, $value) = @_;
            $self->_do_callback(q[log], TRACE,
                                sprintf(q[Entering %s for %s],
                                        [caller 0]->[3], $$self
                                )
            );
            return (
                defined $value
                ? do {
                    $self->_do_callback(q[log], WARN,
                                q[maximum_requests_size is malformed])
                        and return
                        unless $value =~ m[^\d+$];
                    $maximum_requests_size{$self} = $value;
                    }
                : $maximum_requests_size{$self}
            );
        }

        sub maximum_requests_per_peer {
            my ($self, $value) = @_;
            $self->_do_callback(q[log], TRACE,
                                sprintf(q[Entering %s for %s],
                                        [caller 0]->[3], $$self
                                )
            );
            return (
                defined $value
                ? do {
                    $self->_do_callback(q[log], WARN,
                            q[maximum_requests_per_peer is malformed])
                        and return
                        unless $value =~ m[^\d+$];
                    $maximum_requests_per_peer{$self} = $value;
                    }
                : $maximum_requests_per_peer{$self}
            );
        }

        sub timeout {
            my ($self, $value) = @_;
            $self->_do_callback(q[log], TRACE,
                                sprintf(q[Entering %s for %s],
                                        [caller 0]->[3], $$self
                                )
            );
            return (
                defined $value
                ? do {
                    $self->_do_callback(q[log], WARN,
                              q[Timeout is malformed; requires float])
                        and return
                        unless $value
                            =~ m[^([+]?)(?=\d|\.\d)\d*(\.\d*)?([Ee]([+]?\d+))?$];
                    $timeout{$self} = $value;
                    }
                : $timeout{$self}
            );
        }

        sub debug_level {
            my ($self, $value) = @_;
            $self->_do_callback(q[log], TRACE,
                                sprintf(q[Entering %s for %s],
                                        [caller 0]->[3], $$self
                                )
            );
            return (
                defined $value
                ? do {
                    $self->_do_callback(q[log], WARN,
                          q[debug_level is malformed; requires float])
                        and return
                        unless $value
                            =~ m[^([+]?)(?=\d|\.\d)\d*(\.\d*)?([Ee]([+]?\d+))?$];
                    $debug_level{$self} = $value;
                    }
                : $debug_level{$self}
            );
        }

        sub _add_connection {
            my ($self, $connection) = @_;
            $self->_do_callback(q[log], TRACE,
                                sprintf(q[Entering %s for %s],
                                        [caller 0]->[3], $$self
                                )
            );
            return $connections{$self}{$connection->_fileno}
                = $connection;
        }

        sub _remove_connection {
            my ($self, $connection) = @_;
            $self->_do_callback(q[log], TRACE,
                                sprintf(q[Entering %s for %s],
                                        [caller 0]->[3], $$self
                                )
            );
            return
                if not
                    defined $connections{$self}{$connection->_fileno};
            return delete $connections{$self}{$connection->_fileno};
        }

        sub _connections {
            my ($self) = @_;
            $self->_do_callback(q[log], TRACE,
                                sprintf(q[Entering %s for %s],
                                        [caller 0]->[3], $$self
                                )
            );
            $self->_do_callback(
                q[log],
                WARN,
                q[ARG! ...s. Too many of them for Net::BitTorrent::_connections]
            ) if @_ > 1;
            return values %{$connections{$self}};
        }

        sub do_one_loop {
            my ($self) = @_;
            $self->_do_callback(q[log], TRACE,
                                sprintf(q[Entering %s for %s],
                                        [caller 0]->[3], $$self
                                )
            );
            for my $session (shuffle @{$sessions{$self}}) {
                $session->_pulse if $session->_next_pulse < time;
            }
            grep {
                $_->_disconnect(
                    q[Connection timed out before established connection]
                    )
                    if $_ ne $self
                        and (not $_->_connected)
                        and ($_->_connection_timestamp < (time - 60))
            } values %{$connections{$self}};
            my $timeout
                = $timeout{$self}
                ? $timeout{$self} == -1
                    ? undef
                    : $timeout{$self}
                : undef;

            # [id://371720]
            my ($rin, $win, $ein) = (q[], q[], q[]);
        PUSH_SOCKET:
            foreach my $fileno (keys %{$connections{$self}}) {
                vec($ein, $fileno, 1) = 1;
                vec($rin, $fileno, 1) = 1;
                vec($win, $fileno, 1) = 1
                    if $fileno ne $fileno{$self}
                        and
                        $connections{$self}{$fileno}->_queue_outgoing;
            }
            my ($nfound, $timeleft)
                = select($rin, $win, $ein, $timeout);
            if ($nfound and $nfound != -1) {
            POP_SOCKET:
                foreach my $fileno (keys %{$connections{$self}}) {
                    if (vec($ein, $fileno, 1)
                        or not $connections{$self}{$fileno}->_socket)
                    {
                        if ($^E
                            and (($^E != 10036) and ($^E != 10035)))
                        {   $connections{$self}{$fileno}
                                ->_disconnect($^E);
                        }
                        next POP_SOCKET;
                    }
                    elsif ($fileno eq $fileno{$self}) {
                        if (vec($rin, $fileno, 1)) {
                            accept(my ($new_socket), $socket{$self})
                                or $self->_do_callback(q[log], ERROR,
                                   q[Failed to accept new connection])
                                and return;
                            if (scalar(
                                    grep {
                                        $_->isa(
                                             q[Net::BitTorrent::Peer])
                                        } values
                                        %{$connections{$self}}
                                ) >= $maximum_peers_per_client{$self}
                                )
                            {   close $new_socket;
                            }
                            else {
                                my $new_peer
                                    = Net::BitTorrent::Session::Peer
                                    ->new({socket => $new_socket,
                                           client => $self
                                          }
                                    );
                                $self->_add_connection($new_peer)
                                    if $new_peer;
                            }
                        }
                    }
                    else {
                        my $read  = vec($rin, $fileno, 1);
                        my $write = vec($win, $fileno, 1);
                        if ($read or $write) {
                            $connections{$self}{$fileno}
                                ->_process_one(((2**15) * $read),
                                               ((2**15) * $write));
                        }
                    }
                }
            }
            sleep($timeleft) if $timeleft;    # save the CPU
            return 1;
        }

        sub sessions {
            my ($self, $value) = @_;
            $self->_do_callback(q[log], TRACE,
                                sprintf(q[Entering %s for %s],
                                        [caller 0]->[3], $$self
                                )
            );
            return ($sessions{$self} ? $sessions{$self} : []);
        }

        sub add_session {
            my ($self, $args) = @_;
            $self->_do_callback(q[log], TRACE,
                                sprintf(q[Entering %s for %s],
                                        [caller 0]->[3], $$self
                                )
            );
            $args->{q[client]} = $self;
            my $session = Net::BitTorrent::Session->new($args);
            if ($session) {
                push @{$sessions{$self}}, $session;
                $session->hash_check
                    unless $args->{q[skip_hashcheck]};
            }
            return $session;
        }

        sub remove_session {
            my ($self, $session) = @_;
            $self->_do_callback(q[log], TRACE,
                                sprintf(q[Entering %s for %s],
                                        [caller 0]->[3], $$self
                                )
            );
            $session->trackers->[0]->announce(q[stopped])
                if scalar @{$session->trackers};
            $session->close_files;
            return $sessions{$self}
                = [grep { $session ne $_ } @{$sessions{$self}}];
        }

        sub _locate_session {
            my ($self, $infohash) = @_;
            $self->_do_callback(q[log], TRACE,
                                sprintf(q[Entering %s for %s],
                                        [caller 0]->[3], $$self
                                )
            );
            for my $session (@{$sessions{$self}}) {
                return $session if $session->infohash eq $infohash;
            }
            return;
        }

        sub as_string {
            my ($self, $advanced) = @_;
            $self->_do_callback(q[log], TRACE,
                                sprintf(q[Entering %s for %s],
                                        [caller 0]->[3], $$self
                                )
            );

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

            my @values = ($peer_id{$self},
                          $self->sockaddr,
                          $self->sockport,
                          $maximum_peers_per_client{$self},
                          $maximum_peers_per_session{$self},
                          $maximum_peers_half_open{$self},
                          $maximum_buffer_size{$self},
                          $maximum_requests_size{$self},
                          $maximum_requests_per_peer{$self},
            );
            s/(^[-+]?\d+?(?=(?>(?:\d{3})+)(?!\d))|\G\d{3}(?=\d))/$1,/g
                for @values[3 .. 8];
            my $dump = sprintf( <<'END', @values);
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
                $dump .= sprintf( <<'END', @adv_values);
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
        {    # Callback system | So much for code reuse...

            sub _do_callback {
                my ($self, $callback, @params) = @_;
                if ($callback eq q[log]) {
                    return if $debug_level{$self} < $params[0];
                }
                if (ref $callbacks{$self}{$callback} ne q[CODE]) {
                    $self->_do_callback(
                         q[log], DEBUG,
                         sprintf(q[Unhandled callback '%s'], $callback
                         )
                    ) if $callback ne q[log];
                    return;
                }
                return &{$callbacks{$self}{$callback}}($self,
                                                       @params);
            }

            sub set_callback_on_log {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[log]} = $coderef;
            }

            sub set_callback_on_peer_connect {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[peer_connect]} = $coderef;
            }

            sub set_callback_on_peer_disconnect {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[peer_disconnect]}
                    = $coderef;
            }

            sub set_callback_on_peer_incoming_keepalive {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[peer_incoming_keepalive]}
                    = $coderef;
            }

            sub set_callback_on_peer_outgoing_keepalive {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[peer_outgoing_keepalive]}
                    = $coderef;
            }

            sub set_callback_on_peer_incoming_data {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[peer_incoming_data]}
                    = $coderef;
            }

            sub set_callback_on_peer_outgoing_data {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[peer_outgoing_data]}
                    = $coderef;
            }

            sub set_callback_on_peer_incoming_packet {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[peer_incoming_packet]}
                    = $coderef;
            }

            sub set_callback_on_peer_incoming_handshake {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[peer_incoming_handshake]}
                    = $coderef;
            }

            sub set_callback_on_peer_outgoing_handshake {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[peer_outgoing_handshake]}
                    = $coderef;
            }

            sub set_callback_on_peer_incoming_choke {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[peer_incoming_choke]}
                    = $coderef;
            }

            sub set_callback_on_peer_outgoing_choke {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[peer_outgoing_choke]}
                    = $coderef;
            }

            sub set_callback_on_peer_incoming_unchoke {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[peer_incoming_unchoke]}
                    = $coderef;
            }

            sub set_callback_on_peer_outgoing_unchoke {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[peer_outgoing_unchoke]}
                    = $coderef;
            }

            sub set_callback_on_peer_incoming_interested {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[peer_incoming_interested]}
                    = $coderef;
            }

            sub set_callback_on_peer_outgoing_interested {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[peer_outgoing_interested]}
                    = $coderef;
            }

            sub set_callback_on_peer_incoming_disinterested {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}
                    {q[peer_incoming_disinterested]} = $coderef;
            }

            sub set_callback_on_peer_outgoing_disinterested {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}
                    {q[peer_outgoing_disinterested]} = $coderef;
            }

            sub set_callback_on_peer_incoming_have {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[peer_incoming_have]}
                    = $coderef;
            }

            sub set_callback_on_peer_outgoing_have {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[peer_outgoing_have]}
                    = $coderef;
            }

            sub set_callback_on_peer_incoming_bitfield {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[peer_incoming_bitfield]}
                    = $coderef;
            }

            sub set_callback_on_peer_outgoing_bitfield {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[peer_outgoing_bitfield]}
                    = $coderef;
            }

            sub set_callback_on_peer_incoming_request {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[peer_incoming_request]}
                    = $coderef;
            }

            sub set_callback_on_peer_outgoing_request {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[peer_outgoing_request]}
                    = $coderef;
            }

            sub set_callback_on_peer_incoming_block {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[peer_incoming_block]}
                    = $coderef;
            }

            sub set_callback_on_peer_outgoing_block {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[peer_outgoing_block]}
                    = $coderef;
            }

            sub set_callback_on_peer_incoming_cancel {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[peer_incoming_cancel]}
                    = $coderef;
            }

            sub set_callback_on_peer_outgoing_cancel {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[peer_outgoing_cancel]}
                    = $coderef;
            }

            sub set_callback_on_file_read {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[file_read]} = $coderef;
            }

            sub set_callback_on_file_write {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[file_write]} = $coderef;
            }

            sub set_callback_on_file_open {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[file_open]} = $coderef;
            }

            sub set_callback_on_file_close {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[file_close]} = $coderef;
            }

            sub set_callback_on_file_error {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[file_error]} = $coderef;
            }

            sub set_callback_on_piece_hash_pass {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[piece_hash_pass]}
                    = $coderef;
            }

            sub set_callback_on_piece_hash_fail {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[piece_hash_fail]}
                    = $coderef;
            }

            sub set_callback_on_block_write {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[block_write]} = $coderef;
            }

            sub set_callback_on_tracker_connect {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[tracker_connect]}
                    = $coderef;
            }

            sub set_callback_on_tracker_disconnect {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[tracker_disconnect]}
                    = $coderef;
            }

            sub set_callback_on_tracker_scrape {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[tracker_scrape]}
                    = $coderef;
            }

            sub set_callback_on_tracker_announce {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[tracker_announce]}
                    = $coderef;
            }

            sub set_callback_on_tracker_scrape_okay {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[tracker_scrape_okay]}
                    = $coderef;
            }

            sub set_callback_on_tracker_announce_okay {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[tracker_announce_okay]}
                    = $coderef;
            }

            sub set_callback_on_tracker_incoming_data {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[tracker_incoming_data]}
                    = $coderef;
            }

            sub set_callback_on_tracker_outgoing_data {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[tracker_outgoing_data]}
                    = $coderef;
            }

            sub set_callback_on_tracker_error {
                my ($self, $coderef) = @_;
                return unless defined $coderef;
                $self->_do_callback(q[log], WARN,
                                    q[callback is malformed])
                    unless ref $coderef eq q[CODE];
                return $callbacks{$self}{q[tracker_error]} = $coderef;
            }
        }
        {
            sub _ext_FastPeers  {0}
            sub _ext_ExtProtocol {0}

            sub _build_reserved {
                my ($self) = @_;
                $self->_do_callback(q[log], TRACE,
                                    sprintf(q[Entering %s for %s],
                                            [caller 0]->[3], $$self
                                    )
                );
                my @reserved = qw[0 0 0 0 0 0 0 0];
                $reserved[7] |= 0x04
                    if $self->_ext_FastPeers;
                $reserved[5] |= 0x10
                    if $self->_ext_ExtProtocol;
                return join q[], map {chr} @reserved;
            }
        }
        DESTROY {
            my $self = shift;
            delete $peer_id{$self};
            delete $socket{$self};
            delete $use_unicode{$self};
            delete $maximum_peers_per_client{$self};
            delete $maximum_peers_per_session{$self};
            delete $maximum_peers_half_open{$self};
            delete $maximum_buffer_size{$self};
            delete $maximum_requests_size{$self};
            delete $maximum_requests_per_peer{$self};
            delete $timeout{$self};
            delete $debug_level{$self};
            delete $connections{$self};
            delete $callbacks{$self};

            #grep { $self->remove_session($_) } @{$sessions{$self}};
            delete $sessions{$self};
            delete $fileno{$self};
            return 1;
        }
    }
}
1;
__END__

=pod

=head1 NAME

Net::BitTorrent - BitTorrent peer-to-peer protocol class

=head1 SYNOPSIS

    use Net::BitTorrent;

    sub hash_pass {
        my ( $self, $piece ) = @_;
        printf( qq[on_hash_pass: piece number %04d of %s\n],
                $piece->index, $piece->session );
    }

    my $client = Net::BitTorrent->new();

    $client->set_callback_on_piece_hash_pass( \&hash_pass );

    # ...
    # set various callbacks if you so desire
    # ...

    my $torrent
        = $client->add_session( { path => q[a.legal.torrent] } )
        or die q[Cannot load .torrent];

    while (1) {
        $client->do_one_loop();

        # Etc.
    }

=head1 DESCRIPTION

C<Net::BitTorrent> is a class based implementation of the current
BitTorrent Protocol Specification.  Each C<Net::BitTorrent> object is
capable of handling several concurrent .torrent sessions.

=head1 CONSTRUCTOR

=over 4

=item C<new ( { [ARGS] } )>

Creates a C<Net::BitTorrent> object.  C<new ( )> accepts arguments as a
hash, using key-value pairs, all of which are optional.  The most
common are:

=over 4

=item C<LocalAddr>

Local host bind address.  The value must be an IPv4 ("dotted quad")
IP-address of the C<xx.xx.xx.xx> form.  Unlike the
L<LocalAddr|IO::Socket::INET/"new ( [ARGS] )"> key used by
C<IO::Socket::INET>, it does not (currently) support an embedded port
number.  C<LocalHost> is a synonym for C<LocalAddr>.

Default: 0.0.0.0 (any address)

=item C<LocalPort>

TCP port opened to remote peers for incoming connections.  If handed a
list of ports, C<Net::BitTorrent> will traverse the list, attempting
to open on each of the ports until we succeed.  If this value is
C<undef> or C<0>, we allow the OS to choose an open port at random.

Though the default in most clients is a random port in the 6881-6889
range, BitTorrent has not been assigned a port number or range by the
IANA.  Nor is such a standard needed.

Default: 0 (any avalible)

=item C<Timeout>

The maximum amount of time, in seconds, possibly fractional,
C<select()> is allowed to wait before returning in L</do_one_loop>.

Default: C<5.0>

=back

Besides these, there are a number of advanced options that can be set
via the constructor.  Use these with caution as they can greatly
affect the basic functionality and usefullness of the module.

=over 4

=item C<maximum_buffer_size>

Amount of data, in bytes, we store from a peer before dropping their
connection.  Setting this too high leaves you open to DDoS-like
attacks.  Malicious or not.

Default: C<131072> (C<2**17>)  I<(This default may change as the
module matures)>

=item C<maximum_peers_per_client>

Maximum number of peers per client object.

Default: C<300> I<(This default may change as the module matures)>

=item C<maximum_peers_per_session>

Maximum number of peers per session.

Default: C<100> I<(This default may change as the module matures)>

=item C<maximum_peers_half_open>

Maximum number of sockets we have yet to receive a handshake from.

NOTE: On some OSes (WinXP, et al.), setting this too high can cause
problems with the TCP stack.

Default: C<8>

=begin future

=item C<maximum_requests_size>

Maximum size, in bytes, a peer is allowed to request from us as a
single block.

Default: C<32768> (C<2**15>)

=end future

=item C<maximum_requests_per_peer>

Maximum number of requested blocks we keep in queue with each peer.

Default: C<10>

=back

=back

=head1 METHODS

Unless otherwise stated, all methods return either a C<true> or
C<false> value, with C<true> meaning that the operation was a
success.  When a method states that it returns a value, failure will
result in C<undef> or an empty list.

Besides these listed here, there are several C<set_callback[...]>
methods described in the L</CALLBACKS> section.

=over 4

=item C<add_session ( { ... } )>

Loads a .torrent file and adds the new
L<Net::BitTorrent::Session|Net::BitTorrent::Session> object to the
client.

Most arguments passed to this method are handed directly to
L<Net::BitTorrent::Session::new( )|Net::BitTorrent::Session/"new ( { [ARGS] } )">.
The only mandatory parameter is C<path>.  C<path>'s value is the
filename of the .torrent file to load.  Please see
L<Net::BitTorrent::Session::new( )|Net::BitTorrent::Session/"new ( { [ARGS] } )">
for a list of possible parameters.

In addition to
L<Net::BitTorrent::Session::new( )|Net::BitTorrent::Session/"new ( { [ARGS] } )">'s
supported arguments, C<add_session> accepts a C<skip_hashcheck> key.
If this bool value is set to a C<true> value, the files will not be
checked for integrity and we assume that we have none of the data of
this torrent.

This method returns the new
L<Net::BitTorrent::Session|Net::BitTorrent::Session> object on
success.

See also: L<sessions|/sessions ( )>,
L<remove_session|/remove_session ( SESSION )>,
L<Net::BitTorrent::Session|Net::BitTorrent::Session>

=item C<as_string ( [ VERBOSE ] )>

Returns a 'ready to print' dump of the C<Net::BitTorrent> object's
data structure.  If called in void context, the structure is printed
to C<STDERR>.

Note: The serialized version returned by this method is not
a full, accurate representation of the object and cannot be C<eval>ed
into a new C<Net::BitTorrent> object or used as resume data.  The
layout of and the data included in this dump is subject to change in
future versions.  This is a debugging method, not to be used under
normal circumstances.

See also: [id://317520]

=item C<debug_level ( [NEW VALUE] )>

Mutator to get/set the minimum level of messages passed to the log
callback handler.  See
L<LOG LEVELS|Net::BitTorrent::Util/"LOG LEVELS"> for more.

=item C<do_one_loop ( )>

Processes the various socket-containing objects (peers, trackers) held
by this C<Net::BitTorrent> object.  This method should be called
frequently.

=item C<maximum_buffer_size ( [NEW VALUE] )>

Mutator to get/set the amount of unparsed data, in bytes, we store
from a peer before dropping their connection.  Be sure to keep this
value high enough to allow incoming blocks (C<2**16> by default) to be
held in memory without trouble but low enough to keep DDoS-like
attacks at bay.

Default: C<131072> (C<2**17>)  I<(This default may change as the
module matures)>

=item C<maximum_peers_half_open ( [NEW VALUE] )>

Mutator to get/set the maximum number of peers we have yet to receive
a handshake from.  These include sockets that have not connected yet.

NOTE: On some OSes (WinXP, et al.), setting this too high can cause
problems with the TCP stack.

Default: C<8>

=item C<maximum_peers_per_client ( [NEW VALUE] )>

Mutator to get/set the maximum number of peers per client object.

Default: C<300>

See also: theory.org (L<http://tinyurl.com/4jgdnl>)

=item C<maximum_peers_per_session ( [NEW VALUE] )>

Mutator to get/set the maximum number of peers per session.

Default: C<100>

=item C<maximum_requests_size ( [NEW VALUE] )>

Mutator to get/set the maximum size, in bytes, a peer is allowed to
request from us as a single block of data.

Default: C<32768>

See also: theory.org (L<http://tinyurl.com/32k7wu>)

=item C<maximum_requests_per_peer ( [NEW VALUE] )>

Mutator to get/set the maximum number of blocks we have in queue from
each peer.

Default: C<10>

=item C<peer_id ( )>

Returns the Peer ID generated to identify this C<Net::BitTorrent>
object internally, with trackers, and with remote peers.

See also: theory.org (L<http://tinyurl.com/4a9cuv>)

=item C<remove_session ( SESSION )>

Removes a C<Net::BitTorrent::Session> object from the client.

=begin future

Before the torrent session is closed, we announce to the tracker
that we have 'stopped' downloading and the callback to store the
current state is called.

=end future

See also: L<sessions|/"sessions ( )">,
L<add_session|/"add_session ( { ... } )">,
L<Net::BitTorrent::Session|Net::BitTorrent::Session>

=item C<sessions ( )>

Returns a list of loaded
L<Net::BitTorrent::Session|Net::BitTorrent::Session> objects.

See Also: L<add_session|/"add_session ( { ... } )">,
L<remove_session|/"remove_session ( SESSION )">,
L<Net::BitTorrent::Session|Net::BitTorrent::Session>

=item C<sockaddr ( )>

Return the address part of the sockaddr structure for the socket.

See also: L<IO::Socket::INET/sockaddr>

=item C<sockport ( )>

Return the port number that the socket is using on the local host.

See also: L<IO::Socket::INET/sockport>

=item C<timeout ( [TIMEOUT] )>

Mutator which gets or sets the maximum amount of time, in seconds,
possibly fractional, C<select()> is allowed to wait before returning
in L</do_one_loop>.

See Also: L<do_one_loop|/do_one_loop ( )>, C<Timeout> argument of the
L<constructor|/"new ( { [ARGS] } )">

=item C<use_unicode ( [VALUE] )>

Win32 perl mis-handles filenames with extended characters.  Set this
to C<true> to work around that using C<Win32API::File>.

I<This is experimental and may be removed or improved in the future.>

See also L<http://perlmonks.org/?node=538097>,
L<http://groups.google.com/group/perl.unicode/msg/86ab5af239975df7>

=back

=head1 CALLBACKS

C<Net::BitTorrent> provides a convenient callback system.  To set a
callback, use the equivalent C<set_callback_on_[action]> method.  For
example, to catch all attempts to read from a file, use
C<$client-E<gt>set_callback_on_file_read(\&on_read)>.

Here is the current list of events fired by C<Net::BitTorrent> and
related classes as well as a brief description (soon) of them:

=head2 Peer level

Peer level events are triggered by
L<Net::BitTorrent::Peer|Net::BitTorrent::Peer> objects.

=begin future?

This list will be moved to N::B::P's POD.  Same goes for all the
other callbacks.

=end future?

=over

=item C<set_callback_on_peer_connect ( CODEREF )>

Callback arguments: ( CLIENT, PEER )

=item C<set_callback_on_peer_disconnect ( CODEREF )>

Callback arguments: ( CLIENT, PEER, [REASON] )

=item C<set_callback_on_peer_incoming_bitfield ( CODEREF )>

Callback arguments: ( CLIENT, PEER )

=item C<set_callback_on_peer_incoming_block ( CODEREF )>

Callback arguments: ( CLIENT, PEER, BLOCK )

=item C<set_callback_on_peer_incoming_cancel ( CODEREF )>

Callback arguments: ( CLIENT, PEER, REQUEST )

=item C<set_callback_on_peer_incoming_choke ( CODEREF )>

Callback arguments: ( CLIENT, PEER )

=item C<set_callback_on_peer_incoming_data ( CODEREF )>

Callback arguments: ( CLIENT, PEER, LENGTH )

=item C<set_callback_on_peer_incoming_disinterested ( CODEREF )>

Callback arguments: ( CLIENT, PEER )

=item C<set_callback_on_peer_incoming_handshake ( CODEREF )>

Callback arguments: ( CLIENT, PEER )

=item C<set_callback_on_peer_incoming_have ( CODEREF )>

Callback arguments: ( CLIENT, PEER, INDEX )

=item C<set_callback_on_peer_incoming_interested ( CODEREF )>

Callback arguments: ( CLIENT, PEER )

=item C<set_callback_on_peer_incoming_keepalive ( CODEREF )>

Callback arguments: ( CLIENT, PEER )

=item C<set_callback_on_peer_incoming_packet ( CODEREF )>

Callback arguments: ( CLIENT, PEER, PACKET )

=item C<set_callback_on_peer_incoming_request ( CODEREF )>

Callback arguments: ( CLIENT, PEER, REQUEST )

=item C<set_callback_on_peer_incoming_unchoke ( CODEREF )>

Callback arguments: ( CLIENT, PEER )

=item C<set_callback_on_peer_outgoing_bitfield ( CODEREF )>

Callback arguments: ( CLIENT, PEER )

=item C<set_callback_on_peer_outgoing_block ( CODEREF )>

Callback arguments: ( CLIENT, PEER, REQUEST )

=item C<set_callback_on_peer_outgoing_cancel ( CODEREF )>

Callback arguments: ( CLIENT, PEER, BLOCK )

=item C<set_callback_on_peer_outgoing_choke ( CODEREF )>

Callback arguments: ( CLIENT, PEER )

=item C<set_callback_on_peer_outgoing_data ( CODEREF )>

Callback arguments: ( CLIENT, PEER, LENGTH )

=item C<set_callback_on_peer_outgoing_disinterested ( CODEREF )>

Callback arguments: ( CLIENT, PEER )

=item C<set_callback_on_peer_outgoing_handshake ( CODEREF )>

Callback arguments: ( CLIENT, PEER )

=item C<set_callback_on_peer_outgoing_have ( CODEREF )>

Callback arguments: ( CLIENT, PEER, INDEX )

=item C<set_callback_on_peer_outgoing_interested ( CODEREF )>

Callback arguments: ( CLIENT, PEER )

=item C<set_callback_on_peer_outgoing_keepalive ( CODEREF )>

Callback arguments: ( CLIENT, PEER )

=item C<set_callback_on_peer_outgoing_request ( CODEREF )>

Callback arguments: ( CLIENT, PEER, BLOCK )

=item C<set_callback_on_peer_outgoing_unchoke ( CODEREF )>

Callback arguments: ( CLIENT, PEER )

=back

=head2 Tracker level

Peer level events are triggered by
L<Net::BitTorrent::Tracker|Net::BitTorrent::Tracker> objects.

=over

=item C<set_callback_on_tracker_announce ( CODEREF )>

Callback arguments: ( CLIENT, TRACKER )

=item C<set_callback_on_tracker_announce_okay ( CODEREF )>

Callback arguments: ( CLIENT, TRACKER )

=item C<set_callback_on_tracker_connect ( CODEREF )>

Callback arguments: ( CLIENT, TRACKER )

=item C<set_callback_on_tracker_disconnect ( CODEREF )>

Callback arguments: ( CLIENT, TRACKER )

=item C<set_callback_on_tracker_error ( CODEREF )>

Callback arguments: ( CLIENT, TRACKER, MESSAGE )

=item C<set_callback_on_tracker_incoming_data ( CODEREF )>

Callback arguments: ( CLIENT, TRACKER, LENGTH )

=item C<set_callback_on_tracker_outgoing_data ( CODEREF )>

Callback arguments: ( CLIENT, TRACKER, LENGTH )

=item C<set_callback_on_tracker_scrape ( CODEREF )>

Callback arguments: ( CLIENT, TRACKER )

=item C<set_callback_on_tracker_scrape_okay ( CODEREF )>

Callback arguments: ( CLIENT, TRACKER )

=back

=head2 File level

File level events are triggered by
L<Net::BitTorrent::Session::File|Net::BitTorrent::Session::File>
objects.

=over

=item C<set_callback_on_file_close ( CODEREF )>

Callback arguments: ( CLIENT, FILE )

=item C<set_callback_on_file_error ( CODEREF )>

Callback arguments: ( CLIENT, FILE, [REASON] )

=item C<set_callback_on_file_open ( CODEREF )>

Callback arguments: ( CLIENT, FILE )

=item C<set_callback_on_file_read ( CODEREF )>

Callback arguments: ( CLIENT, FILE, LENGTH )

=item C<set_callback_on_file_write ( CODEREF )>

Callback arguments: ( CLIENT, FILE, LENGTH )

=back

=head2 Piece level

Peer level events are triggered by
L<Net::BitTorrent::Session::Piece|Net::BitTorrent::Session::Piece>
objects.

=over

=item C<set_callback_on_piece_hash_fail ( CODEREF )>

Callback arguments: ( CLIENT, PIECE )

=item C<set_callback_on_piece_hash_pass ( CODEREF )>

Callback arguments: ( CLIENT, PIECE )

=back

=head2 Block level

Block level events are triggered by
L<Net::BitTorrent::Session::Piece::Block|Net::BitTorrent::Session::Piece::Block>
objects.

=over

=item C<set_callback_on_block_write ( CODEREF )>

Callback arguments: ( CLIENT, BLOCK )

=back

=head2 Debug level

Debug level callbacks can be from anywhere and are not object
specific.

=over

=item C<set_callback_on_log ( CODEREF )>

Callback arguments: ( CLIENT, LEVEL, STRING )

See also: L<LOG LEVELS|Net::BitTorrent::Util/"LOG LEVELS">

=back

=head1 IMPLEMENTED EXTENTIONS

Um, none yet.  Fast Peers soon.

=head1 BUGS

Numerous, I'm sure.  If you find one not listed in the F<Todo> file
included with this distribution, please report it.

List of know bugs:

=over

=item *

Socket handling is most likely wonky.

=item *

Large files are probably mismanaged.  If someone has the time, try
dl'ing something huge (Fedora's DVD iso?) and let me know how it goes.

=item *

Callback system is incomplete.

=item *

Unicode filenames are un(der)tested and may not work properly.  See
L<perlunifaq>.  Don't blame me.

Okay, blame me...

=item *

Documentation is incomplete.

=item *

Test suite is incomplete.

=item *

This list of bugs is incomplete.

=back

=head1 NOTES

=head2 Availability and Support

Visit the following for support and information related to
C<Net::BitTorrent>:

=over 4

=item The project's website

For wiki and subversion repository access, please visit the project's
home: L<http://net-bittorrent.googlecode.com/>.

=item Bug and Issue Tracker

Use L<http://code.google.com/p/net-bittorrent/issues/list> for bug
tracking.  Please include as much information as possible.

=back

See
L<Net::BitTorrent::FAQ|Net::BitTorrent::FAQ/"How can I stay up to date?">
for links to a mailing list, svn information, and more.

=head2 Dependencies

C<Net::BitTorrent> requires L<version|version>, and
L<Digest::SHA|Digest::SHA>.  As of perl 5.10, these are CORE modules;
they come bundled with the distribution.

=head2 Development Policy

=over 4

=item * B<All APIs are subject to change.>

Changes to documented or well established parts will be clearly
listed and archived in the F<CHANGES> file.

=item * B<All undocumented functionality is subject to change without notice.>

Because it's still early in its development, C<Net::BitTorrent> is
filled with incomplete bits of stuff.  I understand some of it seems
stable, but I reserve the right to change or eliminate code at any
time without warning I<unless> functionality is defined in POD
documentation.

If you sift through the source and find something nifty that isn't
described I<in full> in POD, don't expect your code to work with
future releases.

=back

=head2 Examples

For a demonstration of C<Net::BitTorrent>, see F</scripts/client.pl>.

=head2 Installation

This distribution uses C<Module::Build> for installation, so use the
following procedure:

  perl Build.PL
  ./Build
  ./Build test
  ./Build install

Or, if you're on a platform (like DOS or Windows) that doesn't require
the "./" notation, you can do this:

  perl Build.PL
  Build
  Build test
  Build install

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
the L<CPAN::Reporter|CPAN::Reporter/"DESCRIPTION">
documentation, L<http://cpantesters.perl.org/>, and the CPAN
Testers Wiki (L<http://cpantest.grango.org/>).


=head1 SEE ALSO

L<http://bittorrent.org/beps/bep_0003.html> - BitTorrent Protocol
Specification

L<Net::BitTorrent::FAQ|Net::BitTorrent::FAQ> - Random questions.  More
jibba jabba.

L<Net::BitTorrent::PeerID|Net::BitTorrent::PeerID> - The standard used
to identify C<Net::BitTorrent> in the wild.

=head1 ACKNOWLEDGEMENTS

Bram Cohen, for designing the base protocol and letting the community
decide what to do with it.

L Rotger

#bittorrent on Freenode for letting me idle.

Michel Valdrighi

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
