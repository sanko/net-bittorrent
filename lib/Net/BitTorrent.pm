package Net::BitTorrent;
use strict;
use warnings;
{

    BEGIN {
        use version qw[qv];
        our $SVN
            = q[$Id$];
        our $VERSION = sprintf q[%.3f], version->new(qw$Rev$)->numify / 1000;
    }
    use Socket qw[/F_INET/ /_STREAM/ /_ANY/ SOL_SOCKET /SO_RE/ /SOMAX/];
    use Scalar::Util qw[/weak/];
    use Time::HiRes qw[sleep time];
    use Net::BitTorrent::Session;
    use Net::BitTorrent::Session::Peer;
    use Net::BitTorrent::Util qw[shuffle :log];
    my (%peer_id,                   %socket,
        %fileno,                    %timeout,
        %maximum_requests_per_peer, %maximum_requests_size,
        %maximum_buffer_size,       %maximum_peers_half_open,
        %maximum_peers_per_session, %maximum_peers_per_client,
        %connections,               %callbacks,
        %sessions,                  %use_unicode,
        %debug_level,               %pulse,
        %kBps_up,                   %kBps_down,
        %k_up,                      %k_down
    );

    sub new {
        my ($class, $args) = @_;

        # Let the user pick either LocalHost or LocalAddr like
        # IO::Socket::INET.  ...do I really want to do this?
        $args->{q[LocalAddr]} = $args->{q[LocalHost]}
            if exists $args->{q[LocalHost]}
                && !exists $args->{q[LocalAddr]};
        {

            # Allow the user to pass a list of potential port numbers
            my @portrange
                = defined $args->{q[LocalPort]}
                ? ref $args->{q[LocalPort]} eq q[ARRAY]
                    ? @{$args->{q[LocalPort]}}
                    : $args->{q[LocalPort]}
                : undef;
        PORT: for my $port (@portrange) {
                my ($socket, $host, $port)
                    = _open_socket($args->{q[LocalAddr]}, $port);
                defined $socket or next PORT;

                # Whew, everything's okay.  Let's bless this mess.
                my $self = bless \sprintf(q[%s:%d], $host, $port), $class;
                $socket{$self} = $socket;
                $self->_init();
                $self->_parse_args($args);
                return $self;
            }
        }
        return;
    }

    sub _init {
        my ($self) = @_;
        return if not defined $socket{$self};
        $k_up{$self}   = 0;
        $k_down{$self} = 0;
        $self->_set_pulse($self, time + 1);
        $use_unicode{$self} = 0;
        $debug_level{$self} = ERROR;
        $fileno{$self}      = fileno($socket{$self});
        $peer_id{$self} = pack(
            q[a20],
            (sprintf(
                 q[NB%03dC-%8s%5s],
                 (q[$Rev$] =~ m[(\d+)]g),
                 (join q[],
                  map {
                      [q[A] .. q[Z], q[a] .. q[z], 0 .. 9, qw[- . _ ~]]
                      ->[rand(66)]
                      } 1 .. 8
                 ),
                 q[Oops.],
             )
            )
        );
        $sessions{$self} = [];
        return $self->_add_connection($self);
    }

    sub _parse_args {
        my ($self, $args) = @_;

        # These are sane defaults, but you can change them with params.
        # Please note: these parameters are experimental and may change in a
        # future release.
        $self->maximum_buffer_size(defined $args->{q[maximum_buffer_size]}
                                   ? $args->{q[maximum_buffer_size]}
                                   : 131072
        );
        $self->maximum_peers_per_client(
                                  defined $args->{q[maximum_peers_per_client]}
                                  ? $args->{q[maximum_peers_per_client]}
                                  : 300
        );
        $self->maximum_peers_per_session(
                                 defined $args->{q[maximum_peers_per_session]}
                                 ? $args->{q[maximum_peers_per_session]}
                                 : 100
        );
        $self->maximum_peers_half_open(
                                   defined $args->{q[maximum_peers_half_open]}
                                   ? $args->{q[maximum_peers_half_open]}
                                   : 8
        );
        $self->maximum_requests_size(defined $args->{q[maximum_requests_size]}
                                     ? $args->{q[maximum_requests_size]}
                                     : 32768
        );
        $self->maximum_requests_per_peer(
                                 defined $args->{q[maximum_requests_per_peer]}
                                 ? $args->{q[maximum_requests_per_peer]}
                                 : 10
        );
        $self->timeout(defined $args->{q[Timeout]}
                       ? $args->{q[Timeout]}
                       : 1
        );

        # kBps_up and kBps_down: 0 == unlimited
        $self->kBps_up(defined $args->{q[kBps_up]}
                       ? $args->{q[kBps_up]}
                       : 0
        );
        $self->kBps_down(defined $args->{q[kBps_down]}
                         ? $args->{q[kBps_down]}
                         : 0
        );
        return 1;
    }

    sub _open_socket {    # Not for objects!
        my ($host, $port) = @_;

        # perldoc perlipc
        socket(my ($_socket), PF_INET, SOCK_STREAM, getprotobyname(q[tcp]))
            or return;

        # - What is the difference between SO_REUSEADDR and SO_REUSEPORT?
        #    [http://www.unixguide.net/network/socketfaq/4.11.shtml]
        # - setsockopt - what are the options for ActivePerl under Windows NT?
        #    [http://perlmonks.org/?node_id=63280]
        setsockopt($_socket, SOL_SOCKET, SO_REUSEADDR, pack(q[l], 1))
            or return;
        bind(
            $_socket,
            pack(
                q[Sna4x8],    # Why pack by hand?  .:shrugs:.
                AF_INET,
                (defined $port and $port =~ m[^(\d+)$] ? $1 : 0),
                (defined $host
                     and $host =~ m[^(?:\d+\.?){4}$]
                 ? (join q[], map { chr $_ } ($host =~ m[(\d+)]g))
                 : INADDR_ANY
                )
            )
        ) or return;
        listen($_socket, SOMAXCONN) or return;    # max of five?
        my (undef, $_port, @_address)
            = unpack(q[SnC4x8], getsockname($_socket));
        return ($_socket, sprintf(q[%d.%d.%d.%d], @_address), $_port);
    }

    sub _change_port {
        my ($self, $desired_host, $desired_port) = @_;
        my ($new_socket, @real_host, $real_port)
            = _open_socket($desired_host, $desired_port);
        if ($new_socket) {
            $$self = sprintf q[%d.%d.%d.%d:%d], @real_host, $real_port;
            return $socket{$self} = $new_socket;
        }
        return;
    }

    sub maximum_peers_per_client {
        my ($self, $value) = @_;
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
        return (
            defined $value
            ? do {
                $self->_do_callback(q[log], WARN, q[Timeout is malformed])
                    and return
                    unless $value =~ m[^-?\d+\.?\d*$];
                $timeout{$self} = $value;
                }
            : $timeout{$self}
        );
    }

    sub debug_level {
        my ($self, $value) = @_;
        return (
            defined $value
            ? do {
                $self->_do_callback(q[log], WARN, q[debug_level is malformed])
                    and return
                    unless $value =~ m[^-?\d+\.?\d*$];
                $debug_level{$self} = $value;
                }
            : $debug_level{$self}
        );
    }

    sub kBps_up {
        my ($self, $value) = @_;
        return (
            defined $value
            ? do {
                $self->_do_callback(q[log], WARN, q[kBps_up is malformed])
                    and return
                    unless $value =~ m[^-?\d+\.?\d*$];
                $kBps_up{$self} = $value;
                }
            : $kBps_up{$self}
        );
    }

    sub kBps_down {
        my ($self, $value) = @_;
        return (
            defined $value
            ? do {
                $self->_do_callback(q[log], WARN, q[kBps_up is malformed])
                    and return
                    unless $value =~ m[^-?\d+\.?\d*$];
                $kBps_down{$self} = $value;
                }
            : $kBps_down{$self}
        );
    }
    sub _socket { return $socket{$_[0]}; }
    sub _fileno { return $fileno{$_[0]}; }

    sub do_one_loop {    # Clunky.  I really need to replace this.
        my ($self) = @_;
        $self->_do_callback(q[log], TRACE,
                            sprintf(q[Entering %s for %s],
                                    [caller 0]->[3], $$self
                            )
        );
        grep {
            $_->_disconnect(
                        q[Connection timed out before established connection])
                if $_ ne $self
                    and (not $_->_connected)
                    and ($_->_connection_timestamp < (time - 60))
        } values %{$connections{$self}};

        # [id://371720]
        my ($rin, $win, $ein) = (q[], q[], q[]);
        for my $fileno (keys %{$connections{$self}}) {
            vec($ein, $fileno, 1) = 1;
            vec($rin, $fileno, 1) = 1;
            vec($win, $fileno, 1) = 1
                if $fileno ne $fileno{$self}
                    and $connections{$self}{$fileno}->_queue_outgoing;
        }
        my ($nfound, $timeleft)
            = select($rin, $win, $ein,
                     (  $timeout{$self}
                      ? $timeout{$self} == -1
                              ? undef
                              : $timeout{$self}
                      : undef
                     )
            );
        my $time = time + $timeleft;
        $self->process_connections(\$rin, \$win, \$ein)
            if $nfound and $nfound != -1;
        sleep($time - time) if $time - time > 0;    # save the CPU
        $self->process_timers();
        return 1;
    }

    # Connections. Trackers, Peers, ...even the client itself
    sub _add_connection {
        my ($self, $connection) = @_;
        $self->_do_callback(q[log], TRACE,
                            sprintf(q[Entering %s for %s],
                                    [caller 0]->[3], $$self
                            )
        );
        return $connections{$self}{$connection->_fileno} = $connection;
    }

    sub _remove_connection {
        my ($self, $connection) = @_;
        $self->_do_callback(q[log], TRACE,
                            sprintf(q[Entering %s for %s],
                                    [caller 0]->[3], $$self
                            )
        );
        return
            if not defined $connections{$self}{$connection->_fileno};
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
        return \%{$connections{$self}};
    }

    sub process_connections {
        my ($self, $rin, $win, $ein) = @_;
        foreach my $fileno (shuffle keys %{$connections{$self}}) {
            next
                if not defined $connections{$self}{$fileno};
            if (vec($$ein, $fileno, 1)
                or not $connections{$self}{$fileno}->_socket)
            {   vec($$ein, $fileno, 1) = 0;
                if ($^E
                    and (($^E != 10036) and ($^E != 10035)))
                {   $connections{$self}{$fileno}->_disconnect($^E);
                    next;
                }
            }
            elsif ($fileno eq $fileno{$self}) {
                if (vec($$rin, $fileno, 1)) {
                    vec($$rin, $fileno, 1) = 0;
                    accept(my ($new_socket), $socket{$self})
                        or $self->_do_callback(q[log], ERROR,
                                           q[Failed to accept new connection])
                        and next;
                    if (scalar(
                            grep {
                                defined $_
                                    and $_->isa(q[Net::BitTorrent::Peer])
                                } values %{$connections{$self}}
                        ) >= $maximum_peers_per_client{$self}
                        )
                    {   close $new_socket;
                    }
                    else {
                        my $new_peer =
                            Net::BitTorrent::Session::Peer->new(
                                                    {socket => $new_socket,
                                                     client => $self
                                                    }
                            );
                        $self->_add_connection($new_peer)
                            if $new_peer;
                    }
                }
            }
            else {
                my $read  = vec($$rin, $fileno, 1);
                my $write = vec($$win, $fileno, 1);
                vec($$rin, $fileno, 1) = 0;
                vec($$win, $fileno, 1) = 0;
                if ($read or $write) {
                    my ($this_down, $this_up)
                        = $connections{$self}{$fileno}->_process_one(
                                 (($kBps_down{$self}
                                   ? (($kBps_down{$self} * 1024)
                                      - $k_down{$self})
                                   : 2**15
                                  ) * $read
                                 ),
                                 (($kBps_up{$self}
                                   ? (($kBps_up{$self} * 1024) - $k_up{$self})
                                   : 2**15
                                  ) * $write
                                 ),
                        );
                    $k_down{$self} += $this_down || 0;
                    $k_up{$self}   += $this_up   || 0;
                }
            }
        }
        return 1;
    }
    sub peer_id { return $peer_id{$_[0]}; }

    sub sockport {
        return (unpack(q[SnC4x8], getsockname($socket{$_[0]})))[1];
    }

    sub sockaddr {
        return join q[.],
            (unpack(q[SnC4x8], getsockname($socket{$_[0]})))[2 .. 5];
    }

    sub sessions {
        my ($self) = @_;
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
            return $session
                if $session->infohash =~ m[^$infohash$]i;
        }
        return;
    }

    # Callback system
    sub _do_callback {
        my ($self, $type, @params) = @_;
        if ($type eq q[log]) {
            return if $debug_level{$self} < $params[0];
        }
        if (ref $callbacks{$self}{$type} ne q[CODE]) {
            $self->_do_callback(q[log], DEBUG,
                                sprintf(q[Unhandled callback '%s'], $type))
                if $type ne q[log];
            return;
        }
        return &{$callbacks{$self}{$type}}($self, @params);
    }

    sub set_callback {
        my ($self, $type, $coderef) = @_;
        return unless @_ == 3;
        return unless defined $type;
        if (ref $coderef ne q[CODE]) {
            $self->_do_callback(q[log], WARN, q[callback is malformed]);
            return;
        }
        return $callbacks{$self}{$type} = $coderef;
    }

    # Extension information
    sub _ext_FastPeers   {0}
    sub _ext_ExtProtocol {0}

    sub _build_reserved {
        my ($self) = @_;
        my @reserved = qw[0 0 0 0 0 0 0 0];
        $reserved[7] |= 0x04
            if $self->_ext_FastPeers;
        $reserved[5] |= 0x10
            if $self->_ext_ExtProtocol;
        return join q[], map {chr} @reserved;
    }

    # Internal scheduling
    sub _set_pulse {
        my ($self, $obj, $time) = @_;
        $pulse{$self}{$obj} = {object => $obj,
                               time   => $time
        };
        return weaken $pulse{$self}{$obj}{q[object]};
    }

    sub _del_pulse {
        my ($self, $obj) = @_;
        return delete $pulse{$self}{$obj};
    }

    sub _get_pulse {
        my ($self, $obj) = @_;
        return defined $pulse{$self}{$obj}
            ? $pulse{$self}{$obj}{q[time]}
            : 0;
    }

    sub _pulse {
        my ($self) = @_;
        $k_down{$self} = 0;
        $k_up{$self}   = 0;
        return $self->_set_pulse($self, time + 1);
    }

    sub process_timers {
        my ($self) = @_;
        for my $_pulse (values %{$pulse{$self}}) {
            if ($_pulse->{q[time]} <= time
                and defined $_pulse->{q[object]})
            {   my $obj = $_pulse->{q[object]};
                $self->_del_pulse($obj);
                $obj->_pulse;
            }
        }
    }

    # Debugging
    sub as_string {
        my ($self, $advanced) = @_;
        $self->_do_callback(q[log], TRACE,
                            sprintf(q[Entering %s for %s],
                                    [caller 0]->[3], $$self
                            )
        );
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
        return print STDERR qq[$dump\n]
            unless defined wantarray;
        return $dump;
    }
    DESTROY {
        my $self = shift;
        delete $peer_id{$self};
        delete $socket{$self};
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
        delete $kBps_up{$self};
        delete $kBps_down{$self};
        delete $k_up{$self};
        delete $k_down{$self};
        return 1;
    }
}
1;
__END__

=pod

=head1 NAME

Net::BitTorrent - BitTorrent peer-to-peer protocol class

=head1 Synopsis

    use Net::BitTorrent;

    sub hash_pass {
        my ($self, $piece) = @_;
        printf(qq[hash_pass: piece number %04d of %s\n],
               $piece->index, $piece->session);
    }

    my $client = Net::BitTorrent->new();
    $client->set_callback(q[piece_hash_pass], \&hash_pass);

    # ...
    # set various callbacks if you so desire
    # ...

    my $torrent = $client->add_session({path => q[a.legal.torrent]})
        or die q[Cannot load .torrent];

    while (1) {
        $client->do_one_loop();

        # Etc.
    }

=head1 Description

C<Net::BitTorrent> is a class based implementation of the current
BitTorrent Protocol Specification.  Each C<Net::BitTorrent> object is
capable of handling several concurrent .torrent sessions.

=head1 Constructor

=over 4

=item C<new ( { [ARGS] } )>

Creates a C<Net::BitTorrent> object.  C<new ( )> accepts arguments as a
hash, using key-value pairs, all of which are optional.  The most common
are:

=over 4

=item C<LocalAddr>

Local host bind address.  The value must be an IPv4 ("dotted quad") IP-
address of the C<xx.xx.xx.xx> form.  Unlike the
L<LocalAddr|IO::Socket::INET/"new ( [ARGS] )"> key used by
C<IO::Socket::INET>, it does not (currently) support an embedded port
number.  C<LocalHost> is a synonym for C<LocalAddr>.

Default: 0.0.0.0 (any address)

=item C<LocalPort>

TCP port opened to remote peers for incoming connections.  If handed a
list of ports, C<Net::BitTorrent> will traverse the list, attempting to
open on each of the ports until we succeed.  If this value is C<undef> or
C<0>, we allow the OS to choose an open port at random.

Though the default in most clients is a random port in the 6881-6889
range, BitTorrent has not been assigned a port number or range by the
IANA.  Nor is such a standard needed.

Default: 0 (any available)

=item C<Timeout>

The maximum amount of time, in seconds, possibly fractional, C<select()>
is allowed to wait before returning in L</do_one_loop>.

Default: C<1.0>

=back

Besides these, there are a number of advanced options that can be set via
the constructor.  Use these with caution as they can greatly affect the
basic functionality and usefulness of the module.

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

Maximum size, in bytes, a peer is allowed to request from us as a single
block.

Default: C<32768> (C<2**15>)

=end future

=item C<maximum_requests_per_peer>

Maximum number of requested blocks we keep in queue with each peer.

Default: C<10>

=item C<kBps_up>

Maximum amount of data transfered per second to remote hosts.

Default: C<0> (unlimited)

=item C<kBps_down>

Maximum amount of data transfered per second from remote hosts.

Default: C<0> (unlimited)

=back

=back

=head1 Methods

Unless otherwise stated, all methods return either a C<true> or C<false>
value, with C<true> meaning that the operation was a success.  When a
method states that it returns a value, failure will result in C<undef> or
an empty list.

Besides these listed here, there are several C<set_callback[...]> methods
described in the L</Callbacks> section.

=over 4

=item C<add_session ( { ... } )>

Loads a .torrent file and adds the new
L<Net::BitTorrent::Session|Net::BitTorrent::Session> object to the
client.

Most arguments passed to this method are handed directly to
L<Net::BitTorrent::Session::new( )|Net::BitTorrent::Session/"new ( { [ARGS] } )">.
The only mandatory parameter is C<path>.  C<path>'s value is the filename
of the .torrent file to load.  Please see
L<Net::BitTorrent::Session::new( )|Net::BitTorrent::Session/"new ( { [ARGS] } )">
for a list of possible parameters.

In addition to
L<Net::BitTorrent::Session::new( )|Net::BitTorrent::Session/"new ( { [ARGS] } )">'s
supported arguments, C<add_session> accepts a C<skip_hashcheck> key.  If
this bool value is set to a C<true> value, the files will not be checked
for integrity and we assume that we have none of the data of this
torrent.

This method returns the new
L<Net::BitTorrent::Session|Net::BitTorrent::Session> object on success.

See also: L<sessions|/sessions ( )>,
L<remove_session|/remove_session ( SESSION )>,
L<Net::BitTorrent::Session|Net::BitTorrent::Session>

=item C<as_string ( [ VERBOSE ] )>

Returns a 'ready to print' dump of the C<Net::BitTorrent> object's data
structure.  If called in void context, the structure is printed to
C<STDERR>.

Note: The serialized version returned by this method is not a full,
accurate representation of the object and cannot be C<eval>ed into a new
C<Net::BitTorrent> object or used as resume data.  The layout of and the
data included in this dump is subject to change in future versions.  This
is a debugging method, not to be used under normal circumstances.

See also: [id://317520]

=item C<debug_level ( [NEW VALUE] )>

Mutator to get/set the minimum level of messages passed to the log
callback handler.  See L<LOG LEVELS|Net::BitTorrent::Util/"LOG LEVELS">
for more.

=item C<do_one_loop ( )>

Processes the various socket-containing objects (peers, trackers) held by
this C<Net::BitTorrent> object.  This method should be called frequently.

=item process_connections ( READERSREF, WRITERSREF, ERRORSREF )

Use this method when you want to implement your own C<select> statement
for event processing instead of using C<Net::BitTorrent>'s L<do_one_loop>
method.  The parameters are references to the readers, writers, and
errors parameters used by the select statement.

Use the internal C<_fileno ( )> method to set up the necessary bit
vectors for your C<select ( )> call.

Note: This is a work in progress.

See Also:
L<Alternative Event Processing|/"Alternative Event Processing">

=item process_timers ( )

C<Net::BitTorrent> relies heavily on internal timing of various events
(socket timeouts, tracker requests, etc.) and simply cannot function
without processing these timers from time to time.

Use this method only when you implement your own C<select> statement for
event processing and do not use C<Net::BitTorrent>'s
L<do_one_loop|/"do_one_loop ( )"> method.

Note: This is a work in progress.

=item C<maximum_buffer_size ( [NEW VALUE] )>

Mutator to get/set the amount of unparsed data, in bytes, we store from a
peer before dropping their connection.  Be sure to keep this value high
enough to allow incoming blocks (C<2**16> by default) to be held in
memory without trouble but low enough to keep DDoS-like attacks at bay.

Default: C<131072> (C<2**17>)  I<(This default may change as the module
matures)>

=item C<maximum_peers_half_open ( [NEW VALUE] )>

Mutator to get/set the maximum number of peers we have yet to receive a
handshake from.  These include sockets that have not connected yet.

NOTE: On some OSes (WinXP, et al.), setting this too high can cause
problems with the TCP stack.

Default: C<8>

=item C<maximum_peers_per_client ( [NEW VALUE] )>

Mutator to get/set the maximum number of peers per client object.

Default: C<300>

See also: theory.org (http://tinyurl.com/4jgdnl)

=item C<maximum_peers_per_session ( [NEW VALUE] )>

Mutator to get/set the maximum number of peers per session.

Default: C<100>

=item C<maximum_requests_size ( [NEW VALUE] )>

Mutator to get/set the maximum size, in bytes, a peer is allowed to
request from us as a single block of data.

Default: C<32768>

See also: theory.org (http://tinyurl.com/32k7wu)

=item C<maximum_requests_per_peer ( [NEW VALUE] )>

Mutator to get/set the maximum number of blocks we have in queue from
each peer.

Default: C<10>

=item C<kBps_down ( [NEW VALUE] )>

Mutator to get/set the maximum amount of data transfered per second from
remote hosts in kilobytes.  This rate limits both peers and trackers.  To
remove transfer limits, set this value to C<0>.

Default: C<0> (unlimited)

=item C<kBps_down ( [NEW VALUE] )>

Mutator to get/set the maximum amount of data transfered per second from
remote hosts in kilobytes.  This rate limits both peers and trackers.  To
remove transfer limits, set this value to C<0>.

Default: C<0> (unlimited)

=item C<peer_id ( )>

Returns the Peer ID generated to identify this C<Net::BitTorrent> object
internally, with trackers, and with remote peers.

See also: theory.org (http://tinyurl.com/4a9cuv)

=item C<remove_session ( SESSION )>

Removes a C<Net::BitTorrent::Session> object from the client.

=begin future

Before the torrent session is closed, we announce to the tracker that we
have 'stopped' downloading and the callback to store the current state is
called.

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
possibly fractional, C<select()> is allowed to wait before returning in
L</do_one_loop>.

See Also: L<do_one_loop|/do_one_loop ( )>, C<Timeout> argument of the
L<constructor|/"new ( { [ARGS] } )">

=back

=head1 Callbacks

=over

=item C<set_callback( TYPE, CODEREF )>

C<Net::BitTorrent> provides a convenient callback system.  To set a
callback, use the C<set_callback( )> method.  For example, to catch all
attempts to read from a file, use
C<$client-E<gt>set_callback( 'file_read', \&on_read )>.

=back

Here is the current list of events fired by C<Net::BitTorrent> and
related classes as well as a brief description (soon) of them:

=head2 Peer level

Peer level events are triggered by
L<Net::BitTorrent::Peer|Net::BitTorrent::Peer> objects.

=begin future?

This list will be moved to N::B::P's POD.  Same goes for all the other
callbacks.

=end future?

=over

=item C<peer_connect>

Callback arguments: ( CLIENT, PEER )

=item C<peer_disconnect>

Callback arguments: ( CLIENT, PEER, [REASON] )

=item C<peer_incoming_bitfield>

Callback arguments: ( CLIENT, PEER )

=item C<peer_incoming_block>

Callback arguments: ( CLIENT, PEER, BLOCK )

=item C<peer_incoming_cancel>

Callback arguments: ( CLIENT, PEER, REQUEST )

=item C<peer_incoming_choke>

Callback arguments: ( CLIENT, PEER )

=item C<peer_incoming_data>

Callback arguments: ( CLIENT, PEER, LENGTH )

=item C<peer_incoming_disinterested>

Callback arguments: ( CLIENT, PEER )

=item C<peer_incoming_handshake>

Callback arguments: ( CLIENT, PEER )

=item C<peer_incoming_have>

Callback arguments: ( CLIENT, PEER, INDEX )

=item C<peer_incoming_interested>

Callback arguments: ( CLIENT, PEER )

=item C<peer_incoming_keepalive>

Callback arguments: ( CLIENT, PEER )

=item C<peer_incoming_packet>

Callback arguments: ( CLIENT, PEER, PACKET )

=item C<peer_incoming_request>

Callback arguments: ( CLIENT, PEER, REQUEST )

=item C<peer_incoming_unchoke>

Callback arguments: ( CLIENT, PEER )

=item C<peer_outgoing_bitfield>

Callback arguments: ( CLIENT, PEER )

=item C<peer_outgoing_block>

Callback arguments: ( CLIENT, PEER, REQUEST )

=item C<peer_outgoing_cancel>

Callback arguments: ( CLIENT, PEER, BLOCK )

=item C<peer_outgoing_choke>

Callback arguments: ( CLIENT, PEER )

=item C<peer_outgoing_data>

Callback arguments: ( CLIENT, PEER, LENGTH )

=item C<peer_outgoing_disinterested>

Callback arguments: ( CLIENT, PEER )

=item C<peer_outgoing_handshake>

Callback arguments: ( CLIENT, PEER )

=item C<peer_outgoing_have>

Callback arguments: ( CLIENT, PEER, INDEX )

=item C<peer_outgoing_interested>

Callback arguments: ( CLIENT, PEER )

=item C<peer_outgoing_keepalive>

Callback arguments: ( CLIENT, PEER )

=item C<peer_outgoing_request>

Callback arguments: ( CLIENT, PEER, BLOCK )

=item C<peer_outgoing_unchoke>

Callback arguments: ( CLIENT, PEER )

=back

=head2 Tracker level

Peer level events are triggered by
L<Net::BitTorrent::Tracker|Net::BitTorrent::Tracker> objects.

=over

=item C<tracker_announce>

Callback arguments: ( CLIENT, TRACKER )

=item C<tracker_announce_okay>

Callback arguments: ( CLIENT, TRACKER )

=item C<tracker_connect>

Callback arguments: ( CLIENT, TRACKER )

=item C<tracker_disconnect>

Callback arguments: ( CLIENT, TRACKER )

=item C<tracker_error>

Callback arguments: ( CLIENT, TRACKER, MESSAGE )

=item C<tracker_incoming_data>

Callback arguments: ( CLIENT, TRACKER, LENGTH )

=item C<tracker_outgoing_data>

Callback arguments: ( CLIENT, TRACKER, LENGTH )

=item C<tracker_scrape>

Callback arguments: ( CLIENT, TRACKER )

=item C<tracker_scrape_okay>

Callback arguments: ( CLIENT, TRACKER )

=back

=head2 File level

File level events are triggered by
L<Net::BitTorrent::Session::File|Net::BitTorrent::Session::File> objects.

=over

=item C<file_close>

Callback arguments: ( CLIENT, FILE )

=item C<file_error>

Callback arguments: ( CLIENT, FILE, [REASON] )

=item C<file_open>

Callback arguments: ( CLIENT, FILE )

=item C<file_read>

Callback arguments: ( CLIENT, FILE, LENGTH )

=item C<file_write>

Callback arguments: ( CLIENT, FILE, LENGTH )

=back

=head2 Piece level

Peer level events are triggered by
L<Net::BitTorrent::Session::Piece|Net::BitTorrent::Session::Piece>
objects.

=over

=item C<piece_hash_fail>

Callback arguments: ( CLIENT, PIECE )

=item C<piece_hash_pass>

Callback arguments: ( CLIENT, PIECE )

=back

=head2 Block level

Block level events are triggered by
L<Net::BitTorrent::Session::Piece::Block|Net::BitTorrent::Session::Piece::Block>
objects.

=over

=item C<block_write>

Callback arguments: ( CLIENT, BLOCK )

=back

=head2 Debug level

Debug level callbacks can be from anywhere and are not object specific.

=over

=item C<log>

Callback arguments: ( CLIENT, LEVEL, STRING )

See also: L<LOG LEVELS|Net::BitTorrent::Util/"LOG LEVELS">

=back

=head1 Implemented Extensions

Um, none yet.

=head1 Bugs

Numerous, I'm sure.  If you find one not listed in the F<Todo> file
included with this distribution, please report it.

List of know bugs:

=over

=item *

Test suite is incomplete.

=item *

This list of bugs is incomplete.

=back

Found bugs should be reported through
http://code.google.com/p/net-bittorrent/issues/list.  Please include
as much information as possible.  For more, see
"L<I've found a bug!  Now what?|Net::BitTorrent::FAQ/"I've found a bug!  Now what?">"
in L<Net::BitTorrent::FAQ|Net::BitTorrent::FAQ>.

=head1 Notes

=head2 Support and Availability

Visit the following for support and information related to
C<Net::BitTorrent>:

=over 4

=item The project's website

For wiki and subversion repository access, please visit the project's
home: http://net-bittorrent.googlecode.com/.

=item Bug and Issue Tracker

Use http://code.google.com/p/net-bittorrent/issues/list for bug
tracking.

Before creating and sending a report, please review the following list:

=over 2

=item *

Make sure you are using the most recent release of C<Net::BitTorrent>.
This may mean checking out the latest svn commit.

=item *

Make sure the bug is reproducible.

=item *

See the complete checklist in
L<Net::BitTorrent::FAQ|Net::BitTorrent::FAQ/"I've found a bug!  Now what?">.

=back

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

Changes to documented or well established parts will be clearly listed
and archived in the F<CHANGES> file.

Functions and parameters that are all_lower_case_and_contain_underscores
are typically experimental and have a very good chance of being
depreciated in a future version.

=item * B<All undocumented functionality is subject to change without notice.>

Because it's still early in its development, C<Net::BitTorrent> is filled
with incomplete bits of stuff.  I understand some of it seems stable, but
I reserve the right to change or eliminate code at any time without
warning I<unless> functionality is defined in POD documentation.

If you sift through the source and find something nifty that isn't
described I<in full> in POD, don't expect your code to work with future
releases.

=back

=head2 Examples

For a demonstration of C<Net::BitTorrent>, see F<scripts/client.pl> and
F<scripts/web-gui.pl>.

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

If you would like to contribute automated test reports (and I hope you
do), first install C<CPAN::Reporter> from the CPAN shell and then install
C<Net::BitTorrent>:

 $ cpan
 cpan> install CPAN::Reporter
 cpan> reload cpan
 cpan> o conf init test_report
   [...follow the CPAN::Reporter setup prompts...]
 cpan> o conf commit
 cpan> install Net::BitTorrent

For more on becoming a CPAN tester and why this is useful, please see the
L<CPAN::Reporter|CPAN::Reporter/"Description"> documentation,
http://cpantesters.perl.org/, and the CPAN Testers Wiki
(http://cpantest.grango.org/).

=head1 Alternative Event Processing

To making integrating C<Net::BitTorrent> into an existing C<select>-based
event loop just a little easier, an alternative way of doing event
processing (vs. L<do_one_loop ( )|/do_one_loop ( )>) has been designed...
uh, I mean ganked.  Simply call the L<"process_connections"> method with
references to the lists of readers, writers, and errors given to you by
C<select>.  Connections that don't belong to the object will be ignored,
and connections that do belong to the object will be removed from the
C<select> lists so that you can use the lists for your own purposes.

Here's a painfully simple example:

  while (1) {
      my ($rin, $win, $ein) = (q[], q[], q[]);
      vec($rin, fileno($server), 1) = 1;
      for my $object (values %{$bittorrent->_connections}) {
          vec($rin, $object->_fileno, 1) = 1;
          vec($win, $object->_fileno, 1) = 1
              if $object ne $bittorrent and $object->_queue_outgoing;
      }

      # Add your other sockets to the bit vectors

      $ein = $rin | $win;
      my ($nfound) = select($rin, $win, $ein, 1);
      $bittorrent->process_connections(\$rin, \$win, \$ein)
          if $nfound and $nfound != -1;
      $bittorrent->process_timers;    # Don't forget this!

      # Now $rin, $win, and $ein only have the file descriptors not
      # associated with Net::BitTorrent (related) objects in them -
      # we can process our events.
  }

For a full demonstration, see F<scripts/web-gui.pl>.

I<This is experimental and may be removed or improved in the future.>

=head1 See Also

http://bittorrent.org/beps/bep_0003.html - BitTorrent Protocol
Specification

L<Net::BitTorrent::FAQ|Net::BitTorrent::FAQ> - Random questions.  More
jibba jabba.

L<Net::BitTorrent::PeerID|Net::BitTorrent::PeerID> - The standard used to
identify C<Net::BitTorrent> in the wild.

=head1 Acknowledgments

Bram Cohen, for designing the base protocol and letting the community
decide what to do with it.

L Rotger

#bittorrent on Freenode for letting me idle.

Michel Valdrighi

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
