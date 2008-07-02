package Net::BitTorrent;
use strict;
use warnings;
{

    BEGIN {    # random change to have keywords updated by SVN...
        use version qw[qv];
        our $SVN
            = q[$Id$];
        our $UNSTABLE_RELEASE = 0; our $VERSION = sprintf(($UNSTABLE_RELEASE ? q[%.3f_%03d] : q[%.3f]), (version->new(qw$Rev$)->numify / 1000), $UNSTABLE_RELEASE);
    }
    use Socket qw[/F_INET/ /SOCK_/ /_ANY/ SOL_SOCKET /SO_RE/ /SOMAX/];
    use Scalar::Util qw[/weak/];
    use Time::HiRes qw[sleep time];
    use lib q[../../lib/];
    use Net::BitTorrent::Session;
    use Net::BitTorrent::Session::Peer;
    use Net::BitTorrent::DHT;
    use Net::BitTorrent::Util qw[shuffle :log];
    my (%peer_id,              %socket,            %fileno,
        %ul_slots_per_session, %ul_slot_size,      %max_buffer_per_conn,
        %max_halfopen,         %conns_per_session, %conns_per_client,
        %connections,          %callbacks,         %sessions,
        %debug_level,          %pulse,             %max_ul_rate,
        %max_dl_rate,          %k_up,              %k_down,
        %dht
    );

    sub new {
        my ($class, $args) = @_;
        my $self = undef;

        # Let the user pick either LocalHost or LocalAddr like
        # IO::Socket::INET.  ...do I really want to do this?
        $args->{q[LocalAddr]} = $args->{q[LocalHost]}
            if exists $args->{q[LocalHost]}
                && !exists $args->{q[LocalAddr]};
        {    # Allow the user to pass a list of potential port numbers
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
                $self = bless \sprintf(q[%s:%d], $host, $port), $class;
                $socket{$self} = $socket;
                $self->_init();
                $self->_parse_args($args);
                $dht{$self} = Net::BitTorrent::DHT->new({client => $self});
                warn $^E if not $dht{$self};
            }
        }
        return $self;
    }

    sub _init {
        my ($self) = @_;
        return if not defined $socket{$self};
        $k_up{$self}   = 0;
        $k_down{$self} = 0;
        $self->_set_pulse($self, time + 1);
        $debug_level{$self} = ERROR;
        $fileno{$self}      = fileno($socket{$self});
        $peer_id{$self} = pack(
            q[a20],
            (sprintf(
                 q[NB%03d%1s-%8s%5s],
                 (q[$Rev$] =~ m[(\d+)]g),
                 ($Net::BitTorrent::UNSTABLE_RELEASE ? q[S] : q[C]),
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
        $self->set_conns_per_client(defined $args->{q[conns_per_client]}
                                    ? $args->{q[conns_per_client]}
                                    : 300
        );
        $self->set_conns_per_session(defined $args->{q[conns_per_session]}
                                     ? $args->{q[conns_per_session]}
                                     : 100
        );

        # max_ul_rate and max_dl_rate: 0 == unlimited
        $self->set_max_dl_rate(defined $args->{q[max_dl_rate]}
                               ? $args->{q[max_dl_rate]}
                               : 0
        );
        $self->set_max_ul_rate(defined $args->{q[max_ul_rate]}
                               ? $args->{q[max_ul_rate]}
                               : 0
        );
        $self->set_max_halfopen(defined $args->{q[max_halfopen]}
                                ? $args->{q[max_halfopen]}
                                : 8
        );
        $self->set_max_buffer_per_conn(defined $args->{q[max_buffer_per_conn]}
                                       ? $args->{q[max_buffer_per_conn]}
                                       : 131072
        );
        $self->set_ul_slot_size(defined $args->{q[ul_slot_size]}
                                ? $args->{q[ul_slot_size]}
                                : 32768
        );
        $self->set_ul_slots_per_session(
                                      defined $args->{q[ul_slots_per_session]}
                                      ? $args->{q[ul_slots_per_session]}
                                      : 10
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
                 ? (pack q[C4], ($host =~ m[(\d+)]g))
                 : INADDR_ANY
                )
            )
        ) or return;
        listen($_socket, SOMAXCONN) or return;    # max of five?
        my (undef, $_port, @_address)
            = unpack(q[SnC4x8], getsockname($_socket));
        return ($_socket, sprintf(q[%d.%d.%d.%d], @_address), $_port);
    }

    #sub _change_port {
    #    my ($self, $desired_host, $desired_port) = @_;
    #    my ($new_socket, @real_host, $real_port)
    #        = _open_socket($desired_host, $desired_port);
    #    if ($new_socket) {
    #        $$self = sprintf q[%d.%d.%d.%d:%d], @real_host, $real_port;
    #        $dht{$self}->_open_socket;
    #        return $socket{$self} = $new_socket;
    #    }
    #    return;
    #}
    sub set_conns_per_client {
        my ($self, $value) = @_;
        if ((not defined $value) or ($value !~ m[^-?\d+\.?\d*$])) {
            $self->_do_callback(
                      q[log], ERROR,
                      sprintf(
                          q[new value for conns_per_client is malformed (%s)],
                          $value || q[undef])
            );
        }
        return $conns_per_client{$self} = $value;
    }
    sub get_conns_per_client { return $conns_per_client{$_[0]} }

    sub set_conns_per_session {
        my ($self, $value) = @_;
        if ((not defined $value) or ($value !~ m[^-?\d+\.?\d*$])) {
            $self->_do_callback(
                     q[log], ERROR,
                     sprintf(
                         q[new value for conns_per_session is malformed (%s)],
                         $value || q[undef])
            );
        }
        return $conns_per_session{$self} = $value;
    }

    sub get_conns_per_session {
        return $conns_per_session{$_[0]};
    }

    sub set_max_halfopen {
        my ($self, $value) = @_;
        if ((not defined $value) or ($value !~ m[^-?\d+\.?\d*$])) {
            $self->_do_callback(
                          q[log], ERROR,
                          sprintf(
                              q[new value for max_halfopen is malformed (%s)],
                              $value || q[undef])
            );
        }
        return $max_halfopen{$self} = $value;
    }
    sub get_max_halfopen { return $max_halfopen{$_[0]} }

    sub set_max_buffer_per_conn {
        my ($self, $value) = @_;
        if ((not defined $value) or ($value !~ m[^-?\d+\.?\d*$])) {
            $self->_do_callback(
                q[log], ERROR,
                sprintf(
                    q[new value for max_buffer_per_conn is malformed (%s).  Requires integer.],
                    $value || q[undef])
            );
        }
        return $max_buffer_per_conn{$self} = $value;
    }

    sub get_max_buffer_per_conn {
        return $max_buffer_per_conn{$_[0]};
    }

    sub set_ul_slot_size {
        my ($self, $value) = @_;
        if ((not defined $value) or ($value !~ m[^-?\d+\.?\d*$])) {
            $self->_do_callback(
                q[log], ERROR,
                sprintf(
                    q[new value for ul_slot_size is malformed (%s).  Requires integer.],
                    $value || q[undef])
            );
        }
        return $ul_slot_size{$self} = $value;
    }
    sub get_ul_slot_size { return $ul_slot_size{$_[0]} }

    sub set_ul_slots_per_session {    # TODO
        my ($self, $value) = @_;
        if ((not defined $value) or ($value !~ m[^-?\d+\.?\d*$])) {
            $self->_do_callback(
                q[log], ERROR,
                sprintf(
                    q[new value for ul_slots_per_session is malformed (%s).  Requires integer.],
                    $value || q[undef])
            );
        }
        return $ul_slots_per_session{$self} = $value;
    }

    sub get_ul_slots_per_session {
        return $ul_slots_per_session{$_[0]};
    }    # TODO
    sub get_ul_slots_per_conn { return 80 }
    sub set_ul_slots_per_conn { return; }     # NO OP

    sub set_debug_level {
        my ($self, $value) = @_;
        if ((not defined $value) or ($value !~ m[^-?\d+\.?\d*$])) {
            $self->_do_callback(
                           q[log], ERROR,
                           sprintf(
                               q[new value for debug_level is malformed (%s)],
                               $value || q[undef])
            );
        }
        return $debug_level{$self} = $value;
    }
    sub get_debug_level { return $debug_level{$_[0]} }

    sub set_max_ul_rate {
        my ($self, $value) = @_;
        if ((not defined $value) or ($value !~ m[^-?\d+\.?\d*$])) {
            $self->_do_callback(
                           q[log], ERROR,
                           sprintf(
                               q[new value for max_ul_rate is malformed (%s)],
                               $value || q[undef])
            );
        }
        return $max_ul_rate{$self} = $value;
    }
    sub get_max_ul_rate { return $max_ul_rate{$_[0]} }

    sub set_max_dl_rate {
        my ($self, $value) = @_;
        if ((not defined $value) or ($value !~ m[^-?\d+\.?\d*$])) {
            $self->_do_callback(
                           q[log], ERROR,
                           sprintf(
                               q[new value for max_dl_rate is malformed (%s)],
                               $value || q[undef])
            );
        }
        return $max_dl_rate{$self} = $value;
    }
    sub get_max_dl_rate { return $max_dl_rate{$_[0]} }
    sub _get_socket     { return $socket{$_[0]}; }
    sub _get_fileno     { return $fileno{$_[0]}; }

    sub do_one_loop {    # Clunky.  I really need to replace this.
        my ($self, $timeout) = @_;
        $timeout ||= 1;
        $self->_do_callback(q[log], TRACE,
                            sprintf(q[Entering %s for %s],
                                    [caller 0]->[3], $$self
                            )
        );
        grep {
            $_->_disconnect(
                        q[Connection timed out before established connection])
                if $_ ne $self
                    and ($_ ne $dht{$self})
                    and (not $_->_get_connected)
                    and ($_->_get_connection_timestamp < (time - 60))
        } values %{$connections{$self}};

        # [id://371720]
        my ($rin, $win, $ein) = (q[], q[], q[]);
        for my $fileno (keys %{$connections{$self}}) {
            vec($ein, $fileno, 1) = 1;
            vec($rin, $fileno, 1) = 1;
            vec($win, $fileno, 1) = 1
                if $fileno ne $fileno{$self}
                    and $connections{$self}{$fileno} ne $dht{$self}
                    and $connections{$self}{$fileno}->_get_queue_outgoing;
        }
        my ($nfound, $timeleft)
            = select($rin, $win, $ein,
                     (  $timeout
                      ? $timeout == -1
                              ? undef
                              : $timeout
                      : undef
                     )
            );
        my $time = time + $timeleft;
        $self->process_connections(\$rin, \$win, \$ein)
            if $nfound and $nfound != -1;
        if (($time - time) > 0) { sleep($time - time) }    # save the CPU
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
        return $connections{$self}{$connection->_get_fileno} = $connection;
    }

    sub _remove_connection {
        my ($self, $connection) = @_;
        $self->_do_callback(q[log], TRACE,
                            sprintf(q[Entering %s for %s],
                                    [caller 0]->[3], $$self
                            )
        );
        return
            if not defined $connections{$self}{$connection->_get_fileno};
        return delete $connections{$self}{$connection->_get_fileno};
    }

    sub _get_connections {
        my ($self) = @_;
        $self->_do_callback(q[log], TRACE,
                            sprintf(q[Entering %s for %s],
                                    [caller 0]->[3], $$self
                            )
        );
        $self->_do_callback(
              q[log],
              ERROR,
              q[ARG! ...s. Too many of them for Net::BitTorrent::_connections]
            )
            and return
            if @_ > 1;
        return \%{$connections{$self}};
    }

    sub process_connections {
        my ($self, $rin, $win, $ein) = @_;
        foreach my $fileno (shuffle keys %{$connections{$self}}) {
            next
                if not defined $connections{$self}{$fileno};
            if (vec($$ein, $fileno, 1)
                or not $connections{$self}{$fileno}->_get_socket)
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
                        ) >= $conns_per_client{$self}
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

            #elsif ($connections{$self}{$fileno} eq $dht{$self}) {
            #    if (vec($$rin, $fileno, 1)) {
            #        vec($$rin, $fileno, 1) = 0;
            #        #die q[Yay!];
            #    }
            #}
            else {
                my $read  = vec($$rin, $fileno, 1);
                my $write = vec($$win, $fileno, 1);
                vec($$rin, $fileno, 1) = 0;
                vec($$win, $fileno, 1) = 0;
                if ($read or $write) {
                    my ($this_down, $this_up)
                        = $connections{$self}{$fileno}->_process_one(
                             (( $max_dl_rate{$self}
                                ? (($max_dl_rate{$self} * 1024)
                                   - $k_down{$self})
                                : 2**15
                              ) * $read
                             ),
                             (($max_ul_rate{$self}
                               ? (($max_ul_rate{$self} * 1024) - $k_up{$self})
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
    sub get_peer_id { return $peer_id{$_[0]}; }
    sub get_dht     { return $dht{$_[0]}; }

    sub get_sockport {
        return (unpack(q[SnC4x8], getsockname($socket{$_[0]})))[1];
    }

    sub get_sockaddr {
        return join q[.],
            (unpack(q[SnC4x8], getsockname($socket{$_[0]})))[2 .. 5];
    }

    sub get_sessions {
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
        $session->get_trackers->[0]->_announce(q[stopped])
            if scalar @{$session->get_trackers};
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
                if uc $session->get_infohash eq uc $infohash;
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
            $self->_do_callback(q[log], ERROR, q[callback is malformed]);
        }
        return $callbacks{$self}{$type} = $coderef;
    }

    # Extension information
    sub _ext_DHT {1}

    sub _build_reserved {
        my ($self) = @_;
        my @reserved = qw[0 0 0 0 0 0 0 0];
        $reserved[7] |= 0x04;    # FastPeers
        $reserved[5] |= 0x10;    # Ext Protocol
        $reserved[7] |= 0x01
            if $self->_ext_DHT;
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
        my @timers = values %{$pulse{$self}};
        for my $timer (@timers) {
            if ($timer->{q[time]} <= time
                and defined $timer->{q[object]})
            {   my $obj = $timer->{q[object]};
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
        my @values = ($peer_id{$self},             $self->get_sockaddr,
                      $self->get_sockport,         $conns_per_client{$self},
                      $conns_per_session{$self},   $max_halfopen{$self},
                      $max_buffer_per_conn{$self}, $ul_slot_size{$self},
                      $ul_slots_per_session{$self},
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
        delete $conns_per_client{$self};
        delete $conns_per_session{$self};
        delete $max_halfopen{$self};
        delete $max_buffer_per_conn{$self};
        delete $ul_slot_size{$self};
        delete $ul_slots_per_session{$self};
        delete $debug_level{$self};
        delete $connections{$self};
        delete $callbacks{$self};

        #grep { $self->remove_session($_) } @{$sessions{$self}};
        delete $sessions{$self};
        delete $fileno{$self};
        delete $max_ul_rate{$self};
        delete $max_dl_rate{$self};
        delete $k_up{$self};
        delete $k_down{$self};

        # DHT
        delete $dht{$self};
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
               $piece->get_index, $piece->get_session);
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

L<Net::BitTorrent|Net::BitTorrent> is a class based implementation of the current
BitTorrent Protocol Specification.  Each L<Net::BitTorrent|Net::BitTorrent> object is
capable of handling several concurrent .torrent sessions.

=head1 Constructor

=over 4

=item C<new ( { [ARGS] } )>

Creates a L<Net::BitTorrent|Net::BitTorrent> object.  C<new ( )> accepts arguments as a
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
list of ports, L<Net::BitTorrent|Net::BitTorrent> will traverse the list, attempting to
open on each of the ports until we succeed.  If this value is C<undef> or
C<0>, we allow the OS to choose an open port at random.

Though the default in most clients is a random port in the 6881-6889
range, BitTorrent has not been assigned a port number or range by the
IANA.  Nor is such a standard needed.

Default: 0 (any available)

=back

Besides these, there are a number of advanced options that can be set via
the constructor.  Use these with caution as they can greatly affect the
basic functionality and usefulness of the module.

=over 4

=item C<max_buffer_per_conn>

Amount of data, in bytes, we store from a peer before dropping their
connection.  Setting this too high leaves you open to DDoS-like
attacks.  Malicious or not.

Default: C<131072> (C<2**17>)  I<(This default may change as the
module matures)>

=item C<conns_per_client>

Maximum number of peers per client object.

Default: C<200> I<(This default may change as the module matures)>

=item C<conns_per_session>

Maximum number of peers per session.

Default: C<100> I<(This default may change as the module matures)>

=item C<max_halfopen>

Maximum number of sockets we have yet to receive a handshake from.

NOTE: On some OSes (WinXP, et al.), setting this too high can cause
problems with the TCP stack.

Default: C<8>

=begin future

=item C<ul_slot_size>

Maximum size, in bytes, a peer is allowed to request from us as a single
block.

Default: C<32768> (C<2**15>)

=end future

=item C<ul_slots_per_session>

Maximum number of <requests|Net::BitTorrent::Session::Peer::Request> we
keep in queue with each peer.

Default: C<10>

=item C<max_ul_rate>

Maximum amount of data transfered per second to remote hosts.

Default: C<0> (unlimited)

=item C<max_dl_rate>

Maximum amount of data transfered per second from remote hosts.

Default: C<0> (unlimited)

=back

=back

=head1 Methods

Unless otherwise stated, all methods return either a C<true> or C<false>
value, with C<true> meaning that the operation was a success.  When a
method states that it returns a value, failure will result in C<undef> or
an empty list.

Besides these listed here, there is also the
L<set_callback( )|/"set_callback ( TYPE, CODEREF )"> method described in
the L<Callbacks|/Callbacks> section.

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

See also: L<get_sessions|/get_sessions ( )>,
L<remove_session|/remove_session ( SESSION )>,
L<Net::BitTorrent::Session|Net::BitTorrent::Session>

=item C<as_string ( [ VERBOSE ] )>

Returns a 'ready to print' dump of the L<Net::BitTorrent|Net::BitTorrent> object's data
structure.  If called in void context, the structure is printed to
C<STDERR>.

Note: The serialized version returned by this method is not a full,
accurate representation of the object and cannot be C<eval>ed into a new
L<Net::BitTorrent|Net::BitTorrent> object or used as resume data.  The layout of and the
data included in this dump is subject to change in future versions.  This
is a debugging method, not to be used under normal circumstances.

See also: http://perlmonks.org/?node_id=317520#debugging

=item C<get_debug_level ( )>

Get the minimum level of messages passed to the log callback handler.
See L<LOG LEVELS|Net::BitTorrent::Util/"LOG LEVELS"> for more.

=item C<set_debug_level ( NEWVAL )>

Set the minimum level of messages passed to the log callback handler.
See L<LOG LEVELS|Net::BitTorrent::Util/"LOG LEVELS"> for more.

=item C<do_one_loop ( [TIMEOUT] )>

Processes the various socket-containing objects (peers, trackers) held by
this L<Net::BitTorrent|Net::BitTorrent> object.  This method should be called frequently.

The optional TIMEOUT parameter is the maximum amount of time, in seconds,
possibly fractional, C<select()> is allowed to wait before returning in
L<do_one_loop( )|/"do_one_loop ( [TIMEOUT] )">.  This TIMEOUT defaults to
C<1.0>.  To wait indefinatly, TIMEOUT should be C<-1.0>.

=item C<process_connections ( READERSREF, WRITERSREF, ERRORSREF )>

Use this method when you want to implement your own C<select> statement
for event processing instead of using L<Net::BitTorrent|Net::BitTorrent>'s
L<do_one_loop( )|/"do_one_loop ( [TIMEOUT] )"> method.  The parameters are
references to the readers, writers, and errors parameters used by the
select statement.

Use the internal C<_get_fileno( )> method to set up the necessary bit
vectors for your C<select> call.

I<This is experimental and may be improved in the future.>

See Also:
L<Alternative Event Processing|/"Alternative Event Processing">

=item C<process_timers ( )>

L<Net::BitTorrent|Net::BitTorrent> relies heavily on internal timing of various events
(socket timeouts, tracker requests, etc.) and simply cannot function
without processing these timers from time to time.

Use this method only if you have implemented your own C<select> statement
for event processing instead of L<Net::BitTorrent|Net::BitTorrent>'s
L<do_one_loop( )|/"do_one_loop ( [TIMEOUT] )"> method.

See Also:
L<Alternative Event Processing|/"Alternative Event Processing">

=item C<get_max_buffer_per_conn ( )>

Get the amount of unparsed data, in bytes, we store from a peer before
dropping their connection.

See Also:
L<set_max_buffer_per_conn ( NEWVAL )|/"set_max_buffer_per_conn ( NEWVAL )">

=item C<set_max_buffer_per_conn ( NEWVAL )>

Set the amount of unparsed data, in bytes, we store from a
peer before dropping their connection.  Be sure to keep this value high
enough to allow incoming blocks (C<2**16> by default) to be held in
memory without trouble but low enough to keep DDoS-like attacks at bay.

Default: C<131072> (C<2**17>)  I<(This default may change as the module
matures)>

See Also:
L<get_max_buffer_per_conn ( )|/"get_max_buffer_per_conn ( )">

=item C<get_max_halfopen ( )>

Get the maximum number of peers we have yet to receive a
handshake from.  These include sockets that have not connected yet.

See Also: L<set_max_halfopen ( NEWVAL )|/"set_max_halfopen ( NEWVAL )">

=item C<set_max_halfopen ( NEWVAL )>

Set the maximum number of peers we have yet to receive a handshake from.
These include sockets that have not connected yet.

NOTE: On some OSes (WinXP, et al.), setting this too high can cause
problems with the TCP stack.

Default: C<8>

See Also: L<get_max_halfopen ( )|/"get_max_halfopen ( )">

=item C<get_conns_per_client ( )>

Get the maximum number of peers per client object.

See Also: L<set_conns_per_client ( )|/"set_conns_per_client ( NEWVAL )">

=item C<set_conns_per_client ( NEWVAL )>

Set the maximum number of peers per client object.

Default: C<300>

See also: theory.org (http://tinyurl.com/4jgdnl),
L<get_conns_per_client ( )|/"get_conns_per_client ( )">

=item C<get_conns_per_session ( )>

Get the maximum number of peers per session.

See Also:
L<set_conns_per_session ( )|/"set_conns_per_session ( NEWVAL )">

=item C<set_conns_per_session ( NEWVAL )>

Set the maximum number of peers per session.

Default: C<100>

See Also: L<get_conns_per_session ( )|/"get_conns_per_session ( )">

=item C<get_ul_slot_size ( )>

Get the maximum size, in bytes, a peer is allowed to request from us as a
single block of data.

See Also: L<set_ul_slot_size ( )|/"set_ul_slot_size ( NEWVAL )">

=item C<set_ul_slot_size ( NEWVAL )>

Set the maximum size, in bytes, a peer is allowed to request from us as a
single block of data.

Default: C<32768>

See also: theory.org (http://tinyurl.com/32k7wu),
L<get_ul_slot_size ( )|/"get_ul_slot_size ( )">

=item C<get_ul_slots_per_session ( )>

Get the maximum number of blocks we have in queue from each peer.

See Also:
L<set_ul_slots_per_session ( )|/"set_ul_slots_per_session ( NEWVAL )">

=item C<set_ul_slots_per_session ( NEWVAL )>

Set the maximum number of blocks we have in queue from each peer.

Default: C<10>

See Also: L<get_ul_slots_per_session ( )|/"get_ul_slots_per_session ( )">

=item C<get_ul_slots_per_conn ( )>

Get the maximum number of blocks we have in queue from each peer.
Currently, this is C<80>.

See Also:
L<set_ul_slots_per_conn ( )|/"set_ul_slots_per_conn ( NEWVAL )">

=item C<set_ul_slots_per_conn ( NEWVAL )>

Set the maximum number of blocks we have in queue from each peer.
Currently, this is a NOOP.  Yeah, yeah... It's on my Todo list...

See Also: L<get_ul_slots_per_conn ( )|/"get_ul_slots_per_conn ( )">

=item C<get_max_ul_rate ( )>

Get the maximum amount of data transfered per second from remote hosts in
kilobytes.  This rate limits both peers and trackers.  To remove transfer
limits, set this value to C<0>.

Note: This functionality requires the use of
L<do_one_loop( )|/"do_one_loop ( [TIMEOUT] )"> for event processing.

See Also: L<set_max_ul_rate ( )|/"set_max_ul_rate ( NEWVAL )">

=item C<set_max_ul_rate ( NEWVAL )>

Set the maximum amount of data transfered per second from remote hosts in
kilobytes.  This rate limits both peers and trackers.  To remove transfer
limits, set this value to C<0>.

Note: This functionality requires the use of
L<do_one_loop( )|/"do_one_loop ( [TIMEOUT] )"> for event processing.

Default: C<0> (unlimited)

See Also: L<get_max_ul_rate ( )|/"get_max_ul_rate ( )">

=item C<get_max_dl_rate ( )>

Get the maximum amount of data transfered per second from remote hosts in
kilobytes.  This rate limits both peers and trackers.  To remove transfer
limits, set this value to C<0>.

Note: This functionality requires the use of
L<do_one_loop( )|/"do_one_loop ( [TIMEOUT] )"> for event processing.

See Also: L<get_max_dl_rate ( )|/"get_max_dl_rate ( NEWVAL )">

=item C<set_max_dl_rate ( NEWVAL )>

Set the maximum amount of data transfered per second from remote hosts in
kilobytes.  This rate limits both peers and trackers.  To remove transfer
limits, set this value to C<0>.

Note: This functionality requires the use of
L<do_one_loop( )|/"do_one_loop ( [TIMEOUT] )"> for event processing.

Default: C<0> (unlimited)

See Also: L<get_max_dl_rate ( )|/"get_max_dl_rate ( )">

=item C<get_peer_id ( )>

Returns the Peer ID generated to identify this L<Net::BitTorrent|Net::BitTorrent> object
internally, with trackers, and with remote peers.

See also: theory.org (http://tinyurl.com/4a9cuv),
L<Peer ID Specification|Net::BitTorrent::Notes/"Peer ID Specification">

=item C<get_dht ( )>

Returns the C<Net::BitTorrent::DHT> object related to this client.

See Also: L<Net::BitTorrent::DHT|Net::BitTorrent::DHT>

=item C<remove_session ( SESSION )>

Removes a L<Net::BitTorrent::Session> object from the client.

=begin future

Before the torrent session is closed, we announce to the tracker that we
have 'stopped' downloading and the callback to store the current state is
called.

=end future

See also: L<get_sessions ( )|/"get_sessions ( )">,
L<add_session|/"add_session ( { ... } )">,
L<Net::BitTorrent::Session|Net::BitTorrent::Session>

=item C<get_sessions ( )>

Returns a list of loaded
L<Net::BitTorrent::Session|Net::BitTorrent::Session> objects.

See Also: L<add_session|/"add_session ( { ... } )">,
L<remove_session|/"remove_session ( SESSION )">,
L<Net::BitTorrent::Session|Net::BitTorrent::Session>

=item C<get_sockaddr ( )>

Return the address part of the sockaddr structure for the TCP socket.

See also: L<IO::Socket::INET/sockaddr>

=item C<get_sockport ( )>

Return the port number that the TCP socket is using on the local host.

See also: L<IO::Socket::INET/sockport>

=back

=head1 Callbacks

=over

=item C<set_callback( TYPE, CODEREF )>

L<Net::BitTorrent|Net::BitTorrent> provides a convenient callback system.  To set a
callback, use the C<set_callback( )> method.  For example, to catch all
attempts to read from a file, use
C<$client-E<gt>set_callback( 'file_read', \&on_read )>.

=back

Here is the current list of events fired by L<Net::BitTorrent|Net::BitTorrent> sorted by
their related classes:

=head2 Peer level

Peer level events are triggered by
L<Net::BitTorrent::Session::Peer|Net::BitTorrent::Session::Peer> objects.

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

=item C<peer_outgoing_port>

Callback arguments: ( CLIENT, PEER )

=back

=head2 Tracker level

Peer level events are triggered by
L<Net::BitTorrent::Session::Tracker|Net::BitTorrent::Session::Tracker>
objects.

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

See also: L<Log Levels|Net::BitTorrent::Util/"LOG LEVELS"> in
L<Net::BitTorrent::Util|Net::BitTorrent::Util>

=back

=head1 Implemented Extensions

See L<Net::BitTorrent::Notes|Net::BitTorrent::Notes/"Implemented Extensions">

=head1 Bugs

Numerous, I'm sure.

Found bugs should be reported through
http://code.google.com/p/net-bittorrent/issues/list.  Please include
as much information as possible.  For more, see
L<Net::BitTorrent::Todo|Net::BitTorrent::Todo> as well as
L<Issue Tracker|Net::BitTorrent::Notes/"Issue Tracker">,
L<Bug Reporting|Net::BitTorrent::Notes/"Bug Reporting">, and
L<Co-Development and Patch Submission|Net::BitTorrent::Notes/"Co-Development and Patch Submission">
information from the L<Net::BitTorrent::Notes|Net::BitTorrent::Notes>.

=head1 Notes

=head2 Support and Availability

Visit the following for support and information related to
L<Net::BitTorrent|Net::BitTorrent>:

=over 4

=item The project's website

For wiki and subversion repository access, please visit the project's
home: http://sankorobinson.com/net-bittorrent/.

=item Bug and Issue Tracker

Use http://code.google.com/p/net-bittorrent/issues/list for bug
tracking.

Before creating and sending a report, please review the following list:

=over 2

=item *

Make sure you are using the most recent release of L<Net::BitTorrent|Net::BitTorrent>.
This may mean checking out the latest svn commit.

=item *

Make sure the bug is reproducible.

=item *

See the complete checklist in the
L<Net::BitTorrent::Notes|Net::BitTorrent::Notes/"Bug Reporting">.

=back

=back

See L<Net::BitTorrent::Notes|Net::BitTorrent::Notes/"See Also">
for links to a mailing list, svn information, and more.

=head2 Dependencies

L<Net::BitTorrent|Net::BitTorrent> requires L<version|version> and
L<Digest::SHA|Digest::SHA> to function and relies upon L<Module::Build>
for installation.  As of perl 5.10, these are all CORE modules; they come
bundled with the distribution.

=head2 Development Policy

=over 4

=item * B<All APIs are subject to change.>

Changes to documented or well established parts will be clearly listed
and archived in the F<CHANGES> file.

Functions and parameters that are all_lower_case_and_contain_underscores
are typically experimental and have a very good chance of being
depreciated in a future version.

=item * B<All undocumented functionality is subject to change without notice.>

Because it's still early in its development,
L<Net::BitTorrent|Net::BitTorrent> is filled with incomplete bits of
stuff.  I understand some of it seems stable, but I reserve the right to
change or eliminate code at any time without warning I<unless>
functionality is defined in POD documentation.

If you sift through the source and find something nifty that isn't
described I<in full> in POD, don't expect your code to work with future
releases.

=back

=head2 Examples

For a demonstration of L<Net::BitTorrent|Net::BitTorrent>, see
L<scripts/client.pl|scripts/client.pl> and
L<scripts/web-gui.pl>.

=head2 Installation

This distribution uses L<Module::Build|Module::Build> for installation,
so use the following procedure:

  perl Build.PL
  ./Build
  ./Build test
  ./Build install

Or, if you're on a platform (like DOS or Windows) that doesn't require
the "F<./>" notation, you can do this:

  perl Build.PL
  Build
  Build test
  Build install

If you would like to contribute automated test reports (and I hope you
do), first install L<CPAN::Reporter|CPAN::Reporter> from the CPAN shell
and then install L<Net::BitTorrent|Net::BitTorrent>:

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

To making integrating L<Net::BitTorrent|Net::BitTorrent> into an existing C<select>-based
event loop just a little easier, an alternative way of doing event
processing (vs. L<do_one_loop ( )|/"do_one_loop ( [TIMEOUT] )">) has been
designed... uh, I mean ganked.  Simply call the
L<process_connections( )|/"process_connections ( READERSREF, WRITERSREF, ERRORSREF )">
method with references to the lists of readers, writers, and errors given
to you by C<select>.  Connections that don't belong to the object will be
ignored, and connections that do belong to the object will be removed
from the C<select> lists so that you can use the lists for your own
purposes.

Here's a painfully simple example:

  while (1) {
      my ($rin, $win, $ein) = (q[], q[], q[]);
      vec($rin, fileno($server), 1) = 1;
      for my $object (values %{$bittorrent->_get_connections}) {
          vec($rin, $object->_get_fileno, 1) = 1;
          vec($win, $object->_get_fileno, 1) = 1
              if $object ne $bittorrent and $object->_get_queue_outgoing;
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

For a working demonstration, see
L<scripts/web-gui.pl|scripts/web-gui.pl>.

I<This is experimental and may be improved in the future.>

=head1 See Also

http://bittorrent.org/beps/bep_0003.html - BitTorrent Protocol
Specification

L<Net::BitTorrent::Notes|Net::BitTorrent::Notes> - Random stuff.  More
jibba jabba.

L<Peer ID Specification|Net::BitTorrent::Notes/"Peer ID Specification"> -
The standard used to identify L<Net::BitTorrent|Net::BitTorrent> in the
wild.

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
