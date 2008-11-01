#!C:\perl\bin\perl.exe
package Net::BitTorrent;
{
    use strict;      # core as of perl 5
    use warnings;    # core as of perl 5.006

    #
    use Scalar::Util    # core as of perl 5.007.003
        qw[blessed weaken refaddr];
    use Time::HiRes;    # core as of perl 5.007003
    use Socket          # core as of perl 5
        qw[    /inet_/
        SOCK_STREAM /SO_REUSE/ SOL_SOCKET
        PF_INET     SOMAXCONN
        /pack_sockaddr_in/];
    use Carp            # core as of perl 5
        qw[carp carp];

    #$Carp::Internal{q[Net::BitTorrent]}++;
    # I could also use Errno, but it's so heavy...
    use POSIX qw[];     # core as of perl 5
    sub _EWOULDBLOCK { $^O eq q[MSWin32] ? 10035 : POSIX::EWOULDBLOCK() }
    sub _EINPROGRESS { $^O eq q[MSWin32] ? 10036 : POSIX::EINPROGRESS() }

    #
    use version qw[qv];    # core as of 5.009
    our $SVN = q[$Id$];
    our $UNSTABLE_RELEASE = 4; our $VERSION = sprintf(($UNSTABLE_RELEASE ? q[%.3f_%03d] : q[%.3f]), (version->new((qw$Rev$)[1])->numify / 1000), $UNSTABLE_RELEASE);

    #
    use lib q[../../lib];
    use Net::BitTorrent::Torrent;
    use Net::BitTorrent::Peer;
    use Net::BitTorrent::DHT;
    use Net::BitTorrent::Util qw[:log];
    use Net::BitTorrent::Version;

    # Debugging
    #use Data::Dump qw[pp];
    #
    my (@CONTENTS)
        = \my (%_socket,            %_peerid,      %_schedule,
               %_peers_per_torrent, %_event,       %_dht,
               %_torrents,          %_connections, %_max_ul_rate,
               %_k_up,              %_max_dl_rate, %_k_down,
               %_tid
        );
    my %REGISTRY;

    # Net::BitTorrent
    #~    dht       =>  Net::BitTorrent::DHT
    #~                      Net::BitTorrent::DHT::Azureus
    #~                      Net::BitTorrent::DHT::Mainline
    #~    peers     =>  [Net::BitTorrent::Peer]
    #~    torrent   =>  [Net::BitTorrent::Torrent]
    #~                  [Net::BitTorrent::Torrent::Tracker]
    #~ 		                [Net::BitTorrent::Torrent::Tracker::UDP]
    #~                      [Net::BitTorrent::Torrent::Tracker::HTTP]
    #
    # Constructor
    sub new {
        my ($class, $args) = @_;
        my $self;
        my ($hostname, @portlist) = (q[0.0.0.0], (0));   # Reasonable Defaults
        my ($reuseaddr, $reuseport) = (0, 0);            # XXX - undocumented
        if (defined $args) {
            if (ref($args) ne q[HASH]) {
                carp q[Net::BitTorrent->new({}) requires ]
                    . q[parameters to be passed as a hashref];
                return;
            }
            $hostname = $args->{q[LocalHost]}
                if defined $args->{q[LocalHost]};
            @portlist
                = defined $args->{q[LocalPort]}
                ? (ref($args->{q[LocalPort]}) eq q[ARRAY]
                   ? @{$args->{q[LocalPort]}}
                   : $args->{q[LocalPort]}
                )
                : @portlist;
            {    # XXX - undocumented
                $reuseaddr = $args->{q[ReuseAddr]}
                    if defined $args->{q[ReuseAddr]};
                $reuseport = $args->{q[ReusePort]}
                    if defined $args->{q[ReusePort]};
            }
        }

        # MO:
        # - foreach @portlist
        #   - open socket
        #   - if socket is okay
        #     - last port
        # - if socket is okay
        #   - get port and ip
        #   - bless $self (which stringifies to IPv4:port)
        #   - store socket
        #   - store peerid
        #   - create DHT object ---- off by default
        # - return $self (which, unless blessed, is undef)
        my $_socket;
    PORT: for my $port (@portlist) {
            $_socket = __socket_open($hostname, $port, $args->{q[ReuseAddr]},
                                     $args->{q[ReusePort]});
            last PORT if defined $_socket;
        }
        my ($port, $packed_ip);
        if (defined $_socket) {
            ($port, $packed_ip) = unpack_sockaddr_in(getsockname($_socket));
            my $address = sprintf(q[%s:%d], inet_ntoa($packed_ip), $port);
            $self = bless \$address, $class;
            $_socket{refaddr $self} = $_socket;
            $_peerid{refaddr $self} = Net::BitTorrent::Version::gen_peerid();
            $_tid{refaddr $self} = qq[\0] x 5;    # 26^5 before rollover
            if (not($self->_add_connection($self, q[ro]))) {
                carp q[Could not add server socket to list of connections];
                return;
            }
        }
        if (not $self) {                          # failed to open socket?
            return;
        }

        # Settings
        $_peers_per_torrent{refaddr $self} = 50;
        $_max_dl_rate{refaddr $self}       = 0;
        $_max_ul_rate{refaddr $self}       = 0;

        #
        $_torrents{refaddr $self} = {};           # by infohash

        #
        #$self->_schedule(
        #    {Time    => time + 5,
        #     Code => $self->_add_connections }
        #    }
        #);
        #
        #
        $self->_schedule(
            {Time => time + 5,
             Code => sub {
                 return $_k_down{refaddr $_[0]} = $_k_up{refaddr $_[0]} = 0;
             },
             Object => $self
            }
        );

        #
        weaken($REGISTRY{refaddr $self} = $self);
        return $self;
    }

    # Accessors | Private
    sub _socket      { return $_socket{refaddr +shift} }
    sub _connections { return $_connections{refaddr +shift} }
    sub _dht         { return $_dht{refaddr +shift} }
    sub _max_ul_rate { return $_max_ul_rate{refaddr +shift} }
    sub _max_dl_rate { return $_max_dl_rate{refaddr +shift} }

    # Setters | Private
    sub _set_max_ul_rate {
        warn q[TODO: Param validation];
        return $_max_ul_rate{refaddr +shift} = shift;
    }

    sub _set_dl_rate {
        warn q[TODO: Param validation];
        return $_max_dl_rate{refaddr +shift} = shift;
    }

    sub _port {
        my ($self) = @_;
        return if not defined $_socket{refaddr $self};
        my ($port, undef)
            = unpack_sockaddr_in(getsockname($_socket{refaddr $self}));
        return $port;
    }

    sub _host {
        my ($self) = @_;
        return if not defined $_socket{refaddr $self};
        my (undef, $packed_ip)
            = unpack_sockaddr_in(getsockname($_socket{refaddr $self}));
        return inet_ntoa($packed_ip);
    }
    sub _peers_per_torrent { return $_peers_per_torrent{refaddr +shift}; }

    # Accessors | Public
    sub peerid   { return $_peerid{refaddr +shift} }
    sub torrents { return $_torrents{refaddr +shift} }

    # Methods | Public
    sub do_one_loop {    # Clunky.  I really need to replace this.
        my ($self, $timeout) = @_;
        return if defined $timeout and $timeout !~ m[^\+?\d+\.?\d*$];
        $self->_process_schedule;
        $timeout = ($timeout ? $timeout : 1);

       #warn pp \%_connections;
       #grep {
       #    $_->_disconnect(
       #                q[Connection timed out before established connection])
       #        if $_ ne $self
       #            and ($_ ne $_dht{refaddr $self})
       #            #and (not $_->get_is_connected)
       #            #and ($_->get_connection_timestamp < (time - 60))
       #} values %{$_connections{refaddr $self}};
       # [id://371720]
        my ($rin, $win, $ein) = (q[], q[], q[]);
    PUSHSOCK: for my $fileno (keys %{$_connections{refaddr $self}}) {
            vec($rin, $fileno, 1) = 1
                if $_connections{refaddr $self}{$fileno}{q[Mode]} =~ m[r];
            vec($win, $fileno, 1) = 1
                if $_connections{refaddr $self}{$fileno}{q[Mode]} =~ m[w];
            vec($ein, $fileno, 1) = 1;

            #warn sprintf q[[%d|%s]], $fileno,
            #    $_connections{refaddr $self}{$fileno}{q[Mode]};
        }
        my ($nfound, $timeleft) = select($rin, $win, $ein, $timeout);

        #
        my $time = time + $timeleft;

        #
        $self->_process_connections(\$rin, \$win, \$ein)
            if $nfound and $nfound != -1;

        #
        if (($time - time) > 0) { sleep($time - time) }    # save the CPU
                                                           #
        return 1;
    }

    # Enable/disable dht
    sub _use_dht {
        my ($self, $value) = @_;

        #
        if (not defined $value) {
            carp q[Net::BitTorrent->dht( VALUE ) requires a bool value];
            return;
        }

        #
        if ($value) {
            if (not defined $_dht{refaddr $self}) {
                my ($port, $packed_ip)
                    = unpack_sockaddr_in(
                                        getsockname($_socket{refaddr $self}));
                $_dht{refaddr $self} = Net::BitTorrent::DHT->new(
                    {   Client    => $self,
                        LocalAddr => inet_ntoa($packed_ip),
                        LocalPort => $port,

                        #ReuseAddr => $args->{q[ReuseAddr]},
                        #ReusePort => $args->{q[ReusePort]}
                    }
                );
                if (defined $_dht{refaddr $self}) {
                    $self->_add_connection($_dht{refaddr $self}, q[ro])
                        or return;
                }
            }
            return defined $_dht{refaddr $self};
        }
        else {
            if (defined $_dht{refaddr $self}) {
                $self->_remove_connection($_dht{refaddr $self});
                $_dht{refaddr $self}->_socket->close();
                delete $_dht{refaddr $self};
            }
            return !defined $_dht{refaddr $self};
        }

        #
        return;
    }

    # Methods | Private
    # Connections. Trackers, Peers, ...even the client itself
    sub _add_connection {    # untested
        my ($self, $connection, $mode) = @_;

      # Adds a socket to this objects list for select()ion
      # Expects parameters in list context:
      #   - ref to self    (...this is OOP after all.)
      #   - the connection (Duh.)
      #   - the mode       (What should we ask select() about for this socket.
      #                       'rw', 'wo', or 'ro')
      # Returns:
      #   - true on success
      #   - false on failure (socket already in list, missing params, etc.)
      # Param validation
        if (not defined $connection) {
            carp q[Net::BitTorrent->_add_connection() requires an object];
            return;
        }
        if (not blessed $connection) {
            carp
                q[Net::BitTorrent->_add_connection() requires a blessed object];
            return;
        }
        my $_socket = $connection->_socket;
        if (ref($_socket) ne q[GLOB]) {
            carp
                q[Net::BitTorrent->_add_connection(SOCKET, MODE) requires a GLOB-type socket];
            return;
        }

        #
        if (not defined $mode) {
            carp
                q[Net::BitTorrent->_add_connection(SOCKET, MODE) requires a $mode parameter];
            return;
        }
        if ($mode !~ m[^(?:ro|rw|wo)$]) {
            carp
                sprintf(
                q['%s' is not a valid mode for Net::BitTorrent->_add_connection(SOCKET, MODE)],
                $mode);
            return;
        }
        carp unless ref($_socket) eq q[GLOB];    # untested
        carp unless fileno $_socket;
        if (    (defined $_connections{refaddr $self}{fileno $_socket})
            and
            ($_connections{refaddr $self}{fileno $_socket}{q[Mode]} eq $mode))
        {    #carp q[This connection object is already loaded.];
            return;
        }
        $_connections{refaddr $self}{fileno $_socket} = {
                                                        Object => $connection,
                                                        Mode   => $mode,
            }
            or return;
        if ($connection->isa(q[Net::BitTorrent])) {
            weaken $_connections{refaddr $self}{fileno $_socket}{q[Object]};
        }
        return 1;
    }

    sub _remove_connection {

        # Removes a socket to this objects list for select()ion
        # Expects parameters in list context:
        #   - ref to self (...this is OOP after all.)
        #   - the connection  (Duh.)
        # Returns:
        #   - true on success
        #   - false on failure (socket not in list, missing params, etc.)
        my ($self, $connection) = @_;
        if (not defined $connection) {
            carp q[Net::BitTorrent->_remove_connection() requires an object];
            return;
        }
        if (not blessed $connection) {
            carp
                q[Net::BitTorrent->_remove_connection() requires a blessed object];
            return;
        }

        #
        my $socket = $connection->_socket;
        return if not defined $socket;
        return delete $_connections{refaddr $self}{fileno $socket};
    }

    sub _process_connections {
        my ($self, $rin, $win, $ein) = @_;
        carp unless defined $rin;
        carp unless ref $rin;
        carp if ref $rin ne q[SCALAR];
        carp unless defined $win;
        carp unless ref $win;
        carp if ref $win ne q[SCALAR];
        carp unless defined $ein;
        carp unless ref $ein;
        carp if ref $ein ne q[SCALAR];
    POPSOCK: foreach my $fileno (keys %{$_connections{refaddr $self}}) {
            next POPSOCK unless defined $_connections{refaddr $self}{$fileno};
            if ($fileno eq fileno $_socket{refaddr $self})
            {    # if socket is our N::B obj
                if (vec($$rin, $fileno, 1) == 1) {
                    vec($$rin, $fileno, 1) = 0;
                    accept(my ($new_socket), $_socket{refaddr $self})
                        or

               #$self->_event(q[log], {Level => ERROR,
               #                   Msg => q[Failed to accept new connection]})
               #and
                        next POPSOCK;

           #if (scalar(
           #        grep {
           #            defined $_
           #                and $_->{q[Object]}->isa(q[Net::BitTorrent::Peer])
           #            } values %{$_connections{refaddr $self}}
           #    ) >= $_peers_per_torrent{refaddr $self}
           #    )
           #{   close $new_socket;
           #}
           #else {
                    my $new_peer =
                        Net::BitTorrent::Peer->new({Socket => $new_socket,
                                                    Client => $self
                                                   }
                        );

                    #}
                }
            }

       #elsif ($_connections{refaddr $self}{$fileno} eq $dht{refaddr $self}) {
       #    if (vec($$rin, $fileno, 1)) {
       #        vec($$rin, $fileno, 1) = 0;
       #        #die q[Yay!];
       #    }
       #}
            else {
                my $read  = vec($$rin, $fileno, 1);
                my $write = vec($$win, $fileno, 1);
                my $error = vec($$ein, $fileno, 1)
                    && (   $^E
                        && ($^E != _EINPROGRESS)
                        && ($^E != _EWOULDBLOCK));
                vec($$rin, $fileno, 1) = 0;
                vec($$win, $fileno, 1) = 0;
                vec($$ein, $fileno, 1) = 0;
                if ($read or $write) {

                    #use Data::Dump qw[pp];
                    #warn sprintf q[R:%d | W:%d], $read, $write;
                    #warn pp $_connections{refaddr $self}{$fileno};
                    # Weaken the ref in case...
                    #weaken $_connections{refaddr $self}{$fileno}{q[Object]};
                    #
                    my ($this_down, $this_up)
                        = $_connections{refaddr $self}{$fileno}{q[Object]}
                        ->_rw((($_max_dl_rate{refaddr $self}
                                ? (($_max_dl_rate{refaddr $self} * 1024)
                                   - $_k_down{refaddr $self})
                                : 2**15
                               ) * $read
                              ),
                              (($_max_ul_rate{refaddr $self}
                                ? (($_max_ul_rate{refaddr $self} * 1024)
                                   - $_k_up{refaddr $self})
                                : 2**15
                               ) * $write
                              ),
                              $error
                        );
                    $_k_down{refaddr $self}
                        += defined $this_down ? $this_down : 0;
                    $_k_up{refaddr $self} += defined $this_up ? $this_up : 0;

                    # Make it a strong ref once again...
                    #$_connections{refaddr $self}{$fileno}{q[Object]} =
                    #    $_connections{refaddr $self}{$fileno}{q[Object]}
                }
            }
        }
        return 1;
    }

    # Methods | Private | Torrents
    sub _locate_torrent {
        my ($self, $infohash) = @_;

        #
        carp q[Bad infohash for Net::BitTorrent->_locate_torrent(INFOHASH)]
            if $infohash !~ m[^[\d|a-f]{40}$];

        #
        return
            defined $_torrents{refaddr $self}{$infohash}
            ? $_torrents{refaddr $self}{$infohash}
            : undef;
    }

    # Methods | Public | Torrents
    sub add_torrent {

        # Adds a torrent to the list of loaded .torrents.  Beyond that, this
        # is a simple passthru for Net::BitTorrent::Torrent::new().
        my ($self, $args) = @_;
        if (ref($args) ne q[HASH]) {
            carp
                q[Net::BitTorrent->add_torrent() requires params passed as a hash ref];
            return;
        }
        $args->{q[Client]} = $self;
        my $torrent = Net::BitTorrent::Torrent->new($args);
        return if not defined $torrent;
        return if $self->_locate_torrent($torrent->infohash);
        return $_torrents{refaddr $self}{$torrent->infohash} = $torrent;
    }

    sub remove_torrent {
        my ($self, $torrent) = @_;

        #
        if (   not blessed($torrent)
            or not $torrent->isa(q[Net::BitTorrent::Torrent]))
        {   carp
                q[Net::BitTorrent->remove_torrent(TORRENT) requires a blessed Net::BitTorrent::Torrent object];
            return;
        }

        # close peers
        for my $_peer (@{$torrent->_peers}) {
            $_peer->_disconnect(
                              q[Removing .torrent torrent from local client]);
        }

        # let the trackers know
        for my $_tracker (@{$torrent->trackers}) {
            $_tracker->_urls->[0]->_announce(q[stopped]);
        }

        #
        return delete $_torrents{refaddr $self}{$torrent->infohash};
    }

    # Methods | Public | Callback system
    sub on_event {
        my ($self, $type, $method) = @_;
        $_event{refaddr $self}{$type} = $method;
    }

    # Methods | Private | Callback system
    sub _event {
        my ($self, $type, $args) = @_;
        if (defined $_event{refaddr $self}{$type}) {
            return $_event{refaddr $self}{$type}($self, $args);
        }

        # Debugging
        #my @caller = caller();
        #use 5.010;
        #use Data::Dump qw[pp];
        #say sprintf qq[Unhandled %s | %s at %s line %d], $type, pp($args),
        #    $caller[1], $caller[2];
        #
        return;
    }

    sub _schedule {
        my ($self, $args) = @_;
        if (not defined $args) {
            carp q[Net::BitTorrent->_schedule() requires parameters];
            return;
        }
        if (ref $args ne q[HASH]) {
            carp
                q[Net::BitTorrent->_schedule() requires params to be passed as a HashRef];
            return;
        }
        if (not defined $args->{q[Object]}) {
            carp
                q[Net::BitTorrent->_schedule() requires an 'Object' parameter];
            return;
        }
        if (not blessed $args->{q[Object]}) {
            carp
                q[Net::BitTorrent->_schedule() requires a blessed 'Object' parameter];
            return;
        }
        if (not defined $args->{q[Time]}) {
            carp q[Net::BitTorrent->_schedule() requires a 'Time' parameter];
            return;
        }
        if ($args->{q[Time]} !~ m[^\d+$]) {
            carp
                q[Net::BitTorrent->_schedule() requires 'Time' to be an integer];
            return;
        }
        if (not defined $args->{q[Code]}) {
            carp q[Net::BitTorrent->_schedule() requires a 'Code' parameter];
            return;
        }
        if (ref $args->{q[Code]} ne q[CODE]) {
            carp
                q[Net::BitTorrent->_schedule() requires a 'Code' parameter of type 'CodeRef];
            return;
        }

        #
        my $tid = $self->_generate_token_id();
        $_schedule{refaddr $self}{$tid} = {Timestamp => $args->{q[Time]},
                                           Code      => $args->{q[Code]},
                                           Object    => $args->{q[Object]}
        };
        weaken $_schedule{refaddr $self}{$tid}{q[Object]};
        return $tid;
    }

    sub _cancel {
        my ($self, $tid) = @_;
        if (not defined $tid) {
            carp q[Net::BitTorrent->_cancel( TID ) requires an ID];
            return;
        }
        elsif (not defined $_schedule{refaddr $self}{$tid}) {
            carp sprintf
                q[Net::BitTorrent->_cancel( TID ) cannot find an event with TID == %s],
                $tid;
            return;
        }

        #
        return delete $_schedule{refaddr $self}{$tid};
    }

    sub _process_schedule {
        my ($self) = @_;

        #
        for my $job (keys %{$_schedule{refaddr $self}}) {
            if ($_schedule{refaddr $self}{$job}->{q[Timestamp]} <= time) {
                &{$_schedule{refaddr $self}{$job}->{q[Code]}}(
                                 $_schedule{refaddr $self}{$job}->{q[Object]})
                    if defined $_schedule{refaddr $self}{$job}->{q[Object]};
                delete $_schedule{refaddr $self}{$job};
            }
        }

        #
        return 1;
    }

    # Methods | Private | Various
    sub _generate_token_id {    # automatic rollover/expansion/etc
        return if defined $_[1];
        my ($self) = @_;
        $_tid{refaddr $self} = qq[\0\0\0\0]
            if not defined $_tid{refaddr $self};
        my ($len) = ($_tid{refaddr $self} =~ m[^([a-z]+)]);
        $_tid{refaddr $self} = (
                   ($_tid{refaddr $self} =~ m[^z*(\0*)$])
                   ? ($_tid{refaddr $self} =~ m[\0]
                      ? pack(q[a] . (length $_tid{refaddr $self}),
                             (q[a] x (length($len || q[]) + 1))
                          )
                      : (q[a] . (qq[\0] x (length($_tid{refaddr $self}) - 1)))
                       )
                   : ++$_tid{refaddr $self}
        );
        return $_tid{refaddr $self};
    }

    # Utility, object-agnostic functions
    sub __socket_open {
        my ($host, $port, $reuseaddr, $reuseport) = @_;

        # param validation is [...].
        if (not defined $host) {
            carp q[Net::BitTorrent::__socket_oen( ) ]
                . q[requires a hostname];
            return;
        }
        if (not defined $port) {
            carp q[Net::BitTorrent::__socket_open( ) ]
                . q[requires a port number];
            return;
        }
        if ($port !~ m[^\d+$]) {
            carp q[Net::BitTorrent::__socket_open( ) ]
                . q[requires an integer port number];
            return;
        }
        if (defined $reuseaddr) {    # XXX - Undocumented
            if ($reuseaddr !~ m[^[10]$]) {
                carp q[Net::BitTorrent::__socket_open( ) ]
                    . q[requires a bool ReuseAddr value];
                return;
            }
        }
        if (defined $reuseport) {    # XXX - Undocumented
            if ($reuseport !~ m[^[10]$]) {
                carp q[Net::BitTorrent::__socket_open( ) ]
                    . q[requires a bool ReusePort value];
                return;
            }
        }

        # perldoc perlipc
        socket(my ($_socket), PF_INET, SOCK_STREAM, getprotobyname(q[tcp]))
            or return;

        # - What is the difference between SO_REUSEADDR and SO_REUSEPORT?
        #    [http://www.unixguide.net/network/socketfaq/4.11.shtml]
        # - setsockopt - what are the options for ActivePerl under Windows NT?
        #    [http://perlmonks.org/?node_id=63280]
        if ($reuseaddr) {    # XXX - undocumented
            setsockopt($_socket, SOL_SOCKET, SO_REUSEADDR, pack(q[l], 1))
                or return;
        }

       # SO_REUSEPORT is undefined on Win32... Boo...
       #if ($reuse_port and defined SO_REUSEPORT) {       # XXX - undocumented
       #   setsockopt($_socket, SOL_SOCKET, SO_REUSEPORT, pack(q[l], 1))
       #       or return;
       #}
        my $packed_host = undef;
        if ($host            # XXX - Undocumented
            !~ m[^(?:(?:(?:25[0-5]|2[0-4][0-9]|[0-1]?[0-9]{1,2})[.]?){4})$])
        {                    # it's not an IPv4 so it may be a hostname
                             # No point resolving an IP address.  Right?
            my ($name, $aliases, $addrtype, $length, @addrs)
                = gethostbyname($host)
                or return;
            $packed_host = $addrs[0];
        }
        else { $packed_host = inet_aton($host) }
        bind($_socket, pack_sockaddr_in($port, $packed_host)) or return;
        listen($_socket, SOMAXCONN) or return;    # max of five?
        return $_socket;
    }

    sub __build_reserved {
        my ($self) = @_;
        my @reserved = qw[0 0 0 0 0 0 0 0];
        $reserved[5] |= 0x10;                     # Ext Protocol
        return join q[], map {chr} @reserved;
    }

    sub _as_string {
        my ($self, $advanced) = @_;
        my $dump = q[TODO];
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

            #  do some silly stuff to avoid mistakes
            delete $_schedule{$_nID};

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

Net::BitTorrent - BitTorrent peer-to-peer protocol class

=head1 Synopsis

  use Net::BitTorrent;

  my $client = Net::BitTorrent->new();

  # ...
  # Set various callbacks if you so desire
  # ...
  $client->on_event(
      q[piece_hash_pass],
      sub {
          my ($self, $args) = @_;
          printf(qq[pass: piece number %04d of %s\n],
                 $args->{q[Index]}, $args->{q[Torrent]}->infohash);
      }
  );
  $client->on_event(
      q[piece_hash_fail],
      sub {
          my ($self, $args) = @_;
          printf(qq[fail: piece number %04d of %s\n],
                 $args->{q[Index]}, $args->{q[Torrent]}->infohash);
      }
  );

  my $torrent = $client->add_torrent({Path => q[a.legal.torrent]})
      or die q[Cannot load .torrent];

  $torrent->hashcheck;    # Verify any existing data

  while (1) { $client->do_one_loop(); }

=head1 Description

L<Net::BitTorrent|Net::BitTorrent> is a class based implementation of the
BitTorrent Protocol for distributed data exchange.

=head1 Constructor

=over 4

=item C<new ( { [ARGS] } )>

Creates a L<Net::BitTorrent|Net::BitTorrent> object.  C<new ( )> accepts
arguments as a hash, using key-value pairs, all of which are optional.
The most common are:

=over 4

=item C<LocalHost>

Local host bind address.  The value must be an IPv4 ("dotted quad") IP-
address of the C<xxx.xxx.xxx.xxx> form.

Default: 0.0.0.0 (any address)

=item C<LocalPort>

TCP port opened to remote peers for incoming connections.  If handed a
list of ports, L<Net::BitTorrent|Net::BitTorrent> will traverse the list,
attempting to open on each of the ports until we succeed.  If this value
is C<undef> or C<0>, we allow the OS to choose an open port at random.

Though the default in most clients is a random port in the 6881..6889
range, BitTorrent has not been assigned a port number or range by the
IANA.  Nor is such a standard needed.

Default: 0 (any available)

=back

=back

=head1 Methods

Unless otherwise stated, all methods return either a C<true> or C<false>
value, with C<true> meaning that the operation was a success.  When a
method states that it returns some other specific value, failure will
result in C<undef> or an empty list.

=over 4

=item C<peerid ( )>

Returns the Peer ID generated to identify this
L<Net::BitTorrent|Net::BitTorrent> object internally, with trackers, and
with remote L<peers|Net::BitTorrent::Peer>.

See also: theory.org (http://tinyurl.com/4a9cuv),
L<Peer ID Specification|Net::BitTorrent::Notes/"Peer ID Specification">

=item C<torrents( )>

Returns the list of loaded .torrent L<Torrents|Net::BitTorrent::Torrent>.

See also: L<add_torrent ( )|/add_torrent ( { ... } )>,
L<remove_torrent ( )|/remove_torrent ( TORRENT )>

=item C<add_torrent ( { ... } )>

Loads a .torrent file and adds the new
L<Net::BitTorrent::Torrent|Net::BitTorrent::Torrent> object to the
client.

Most arguments passed to this method are handed directly to
L<Net::BitTorrent::Torrent::new( )|Net::BitTorrent::Torrent/"new ( { [ARGS] } )">.
The only mandatory parameter is C<Path>.  C<Path>'s value is the filename
of the .torrent file to load.  Please see
L<Net::BitTorrent::Torrent::new( )|Net::BitTorrent::Torrent/"new ( { [ARGS] } )">
for a list of possible parameters.

This method returns the new
L<Net::BitTorrent::Torrent|Net::BitTorrent::Torrent> object on success.

See also: L<torrents|/torrents( )>,
L<remove_torrent|/remove_torrent ( TORRENT )>,
L<Net::BitTorrent::Torrent|Net::BitTorrent::Torrent>

=item C<remove_torrent ( TORRENT )>

Removes a L<Net::BitTorrent::Torrent|Net::BitTorrent::Torrent> object
from the client.

=begin future

Before the torrent torrent is closed, we announce to the tracker that we
have 'stopped' downloading and the callback to store the current state is
called.

=end future

See also: L<torrents ( )|/torrents( )>,
L<add_torrent|/"add_torrent ( { ... } )">,
L<Net::BitTorrent::Torrent|Net::BitTorrent::Torrent>

=item C<do_one_loop ( [TIMEOUT] )>

Processes the various socket-containing objects (peers, trackers) held by
this L<Net::BitTorrent|Net::BitTorrent> object.  This method should be called frequently.

The optional TIMEOUT parameter is the maximum amount of time, in seconds,
possibly fractional, C<select()> is allowed to wait before returning in
L<do_one_loop( )|/"do_one_loop ( [TIMEOUT] )">.  This TIMEOUT defaults to
C<1.0>.  To wait indefinatly, TIMEOUT should be C<-1.0>.

=item C<on_event ( TYPE, CODEREF )>

Net::BitTorrent provides a convenient callback system.  To set a callback,
use the C<on_event( )> method.  For example, to catch all attempts to read
from a file, use C<$client->on_event( 'file_read', \&on_read )>.

See the L<Events|/Events> section for a list of events sorted by their
related classes.

=back

=head1 Events

C<Net::BitTorrent> and related classes trigger a number of events










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
