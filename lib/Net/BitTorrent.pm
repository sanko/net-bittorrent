package Net::BitTorrent;
{
    use strict;      # core as of perl 5
    use warnings;    # core as of perl 5.006

    #
    use Scalar::Util    # core as of perl 5.007.003
        qw[blessed weaken];
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
    our $UNSTABLE_RELEASE = 6; our $VERSION = sprintf(($UNSTABLE_RELEASE ? q[%.3f_%03d] : q[%.3f]), (version->new(qw$Rev$)->numify / 1000), $UNSTABLE_RELEASE);

    #
    use lib q[../../lib];
    use Net::BitTorrent::Session;
    use Net::BitTorrent::Peer;
    use Net::BitTorrent::DHT;
    use Net::BitTorrent::Util qw[:log];

    # Debugging
    #use Data::Dump qw[pp];
    #
    my (%_socket, %_peerid, %_schedule, %_peers_per_session, %_event);
    my (%_dht, %_sessions, %_connections);
    my (%_max_ul_rate, %_k_up, %_max_dl_rate, %_k_down);
    my (%_tid);

    # Net::BitTorrent
    #~    dht       =>  Net::BitTorrent::DHT
    #~                      Net::BitTorrent::DHT::Azureus
    #~                      Net::BitTorrent::DHT::Mainline
    #~    peers     =>  [Net::BitTorrent::Peer]
    #~    session   =>  [Net::BitTorrent::Session]
    #~                  [Net::BitTorrent::Session::Tracker]
    #~ 		                [Net::BitTorrent::Session::Tracker::UDP]
    #~                      [Net::BitTorrent::Session::Tracker::HTTP]
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
        #   - TODO: create DHT object
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
            $_socket{$self} = $_socket;
            $_peerid{$self} = pack(
                q[a20],
                (sprintf(
                     q[NB%03d%1s-%8s%5s],
                     (q[$Rev$] =~ m[(\d+)]g),
                     ($UNSTABLE_RELEASE ? q[S] : q[C]),
                     (join q[],
                      map {
                          [q[A] .. q[Z], q[a] .. q[z], 0 .. 9, qw[- . _ ~]]
                          ->[rand(66)]
                          } 1 .. 8
                     ),
                     q[Sanko],
                 )
                )
            );
            $_tid{$self} = qq[\0] x 5;    # 26^5 before rollover
            if (not($self->_add_connection($self, q[ro]))) {
                carp q[Could not add server socket to list of connections];
                return;
            }

            #
            $_dht{$self} =
                Net::BitTorrent::DHT->new(
                                       {Client    => $self,
                                        LocalAddr => inet_ntoa($packed_ip),
                                        LocalPort => $port,
                                        ReuseAddr => $args->{q[ReuseAddr]},
                                        ReusePort => $args->{q[ReusePort]}
                                       }
                ) or return;


        }
        if (not $self) {    # failed to open socket?
            return;
        }

        # Settings
        $_peers_per_session{$self} = 50;
        $_max_dl_rate{$self}       = 0;
        $_max_ul_rate{$self}       = 0;

        #
        #$_dht{$self}=Net::BitTorrent::DHT->new();
        $_sessions{$self} = {};    # by infohash

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
             Code => sub {return$_k_down{$_[0]}=$_k_up{$_[0]}=0;},
             Object => $self
            }
        );

        #
        return $self;
    }

    # Accessors | Private
    sub _socket      { return $_socket{+shift} }
    sub _connections { return $_connections{+shift} }
    sub _dht         { return $_dht{+shift} }

    sub _port {
        my ($port, undef) = unpack_sockaddr_in(getsockname($_socket{+shift}));
        return $port;
    }
    sub _peers_per_session { return $_peers_per_session{+shift}; }

    # Accessors | Public
    sub peerid   { return $_peerid{+shift} }
    sub sessions { return $_sessions{+shift} }

    # Methods | Private
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
       #            and ($_ ne $_dht{$self})
       #            #and (not $_->get_is_connected)
       #            #and ($_->get_connection_timestamp < (time - 60))
       #} values %{$_connections{$self}};
       # [id://371720]
        my ($rin, $win, $ein) = (q[], q[], q[]);
    PUSHSOCK: for my $fileno (keys %{$_connections{$self}}) {
            vec($rin, $fileno, 1) = 1
                if $_connections{$self}{$fileno}{q[Mode]} =~ m[r];
            vec($win, $fileno, 1) = 1
                if $_connections{$self}{$fileno}{q[Mode]} =~ m[w];
            vec($ein, $fileno, 1) = 1;

            #warn sprintf q[[%d|%s]], $fileno,
            #    $_connections{$self}{$fileno}{q[Mode]};
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

    # Connections. Trackers, Peers, ...even the client itself
    sub _add_connection {                                  # untested
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
        if (not(   $connection->isa(q[Net::BitTorrent])
                or $connection->isa(q[Net::BitTorrent::Peer])
                or
                $connection->isa(q[Net::BitTorrent::Session::Tracker::HTTP])
                or $connection->isa(q[Net::BitTorrent::Session::Tracker::UDP])
                or $connection->isa(q[Net::BitTorrent::DHT])
                or $connection->isa(q[Net::BitTorrent::DHT::Node::Mainline])
                or $connection->isa(q[Net::BitTorrent::DHT::Node::Azureus]))
            )
        {   carp
                q[Net::BitTorrent->_add_connection() requires a Net::BitTorrent-related object];
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
        if (defined $_connections{$self}{fileno $_socket}) {
            carp q[This connection object is already loaded.];
            return;
        }
        $_connections{$self}{fileno $_socket} = {
                                                        Object => $connection,
                                                        Mode   => $mode,
                 } or return ;

        if($connection->isa(q[Net::BitTorrent])) {
            weaken $_connections{$self}{fileno $_socket}{q[Object]}
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
        if (not($connection->isa(q[Net::BitTorrent]
                )    # XXX - ...who would remove the client itself?
                or $connection->isa(q[Net::BitTorrent::Peer])
                or
                $connection->isa(q[Net::BitTorrent::Session::Tracker::HTTP])
                or $connection->isa(q[Net::BitTorrent::Session::Tracker::UDP])
                or $connection->isa(q[Net::BitTorrent::DHT])
                or $connection->isa(q[Net::BitTorrent::DHT::Node::Mainline])
                or $connection->isa(q[Net::BitTorrent::DHT::Node::Azureus])
            )
            )
        {   carp
                q[Net::BitTorrent->_remove_connection() requires a Net::BitTorrent-related object];
            return;
        }
        my $socket = $connection->_socket;
        return    # a disconnected peer?  Bug for sure.
            unless ref($socket) eq q[GLOB];    # untested
        return delete $_connections{$self}{fileno $socket};
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
    POPSOCK: foreach my $fileno (keys %{$_connections{$self}}) {
            next POPSOCK unless defined $_connections{$self}{$fileno};
            if ($fileno eq fileno $_socket{$self})
            {    # if socket is our N::B obj
                if (vec($$rin, $fileno, 1) == 1) {
                    vec($$rin, $fileno, 1) = 0;
                    accept(my ($new_socket), $_socket{$self})
                        or

                       #$self->_do_callback(q[log], ERROR,
                       #                   q[Failed to accept new connection])
                       #and
                        next POPSOCK;

           #if (scalar(
           #        grep {
           #            defined $_
           #                and $_->{q[Object]}->isa(q[Net::BitTorrent::Peer])
           #            } values %{$_connections{$self}}
           #    ) >= $_peers_per_session{$self}
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

            #elsif ($_connections{$self}{$fileno} eq $dht{$self}) {
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
                    #warn pp $_connections{$self}{$fileno};
                    # Weaken the ref in case...
                    #weaken $_connections{$self}{$fileno}{q[Object]};
                    #
                    my ($this_down, $this_up)
                        = $_connections{$self}{$fileno}{q[Object]}->_rw(
                           ((   $_max_dl_rate{$self}
                                ? (($_max_dl_rate{$self} * 1024)
                                   - $_k_down{$self})
                                : 2**15
                            ) * $read
                           ),
                           (($_max_ul_rate{$self}
                             ? (($_max_ul_rate{$self} * 1024) - $_k_up{$self})
                             : 2**15
                            ) * $write
                           ),
                           $error
                        );


                    $_k_down{$self} += defined $this_down?$this_down:0;
                    $_k_up{$self}   += defined $this_up?$this_up:0;

                    # Make it a strong ref once again...
                    #$_connections{$self}{$fileno}{q[Object]} =
                    #    $_connections{$self}{$fileno}{q[Object]}
                }
            }
        }
        return 1;
    }

    # Methods | Private | Sessions
    sub _locate_session {
        my ($self, $infohash) = @_;

        #
        carp q[Bad infohash for Net::BitTorrent->_locate_session(INFOHASH)]
            if $infohash !~ m[[\d|a-f]{40}]i;

        #
        return
            defined $_sessions{$self}{$infohash}
            ? $_sessions{$self}{$infohash}
            : undef;
    }

    # Methods | Public | Sessions
    sub add_session {

        # Adds a session to the list of loaded .torrents.  Beyond that, this
        # is a simple passthru for Net::BitTorrent::Session::new().
        my ($self, $args) = @_;
        if (ref($args) ne q[HASH]) {
            carp
                q[Net::BitTorrent->add_session() requires params passed as a hash ref];
            return;
        }
        $args->{q[Client]} = $self;
        my $session = Net::BitTorrent::Session->new($args);
        return if defined $_sessions{$self}{$$session};    # XXX - Untested
        return $_sessions{$self}{$$session} = $session;
    }

    sub remove_session {
        my ($self, $session) = @_;

        #
        if (   not blessed($session)
            or not $session->isa(q[Net::BitTorrent::Session]))
        {   carp
                q[Net::BitTorrent->remove_session(SESSION) requires a blessed Net::BitTorrent::Session object];
            return;
        }

        #
        return delete $_sessions{$self}{$session->infohash};
    }

    # callback system
    sub on_event {
        my ($self, $type, $method) = @_;
        $_event{$self}{$type} = $method;
    }

    sub _event {
        my ($self, $type, $args) = @_;
        if (defined $_event{$self}{$type}) {
            return $_event{$self}{$type}($self, $args);
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
            carp q[Ouch];
            return;
        }
        if (ref $args ne q[HASH]) {
            carp q[Ouch];
            return;
        }
        if (not defined $args->{q[Object]}) {
            carp q[Need obj];
            return;
        }
        if (not blessed $args->{q[Object]}) {
            carp q[Need blessed obj];
            return;
        }
        if (not defined $args->{q[Time]}) {
            carp q[Ouch!];
            return;
        }
        if ($args->{q[Time]} !~ m[^\d+$]) {
            carp q[Ouch!!!];
            return;
        }
        if (not defined $args->{q[Code]}) {
            carp q[Ouch!??];
            return;
        }
        if (ref $args->{q[Code]} ne q[CODE]) {
            carp q[Ouch!!!??];
            return;
        }

        #
        my $tid = $self->_generate_token_id();
        $_schedule{$self}{$tid} = {Timestamp => $args->{q[Time]},
                                   Code      => $args->{q[Code]},
                                   Object    => $args->{q[Object]}
        };
        weaken $_schedule{$self}{$tid}{q[Object]};
        return $tid;
    }

    sub _cancel {
        my ($self, $tid) = @_;
    if (not defined $tid){
        carp q[Net::BitTorrent->_cancel( TID ) requires an ID];
        return;
    }
    elsif (not defined $_schedule{$self}{$tid}){
        carp sprintf q[Net::BitTorrent->_cancel( TID ) cannot find an event with TID == %s], $tid;
        return;
    }
        #
        return delete $_schedule{$self}{$tid};
    }

    sub _process_schedule {
        my ($self) = @_;

        #
        for my $job (keys %{$_schedule{$self}}) {
            if ($_schedule{$self}{$job}->{q[Timestamp]} <= time) {
                &{$_schedule{$self}{$job}->{q[Code]}}(
                                        $_schedule{$self}{$job}->{q[Object]});
                delete $_schedule{$self}{$job};
            }
        }

        #
        return 1;
    }

    sub _generate_token_id {    # automatic rollover/expansion/etc
        return if defined $_[1];
        my ($self) = @_;
        $_tid{$self} = qq[\0\0\0\0] if not defined $_tid{$self};
        my ($len) = ($_tid{$self} =~ m[^([a-z]+)]);
        $_tid{$self} = (($_tid{$self} =~ m[^z*(\0*)$])
                        ? ($_tid{$self} =~ m[\0]
                           ? pack(q[a] . (length $_tid{$self}),
                                  (q[a] x (length($len || q[]) + 1))
                               )
                           : (q[a] . (qq[\0] x (length($_tid{$self}) - 1)))
                            )
                        : ++$_tid{$self}
        );
        return $_tid{$self};
    }

    # Utility, object-neutral functions
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
        my @reserved = qw[0 0 0 0 0 0 0 0];
        $reserved[5] |= 0x10;                     # Ext Protocol
        return join q[], map {chr} @reserved;
    }

    #
    DESTROY {
        my ($self) = @_;
        #warn sprintf q[Goodbye, %s], $$self;
        delete $_socket{$self};
        delete $_peerid{$self};
        delete $_sessions{$self};
        delete $_connections{$self};
        delete $_schedule{$self};
        delete $_tid{$self};
        delete $_peers_per_session{$self};
        delete $_event{$self};
    }

=pod

=head1 NAME

Net::BitTorrent - BitTorrent peer-to-peer protocol class

=head1 Synopsis

 use Net::BitTorrent;

 my $client = Net::BitTorrent->new();

 # ...
 # set various callbacks if you so desire
 # ...
 $client->set_callback(
   q[piece_hash_pass],
   sub {
     my ($self, $piece) = @_;
     printf(qq[hash_pass: piece number %04d of %s\n],
       $piece->get_index, $piece->get_session);
   }
 );

 my $torrent = $client->add_session({path => q[a.legal.torrent]})
   or die q[Cannot load .torrent];

 while (1) {
   $client->do_one_loop();
   # Etc.
 }

=head1 Description

L<Net::BitTorrent|Net::BitTorrent> is a class based implementation of the
current BitTorrent Protocol Specification.  Each
L<Net::BitTorrent|Net::BitTorrent> object is capable of handling several
concurrent .torrent L<sessions|Net::BitTorrent::Session>.

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

=head1 Accessors

=over 4

=item C<peerid ( )>

Returns the Peer ID generated to identify this
L<Net::BitTorrent|Net::BitTorrent> object internally, with trackers, and
with remote L<peers|Net::BitTorrent::Peer>.

See also: theory.org (http://tinyurl.com/4a9cuv),
L<Peer ID Specification|Net::BitTorrent::Notes/"Peer ID Specification">

=item C<sessions( )>

Returns the list of loaded .torrent L<sessions|Net::BitTorrent::Session>.

See also: L<add_session ( )|/add_session ( )>,
L<remove_session ( )|/remove_session ( )>

=back

=head1 Methods

Unless otherwise stated, all methods return either a C<true> or C<false>
value, with C<true> meaning that the operation was a success.  When a
method states that it returns a value, failure will result in C<undef> or
an empty list.

Besides these listed here, there is also the
L<on_event ( )|/"on_event ( TYPE, CODEREF )"> method described in
the L<Events|/Events> section.

=over 4

=item C<add_session ( ARGS )>

=item C<do_one_loop ( [TIMEOUT] )>

=item C<remove_session ( SESSION )>

=item C<on_event ( TYPE, CODEREF )>

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

    1;
}
