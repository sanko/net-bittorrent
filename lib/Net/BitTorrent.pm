package Net::BitTorrent;
{
    use 5.010;
    use Moose;
    our $MAJOR = 0; our $MINOR = 74; our $DEV = 14; our $VERSION = sprintf('%0d.%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    #
    use lib '../';
    use Net::BitTorrent::Protocol::BEP03::Types;
    use Net::BitTorrent::Torrent;

    #
    sub BUILD {1}

    #
    has peer_id => (isa => 'Net::BitTorrent::Protocol::BEP03::Types::PeerID',
                    is  => 'ro',
                    lazy_build => 1,
                    init_arg   => undef
    );

    sub _build_peer_id {
        sprintf 'NB%03d%1s-%7s-%-5s',
            ($MAJOR * 100) + ($MINOR), $DEV > 0 ? 'U' : 'S', join(
            '',
            map {
                ['A' .. 'Z', 'a' .. 'z', 0 .. 9, qw[- . _ ~]]->[rand(66)]
                } 1 .. 7
            ),
            [qw[April KaiLi]]->[rand 2];
    }
    has torrents => (traits  => ['Array'],
                     isa     => 'ArrayRef[Net::BitTorrent::Torrent]',
                     is      => 'ro',
                     lazy    => 1,
                     default => sub { [] },
                     handles => {
                                add_torrent     => 'push',
                                clear_torrents  => 'clear',
                                count_torrents  => 'count',
                                filter_torrents => 'grep',
                                find_torrent    => 'first',
                                has_torrents    => 'count',
                                info_hashes => ['map', sub { $_->info_hash }],
                                map_torrents           => 'map',
                                no_torrents            => 'is_empty',
                                shuffle_torrents       => 'shuffle',
                                sort_torrents          => 'sort',
                                sort_torrents_in_place => 'sort_in_place',
                                torrent                => 'get',
                     }
    );
    around add_torrent => sub {
        my ($code, $self) = (shift, shift);
        my $torrent;
        if (blessed $_[0]) { $torrent = $_[0]; }
        else {
            require Net::BitTorrent::Torrent;
            $torrent = Net::BitTorrent::Torrent->new(@_) or return;
        }
        return
               blessed $torrent
            && $code->($self, $torrent)
            && $torrent->_set_client($self) // 1
            && require Net::BitTorrent::Torrent::Queued
            && Net::BitTorrent::Torrent::Queued->meta->apply($torrent,
                                             client => $self) ? $torrent : ();
    };
    my $info_hash_constraint;
    around torrent => sub {
        my ($code, $self, $index) = @_;
        my $torrent;
        {
            $info_hash_constraint //=
                Moose::Util::TypeConstraints::find_type_constraint(
                                 'Net::BitTorrent::Types::Torrent::Infohash');
            my $info_hash = $info_hash_constraint->coerce($index);
            $torrent = $self->find_torrent(
                sub {

                    #$_->_has_info_hash &&
                    $_->info_hash->Lexicompare($info_hash) == 0;
                }
            );
        }
        $torrent = $code->($self, $index)
            if !defined $torrent && $index =~ m[^\d$];
        return $torrent;
    };

    # From previous version
    # Sockets
    has 'port' => (
        is      => 'ro',
        isa     => 'Int|ArrayRef[Int]',
        default => 0,
        writer  => '_set_port',

        #trigger => sub {...}
    );
    {
        for my $prot (qw[tcp udp]) {
            for my $ipv (4, 6) {
                has $prot
                    . $ipv => (is         => 'ro',
                               init_arg   => undef,
                               isa        => 'Maybe[Object]',
                               lazy_build => 1,
                               writer     => '_set_' . $prot . $ipv
                    );
                has $prot 
                    . $ipv
                    . '_sock' => (is         => 'ro',
                                  init_arg   => undef,
                                  isa        => 'GlobRef',
                                  lazy_build => 1,
                                  weak_ref   => 1,
                                  writer => '_set_' . $prot . $ipv . '_sock'
                    );
                has $prot 
                    . $ipv
                    . '_host' => (is         => 'ro',
                                  isa        => 'Str',
                                  lazy_build => 1,
                                  builder    => '_build_' . $ipv . '_host',
                                  writer => '_set_' . $prot . $ipv . '_host'
                    );
            }
        }
        sub _build_4_host {'0.0.0.0'}
        sub _build_6_host {'::'}
    }
    after 'BUILDALL' => sub { $_[0]->$_() for qw[tcp6 tcp4 udp4 udp6] };

    sub _build_tcp6 {
        my $s = shift;
        my ($server, $actual_socket, $actual_host, $actual_port);
        for my $port (ref $s->port ? @{$s->port} : $s->port) {
            require Net::BitTorrent::Network::Utility;
            $server = Net::BitTorrent::Network::Utility::server(
                $s->tcp6_host,
                $port,
                sub { $s->_on_tcp6_in(@_); },
                sub {
                    ($actual_socket, $actual_host, $actual_port) = @_;

                    #if ($self->port != $port) { ...; }
                    $s->_set_tcp6_sock($actual_socket);
                    $s->_set_tcp6_host($actual_host);
                    $s->_set_port($actual_port);
                    8;
                },
                'tcp'
            );
            last if defined $server;
        }
        if ($server) {
            $s->trigger_listen_success(
                     {port     => $actual_port,
                      protocol => 'tcp6',
                      severity => 'debug',
                      event    => 'listen_success',
                      message  => sprintf
                          'Opened TCP port %d to the outside world over IPv6',
                      $actual_port
                     }
            );
        }
        else {
            $s->trigger_listen_failure(
                 {port     => $s->port,
                  protocol => 'tcp6',
                  severity => 'fatal',
                  event    => 'listen_failure',
                  message =>
                      'Failed to open TCP port to the outside world over IPv6'
                 }
            );
        }
        return $server;
    }

    sub _build_tcp4 {
        my $s = shift;
        my ($server, $actual_socket, $actual_host, $actual_port);
        for my $port (ref $s->port ? @{$s->port} : $s->port) {
            require Net::BitTorrent::Network::Utility;
            $server = Net::BitTorrent::Network::Utility::server(
                $s->tcp4_host,
                $port,
                sub { $s->_on_tcp4_in(@_); },
                sub {
                    ($actual_socket, $actual_host, $actual_port) = @_;

                    #if ($self->port != $port) { ...; }
                    $s->_set_tcp4_sock($actual_socket);
                    $s->_set_tcp4_host($actual_host);
                    $s->_set_port($actual_port);
                    8;
                },
                'tcp'
            );
            last if defined $server;
        }
        if ($server) {
            $s->trigger_listen_success(
                     {port     => $actual_port,
                      protocol => 'tcp4',
                      severity => 'debug',
                      event    => 'listen_success',
                      message  => sprintf
                          'Opened TCP port %d to the outside world over IPv6',
                      $actual_port
                     }
            );
        }
        else {
            $s->trigger_listen_failure(
                 {port     => $s->port,
                  protocol => 'tcp4',
                  severity => 'fatal',
                  event    => 'listen_failure',
                  message =>
                      'Failed to open TCP port to the outside world over IPv4'
                 }
            );
        }
        return $server;
    }

    sub _build_udp6 {
        my $s = shift;
        my ($server, $actual_socket, $actual_host, $actual_port);
        for my $port (ref $s->port ? @{$s->port} : $s->port) {
            require Net::BitTorrent::Network::Utility;
            $server = Net::BitTorrent::Network::Utility::server(
                $s->udp6_host,
                $port,
                sub { $s->_on_udp6_in(@_); },
                sub {
                    ($actual_socket, $actual_host, $actual_port) = @_;

                    #if ($self->port != $port) { ...; }
                    $s->_set_udp6_sock($actual_socket);
                    $s->_set_udp6_host($actual_host);
                    $s->_set_port($actual_port);
                },
                'udp'
            );
            last if defined $server;
        }
        if ($server) {
            $s->trigger_listen_success(
                      {port     => $actual_port,
                       protocol => 'udp6',
                       severity => 'debug',
                       event    => 'listen_success',
                       message  => sprintf
                           'Bound TCP port %d to the outside world over IPv6',
                       $actual_port
                      }
            );
        }
        else {
            $s->trigger_listen_failure(
                {port     => $s->port,
                 protocol => 'udp6',
                 severity => 'fatal',
                 event    => 'listen_failure',
                 message =>
                     'Failed to bind UDP port for the outside world over IPv6'
                }
            );
        }
        return $server;
    }

    sub _build_udp4 {
        my $s = shift;
        my ($server, $actual_socket, $actual_host, $actual_port);
        for my $port (ref $s->port ? @{$s->port} : $s->port) {
            require Net::BitTorrent::Network::Utility;
            $server = Net::BitTorrent::Network::Utility::server(
                $s->udp4_host,
                $port,
                sub { $s->_on_udp4_in(@_); },
                sub {
                    ($actual_socket, $actual_host, $actual_port) = @_;

                    #if ($self->port != $port) { ...; }
                    $s->_set_udp4_sock($actual_socket);
                    $s->_set_udp4_host($actual_host);
                    $s->_set_port($actual_port);
                },
                'udp'
            );
            last if defined $server;
        }
        if ($server) {
            $s->trigger_listen_success(
                      {port     => $actual_port,
                       protocol => 'udp4',
                       severity => 'debug',
                       event    => 'listen_success',
                       message  => sprintf
                           'Bound UDP port %d to the outside world over IPv4',
                       $actual_port
                      }
            );
        }
        else {
            $s->trigger_listen_failure(
                {port     => $s->port,
                 protocol => 'udp4',
                 severity => 'fatal',
                 event    => 'listen_failure',
                 message =>
                     'Failed to bind UDP port for the outside world over IPv4'
                }
            );
        }
        return $server;
    }

    sub _on_tcp4_in {
        my ($self, $peer, $paddr, $host, $port) = @_;
        my $rule = $self->ip_filter->is_banned($host);
        if (defined $rule) {
            $self->trigger_ip_filter(
                     {protocol => 'tcp4',
                      severity => 'debug',
                      event    => 'ip_filter',
                      address  => [$host, $port],
                      rule     => $rule,
                      message => 'Incoming connection was blocked by ipfilter'
                     }
            );
            shutdown $peer, 2;
            return close $peer;
        }
        require Net::BitTorrent::Protocol::BEP03::Peer::Incoming;
        require AnyEvent::Handle::Throttle;
        $peer = Net::BitTorrent::Protocol::BEP03::Peer::Incoming->new(
            client => $self,
            handle => AnyEvent::Handle::Throttle->new(
                fh       => $peer,
                on_error => sub {
                    return if !defined $peer;
                    my ($hdl, $fatal, $msg) = @_;

                    #$peer->handle->destroy;
                    $peer->disconnect('Socket error: ' . $msg) if $fatal;
                },
                on_eof => sub {
                    return if !defined $peer;
                    $peer->handle->push_shutdown
                        if defined $peer->handle->{'fh'};
                    $peer->handle->destroy;
                    $peer->disconnect(
                                    'Connection closed by remote connection');
                }
            )
        );
        $self->add_peer($peer);
        $self->trigger_peer_connect(
                   {severity => 'info',
                    event    => 'peer_connect',
                    peer     => $peer,
                    message => sprintf 'Incomming peer connection from %s:%s',
                    $peer->host, $peer->port
                   }
        );
    }

    sub _on_tcp6_in {
        my ($self, $peer, $paddr, $host, $port) = @_;
        my $rule = $self->ip_filter->is_banned($host);
        if (defined $rule) {
            $self->trigger_ip_filter(
                     {protocol => 'tcp4',
                      severity => 'debug',
                      event    => 'ip_filter',
                      address  => [$host, $port],
                      rule     => $rule,
                      message => 'Incoming connection was blocked by ipfilter'
                     }
            );
            shutdown $peer, 2;
            return close $peer;
        }
        require Net::BitTorrent::Protocol::BEP03::Peer::Incoming;
        require AnyEvent::Handle::Throttle;
        $peer = Net::BitTorrent::Protocol::BEP03::Peer::Incoming->new(
            client => $self,
            handle => AnyEvent::Handle::Throttle->new(
                fh       => $peer,
                on_error => sub {
                    return if !defined $peer;
                    my ($hdl, $fatal, $msg) = @_;

                    #$s->handle->destroy;
                    $peer->disconnect('Socket error: ' . $msg) if $fatal;
                },
                on_eof => sub {
                    return if !defined $peer;
                    $peer->handle->push_shutdown
                        if defined $peer->handle->{'fh'};
                    $peer->handle->destroy;
                    $peer->disconnect(
                                    'Connection closed by remote connection');
                }
            )
        );
        $self->add_peer($peer);
        $self->trigger_peer_connect(
                   {severity => 'info',
                    event    => 'peer_connect',
                    peer     => $peer,
                    message => sprintf 'Incomming peer connection from %s:%s',
                    $peer->host, $peer->port
                   }
        );
    }

    sub _on_udp4_in {
        my $s = shift;
        my ($udp, $sock, $paddr, $host, $port, $data, $flags) = @_;
        my $rule = $s->ip_filter->is_banned($host);
        if (defined $rule) {
            $s->trigger_ip_filter(
                           {protocol => 'udp4',
                            severity => 'debug',
                            event    => 'ip_filter',
                            address  => [$host, $port],
                            rule     => $rule,
                            message => 'Incoming data was blocked by ipfilter'
                           }
            );
            return;
        }
        $s->dht->_on_udp4_in(@_);
    }

    sub _on_udp6_in {
        my $s = shift;
        my ($udp, $sock, $paddr, $host, $port, $data, $flags) = @_;
        my $rule = $s->ip_filter->is_banned($host);
        if (defined $rule) {
            $s->trigger_ip_filter(
                           {protocol => 'upd6',
                            severity => 'debug',
                            event    => 'ip_filter',
                            address  => [$host, $port],
                            rule     => $rule,
                            message => 'Incoming data was blocked by ipfilter'
                           }
            );
            return;
        }
        $s->dht->_on_udp6_in(@_);
    }

    #
    __PACKAGE__->meta->make_immutable;
    no Moose;
}
1;
