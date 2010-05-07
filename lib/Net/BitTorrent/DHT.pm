package Net::BitTorrent::DHT;
{
    use Moose;
    use AnyEvent;
    use lib '../../../lib';
    use Net::BitTorrent::Protocol::BEP03::Bencode qw[bdecode];
    use Net::BitTorrent::Protocol::BEP05::Packets qw[:all];
    use Net::BitTorrent::Network::Utility qw[:paddr :sockaddr];
    use Net::BitTorrent::Types;
    use 5.10.0;
    our $MAJOR = 0.075; our $MINOR = 0; our $DEV = -1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    # Stub
    sub BUILD {1}

    #
    has 'client' => (isa       => 'Net::BitTorrent',
                     is        => 'ro',
                     predicate => 'has_client',
                     handles   => {udp => 'udp'},
    );
    after 'BUILD' => sub {
        my ($self, $args) = @_;
        return if $self->has_client;
        require Moose::Util;
        return
            Moose::Util::apply_all_roles($self,
                                         'Net::BitTorrent::DHT::Standalone');
    };

    #
    has 'nodeid' => (    # uses the same 160-bit space
                      isa        => 'NBTypes::Infohash::Packed',
                      is         => 'ro',
                      lazy_build => 1,
                      coerce     => 1
    );

    sub _build_nodeid {
        require Digest::SHA;
        return Digest::SHA::sha1_hex(
                                   rand(time * $^T) . $0 . 'Sanko was here.');
    }

    #
    has 'boot_nodes' => (isa        => 'ArrayRef[Str]',
                         is         => 'rw',
                         lazy_build => 1
    );

    sub _build_boot_nodes {    # yeah, yeah, yeah...
        return [
            qw[ router.bitcomet.com:7777 router.bittorrent.com:7777
                router.utorrent.com:7777]
        ];
    }
    after 'BUILD' => sub {
        my ($self, $args) = @_;
        {                      # Non-blocking boot
            $self->udp;
            my @nodes = @{$self->boot_nodes};
            my $cv    = AnyEvent->condvar;
            $cv->begin;
            my (@watchers, $coderef);
            $coderef = sub {
                shift @watchers if @watchers;
                $self->ping(shift @nodes);
                push @watchers,
                    AE::idle(@nodes ? $coderef : sub { $cv->end });
            };
            push @watchers, AE::idle($coderef);
            $cv->recv;
            shift @watchers;
        }
        return 1;
    };

    # Sockets
    has 'port' => (
        isa     => 'Int',
        is      => 'ro',
        writer  => '_port',
        default => 0,
        trigger => sub {
            my ($self, $new, $old) = @_;
            if (defined $old && $new && $old) {
                warn "TODO: Re-open servers";
            }
        }
    );

    #
    has 'cache' => (    # by infohash
                     isa     => 'HashRef',
                     is      => 'ro',
                     traits  => ['Hash'],
                     handles => {}
    );

    #
    has 'routing_table' => (
        isa        => 'ArrayRef[NBTypes::DHT::Bucket]',
        is         => 'ro',
        traits     => ['Array'],
        lazy_build => 1,
        handles    => {add_node => 'push'},

        #coerce=>1
    );
    sub _build_routing_table { [[]]; }
    around 'add_node' => sub {
    };

    #
    has 'outstanding_queries' => (    # by tid
        is      => 'ro',
        isa     => 'HashRef',
        traits  => ['Hash'],
        default => sub { {} },
        handles => {new_query                  => 'set',
                    get_query                  => 'get',
                    has_no_outstanding_queries => 'is_empty',
                    num_outstanding_queries    => 'count',
                    delete_query               => 'delete',
                    expire_query               => 'delete',
                    expecting                  => 'defined',
                    pairs                      => 'kv'
        }
    );
    around 'new_query' => sub {
        my ($code, $self, $tid, $packet, $node, $cb) = @_;
        my ($host, $port) = split ':', $node;
        my $paddr = ip2paddr($host);
        return if !defined $paddr;    # Failed to resolve
        warn $self->udp->ipv4_sock;
        warn 'Sending ' . $packet . ' to ' . $node;
        warn send($self->udp->ipv4_sock, $packet, 0,
                  pack_sockaddr($port, $paddr));
        return    # TODO - Define Moose type for outstanding queries?
            $code->($self, $tid,
                    {paddr  => $paddr,
                     node   => $node,
                     packet => $packet,
                     timeout =>
                         AE::timer(30, 0, sub { $self->expire_query($tid); }),
                     (defined $cb ? (cb => $cb) : ())
                    }
            );
    };

    #
    sub ping {
        my ($self, $node, $code) = @_;
        state $tid = 'aaaaaaaa';
        $self->new_query('ping_' . $tid,
                         build_dht_query_ping('ping_' . $tid, $self->nodeid),
                         $node, $code);
        $tid++;
    }

    sub get_peers {
        my ($self, $infohash, $code) = @_;
        state $tid = 'aaaaaaaa';
        $self->new_query('get_peers_' . $tid,
                         build_dht_query_get_peers('get_peers_' . $tid,
                                                   $self->nodeid, $infohash
                         ),
                         $infohash,
                         $code
        );
        $tid++;
    }

    #
    sub _on_data_in {
        use Data::Dump;
        my ($self, $udp, $sock, $paddr, $host, $port, $data, $flags) = @_;
        my $packet = bdecode $data;
        return if !$packet;
        if (defined $packet->{'r'}) {
            if ($self->expecting(
                                $packet->{'t'}) # Be sure we're expecting this
                && $self->get_query($packet->{'t'})->{'node'} eq
                "$host:$port"                   # from this guy
                )
            {   my $req = $self->get_query($packet->{'t'});   # For future ref
                $self->delete_query($packet->{'t'});
                $req->{'cb'}->($packet, $host, $port) if defined $req->{'cb'};
                my ($type, undef) = split '_', $packet->{'t'}, 2;
                if ($type eq 'ping') {
                    warn 'Yay! Pong!'

                        # Add node to router table
                }
            }
            else {
                ...;    # A reply we are not expecting. Strange.
            }
        }
    }
}
1;
