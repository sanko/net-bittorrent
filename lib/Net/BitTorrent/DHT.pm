package Net::BitTorrent::DHT;
{
    use Moose;
    use AnyEvent;
    use lib '../../../lib';
    use Net::BitTorrent::Protocol::BEP03::Bencode qw[bdecode];
    use Net::BitTorrent::Protocol::BEP05::Packets qw[:all];
    use Net::BitTorrent::Network::Utility qw[:paddr :sockaddr];
    use Net::BitTorrent::Types qw[:dht];
    use Net::BitTorrent::Protocol::BEP05::RoutingTable;
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
        Moose::Util::apply_all_roles($self,
                                     'Net::BitTorrent::DHT::Standalone',
                                     {rebless_params => $args});
    };

    #
    has 'nodeid' => (isa        => 'NBTypes::DHT::NodeID',
                     is         => 'ro',
                     lazy_build => 1,
                     builder    => '_build_nodeid',
                     coerce     => 1
    );

    sub _build_nodeid {
        require Digest::SHA;
        return Digest::SHA::sha1(rand(time * $^T) . $0 . 'Sanko was here.');
    }

    #
    }

    #
    has 'routing_table' => (
        isa        => 'Net::BitTorrent::Protocol::BEP05::RoutingTable',
        is         => 'ro',
        lazy_build => 1,
        handles    => {add_node => 'add_node', buckets => 'buckets'},

        #coerce=>1
    );

    sub _build_routing_table {
        Net::BitTorrent::Protocol::BEP05::RoutingTable->new(dht => shift);
    }
    after 'BUILD' => sub {
        my ($self, $args) = @_;
        return if !defined $args->{'boot_nodes'};
        $self->routing_table->add_node($_) for @{$args->{'boot_nodes'}};
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
        use Data::Dump;
        ddx \@_;
        my ($host, $port) = @$node;
        my $paddr = ip2paddr($host);
        return if !defined $paddr;    # Failed to resolve
        warn $self->udp->ipv4_sock;
        warn 'Sending ' . $packet . ' to ' . $node;
        my $sent = send($self->udp->ipv4_sock, $packet, 0,
                        pack_sockaddr($port, $paddr));
        warn sprintf 'sent %d bytes', $sent || 0;
        return !$sent;
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
