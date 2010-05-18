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
    sub send {
        my ($self, $node, $packet) = @_;
        return
            send((  $node->ipv6
                  ? $self->udp->ipv6_sock
                  : $self->udp->ipv4_sock
                 ),
                 $packet, 0,
                 $node->sockaddr
            );
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
    has '_quests' => (isa      => 'ArrayRef[Ref]',
                      is       => 'ro',
                      init_arg => undef,
                      traits   => ['Array'],
                      handles  => {
                                  add_quest   => 'push',
                                  quests      => 'elements',
                                  get_quest   => 'get',
                                  grep_quests => 'grep',
                                  map_quests  => 'map'
                      },
                      default => sub { [] }
    );
    after 'add_quest' => sub {
        require Scalar::Util;
        Scalar::Util::weaken $_[0]->{'_quests'}->[-1];
    };

    #
    sub get_peers {
        my ($self, $infohash, $code) = @_;
        if (!blessed $infohash) {
            require Bit::Vector;
            $infohash =
                Bit::Vector->new_Hex(160,
                        $infohash =~ m[^[a-f\d]+$]i ? $infohash : unpack 'H*',
                        $infohash);
        }
        my $quest = [
            $infohash,
            $code, '',
            AE::timer(
                15,
                60 * 5,
                sub {
                    $_->get_peers($infohash)
                        for @{$self->routing_table->nearest_bucket($infohash)
                            ->nodes};
                }
            )
        ];
        $self->add_quest($quest);
        return $quest;
    }

    #
    sub _on_data_in {
        my ($self, $udp, $sock, $sockaddr, $host, $port, $data, $flags) = @_;
        my $packet = bdecode $data;
        return if !$packet;
        my $node = $self->routing_table->find_node_by_sockaddr($sockaddr);
        if (!defined $node) {
            $node =
                Net::BitTorrent::Protocol::BEP05::Node->new(
                                        host          => $host,
                                        port          => $port,
                                        routing_table => $self->routing_table,
                                        sockaddr      => $sockaddr
                );
        }
        # Basic identity checks
        # TODO - if v is set, make sure it matches
        #      - make note of changes in nodeid/sockaddr combinations
        return
            if $node->has_nodeid    # Wait, this is me!
                && $node->nodeid->Lexicompare($self->nodeid) == 0;
        #
        if ($packet->{'y'} eq 'r') {
            if (defined $packet->{'r'}) {
                if ($node->is_expecting($packet->{'t'})) {
                    $node->touch;
                    $node->_v($packet->{'v'})
                        if !$node->_has_v && defined $packet->{'v'};
                    $node->_seen(1) if !$node->_has_seen;
                    my $req
                        = $node->del_request($packet->{'t'}); # For future ref
                    $req->{'cb'}->($packet, $host, $port)
                        if defined $req->{'cb'};
                    my $type = $req->{'type'};
                    if ($type eq 'ping') {
                        $node->_nodeid($packet->{'r'}{'id'})
                            if !$node->has_nodeid; # Adds node to router table
                    }
                    elsif ($type eq 'find_node') {
                        warn 'Yay find_node reply!';
                        require Net::BitTorrent::Protocol::BEP23::Compact;
                        for my $new_node (
                            Net::BitTorrent::Protocol::BEP23::Compact::uncompact_ipv4(
                                                       $packet->{'r'}{'nodes'}
                            )
                            )
                        {   my ($host, $port)
                                = ($new_node =~ m[^(.*):(\d+)$]);
                            my $node = $self->add_node([$host, $port]);
                        }
                    }
                    elsif ($type eq 'get_peers') {

                        # TODO - store token by id
                        if (!(    defined $packet->{'r'}{'nodes'}
                               || defined $packet->{'r'}{'values'}
                            )
                            )
                        {    # Malformed packet
                            ...;
                        }
                        if (defined $packet->{'r'}{'nodes'}) {
                            require Net::BitTorrent::Protocol::BEP23::Compact;
                            for my $new_node (
                                Net::BitTorrent::Protocol::BEP23::Compact::uncompact_ipv4(
                                                       $packet->{'r'}{'nodes'}
                                )
                                )
                            {   my ($host, $port)
                                    = ($new_node =~ m[^(.*):(\d+)$]);
                                my $node = $self->add_node([$host, $port]);
                                $node->get_peers($req->{'info_hash'});
                            }
                        }
                        if (defined $packet->{'r'}{'values'}) {
                            my ($quest)
                                = $self->grep_quests(
                                 sub { $req->{'info_hash'}->equal($_->[0]) });
                            require Net::BitTorrent::Protocol::BEP23::Compact;
                            $quest->[2]
                                = Net::BitTorrent::Protocol::BEP23::Compact::compact_ipv4(
                                Net::BitTorrent::Protocol::BEP23::Compact::uncompact_ipv4(
                                                   join '', $quest->[2],
                                                   @{$packet->{'r'}{'values'}}
                                )
                                );
                            $quest->[1]->($req->{'info_hash'}, $node,
                                          $packet->{'r'}{'values'});
                        }

=begin comment
  Get peers associated with a torrent infohash. "q" = "get_peers" A get_peers
  query has two arguments, "id" containing the node ID of the querying node, and
  "info_hash" containing the infohash of the torrent. If the queried node has
  peers for the infohash, they are returned in a key "values" as a list of
  strings. Each string containing "compact" format peer information for a single
  peer. If the queried node has no peers for the infohash, a key "nodes" is
  returned containing the C<K> nodes in the queried nodes routing table closest
  to the infohash supplied in the query. In either case a C<token> key is also
  included in the return value. The token value is a required argument for a
  future C<announce_peer> query. The token value should be a short binary
  string.
=cut
                    }
                    else {
                        ...;
                    }
                }
                else {    # A reply we are not expecting. Strange.
                    $node->miss;
                    $self->add_node($node);

                    #...;
                }
            }
        }
        elsif ($packet->{'y'} eq 'q' && defined $packet->{'a'}) {
            $node->touch;
            $node->_nodeid($packet->{'a'}{'id'})
                if !$node->has_nodeid;    # Adds node to router table
            my $type = $packet->{'q'};
            return $node->_reply_ping($packet->{'t'})
                if $type eq 'ping' && defined $packet->{'t'};

            #...;
        }
        else {
            use Data::Dump;
            ddx $packet;
            ...;

            # TODO: ID checks against $packet->{'a'}{'id'}
        }
    }

    sub as_string {
        my ($self, $detail) = @_;
        my $return = $self->nodeid;
        if ($detail) {
            $return = sprintf "Num buckets: %d. My DHT ID: %s\n",
                $self->routing_table->count_buckets, $self->nodeid->to_Hex;
            my $x = 0;
            for my $bucket (@{$self->routing_table->buckets}) {
                $return .= sprintf
                    "Bucket %s: %s (replacement cache: %d)\n",
                    $x++, $bucket->floor->to_Hex, $bucket->count_backup_nodes;
                for my $node (@{$bucket->nodes}) {
                    $return
                        .= sprintf
                        "    %s %s:%d fail:%d seen:%d age:%s ver:%s\n",
                        $node->nodeid->to_Hex, $node->host,
                        $node->port, $node->fail || 0, $node->seen || 0,
                        __duration(time - $node->birth), $node->v || '?';
                }
            }
        }
        return wantarray ? $return : say $return;
    }

    sub __duration ($) {
        my %dhms = (d => int($_[0] / (24 * 60 * 60)),
                    h => ($_[0] / (60 * 60)) % 24,
                    m => ($_[0] / 60) % 60,
                    s => $_[0] % 60
        );
        return join ' ',
            map { $dhms{$_} ? $dhms{$_} . $_ : () } sort keys %dhms;
    }
}
1;
