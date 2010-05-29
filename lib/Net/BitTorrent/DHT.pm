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
                     handles   => ['udp']
    );
    for my $type (qw[requests replies]) {
        for my $var (qw[count length]) {
            my $attr = join '_', '', 'recv_invalid', $var;
            has $attr => (isa      => 'Int',
                          is       => 'ro',
                          init_arg => undef,
                          traits   => ['Counter'],
                          handles  => {'_inc' . $attr => 'inc'},
                          default  => 0
            );
            for my $dir (qw[recv send]) {
                my $attr = join '_', '', $dir, $type, $var;
                has $attr => (isa      => 'Int',
                              is       => 'ro',
                              init_arg => undef,
                              traits   => ['Counter'],
                              handles  => {'_inc' . $attr => 'inc'},
                              default  => 0
                );
            }
        }
    }
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
        my ($self, $node, $packet, $reply) = @_;
        my $sent = send((  $node->ipv6
                         ? $self->udp->ipv6_sock
                         : $self->udp->ipv4_sock
                        ),
                        $packet, 0,
                        $node->sockaddr
        );
        if ($reply) {
            $self->_inc_send_replies_count;
            $self->_inc_send_replies_length($sent);
        }
        else {
            $self->_inc_send_requests_count;
            $self->_inc_send_requests_length($sent);
        }
        return $sent;
    }

    #
    has 'routing_table' => (
                      isa => 'Net::BitTorrent::Protocol::BEP05::RoutingTable',
                      is  => 'ro',
                      lazy_build => 1,
                      handles    => [qw[add_node buckets]]
    );

    sub _build_routing_table {
        Net::BitTorrent::Protocol::BEP05::RoutingTable->new(dht => shift);
    }
    after 'BUILD' => sub {
        my ($self, $args) = @_;
        return if !defined $args->{'boot_nodes'};
        $self->routing_table->add_node($_)->find_node($self->nodeid)
            for @{$args->{'boot_nodes'}};
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
                15, 60,
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

    sub find_node {
        my ($self, $nodeid, $code) = @_;
        if (!blessed $nodeid) {
            require Bit::Vector;
            $nodeid =
                Bit::Vector->new_Hex(160,
                            $nodeid =~ m[^[a-f\d]+$]i ? $nodeid : unpack 'H*',
                            $nodeid);
        }
        my $quest = [
            $nodeid, $code, '',
            AE::timer(
                15, 60,
                sub {
                    $_ && $_->find_node($nodeid)
                        for @{$self->routing_table->nearest_bucket($nodeid)
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
        if (!$packet || !ref $packet) {
            $self->_inc_recv_invalid_count;
            $self->_inc_recv_invalid_length(length $data);
            return;
        }
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
                    $self->_inc_recv_replies_count;
                    $self->_inc_recv_replies_length(length $data);
                    $node->touch;
                    $node->_v($packet->{'v'})
                        if !$node->_has_v && defined $packet->{'v'};
                    my $req
                        = $node->del_request($packet->{'t'}); # For future ref
                    $req->{'cb'}->($packet, $host, $port)
                        if defined $req->{'cb'};
                    my $type = $req->{'type'};
                    $node->_nodeid($packet->{'r'}{'id'})
                        if !$node->has_nodeid;    # Adds node to router table
                    $node->touch;

                    if ($type eq 'ping') {
                    }
                    elsif ($type eq 'find_node') {
                        my ($quest) = $self->grep_quests(
                            sub {
                                defined $_
                                    && $req->{'nodeid'}->equal($_->[0]);
                            }
                        );
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
                        $quest->[1]->($req->{'nodeid'}, $node,
                                      $packet->{'r'}{'nodes'}
                        ) if $quest->[1];
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
                                $node->get_peers($req->{'info_hash'})
                                    if $node;
                            }
                        }
                        if (defined $packet->{'r'}{'values'}) {    # peers
                            my ($quest) = $self->grep_quests(
                                sub {
                                    defined $_
                                        && $req->{'info_hash'}
                                        ->equal($_->[0]);
                                }
                            );
                            return if !defined $quest;
                            require Net::BitTorrent::Protocol::BEP23::Compact;
                            my @values = map {
                                Net::BitTorrent::Protocol::BEP23::Compact::uncompact_ipv4(
                                                                           $_)
                                } ref $packet->{'r'}{'values'}
                                ? @{$packet->{'r'}{'values'}}
                                : $packet->{'r'}{'values'};
                            $quest->[2]
                                = Net::BitTorrent::Protocol::BEP23::Compact::compact_ipv4(
                                Net::BitTorrent::Protocol::BEP23::Compact::uncompact_ipv4(
                                                         join '', $quest->[2],
                                ),
                                @values
                                );
                            $quest->[1]
                                ->($req->{'info_hash'}, $node, \@values);
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
                    $node->inc_fail;
                    $self->_inc_recv_invalid_count;
                    $self->_inc_recv_invalid_length(length $data);

                    #...;
                }
            }
        }
        elsif ($packet->{'y'} eq 'q' && defined $packet->{'a'}) {
            $self->_inc_recv_requests_count;
            $self->_inc_recv_requests_length(length $data);
            my $type = $packet->{'q'};
            $node->_nodeid($packet->{'a'}{'id'})
                if !$node->has_nodeid;    # Adds node to router table
            $node->touch;
            if ($type eq 'ping' && defined $packet->{'t'}) {
                return $node->_reply_ping($packet->{'t'});
            }
            elsif ($type eq 'get_peers'
                   && defined $packet->{'a'}{'info_hash'})
            {   require Bit::Vector;
                return $node->_reply_get_peers(
                      $packet->{'t'},
                      Bit::Vector->new_Hex(160,  unpack'H*',$packet->{'a'}{'info_hash'}));
            }
            elsif ($type eq 'find_node' && defined $packet->{'a'}{'target'}) {
                require Bit::Vector;
                return $node->_reply_find_node(
                $packet->{'t'},
                         Bit::Vector->new_Hex(160, unpack'H*',$packet->{'a'}{'target'}));
            }
            elsif ($type eq 'announce' && defined $packet->{'a'}{'info_hash'})
            {   require Bit::Vector;
                return $node->_reply_announce(
                $packet->{'t'},
                      Bit::Vector->new_Hex(160,  unpack'H*',$packet->{'a'}{'info_hash'}));
            }
        }
        else {
            use Data::Dump;
            warn sprintf '%s:%d', $node->host, $node->port;
            ddx $packet;

            #...;
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
                        $node->port, $node->fail || 0, $node->seen,
                        __duration(time - $node->birth), $node->v || '?';
                }
            }

           #[2010-05-20 08:00:37]  Total peers: 169 (in replacement cache 160)
            $return .= sprintf "Outstanding add nodes: %d\n",
                scalar $self->routing_table->outstanding_add_nodes;
            $return
                .= sprintf
                "Received: %d requests (%s), %d replies (%s), %d invalid (%s)\n",
                $self->_recv_requests_count,
                __data($self->_recv_requests_length),
                $self->_recv_replies_count,
                __data($self->_recv_replies_length),
                $self->_recv_invalid_count,
                __data($self->_recv_invalid_length);
            $return .= sprintf "Sent: %d requests (%s), %d replies (%s)",
                $self->_send_requests_count,
                __data($self->_send_requests_length),
                $self->_send_replies_count,
                __data($self->_send_replies_length);
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

    sub __data($) {
              $_[0] >= 1073741824 ? sprintf('%0.2f GB', $_[0] / 1073741824)
            : $_[0] >= 1048576    ? sprintf('%0.2f MB', $_[0] / 1048576)
            : $_[0] >= 1024       ? sprintf('%0.2f KB', $_[0] / 1024)
            :                       $_[0] . ' bytes';
    }
}
1;
