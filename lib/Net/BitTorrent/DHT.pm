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
    has 'ipv4_routing_table' => (
                      isa => 'Net::BitTorrent::Protocol::BEP05::RoutingTable',
                      is  => 'ro',
                      lazy_build => 1,
                      handles    => {
                                  ipv4_add_node => 'add_node',
                                  ipv4_buckets  => 'buckets'
                      }
    );
    has 'ipv6_routing_table' => (
                      isa => 'Net::BitTorrent::Protocol::BEP05::RoutingTable',
                      is  => 'ro',
                      lazy_build => 1,
                      handles    => {
                                  ipv6_add_node => 'add_node',
                                  ipv6_buckets  => 'buckets'
                      }
    );

    sub _build_ipv4_routing_table {
        Net::BitTorrent::Protocol::BEP05::RoutingTable->new(dht => shift);
    }

    sub _build_ipv6_routing_table {
        Net::BitTorrent::Protocol::BEP05::RoutingTable->new(dht => shift);
    }
    after 'BUILD' => sub {
        my ($self, $args) = @_;
        return if !defined $args->{'boot_nodes'};
        require Net::BitTorrent::Protocol::BEP05::Node;
        for my $node (@{$args->{'boot_nodes'}}) {
            $node =
                Net::BitTorrent::Protocol::BEP05::Node->new(
                                                           host => $node->[0],
                                                           port => $node->[1]
                );
            (  $node->ipv6
             ? $self->ipv6_routing_table->add_node($node)
             : $self->ipv4_routing_table->add_node($node)
            )->find_node($self->nodeid);
        }
    };

    #
    for my $type (qw[get_peers announce_peer find_node]) {
        has "_${type}_quests" => (isa      => 'ArrayRef[Ref]',
                                  is       => 'ro',
                                  init_arg => undef,
                                  traits   => ['Array'],
                                  handles  => {
                                              "add_${type}_quest" => 'push',
                                              "${type}_quests" => 'elements',
                                              "get_${type}_quest"   => 'get',
                                              "grep_${type}_quests" => 'grep',
                                              "map_${type}_quests"  => 'map'
                                  },
                                  default => sub { [] }
        );
        after "add_${type}_quest" => sub {
            require Scalar::Util;
            Scalar::Util::weaken $_[0]->{"_${type}_quests"}->[-1];
        };
    }

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
                0,
                2 * 60,
                sub {
                    return if !$self;
                    for my $rt ($self->ipv6_routing_table,
                                $self->ipv4_routing_table)
                    {   for my $node (
                                     @{$rt->nearest_bucket($infohash)->nodes})
                        {   $node->get_peers($infohash);
                        }
                    }
                }
            )
        ];
        $self->add_get_peers_quest($quest);
        return $quest;
    }

    sub announce_peer {
        my ($self, $infohash, $port, $code) = @_;
        if (!blessed $infohash) {
            require Bit::Vector;
            $infohash =
                Bit::Vector->new_Hex(160,
                        $infohash =~ m[^[a-f\d]+$]i ? $infohash : unpack 'H*',
                        $infohash);
        }
        my $quest = [
            $infohash,
            $code, $port, '',
            AE::timer(
                0,
                2 * 60,
                sub {
                    return if !$self;
                    for my $rt ($self->ipv6_routing_table,
                                $self->ipv4_routing_table)
                    {   for my $node (
                                     @{$rt->nearest_bucket($infohash)->nodes})
                        {   $node->announce_peer($infohash, $port);
                        }
                    }
                }
            )
        ];
        $self->add_announce_peer_quest($quest);
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
                0,
                1.5 * 60,
                sub {
                    return if !$self;
                    for my $rt ($self->ipv6_routing_table,
                                $self->ipv4_routing_table)
                    {   for my $node (@{$rt->nearest_bucket($nodeid)->nodes})
                        {   $node->find_node($nodeid);
                        }
                    }
                }
            )
        ];
        $self->add_find_node_quest($quest);
        return $quest;
    }

    #
    sub _ipv6_on_data_in {
        my ($self, $udp, $sock, $sockaddr, $host, $port, $data, $flags) = @_;
        my $packet = bdecode $data;
        if (   !$packet
            || !ref $packet
            || ref $packet ne 'HASH'
            || !keys %$packet)
        {   $self->_inc_recv_invalid_count;
            $self->_inc_recv_invalid_length(length $data);
            return;
        }
        my $node
            = $self->ipv6_routing_table->find_node_by_sockaddr($sockaddr);
        if (!defined $node) {
            $node =
                Net::BitTorrent::Protocol::BEP05::Node->new(
                                   host          => $host,
                                   port          => $port,
                                   routing_table => $self->ipv6_routing_table,
                                   sockaddr      => $sockaddr
                );
        }
    }

    sub _ipv4_on_data_in {
        my ($self, $udp, $sock, $sockaddr, $host, $port, $data, $flags) = @_;
        my $packet = bdecode $data;
        if (   !$packet
            || !ref $packet
            || ref $packet ne 'HASH'
            || !keys %$packet)
        {   $self->_inc_recv_invalid_count;
            $self->_inc_recv_invalid_length(length $data);
            return;
        }
        my $node
            = $self->ipv4_routing_table->find_node_by_sockaddr($sockaddr);
        if (!defined $node) {
            $node =
                Net::BitTorrent::Protocol::BEP05::Node->new(
                                   host          => $host,
                                   port          => $port,
                                   routing_table => $self->ipv4_routing_table,
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
                        my ($quest) = $self->grep_find_node_quests(
                            sub {
                                defined $_
                                    && $req->{'nodeid'}->equal($_->[0]);
                            }
                        );
                        return if !defined $quest;
                        require Net::BitTorrent::Protocol::BEP23::Compact;
                        for my $new_node (    # XXX - May be ipv6
                            Net::BitTorrent::Protocol::BEP23::Compact::uncompact_ipv4(
                                                       $packet->{'r'}{'nodes'}
                            )
                            )
                        {   my ($host, $port)
                                = ($new_node =~ m[^(.*):(\d+)$]);
                            my $node = $self->ipv4_add_node([$host, $port]);
                        }
                        $quest->[1]->(
                            $req->{'nodeid'}, $node,
                            $packet->{'r'}{'nodes'}    # XXX - uncompact
                        );
                    }
                    elsif ($type eq 'get_peers') {

                        # TODO - store token by id
                        if (!(    defined $packet->{'r'}{'nodes'}
                               || defined $packet->{'r'}{'values'}
                            )
                            )
                        {                              # Malformed packet
                            ...;
                        }
                        if (defined $packet->{'r'}{'nodes'}) {
                            require Net::BitTorrent::Protocol::BEP23::Compact;
                            for my $new_node (         # XXX - may be ipv6
                                Net::BitTorrent::Protocol::BEP23::Compact::uncompact_ipv4(
                                                       $packet->{'r'}{'nodes'}
                                )
                                )
                            {   my ($host, $port)
                                    = ($new_node =~ m[^(.*):(\d+)$]);
                                my $node
                                    = $self->ipv4_add_node([$host, $port]);
                                $node->get_peers($req->{'info_hash'})
                                    if $node;
                            }
                        }
                        if (defined $packet->{'r'}{'values'}) {    # peers
                            my ($quest) = $self->grep_get_peers_quests(
                                sub {
                                    defined $_
                                        && $req->{'info_hash'}
                                        ->equal($_->[0]);
                                }
                            );
                            return if !defined $quest;
                            push @{$quest->[2]}, @{$packet->{'r'}{'values'}};
                            require Net::BitTorrent::Protocol::BEP23::Compact;
                            $quest->[1]->(
                                $req->{'info_hash'},
                                $node,
                                [map {
                                     Net::BitTorrent::Protocol::BEP23::Compact::uncompact_ipv4(
                                                                           $_)
                                     } @{$packet->{'r'}{'values'}}
                                ]
                            );
                        }
                        if (defined $packet->{'r'}{'token'})
                        {    # for announce_peer
                            $node->_set_announce_peer_token_in(
                                                  $req->{'info_hash'}->to_Hex,
                                                  $packet->{'r'}{'token'});
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
                    elsif ($type eq 'announce_peer') {
                        my ($quest) = $self->grep_announce_peer_quests(
                            sub {
                                defined $_
                                    && $req->{'info_hash'}->equal($_->[0]);
                            }
                        );
                        return if !defined $quest;
                        $quest->[3]
                            = Net::BitTorrent::Protocol::BEP23::Compact::compact_ipv4(
                            Net::BitTorrent::Protocol::BEP23::Compact::uncompact_ipv4(
                                                         join '', $quest->[2],
                            ),
                            sprintf '%s:%s',
                            $node->host,
                            $node->port
                            );
                        $quest->[1]
                            ->($req->{'info_hash'}, $quest->[2], $node);
                    }
                    else {
                        use Data::Dump;
                        warn sprintf '%s:%d', $node->host, $node->port;
                        ddx $packet;
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
                return
                    $node->_reply_get_peers(
                             $packet->{'t'},
                             Bit::Vector->new_Hex(
                                 160, unpack 'H*', $packet->{'a'}{'info_hash'}
                             )
                    );
            }
            elsif ($type eq 'find_node' && defined $packet->{'a'}{'target'}) {
                require Bit::Vector;
                return
                    $node->_reply_find_node(
                                $packet->{'t'},
                                Bit::Vector->new_Hex(
                                    160, unpack 'H*', $packet->{'a'}{'target'}
                                )
                    );
            }
            elsif ($type eq 'announce_peer'
                   && defined $packet->{'a'}{'info_hash'})
            {   require Bit::Vector;
                return
                    $node->_reply_announce_peer(
                             $packet->{'t'},
                             Bit::Vector->new_Hex(
                                 160, unpack 'H*', $packet->{'a'}{'info_hash'}
                             ),
                             $packet->{'a'},
                    );
            }
        }
        elsif ($packet->{'y'} eq 'q' && defined $packet->{'a'}) {
            use Data::Dump;
            warn sprintf 'Error from %s:%d', $node->host, $node->port;
            ddx $packet;
        }
        else {
            use Data::Dump;
            warn sprintf '%s:%d', $node->host, $node->port;
            ddx $packet;

            #...;
            # TODO: ID checks against $packet->{'a'}{'id'}
        }
    }

    sub dump_ipv4_buckets {
        my @return = _dump_buckets($_[0], $_[0]->ipv4_routing_table());
        return wantarray ? @return : sub { say $_ for @_ }
            ->(@return);
    }

    sub dump_ipv6_buckets {
        my @return = _dump_buckets($_[0], $_[0]->ipv6_routing_table());
        return wantarray ? @return : sub { say $_ for @_ }
            ->(@return);
    }

    sub _dump_buckets {
        my ($self, $routing_table) = @_;
        my @return = sprintf 'Num buckets: %d. My DHT ID: %s',
            $routing_table->count_buckets, $self->nodeid->to_Hex;
        my ($x, $t_primary, $t_backup) = (0, 0, 0);
        for my $bucket (@{$routing_table->buckets}) {
            push @return, sprintf 'Bucket %s: %s (replacement cache: %d)',
                $x++, $bucket->floor->to_Hex, $bucket->count_backup_nodes;
            for my $node (@{$bucket->nodes}) {
                push @return,
                    sprintf '    %s %s:%d fail:%d seen:%d age:%s ver:%s',
                    $node->nodeid->to_Hex, $node->host,
                    $node->port, $node->fail || 0, $node->seen,
                    __duration(time - $node->birth), $node->v || '?';
            }
            $t_primary += $bucket->count_nodes;
            $t_backup  += $bucket->count_backup_nodes;
        }
        push @return, sprintf 'Total peers: %d (in replacement cache %d)',
            $t_primary + $t_backup, $t_backup;
        push @return, sprintf 'Outstanding add nodes: %d',
            scalar $routing_table->outstanding_add_nodes;
        push @return,
            sprintf
            'Received: %d requests (%s), %d replies (%s), %d invalid (%s)',
            $self->_recv_requests_count,
            __data($self->_recv_requests_length),
            $self->_recv_replies_count,
            __data($self->_recv_replies_length),
            $self->_recv_invalid_count,
            __data($self->_recv_invalid_length);
        push @return, sprintf 'Sent: %d requests (%s), %d replies (%s)',
            $self->_send_requests_count,
            __data($self->_send_requests_length),
            $self->_send_replies_count,
            __data($self->_send_replies_length);
        return @return;
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
