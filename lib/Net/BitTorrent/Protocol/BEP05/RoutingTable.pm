package Net::BitTorrent::Protocol::BEP05::RoutingTable;
{
    use Moose;
    use AnyEvent;
    use lib '../../../../../lib';
    use Net::BitTorrent::Protocol::BEP03::Bencode qw[bdecode];
    use Net::BitTorrent::Protocol::BEP05::Packets qw[:all];
    use Net::BitTorrent::Network::Utility qw[:paddr :sockaddr];
    use Net::BitTorrent::Types;
    use 5.10.0;
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    #
    has 'tracker' => (isa      => 'Net::BitTorrent::Protocol::BEP05::Tracker',
                      is       => 'ro',
                      init_arg => undef,
                      lazy_build => 1
    );

    sub _build_tracker {
        require Net::BitTorrent::Protocol::BEP05::Tracker;
        Net::BitTorrent::Protocol::BEP05::Tracker->new(
                                                      routing_table => shift);
    }
    has 'nodes' => (isa => 'HashRef[Net::BitTorrent::Protocol::BEP05::Node]',
                    is  => 'ro',
                    init_arg => undef,
                    traits   => ['Hash'],
                    handles  => {
                                add_node     => 'set',
                                get_node     => 'get',
                                del_node     => 'delete',
                                defined_node => 'defined',
                                count_nodes  => 'count',
                                all_nodes    => 'values'
                    },
                    default => sub { {} }
    );
    around 'add_node' => sub {
        my ($code, $self, $node) = @_;
        if (!blessed $node) {
            require Net::BitTorrent::Protocol::BEP05::Node;
            $node =
                Net::BitTorrent::Protocol::BEP05::Node->new(
                                                        host => $node->[0],
                                                        port => $node->[1],
                                                        routing_table => $self
                );
        }
        elsif (!$node->has_routing_table) { $node->_routing_table($self) }
        return $code->($self, $node->sockaddr, $node);
    };
    around 'del_node' => sub {
        my ($code, $self, $node) = @_;
        $code->($self, blessed($node) ? $node->sockaddr : $node);
    };
    after 'del_node' =>
        sub { $_[1]->bucket->_del_node($_[1]) if $_[1]->has_bucket };
    has 'buckets' => (
        isa        => 'ArrayRef[Net::BitTorrent::Protocol::BEP05::Bucket]',
        is         => 'ro',
        lazy_build => 1,
        init_arg   => undef,
        traits     => ['Array'],
        handles    => {
            sort_buckets => [
                'sort_in_place',
                sub {
                    $_[0]->floor->Lexicompare($_[1]->floor);
                    }
            ],
            first_bucket  => 'first',
            grep_buckets  => 'grep',
            count_buckets => 'count',
            add_bucket    => 'push'
        }
    );
    after 'add_bucket' => sub { shift->sort_buckets; };

    sub _build_buckets {
        my ($self) = @_;
        require Net::BitTorrent::Protocol::BEP05::Bucket;
        [Net::BitTorrent::Protocol::BEP05::Bucket->new(routing_table => $self
         )
        ];
    }
    has 'dht' => (isa      => 'Net::BitTorrent::DHT',
                  required => 1,
                  is       => 'ro',
                  weak_ref => 1,
                  handles  => [qw[send]],
                  init_arg => 'dht'
    );

    sub nearest_bucket {
        my ($self, $target) = @_;
        for my $bucket (reverse @{$self->buckets}) {
            return $bucket if $bucket->floor->Lexicompare($target) != 1;
        }
    }
    before 'nearest_bucket' => sub { shift->sort_buckets; };

    sub assign_node {
        my ($self, $node) = @_;
        $self->nearest_bucket($node->nodeid)->add_node($node);
    }

    sub find_node_by_sockaddr {
        my ($self, $sockaddr) = @_;
        my $node = $self->get_node($sockaddr);
        if (!$node) {
            for my $bucket (@{$self->buckets}) {
                $node
                    = $bucket->first_node(sub { $_->sockaddr eq $sockaddr });
                last if $node;
            }
        }
        return $node;
    }

    sub outstanding_add_nodes {
        grep { defined $_ && !$_->has_bucket } $_[0]->all_nodes;
    }

=pod

Every node maintains a routing table of known good nodes. The nodes in the
routing table are used as starting points for queries in the DHT. Nodes from
the routing table are returned in response to queries from other nodes.

Not all nodes that we learn about are equal. Some are "good" and some are not.
Many nodes using the DHT are able to send queries and receive responses, but
are not able to respond to queries from other nodes. It is important that each
node's routing table must contain only known good nodes. A good node is a node
has responded to one of our queries within the last 15 minutes. A node is also
good if it has ever responded to one of our queries and has sent us a query
within the last 15 minutes. After 15 minutes of inactivity, a node becomes
questionable. Nodes become bad when they fail to respond to multiple queries
in a row. Nodes that we know are good are given priority over nodes with
unknown status.

The routing table covers the entire node ID space from C<0> to C<2**160>. The
routing table is subdivided into "buckets" that each cover a portion of the
space. An empty table has one bucket with an ID space range of
C<min=0, max=2**160>. When a node with ID "C<N>" is inserted into the table,
it is placed within the bucket that has C<min <= N < max>. An empty table has
only one bucket so any node must fit within it. Each bucket can only hold C<K>
nodes, currently eight, before becoming "full." When a bucket is full of known
good nodes, no more nodes may be added unless our own node ID falls within the
range of the bucket. In that case, the bucket is replaced by two new buckets
each with half the range of the old bucket and the nodes from the old bucket
are distributed among the two new ones. For a new table with only one bucket,
the full bucket is always split into two new buckets covering the ranges
C<0..2**159> and C<2**159..2**160>.

When the bucket is full of good nodes, the new node is simply discarded. If
any nodes in the bucket are known to have become bad, then one is replaced by
the new node. If there are any questionable nodes in the bucket have not been
seen in the last 15 minutes, the least recently seen node is pinged. If the
pinged node responds then the next least recently seen questionable node is
pinged until one fails to respond or all of the nodes in the bucket are known
to be good. If a node in the bucket fails to respond to a ping, it is
suggested to try once more before discarding the node and replacing it with a
new good node. In this way, the table fills with stable long running nodes.

Each bucket should maintain a "last changed" property to indicate how "fresh"
the contents are. When a node in a bucket is pinged and it responds, or a node
is added to a bucket, or a node in a bucket is replaced with another node, the
bucket's last changed property should be updated. Buckets that have not been
changed in 15 minutes should be "refreshed." This is done by picking a random
ID in the range of the bucket and performing a find_nodes search on it. Nodes
that are able to receive queries from other nodes usually do not need to
refresh buckets often. Nodes that are not able to receive queries from other
nodes usually will need to refresh all buckets periodically to ensure there
are good nodes in their table when the DHT is needed.

Upon inserting the first node into its routing table and when starting up
thereafter, the node should attempt to find the closest nodes in the DHT to
itself. It does this by issuing find_node messages to closer and closer nodes
until it cannot find any closer. The routing table should be saved between
invocations of the client software.

=cut

}
1;

=begin comment use strict;
  use warnings;
  $|++;
  use Net::BitTorrent::DHT;
  my $table = __PACKAGE__->new(node => Net::BitTorrent::DHT->new());
  use Data::Dump;
  ddx $table->buckets;
  $table->add_node(pack 'H40', 'AAAAAAAAAA' . $_) for reverse 1..19;
  $table->sort_buckets;
  ddx $table;
=cut
