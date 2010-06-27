package Net::BitTorrent::Protocol::BEP05::Bucket;
{
    use Moose;
    use Moose::Util::TypeConstraints;
    use AnyEvent;
    use lib '../../../../../lib';
    use Net::BitTorrent::Types qw[NBTypes::DHT::NodeID];
    use 5.10.0;
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    # Stub
    sub BUILD {1}

    #
    my $K = 8;    # max nodes per-bucket

    #
    has '_id' => (isa => 'Str', is => 'ro', lazy_build => 1);
    sub _build__id { state $id = 'a'; $id++ }
    has 'floor' => (isa        => 'NBTypes::DHT::NodeID',
                    is         => 'ro',
                    lazy_build => 1,
                    coerce     => 1
    );
    sub _build_floor {0}
    has 'next' => (isa       => 'Net::BitTorrent::Protocol::BEP05::Bucket',
                   is        => 'ro',
                   writer    => '_next',
                   predicate => 'has_next',
                   weak_ref  => 1
    );
    for my $type ('', 'backup_') {
        has $type . 'nodes' => (
            isa => subtype(
                as 'ArrayRef[Net::BitTorrent::Protocol::BEP05::Node]' =>
                    where { scalar @$_ <= $K } => message {
                    sprintf 'Too many %s nodes! %d with max %d', $type,
                        scalar @$_, $K;
                }
            ),
            is      => 'ro',
            default => sub { [] },
            traits  => ['Array'],
            handles => {'pop_' . $type . 'nodes'     => 'pop',
                        'add_' . $type . 'node'      => 'push',
                        'shift_' . $type . 'nodes'   => 'shift',
                        'unshift_' . $type . 'nodes' => 'unshift',
                        'splice_' . $type . 'nodes'  => 'splice',
                        'count_' . $type . 'nodes'   => 'count',
                        'grep_' . $type . 'nodes'    => 'grep',
                        'map_' . $type . 'nodes'     => 'map',
                        'first_' . $type . 'node'    => 'first',
                        'clear_' . $type . 'nodes'   => 'clear'
            }
        );
    }
    around 'add_node' => sub {
        my ($code, $self, $node) = @_;
        if ($self->count_nodes == $K) {
            if ($self->_id eq
                $self->routing_table->nearest_bucket($self->dht->nodeid)->_id)
            {   return $self->add_node($node) if $self->split();
            }
            return $self->add_backup_node($node);
        }
        return if $self->grep_nodes(
            sub {
                $_->nodeid->Lexicompare($node->nodeid) == 0;
            }
        );
        $code->($self, $node);
        return $node->assign_bucket($self);
    };
    around 'add_backup_node' => sub {
        my ($code, $self, $node) = @_;
        return if $self->count_backup_nodes == $K;
        return
            if $self->grep_backup_nodes(
                         sub { $_->nodeid->Lexicompare($node->nodeid) == 0 });
        return $code->($self, $node);
    };
    has 'routing_table' => (
                      isa => 'Net::BitTorrent::Protocol::BEP05::RoutingTable',
                      is  => 'ro',
                      required => 1,
                      weak_ref => 1,
                      handles  => [qw[dht]]
    );
    has 'last_changed' => (isa => 'Int', is => 'rw', default => time);
    has 'clearing_house' =>
        (isa => 'ArrayRef', is => 'ro', writer => '_build_clearing_house');
    after 'BUILD' => sub {
        my $self = shift;
        $self->_build_clearing_house(
            AE::timer(
                30 * 60,
                30 * 60,
                sub {
                    require Scalar::Util;
                    Scalar::Util::weaken($self)
                        if !Scalar::Util::isweak($self);
                    $self->routing_table->del_node($_)
                        for $self->grep_nodes(sub { $_->fail && !$_->seen });
                }
            )
        );
    };
    has 'find_node_quest' => (isa        => 'ArrayRef|Maybe',
                              is         => 'ro',
                              init_arg   => undef,
                              lazy_build => 1,
                              writer     => '_find_node_quest',
                              clearer    => '_reset_find_node_quest'
    );

    sub _build_find_node_quest {
        my ($self) = @_;
        $self->dht->find_node(
            $self->middle,
            sub {
                require Scalar::Util;
                Scalar::Util::weaken($self) if !Scalar::Util::isweak($self);
                $self->_find_node_quest(undef)
                    if $self->count_backup_nodes == $K;
            }
        );
    }
    after 'BUILD' => sub { $_[0]->find_node_quest() };
    after 'BUILD' => sub { };

    #
    sub ceil {
        my $self = shift;
        require Bit::Vector;
        my $ceil = Bit::Vector->new_Hex(160, 'F' x 40);
        if ($self->has_next) {
            $ceil->Copy($self->next->floor);
            $ceil->add($ceil, Bit::Vector->new_Dec(160, 1), 0);
        }
        return $ceil;
    }

    sub middle {
        my $self = shift;
        require Bit::Vector;
        my $floor = Bit::Vector->new(160);
        $floor->Copy($self->floor);
        my $range = Bit::Vector->new(160);
        $range->subtract($self->ceil, $floor, 0);
        my $new_floor = Bit::Vector->new(160);
        {    # Resize for overflow
            $_->Resize(161) for $range, $floor, $new_floor;
            my $half_range = Bit::Vector->new(161);
            $half_range->Divide($range, Bit::Vector->new_Dec(161, 2),
                                Bit::Vector->new(161));
            $new_floor->add($floor, $half_range, 0);
            $_->Resize(160) for $new_floor;
        }
        return $new_floor;
    }

    sub split {
        my ($self) = @_;
        my $new_floor = $self->middle;
        my $new_bucket =
            Net::BitTorrent::Protocol::BEP05::Bucket->new(
                                routing_table => $self->routing_table,
                                ($self->has_next ? (next => $self->next) : ())
            );
        $new_bucket->floor->Copy($new_floor);
        $self->_next($new_bucket);
        $self->routing_table->add_bucket($new_bucket);
        {
            my @nodes = (@{$self->nodes}, @{$self->backup_nodes});
            $self->clear_nodes;
            $self->clear_backup_nodes;
            $self->routing_table->assign_node($_) for @nodes;
        }
        return 1;
    }

    sub _del_node {
        my ($self, $node) = @_;
        for my $i (0 .. $self->count_nodes) {
            if ($node->nodeid->Lexicompare($self->nodes->[$i]->nodeid) == 0) {
                $self->splice_nodes($i, 1, ());    # Post 'Help Wanted' sign
                last;
            }
        }
        $self->add_node($self->shift_backup_nodes)
            if $self->count_backup_nodes;          # Take sign down
        $self->_reset_find_node_quest if $self->count_nodes < $K;
    }
}
1;
