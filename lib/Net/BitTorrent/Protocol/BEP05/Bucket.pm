package Net::BitTorrent::Protocol::BEP05::Bucket;
{
    use Moose;
    use Moose::Util::TypeConstraints;
    use lib '../../../../../lib';
    use Net::BitTorrent::Types qw[NBTypes::DHT::NodeID];
    use 5.10.0;
    our $MAJOR = 0.075; our $MINOR = 0; our $DEV = -1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    #
    my $K = 8;    # max nodes per-bucket

    #
    has 'id' => (isa => 'Str', is => 'ro', lazy_build => 1);
    sub _build_id { state $id = 'a'; $id++ }
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
                    sprintf 'Too many %snodes! %d with max %d', $type,
                        scalar @$_, $K;
                }
            ),
            is      => 'ro',
            coerce  => 1,
            default => sub { [] },
            traits  => ['Array'],
            handles => {'pop_' . $type . 'node'     => 'pop',
                        'push_' . $type . 'node'    => 'push',
                        'shift_' . $type . 'node'   => 'shift',
                        'unshift_' . $type . 'node' => 'unshift',
                        'splice_' . $type . 'nodes' => 'splice',
                        'count_' . $type . 'nodes'  => 'count',
                        'add_' . $type . 'node'     => 'push',
                        'sort_' 
                            . $type
                            . 'nodes' => [
                             'sort_in_place',
                             sub { $_[0]->nodeid->Lexicompare($_[1]->nodeid) }
                            ],
                        'grep_' . $type . 'nodes'  => 'grep',
                        'map_' . $type . 'nodes'   => 'map',
                        'first_' . $type . 'node'  => 'first',
                        'clear_' . $type . 'nodes' => 'clear'
            }
        );
    }
    around 'add_node' => sub {
        my ($code, $self, $node) = @_;
        if ($self->count_nodes == $K) {
            if ($self->id eq
                $self->routing_table->nearest_bucket($node->nodeid)->id)
            {   return $self->add_node($node) if $self->split();
            }
            die
                'Adding backup node!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!';
            return $self->add_backup_node($node);
        }
        return if $self->grep_nodes(
            sub {
                $_->nodeid->Lexicompare($node->nodeid) == 0;
            }
        );
        return $code->($self, $node);
    };
    around 'add_backup_node' => sub {
        my ($code, $self, $node) = @_;
        return if $self->count_backup_nodes >= $K * 3;
        return
            if $self->grep_backup_nodes(
                           sub { $_->nodeid->Lexicompare($node->nodeid) == 0 }
            );
        return $code->($self, $node);
    };
    has 'routing_table' => (
                      isa => 'Net::BitTorrent::Protocol::BEP05::RoutingTable',
                      is  => 'ro',
                      required => 1,
                      weak_ref => 1
    );
    has 'last_changed' => (isa => 'Int', is => 'rw', default => time);

    #
    sub split {
        my ($self) = @_;

        #return if $self->routing_table->count_buckets >= 30;
        my $ceil = Bit::Vector->new_Hex(160, 'F' x 40);
        if ($self->has_next) {
            $ceil->Copy($self->next->floor);
            $ceil->add($ceil, Bit::Vector->new_Dec(160, 1), 0);
        }
        my $floor = Bit::Vector->new(160);
        $floor->Copy($self->floor);
        my $range = Bit::Vector->new(160);
        $range->subtract($ceil, $floor, 0);
        my $new_floor = Bit::Vector->new(160);
        {    # Resize for overflow
            $_->Resize(161) for $range, $floor, $new_floor;
            my $half_range = Bit::Vector->new(161);
            $half_range->Divide($range, Bit::Vector->new_Dec(161, 2),
                                Bit::Vector->new(161));
            $new_floor->add($floor, $half_range, 0);
            $_->Resize(160) for $new_floor;
        }
        my $new_bucket =
            Net::BitTorrent::Protocol::BEP05::Bucket->new(
                                routing_table => $self->routing_table,
                                ($self->has_next ? (next => $self->next) : ())
            );
        $new_bucket->floor->Copy($new_floor);
        $self->_next($new_bucket);
        $self->routing_table->add_bucket($new_bucket);
        {
            my @nodes = @{$self->nodes};
            $self->clear_nodes;
            $self->routing_table->assign_node($_) for @nodes;
        }
        {
            my @nodes = @{$self->backup_nodes};
            $self->clear_backup_nodes;
            $self->routing_table->assign_node($_) for @nodes;
        }
        return 1;
    }

    sub del_node {
        my ($self, $node) = @_;
        for my $i (0 .. $self->count_nodes) {
            return $self->splice_nodes($i, 1, ());
        }
    }
}
1;
