package Net::BitTorrent::Protocol::BEP05::Tracker;
{
    use Moose;
    use lib '../../../../../lib';
    use 5.10.0;
    our $MAJOR = 0.075; our $MINOR = 0; our $DEV = -1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    # Stub
    sub BUILD {1}

    #
    has 'peers' => (isa      => 'Hash[ArrayRef]',
                    is       => 'ro',
                    default  => sub { {} },
                    init_arg => undef,
                    traits   => {'Hash'},
                    handles  => [qw[get set defined delete]]
    );
    has 'routing_table' => (
                      isa => 'Net::BitTorrent::Protocol::BEP05::RoutingTable',
                      is  => 'ro',
                      required => 1,
                      weak_ref => 1,
                      handles  => [qw[dht]]
    );

    sub announce {
        my ($self, $infohash, $node) = @_;
        ... state $tid = 'a';
        return 'a_' . $tid++;
    }

    sub get_peers {
        my ($self, $infohash, $limit) = @_;
        $limit //= 25;
        ...;
    }
}
1;
