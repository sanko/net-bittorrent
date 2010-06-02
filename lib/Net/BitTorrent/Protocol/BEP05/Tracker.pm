package Net::BitTorrent::Protocol::BEP05::Tracker;
{
    use Moose;
    use lib '../../../../../lib';
    use 5.10.0;
    our $MAJOR = 0.075; our $MINOR = 0; our $DEV = -1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    # Stub
    sub BUILD {1}

    #
    has 'peers' => (isa      => 'HashRef[ArrayRef]',
                    is       => 'ro',
                    default  => sub { {} },
                    init_arg => undef,
                    traits   => ['Hash'],
                    handles  => {
                                get_peers => 'get',
                                set_peers => 'set',
                                has_peers => 'defined',
                                del_peers => 'delete'
                    }
    );
    around 'get_peers' => sub {
        my ($code, $self, $infohash) = @_;
        $code->($self, blessed $infohash ? $infohash->to_Hex : $infohash);
    };

    sub add_peer {
        my ($self, $infohash, $peer) = @_;
        return $self->has_peers($infohash->to_Hex)
            ? push(@{$self->get_peers($infohash->to_Hex)}, $peer)
            : $self->set_peers($infohash->to_Hex, [$peer]);
    }
    has 'routing_table' => (
                      isa => 'Net::BitTorrent::Protocol::BEP05::RoutingTable',
                      is  => 'ro',
                      required => 1,
                      weak_ref => 1,
                      handles  => [qw[dht]]
    );
}
1;
