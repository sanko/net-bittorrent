package Net::BitTorrent::DHT::Standalone;
{
    use Moose::Role;
    use lib '../../../../lib';
    use Net::BitTorrent::Network::UDP;

    #If I weren't a role, I'd...
    #extends 'Net::BitTorrent::DHT';
    #has '+client' => (required => 0, handles => {});
    has 'udp' => (init_arg   => undef,
                  is         => 'ro',
                  isa        => 'Net::BitTorrent::Network::UDP',
                  lazy_build => 1
    );

    sub _build_udp {
        my ($self) = @_;
        Net::BitTorrent::Network::UDP->new(
                                  port       => $self->port,
                                  on_data_in => sub { $self->_on_data_in(@_) }
        );
    }
    has 'port' => (is      => 'ro',
                   default => '0',
                   isa     => 'Int'
    );
}
1;
