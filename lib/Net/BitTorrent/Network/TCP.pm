package Net::BitTorrent::Network::TCP;
{
    use Moose;
    our $MAJOR = 0.075; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use lib '../../../../lib';
    extends 'Net::BitTorrent::Network';
    use Net::BitTorrent::Network::Utility qw[server];

    sub _build_ipv4 {
        my ($self) = @_;
        return server(
            $self->ipv4_host,
            $self->ipv4_port,
            sub { $self->trigger_ipv4_on_data_in(@_); },
            sub {
                my ($sock, $host, $port) = @_;
                if ($self->ipv4_port && $self->ipv4_port != $port) {
                    ...;
                }
                $self->_ipv4_sock($sock);
                $self->_ipv4_host($host);
                $self->_ipv4_port($port);
            },
            'tcp'
        );
    }

    sub _build_ipv6 {
        my ($self) = @_;
        return server(
            $self->ipv6_host,
            $self->ipv6_port,
            sub { $self->trigger_ipv6_on_data_in(@_); },
            sub {
                my ($sock, $host, $port) = @_;
                if ($self->ipv6_port && $self->ipv6_port != $port) {
                    ...;
                }
                $self->_ipv6_sock($sock);
                $self->_ipv6_host($host);
                $self->_ipv6_port($port);
            },
            'tcp'
        );
    }
    no Moose;
    __PACKAGE__->meta->make_immutable;
}
1;
