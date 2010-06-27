package Net::BitTorrent::Network::UDP;
{
    use Moose::Role;
    use AnyEvent;
    use lib '../../../../lib';
    use Net::BitTorrent::Network::Utility qw[server];
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    #
    has '_port' => (is       => 'ro',
                    isa      => 'Int',
                    init_arg => 'port',
                    default  => 0         # random
    );
    my %_sock_types = (4 => '0.0.0.0', 6 => '::');
    for my $ipv (keys %_sock_types) {
        has 'udp'
            . $ipv => (is         => 'ro',
                       init_arg   => undef,
                       isa        => 'Object',
                       lazy_build => 1,
                       predicate  => '_has_udp' . $ipv
            );
        has 'udp' 
            . $ipv
            . '_sock' => (is         => 'ro',
                          init_arg   => undef,
                          isa        => 'GlobRef',
                          lazy_build => 1,
                          weak_ref   => 1,
                          writer     => '_udp' . $ipv . '_sock',
                          predicate  => '_has_udp' . $ipv . '_sock'
            );
        has 'udp' 
            . $ipv
            . '_host' => (is        => 'ro',
                          isa       => 'Str',
                          default   => $_sock_types{$ipv},
                          writer    => '_udp' . $ipv . '_host',
                          predicate => '_has_udp' . $ipv . '_host'
            );
        has 'udp' . $ipv . '_port' => (
            is        => 'ro',
            isa       => 'Int',
            writer    => '_udp' . $ipv . '_port',
            predicate => '_has_udp' . $ipv . '_port',
            default   => 0,
            trigger   => sub {
                my ($self, $new, $old) = @_;
                if (defined $old && $old != $new) {

                    # XXX - Something's not right.
                }
            }
        );
    }
    {

        sub _build_udp6 {
            my ($self) = @_;
            my $port
                = $self->_has_udp6_port ? $self->udp6_port
                : $self->_has_udp4_port ? $self->udp4_port
                :                         $self->_port;
            warn $port;
            return server(
                $self->udp6_host,
                $port,
                sub { $self->_on_udp6_in(@_); },
                sub {
                    my ($sock, $host, $port) = @_;
                    if ($self->udp6_port && $self->udp6_port != $port) {
                        ...;
                    }
                    $self->_udp6_sock($sock);
                    $self->_udp6_host($host);
                    $self->_udp6_port($port);
                    warn 'udp6 :' . $port;
                },
                'udp'
            );
        }

        sub _build_udp4 {
            my ($self) = @_;
            my $port
                = $self->_has_udp4_port ? $self->udp4_port
                : $self->_has_udp6_port ? $self->udp6_port
                :                         $self->_port;
            warn $port;
            return server(
                $self->udp4_host,
                $port,
                sub { $self->_on_udp4_in(@_); },
                sub {
                    my ($sock, $host, $port) = @_;
                    if ($self->udp4_port && $self->udp4_port != $port) {
                        ...;
                    }
                    $self->_udp4_sock($sock);
                    $self->_udp4_host($host);
                    $self->_udp4_port($port);
                    warn 'udp4 :' . $port;
                },
                'udp'
            );
        }
    }
}
1;
