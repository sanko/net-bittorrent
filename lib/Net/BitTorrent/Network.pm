package Net::BitTorrent::Network;
{
    use Moose;
    use AnyEvent;
    our $MAJOR = 0.075; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    #
    my %_sock_types = (ipv4 => '0.0.0.0', ipv6 => '::');
    for my $type (keys %_sock_types) {
        has $type => (is         => 'ro',
                      init_arg   => undef,
                      isa        => 'Object',
                      lazy_build => 1
        );
        has $type
            . '_sock' => (is         => 'ro',
                          init_arg   => undef,
                          isa        => 'GlobRef',
                          lazy_build => 1,
                          weak_ref   => 1,
                          writer     => '_' . $type . '_sock'
            );
        has $type
            . '_host' => (is      => 'ro',
                          isa     => 'Str',
                          default => $_sock_types{$type},
                          writer  => '_' . $type . '_host'
            );
        has $type . '_port' => (
            is      => 'ro',
            isa     => 'Int',
            default => $_sock_types{$type},
            writer  => '_' . $type . '_port',
            default => sub {0},                 # random
            trigger => sub {
                my ($self, $new, $old) = @_;
                if (defined $old && $old != $new) {

                    # XXX - Something's not right.
                }
            }
        );
        has $type . '_on_data_in' => (
            is      => 'rw',
            isa     => 'CodeRef',
            traits  => ['Code'],
            default => sub {
                sub {1}
            },
            handles =>
                {'trigger_' . $type . '_on_data_in' => 'execute_method'}
        );
    }

    sub BUILD {
        my ($self, $args) = @_;
        if (defined $args->{'port'}) {
            $self->_ipv4_port($args->{'port'});
            $self->_ipv6_port($args->{'port'});
        }
        if (defined $args->{'on_data_in'})
        {    # XXX - Separate callbacks are undocumented
            $self->_ipv4_on_data_in($args->{'on_data_in'});
            $self->_ipv6_on_data_in($args->{'on_data_in'});
        }
        {    # Non-blocking socket creation
            my @sock_types = grep {
                my ($ipv) = (m[(ipv\d)$]);
                $args->{'disable_' . $ipv}    # !!! - Disable IPv4 or IPv6
                    ? ()
                    : $_
            } keys %_sock_types;
            my $cv = AnyEvent->condvar;
            $cv->begin;
            my (@watchers, $coderef);
            $coderef = sub {
                shift @watchers if @watchers;
                my $sock_type = shift @sock_types;
                $self->$sock_type();
                push @watchers,
                    AE::idle(@sock_types ? $coderef : sub { $cv->end });
            };
            push @watchers, AE::idle($coderef);
            $cv->recv;
            shift @watchers;
        }
    }
    no Moose;
    __PACKAGE__->meta->make_immutable;
}
1;
