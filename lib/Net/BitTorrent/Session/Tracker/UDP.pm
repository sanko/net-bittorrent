package Net::BitTorrent::Session::Tracker::UDP;
{
    use strict;      # core as of perl 5
    use warnings;    # core as of perl 5.006

    #
    use Carp qw[confess];                      # core as of perl 5
    use Scalar::Util qw[blessed weaken];    # core since perl 5.007003

    #
    my (%url, %tier);                       # param to new()
    my (%socket);

    #
    sub new {
        my ($class, $args) = @_;
        my $self;
        if (not defined $args) {
            confess __PACKAGE__ . q[->new() requires params];
            return;
        }
        if (not defined $args->{q[URL]}) {
            confess __PACKAGE__ . q[->new() requires a 'URL' param];
            return;
        }
        if ($args->{q[URL]} !~ m[^udp://]i) {
            confess
                sprintf(
                  q[%s->new() doesn't know what to do with malformed url: %s],
                  __PACKAGE__, $args->{q[URL]});
            return;
        }
        if (not defined $args->{q[Tier]}) {
            confess __PACKAGE__ . q[->new() requires a 'Tier' param];
            return;
        }
        if (not $args->{q[Tier]}->isa(q[Net::BitTorrent::Session::Tracker])) {
            confess __PACKAGE__ . q[->new() requires a blessed Tracker 'Tier'];
            return;
        }

        #
        $self = bless \$args->{q[URL]}, $class;

        #
        $url{$self}  = $args->{q[URL]};
        $tier{$self} = $args->{q[Tier]};
        weaken $tier{$self};

        #
        return $self;
    }

    sub _announce {
        my ($self, $event) = @_;
        if (defined $event) {
            if ($event !~ m[([started|stopped|complete])]) {
                confess sprintf q[Invalid event for announce: %s], $event;
                return;
            }
        }
        warn sprintf q[UDP!!!!!!!!!!!!!!!!!!!! | %s|%s], $self, $event;
    }

    #
    DESTROY {
        my ($self) = @_;

        #
        delete $tier{$self};
        delete $url{$self};
        delete $socket{$self};

        #
        return 1;
    }

    #
    1;
}
