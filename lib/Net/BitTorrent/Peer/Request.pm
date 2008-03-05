{

    package Net::BitTorrent::Peer::Request;

    BEGIN {

        use vars qw[$VERSION];
        use version qw[qv];
        our $SVN = q[$Id$];
        our $VERSION = sprintf q[%.3f], version->new(qw$Rev$)->numify / 1000;
    }

    use strict;
    use warnings 'all';
    use Carp qw[carp croak];

    {
        my (%peer, %index, %offset, %length, %timestamp);

        sub new {
            my ($class, $args) = @_;
            my $self = undef;
            if (    defined $args->{q[peer]}
                and defined $args->{q[index]}
                and defined $args->{q[offset]}
                and defined $args->{q[length]})
            {

                $self = bless \sprintf(q[R I:%d:O:%d:L:%d],
                    $args->{q[index]}, $args->{q[offset]}, $args->{q[length]}),
                  $class;

                $peer{$self}      = $args->{q[peer]};
                $index{$self}     = $args->{q[index]};
                $offset{$self}    = $args->{q[offset]};
                $length{$self}    = $args->{q[length]};
                $timestamp{$self} = time;
            }
            return $self;
        }

        sub peer      { return $peer{+shift} }
        sub session   { return $peer{+shift}->session }
        sub client    { return $peer{+shift}->session->client }
        sub index     { return $index{+shift} }
        sub offset    { return $offset{+shift} }
        sub length    { return $length{+shift} }
        sub timestamp { return $timestamp{+shift} }

        sub piece {
            my ($self) = @_;
            return $peer{$self}->session->pieces->[$index{$self}];
        }

        sub build_packet_args {    # unused
            my ($self) = @_;
            return (
                index  => $index{$self},
                offset => $offset{$self},
                length => $length{$self},
                data   => $self->read
            );
        }

        sub read {
            my ($self) = @_;
            return $self->piece->read($offset{$self}, $length{$self});
        }

        DESTROY {
            my ($self) = @_;
            delete $peer{$self};
            delete $index{$self};
            delete $offset{$self};
            delete $length{$self};
            delete $timestamp{$self};
            return 1;
        }
    }
    1;
}
