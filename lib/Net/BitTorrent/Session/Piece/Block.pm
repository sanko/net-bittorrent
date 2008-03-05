{

    package Net::BitTorrent::Session::Piece::Block;

    BEGIN {
        use vars qw[$VERSION];
        use version qw[qv];
        our $SVN = q[$Id$];
        our $VERSION = sprintf q[%.3f], version->new(qw$Rev$)->numify / 1000;
    }

    use strict;
    use warnings 'all';
    use Scalar::Util qw[/weak/];
    use Carp qw[carp croak];

    {    # constructor
        my (%offset, %length, %piece, %peer);

        sub new {
            my ($class, $args) = @_;
            my $self = undef;
            if (    defined $args->{q[piece]}
                and defined $args->{q[offset]}
                and defined $args->{q[length]})
            {
                $self = bless \sprintf(
                    q[B I:%d:O:%d:L:%d],
                    $args->{q[piece]}->index,
                    $args->{q[offset]}, $args->{q[length]}
                ), $class;
                $length{$self} = $args->{q[length]};
                $offset{$self} = $args->{q[offset]};
                $piece{$self}  = $args->{q[piece]};
            }
            return $self;
        }

        sub piece   { return $piece{+shift}; }
        sub session { return $piece{+shift}->session; }
        sub client  { return $piece{+shift}->client; }
        sub index   { return $piece{+shift}->index; }
        sub offset  { return $offset{+shift}; }
        sub length  { return $length{+shift}; }

        sub peers {
            my ($self) = @_;
            wantarray
              ? grep { defined } map { $_->{q[peer]} } values %{$peer{$self}}
              : grep { defined $_->{q[peer]} } values %{$peer{$self}};
        }

        sub add_peer {
            my ($self, $peer) = @_;
            $peer{$self}{$peer} = {peer => $peer, timestamp => time};
            return weaken $peer{$self}{$peer}{q[peer]};
        }

        sub remove_peer {
            my ($self, $peer) = @_;
            return delete $peer{$self}{$peer};
        }

        sub request_timestamp {
            my ($self, $peer) = @_;
            return $peer{$self}{$peer}{q[timestamp]};
        }

        sub build_packet_args {
            my ($self) = @_;
            return (
                index  => $piece{$self}->index,
                offset => $offset{$self},
                length => $length{$self}
            );

        }

        sub write {
            my ($self) = @_;
            $self->client->do_callback(q[block_write], $self);
            return $piece{$self}->write($_[1], $offset{$self});
        }

        sub as_string { die; }

        DESTROY {
            my ($self) = @_;
            delete $offset{$self};
            delete $length{$self};
            delete $piece{$self};
            delete $peer{$self};
            return 1;
        }
    }
    1;
}
