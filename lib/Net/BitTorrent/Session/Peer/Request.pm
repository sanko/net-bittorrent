package Net::BitTorrent::Session::Peer::Request;
use strict;
use warnings;
{

    BEGIN {
        use vars qw[$VERSION];
        use version qw[qv];
        our $SVN
            = q[$Id: Request.pm 3 2008-03-16 05:46:16Z sanko@cpan.org $];
        our $VERSION = sprintf q[%.3f], version->new(qw$Rev: 3 $)->numify / 1000;
    }
    use Carp qw[carp croak];
    {
        my ( %peer, %index, %offset, %length, %timestamp );

        sub new {
            my ( $class, $args ) = @_;
            my $self = undef;
            if (     defined $args->{q[peer]}
                 and defined $args->{q[index]}
                 and defined $args->{q[offset]}
                 and defined $args->{q[length]} )
            {
                $self =
                    bless \sprintf( q[R I:%d:O:%d:L:%d],
                                $args->{q[index]}, $args->{q[offset]},
                                $args->{q[length]} ),
                    $class;
                $peer{$self}      = $args->{q[peer]};
                $index{$self}     = $args->{q[index]};
                $offset{$self}    = $args->{q[offset]};
                $length{$self}    = $args->{q[length]};
                $timestamp{$self} = time;
            }
            return $self;
        }
        sub peer    { my ($self) = @_; return $peer{$self} }
        sub session { my ($self) = @_; return $peer{$self}->session }

        sub client {
            my ($self) = @_;
            return $peer{$self}->session->client;
        }
        sub index     { my ($self) = @_; return $index{$self} }
        sub offset    { my ($self) = @_; return $offset{$self} }
        sub length    { my ($self) = @_; return $length{$self} }
        sub timestamp { my ($self) = @_; return $timestamp{$self} }

        sub piece {
            my ($self) = @_;
            return $peer{$self}->session->pieces->[ $index{$self} ];
        }

        sub _build_packet_args {    # unused
            my ($self) = @_;
            return ( index  => $index{$self},
                     offset => $offset{$self},
                     length => $length{$self},
                     data   => $self->_read
            );
        }

        sub _read {
            my ($self) = @_;
            return $self->piece->_read( $offset{$self},
                                       $length{$self} );
        }

        sub as_string {
            my ( $self, $advanced ) = @_;
            my $dump = $self . q[ [TODO]];
            return print STDERR qq[$dump\n] unless defined wantarray;
            return $dump;
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

}
1;
__END__

=pod

=head1 NAME

Net::BitTorrent::Session::Peer::Request - BitTorrent client class

=head1 DESCRIPTION

TODO

=head1 METHODS

=over 4

=item C<peer ( )>

TODO

=item C<piece ( )>

TODO

=item C<session ( )>

TODO

=item C<client ( )>

TODO

=item C<index ( )>

TODO

=item C<offset ( )>

TODO

=item C<length ( )>

TODO

=back

=head1 AUTHOR

Sanko Robinson <sanko@cpan.org> - [http://sankorobinson.com/]

=head1 LICENSE AND LEGAL

Copyright 2008 by Sanko Robinson E<lt>sanko@cpan.orgE<gt>

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

See [http://www.perl.com/perl/misc/Artistic.html] or the LICENSE file
included with this module.

Neither this module nor the L<AUTHOR|/AUTHOR> is affiliated with
BitTorrent, Inc.

=for svn $Id$

=cut
