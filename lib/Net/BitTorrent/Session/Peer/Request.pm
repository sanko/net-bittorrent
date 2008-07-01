package Net::BitTorrent::Session::Peer::Request;
use strict;
use warnings;
{

    BEGIN {
        use version qw[qv];
        our $SVN
            = q[$Id$];
        our $VERSION = sprintf q[%.3f], version->new(qw$Rev$)->numify / 1000;
    }
    use Net::BitTorrent::Util qw[:log];
    {
        my (%peer, %index, %offset, %length, %timestamp);

        sub new {
            my ($class, $args) = @_;
            my $self = undef;
            if (    defined $args->{q[peer]}
                and defined $args->{q[index]}
                and defined $args->{q[offset]}
                and defined $args->{q[length]})
            {   $self =
                    bless \sprintf(q[R I:%d:O:%d:L:%d],
                                   $args->{q[index]}, $args->{q[offset]},
                                   $args->{q[length]}),
                    $class;
                $peer{$self}      = $args->{q[peer]};
                $index{$self}     = $args->{q[index]};
                $offset{$self}    = $args->{q[offset]};
                $length{$self}    = $args->{q[length]};
                $timestamp{$self} = time;
            }
            return $self;
        }

        sub get_peer {
            my ($self) = @_;
            $peer{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            return $peer{$self};
        }

        sub get_session {
            my ($self) = @_;
            $peer{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            return $peer{$self}->get_session;
        }

        sub get_client {
            my ($self) = @_;
            $peer{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            return $peer{$self}->get_client;
        }

        sub get_index {
            my ($self) = @_;
            $peer{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            return $index{$self};
        }

        sub get_offset {
            my ($self) = @_;
            $peer{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            return $offset{$self};
        }

        sub get_length {
            my ($self) = @_;
            $peer{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            return $length{$self};
        }

        sub get_timestamp {
            my ($self) = @_;
            $peer{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            return $timestamp{$self};
        }

        sub get_piece {
            my ($self) = @_;
            $peer{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            return $peer{$self}->get_session->get_pieces->[$index{$self}];
        }

        sub _build_packet_args {    # unused
            my ($self) = @_;
            $peer{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            return (index  => $index{$self},
                    offset => $offset{$self},
                    length => $length{$self},
                    data   => $self->_read
            );
        }

        sub _read {
            my ($self) = @_;
            $peer{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            return $self->get_piece->_read($offset{$self}, $length{$self});
        }

        sub as_string {
            my ($self, $advanced) = @_;
            $peer{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
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

Net::BitTorrent::Session::Peer::Request - Incoming request

=head1 Constructor

=over 4

=item C<new ( { [ARGS] } )>

Creates a C<Net::BitTorrent::Session::Peer::Request> object.  This
constructor should not be used directly.

=back

=head1 Methods

=over 4

=item C<as_string ( [ VERBOSE ] )>

Returns a 'ready to print' dump of the
C<Net::BitTorrent::Session::Peer::Request> object's data structure.
If called in void context, the structure is printed to C<STDERR>.

See also: L<Net::BitTorrent|Net::BitTorrent/"as_string ( [ VERBOSE ] )">

=item C<get_client ( )>

Returns the L<Net::BitTorrent|Net::BitTorrent> object related to this
request.

=item C<get_index ( )>

Returns the zero based index of the related
L<Net::BitTorrent::Session::Piece|Net::BitTorrent::Session::Piece>
object.

=item C<get_length ( )>

Returns the amount of data the peer requested.

=item C<get_offset ( )>

Returns the offset of data the peer requested.

=item C<get_peer ( )>

Returns the
L<Net::BitTorrent::Session::Peer|Net::BitTorrent::Session::Peer>
object related to this request.

=item C<get_piece ( )>

Returns the
L<Net::BitTorrent::Session::Piece|Net::BitTorrent::Session::Piece>
object related to this request.

=item C<get_session ( )>

Returns the L<Net::BitTorrent::Session|Net::BitTorrent::Session>
object related to this request.

=item C<get_timestamp ( )>

Returns the time when the request was made.

=back

=head1 Author

Sanko Robinson <sanko@cpan.org> - http://sankorobinson.com/

CPAN ID: SANKO

=head1 License and Legal

Copyright 2008 by Sanko Robinson E<lt>sanko@cpan.orgE<gt>

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl 5.10 (or higher).  See
http://www.perl.com/perl/misc/Artistic.html or the F<LICENSE> file
included with this distribution.

All POD documentation is covered by the Creative Commons Attribution-
Noncommercial-Share Alike 3.0 License
(http://creativecommons.org/licenses/by-nc-sa/3.0/us/).

Neither this module nor the L<Author|/Author> is affiliated with
BitTorrent, Inc.


=for svn $Id$

=cut
