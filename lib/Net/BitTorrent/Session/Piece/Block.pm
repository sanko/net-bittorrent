package Net::BitTorrent::Session::Piece::Block;
use strict;
use warnings;
{

    BEGIN {
        use vars qw[$VERSION];
        use version qw[qv];
        our $SVN
            = q[$Id$];
        our $VERSION = sprintf q[%.3f], version->new(qw$Rev$)->numify / 1000;
    }
    use Scalar::Util qw[/weak/];
    use Carp qw[carp];
    use Net::BitTorrent::Util qw[:log];
    {
        my (%offset, %length, %piece, %peer);

        sub new {
            my ($class, $args) = @_;

            my $self = undef;
            if (    defined $args->{q[piece]}
                and defined $args->{q[offset]}
                and defined $args->{q[length]})
            {   $self =
                    bless \sprintf(q[B I:%d:O:%d:L:%d],
                                   $args->{q[piece]}->index,
                                   $args->{q[offset]},
                                   $args->{q[length]}
                    ),
                    $class;
                $length{$self} = $args->{q[length]};
                $offset{$self} = $args->{q[offset]};
                $piece{$self}  = $args->{q[piece]};
            }
            return $self;
        }

        sub piece {
            my ($self) = @_;
            $piece{$self}->client->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
            );
            return $piece{$self};
        }

        sub session {
            my ($self) = @_;
            $piece{$self}->client->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
            );
            return $piece{$self}->session;
        }

        sub client {
            my ($self) = @_;
            $piece{$self}->client->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
            );
            return $piece{$self}->client;
        }

        sub index {
            my ($self) = @_;
            $piece{$self}->client->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
            );
            return $piece{$self}->index;
        }

        sub offset {
            my ($self) = @_;
            $piece{$self}->client->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
            );
            return $offset{$self};
        }

        sub length {
            my ($self) = @_;
            $piece{$self}->client->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
            );
            return $length{$self};
        }

        sub peers {
            my ($self) = @_;
            wantarray
                ? grep {defined}
                map    {$_->{q[peer]}} values %{$peer{$self}}
                : grep {defined $_->{q[peer]}} values %{$peer{$self}};
        }

        sub _add_peer {
            my ($self, $peer) = @_;
            $piece{$self}->client->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
            );
            $peer{$self}{$peer} = {peer => $peer, timestamp => time};
            return weaken $peer{$self}{$peer}{q[peer]};
        }

        sub _remove_peer {
            my ($self, $peer) = @_;
            $piece{$self}->client->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
            );
            return delete $peer{$self}{$peer};
        }

        sub _request_timestamp {
            my ($self, $peer) = @_;
            $piece{$self}->client->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
            );
            return $peer{$self}{$peer}{q[timestamp]};
        }

        sub _build_packet_args {
            my ($self) = @_;
            $piece{$self}->client->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
            );
            return (index  => $piece{$self}->index,
                    offset => $offset{$self},
                    length => $length{$self}
            );
        }

        sub _write {
            my ($self) = @_;
            $piece{$self}->client->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
            );
            $self->client->_do_callback(q[block_write], $self);
            return $piece{$self}->_write($_[1], $offset{$self});
        }

        sub as_string {
            my ($self, $advanced) = @_;
            $piece{$self}->client->_do_callback(
                                       q[log], TRACE,
                                       sprintf(q[Entering %s for %s],
                                               [caller 0]->[3], $$self
                                       )
            );
            my $dump = $self . q[ [TODO]];
            return print STDERR qq[$dump\n] unless defined wantarray;
            return $dump;
        }
        DESTROY {
            my ($self) = @_;
            delete $offset{$self};
            delete $length{$self};
            delete $piece{$self};
            delete $peer{$self};
            return 1;
        }
    }
}
1;
__END__

=pod

=head1 NAME

Net::BitTorrent::Session::Piece::Block - Outgoing request

=head1 CONSTRUCTOR

=over 4

=item C<new ( { [ARGS] } )>

Creates a C<Net::BitTorrent::Session::Piece::Block> object.  This
constructor should not be used directly.

=back

=head1 METHODS

=over 4

=item C<as_string ( [ VERBOSE ] )>

Returns a 'ready to print' dump of the
C<Net::BitTorrent::Session::Piece::Block> object's data structure.  If
called in void context, the structure is printed to C<STDERR>.

See also: [id://317520],
L<Net::BitTorrent::as_string()|Net::BitTorrent/as_string ( [ VERBOSE ] )>

=item C<client ( )>

Returns the L<Net::BitTorrent|Net::BitTorrent> object related to this
block.

=item C<index ( )>

Returns the zero based index of the related
L<Net::BitTorrent::Session::Piece|Net::BitTorrent::Session::Piece>
object.

=item C<length ( )>

Returns the amount of data covered by the block.

=item C<offset ( )>

Returns the offset of the block in reference to the beginning of the
L<piece|Net::BitTorrent::Session::Piece>.

=item C<peers ( )>

Returns a list of all related
L<Net::BitTorrent::Session::Peer|Net::BitTorrent::Session::Peer>
objects.  Unless the L<session|Net::BitTorrent::Session> is in endgame
mode, the list will contain a single peer.

=item C<piece ( )>

Returns the
L<Net::BitTorrent::Session::Piece|Net::BitTorrent::Session::Piece>
object related to this piece.

=item C<session ( )>

Returns the L<Net::BitTorrent::Session|Net::BitTorrent::Session>
object related to this block.

=back

=head1 AUTHOR

Sanko Robinson <sanko@cpan.org> - L<http://sankorobinson.com/>

CPAN ID: SANKO

=head1 LICENSE AND LEGAL

Copyright 2008 by Sanko Robinson E<lt>sanko@cpan.orgE<gt>

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.  See
L<http://www.perl.com/perl/misc/Artistic.html> or the F<LICENSE> file
included with this module.

All POD documentation is covered by the Creative Commons
Attribution-Noncommercial-Share Alike 3.0 License
(L<http://creativecommons.org/licenses/by-nc-sa/3.0/us/>).

Neither this module nor the L<AUTHOR|/AUTHOR> is affiliated with
BitTorrent, Inc.

=for svn $Id$

=cut
