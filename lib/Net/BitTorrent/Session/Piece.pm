package Net::BitTorrent::Session::Piece;
use strict;
use warnings;
{

    BEGIN {
        use version qw[qv];
        our $SVN = q[$Id$];
        our $VERSION = sprintf q[%.3f], version->new(qw$Rev$)->numify / 1000;
    }
    use lib q[../../../../lib];
    use Net::BitTorrent::Util qw[:log];
    use Net::BitTorrent::Session::Piece::Block;
    {
        my (%hash, %index, %session, %blocks, %check, %priority, %working,
            %previous_incoming_block);

        sub new {
            my ($class, $args) = @_;
            my $self = undef;
            if (    defined $args->{q[session]}
                and defined $args->{q[hash]}
                and defined $args->{q[index]})
            {   $self = bless \$args->{q[hash]}, $class;
                $index{$self}   = $args->{q[index]};
                $session{$self} = $args->{q[session]};
                $hash{$self}    = $args->{q[hash]};

                # Set defaults
                $working{$self}                 = 0;
                $check{$self}                   = 0;
                $priority{$self}                = 2;
                $previous_incoming_block{$self} = time;    # lies
            }
            return $self;
        }

        sub get_hash {
            my ($self) = @_;
            die if $_[1];
            $session{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            return $hash{$self};
        }

        sub get_index {
            my ($self) = @_;
            die if $_[1];
            $session{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            return $index{$self};
        }

        sub get_session {
            my ($self) = @_;
            die if $_[1];
            $session{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            return $session{$self};
        }

        sub get_client {
            my ($self) = @_;
            die if $_[1];
            $session{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            return $session{$self}->get_client;
        }

        sub get_blocks {
            my ($self) = @_;
            die if $_[1];
            $session{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            return $blocks{$self};
        }

        sub get_cached_integrity {
            my ($self) = @_;
            die if $_[1];
            $session{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            return $check{$self};
        }

        sub _set_previous_incoming_block {
            my ($self, $value) = @_;
            $session{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            if ((not defined $value) or ($value !~ m[^-?\d+\.?\d*$])) {
                $self->_do_callback(
                    q[log], FATAL,
                    sprintf(
                        q[new value for _set_previous_incoming_block is malformed (%s)],
                        $value || q[undef])
                );

                # confess;
            }
            return $previous_incoming_block{$self} = $value;
        }

        sub _get_previous_incoming_block {    # unused?
            die if $_[1];
            return $previous_incoming_block{$_[0]};
        }

        sub set_priority {
            my ($self, $value) = @_;
            $session{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            if ((not defined $value) or ($value !~ m[^-?\d+\.?\d*$])) {
                $self->_do_callback(
                    q[log], WARN,
                    sprintf(
                        q[new value for priority is malformed (%s).  Requires integer.],
                        $value || q[undef])
                );
                return;
            }
            return $priority{$self} = $value;
        }

        sub get_priority {
            die if $_[1];
            return $priority{$_[0]};
        }

        sub set_working {
            my ($self, $value) = @_;
            $session{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            if ((not defined $value) or ($value !~ m[^[10]$])) {
                $self->_do_callback(
                    q[log], WARN,
                    sprintf(
                        q[new value for priority is malformed (%s).  Requires boolean.],
                        $value || q[undef])
                );
                return;
            }
            $working{$self} = $value;
            if ($value and not $blocks{$self}) {
                my $_offset = 0;
                my $_length = (
                      $self->get_index == $self->get_session->get_piece_count
                      ? ($self->get_size % $self->get_session->get_piece_size)
                      : $self->get_session->get_piece_size
                );
                my $_block_length =
                    Net::BitTorrent::Util::min(
                                           $self->get_session->get_block_size,
                                           $_length);
                return if $_block_length < 1;
                for my $_int (0 .. int($_length / $_block_length) - 1) {
                    $blocks{$self}{$_offset}
                        = Net::BitTorrent::Session::Piece::Block->new(
                                                 {offset => $_offset,
                                                  length => $_block_length,
                                                  piece  => $self
                                                 }
                        );
                    $_offset += $_block_length;
                }
                $blocks{$self}{$_offset}
                    = Net::BitTorrent::Session::Piece::Block->new(
                                    {offset => $_offset,
                                     length => ($_length % $_block_length),
                                     piece  => $self
                                    }
                    ) if $_length % $_block_length;
                $previous_incoming_block{$self} = time    # lies
            }
            elsif (    not $value
                   and $blocks{$self}
                   and $session{$self}->_get_endgame_status_status)
            {   $session{$self}->get_client->_do_callback(q[log], DEBUG,
                        q[Removal of blocks from slow piece during endgame.]);
                delete $blocks{$self};
            }
        }

        sub get_working {
            die if $_[1];
            return $working{$_[0]};
        }

        sub get_size {
            my ($self) = @_;
            die if $_[1];
            $session{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            return ($self->get_index == $self->get_session->get_piece_count)
                ? ($self->get_session->get_total_size
                   % $self->get_session->get_piece_size)
                : $self->get_session->get_piece_size;
        }

        sub _read {
            my ($self, $offset, $length) = @_;
            $session{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            $session{$self}
                ->get_client->_do_callback(q[log], ERROR, q[Bad length!])
                if defined $length
                    and (   $length !~ m[^\d+$]
                         or $length > $self->get_session->get_piece_size);
            $session{$self}
                ->get_client->_do_callback(q[log], WARN, q[Bad offset!])
                if defined $offset and $offset !~ m[^\d+$];
            my $_RETURN = q[];
            my $_LENGTH = defined $length ? $length : $self->get_size;
            my $pos
                = int(
                     (($self->get_index * $self->get_session->get_piece_size))
                     + ($offset || 0));
            if (($pos + $_LENGTH) > $self->get_session->get_total_size) {
                return;
            }
            my $f = 0;
            return
                if not defined $self->get_session->get_files->[$f]
            ;    # See Issue #1
        SEARCH:
            while ($pos > $self->get_session->get_files->[$f]->get_size) {
                $pos -= $self->get_session->get_files->[$f++]->get_size;
                last SEARCH
                    if not defined $self->get_session->get_files->[$f];
            }
        READ: while ($_LENGTH > 0) {
                my $this_read
                    = ($pos + $_LENGTH
                       > $self->get_session->get_files->[$f]->get_size)
                    ? $self->get_session->get_files->[$f]->get_size - $pos
                    : $_LENGTH;
                $self->get_session->get_files->[$f]->_open(q[r]) or return;
                $self->get_session->get_files->[$f]->_seek($pos) or return;
                $_RETURN
                    .= $self->get_session->get_files->[$f]->_read($this_read)
                    or return;
                $f++;
                last READ if not defined $self->get_session->get_files->[$f];
                $pos = 0;
                $_LENGTH -= $this_read;
            }
            return $_RETURN;
        }

        sub _write {
            my ($self, $data, $offset) = @_;
            $session{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            my $f = 0;
            $offset ||= 0;
            if (length($data) + $offset > $self->get_size) { return; }
            my $pos
                = int(
                     (($self->get_index * $self->get_session->get_piece_size))
                     + ($offset || 0));
            return
                if not defined $self->get_session->get_files->[$f]
            ;    # See Issue #1
        SEARCH:

            while ($pos > $self->get_session->get_files->[$f]->get_size) {
                $pos -= $self->get_session->get_files->[$f]->get_size;
                $f++;
                last SEARCH
                    if not defined $self->get_session->get_files->[$f]
                ;    # XXX - should this simply return?
            }
        WRITE: while (length $data > 0) {
                my $this_write
                    = ($pos + length $data
                       > $self->get_session->get_files->[$f]->get_size)
                    ? $self->get_session->get_files->[$f]->get_size - $pos
                    : length $data;
                $self->get_session->get_files->[$f]->_open(q[w]) or return;
                $self->get_session->get_files->[$f]->_seek($pos) or return;
                $self->get_session->get_files->[$f]
                    ->_write(substr($data, 0, $this_write, q[]))
                    or return;
                $f++;
                last WRITE if not defined $self->get_session->get_files->[$f];
                $pos = 0;
            }
            return 1;
        }

        sub get_verified_integrity {
            my ($self) = @_;
            die if $_[1];
            $session{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            delete $blocks{$self}; # XXX - force client to start from scratch?
            $working{$self} = 0;
            if (Digest::SHA::sha1($self->_read) eq $self->get_hash) {
                my $old_value = $check{$self};
                $check{$self} = 1;
                $session{$self}
                    ->get_client->_do_callback(q[piece_hash_pass], $self)
                    if not $old_value
                ;    # no point telling us the piece is good twice
                $session{$self}->_check_endgame_status;
                return 1;
            }

            # failed
            # TODO: penalize all peers related to piece
            $check{$self} = 0;
            $session{$self}
                ->get_client->_do_callback(q[piece_hash_fail], $self);
            return 0;
        }

        sub _locate_unrequested_block {
            my ($self) = @_;
            die if $_[1];
            $session{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            return if not $working{$self};
            for my $block (
                sort {
                    scalar $a->get_peers <=> scalar $b->get_peers
                } values %{$blocks{$self}}
                )
            {   if ($session{$self}->_get_endgame_status) { return $block; }
                if (not scalar $block->get_peers)  { return $block; }

            # TODO: check for peer() instead of requested() but remember to...
            #       ...ignore both during endgame.
            }
            return;
        }

        sub as_string {
            my ($self, $advanced) = @_;
            $session{$self}->get_client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            my $dump = $self . q[ [TODO]];
            return print STDERR qq[$dump\n] unless defined wantarray;
            return $dump;
        }
        DESTROY {
            my $self = shift;
            delete $hash{$self};
            delete $priority{$self};
            delete $index{$self};
            delete $working{$self};
            delete $blocks{$self};
            delete $check{$self};
            delete $session{$self};
            delete $previous_incoming_block{$self};
            return 1;
        }
    }
    1;
}
__END__

=pod

=head1 NAME

Net::BitTorrent::Session::Piece - Single piece

=head1 Constructor

=over 4

=item C<new ( { [ARGS] } )>

Creates a C<Net::BitTorrent::Session::Piece> object.  This constructor
should not be used directly.

=back

=head1 Methods

=over 4

=item C<as_string ( [ VERBOSE ] )>

Returns a 'ready to print' dump of the
C<Net::BitTorrent::Session::Piece> object's data structure.  If called
in void context, the structure is printed to C<STDERR>.

See also:
L<Net::BitTorrent|Net::BitTorrent/"as_string ( [ VERBOSE ] )">

=item C<get_blocks ( )>

Returns a hash of key/value pairs for each
L<Net::BitTorrent::Session::Piece::Block|Net::BitTorrent::Session::Piece::Block>
object related to this piece.  The keys of this hash are
L<offsets|Net::BitTorrent::Session::Piece::Block/"get_offset ( )">.

If this piece is not marked L<working|/"get_working ( )">, C<undef> is the
return value.

=item C<get_hash ( )>

Returns the 20-byte SHA1 hash used to verify the contents of this
piece.

See also: L<get_verified_integrity ( )|/"get_verified_integrity ( )">

=item C<get_cached_integrity ( )>

Returns a cached boolean value indicating whether or not this piece
passes hash checking.  This value is cached to save time; to be sure
that this value is accurate, use
L<get_verified_integrity ( )|/"get_verified_integrity ( )">.

=item C<get_verified_integrity ( )>

Verifies data integrity of this piece by checking against the SHA1
hash.

See also: L<get_cached_integrity ( )|/"get_cached_integrity ( )">,
L<get_hash ( )|/"get_hash ( )">

=item C<get_client ( )>

Returns the L<Net::BitTorrent|Net::BitTorrent> object related to this
file.

=item C<get_index ( )>

Returns the zero based index of this piece according to the related
L<Net::BitTorrent::Session|Net::BitTorrent::Session> object.

=item C<get_priority ( )>

Get the download priority of this piece.

See also:
L<Net::BitTorrent::Session::File|Net::BitTorrent::Session::File/"get_priority ( )">

=item C<set_priority ( [NEWVAL] )>

Set the download priority of this piece.

By default, all pieces begin with a priority of two (C<2>).

See also:
L<Net::BitTorrent::Session::File|Net::BitTorrent::Session::File/"set_priority ( )">

=item C<get_session ( )>

Returns the L<Net::BitTorrent::Session|Net::BitTorrent::Session> object
related to this file.

=item C<get_size ( )>

Returns the size of the piece represented by this object.

=item C<get_working ( )>

Returns a boolean value indicating whether or not we are actively
requesting L<blocks|Net::BitTorrent::Session::Piece::Block> from this
piece.

=item C<set_working ( NEWVAL )>

Sets a boolean value indicating whether or not we are actively requesting
L<blocks|Net::BitTorrent::Session::Piece::Block> from this piece.

I<NOTE: This is an advanced function that should not be used under normal
conditions.>

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
