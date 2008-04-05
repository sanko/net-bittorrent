package Net::BitTorrent::Session::Piece;
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
    use Carp qw[carp cluck];
    use Net::BitTorrent::Session::Piece::Block;
    {    # constructor
        my ( %hash, %index, %session, %blocks, %check,
        %priority, %working, %previous_incoming_block );

        sub new {
            my ( $class, $args ) = @_;
            my $self = undef;
            if (     defined $args->{q[session]}
                 and defined $args->{q[hash]}
                 and defined $args->{q[index]} )
            {
                $self = bless \$args->{q[hash]}, $class;
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
        sub hash    { my ($self) = @_; $hash{$self}; }
        sub index   { my ($self) = @_; $index{$self}; }
        sub session { my ($self) = @_; $session{$self}; }
        sub client  { my ($self) = @_; $session{$self}->client; }
        sub blocks  { my ($self) = @_; $blocks{$self}; }
        sub check   { my ($self) = @_; $check{$self}; }

        sub _previous_incoming_block { # unused
            my ( $self, $value ) = @_;
            return ( defined $value
                     ? $previous_incoming_block{$self}
                         = $value
                     : $previous_incoming_block{$self}
            );
        }

        sub priority {
            my ( $self, $value ) = @_;
            return ( defined $value
                     ? $priority{$self}
                         = $value
                     : $priority{$self}
            );
        }

        sub working {
            my ( $self, $value ) = @_;
            return (
                defined $value
                ? do {
                    cluck(q[working is malformed]) and return
                        unless $value =~ m[^[10]$];
                    $working{$self} = $value;
                    if ( $value and not $blocks{$self} ) {
                        my $_offset = 0;
                        my $_length = (
                                $self->index
                                    == $self->session->piece_count
                                ? ( $self->size
                                    % $self->session->piece_size )
                                : $self->session->piece_size
                        );
                        my $_block_length
                            = Net::BitTorrent::Util::min(
                                           $self->session->block_size,
                                           $_length );
                        return if $_block_length < 1;
                        for my $_int (
                                 0 .. int( $_length / $_block_length )
                                 - 1 )
                        {
                            $blocks{$self}{$_offset}
                                = Net::BitTorrent::Session::Piece::Block
                                ->new( { offset => $_offset,
                                         length => $_block_length,
                                         piece  => $self
                                       }
                                );
                            $_offset += $_block_length;
                        }
                        $blocks{$self}{$_offset}
                            = Net::BitTorrent::Session::Piece::Block
                            ->new(
                                 { offset => $_offset,
                                   length =>
                                       ( $_length % $_block_length ),
                                   piece => $self
                                 }
                            ) if $_length % $_block_length;
                        $previous_incoming_block{$self} = time  # lies
                    }

                    #elsif (not $value and $blocks{$self}) {
                    #    delete $blocks{$self};
                    #}
                    }
                : $working{$self}
            );
        }

        sub size {
            my ($self) = @_;
            return ( $self->index == $self->session->piece_count )
                ? ( $self->session->total_size
                    % $self->session->piece_size )
                : $self->session->piece_size;
        }

        sub _read {
            my ( $self, $offset, $length ) = @_;
            carp(q[Bad length!])
                if defined $length
                    and (    $length !~ m[^\d+$]
                          or $length > $self->session->piece_size );
            carp(q[Bad offset!])
                if defined $offset and $offset !~ m[^\d+$];
            my $_RETURN = q[];
            my $_LENGTH = defined $length ? $length : $self->size;
            my $pos
                = int(
                     ( ( $self->index * $self->session->piece_size ) )
                     + ( $offset || 0 ) );
            if ( ( $pos + $_LENGTH ) > $self->session->total_size ) {
                return;
            }
            my $f = 0;
            while ( $pos > $self->session->files->[$f]->size ) {
                $pos -= $self->session->files->[ $f++ ]->size;
                last if not defined $self->session->files->[$f];
            }
            while ( $_LENGTH > 0 ) {
                my $this_read
                    = ( $pos + $_LENGTH
                        > $self->session->files->[$f]->size )
                    ? $self->session->files->[$f]->size - $pos
                    : $_LENGTH;
                $self->session->files->[$f]->_open(q[r]) or return;
                $self->session->files->[$f]->_seek($pos) or return;

                $_RETURN
                    .= $self->session->files->[$f]->_read($this_read)
                    or return;
                $f++;
                $pos = 0;
                $_LENGTH -= $this_read;
            }
            return $_RETURN;
        }

        sub _write {
            my ( $self, $data, $offset ) = @_;
            my $f = 0;
            $offset ||=0;
            if ( length($data) + $offset > $self->size ) { return; }
            my $pos
                = int(
                     ( ( $self->index * $self->session->piece_size ) )
                     + ( $offset || 0 ) );
            while ( $pos > $self->session->files->[$f]->size ) {
                $pos -= $self->session->files->[$f]->size;
                $f++;
            }
            while ( length $data > 0 ) {
                my $this_write
                    = ( $pos + length $data
                        > $self->session->files->[$f]->size )
                    ? $self->session->files->[$f]->size - $pos
                    : length $data;
                $self->session->files->[$f]->_open(q[w]) or return;
                $self->session->files->[$f]->_seek($pos) or return;
                $self->session->files->[$f]
                    ->_write( substr( $data, 0, $this_write, q[] ) )
                    or return;
                $f++;
                $pos = 0;
            }
            return 1;
        }

        sub verify {
            my ($self) = @_;
            delete $blocks{$self};
            $working{$self} = 0;
            if ( Digest::SHA::sha1( $self->_read ) eq $self->hash ) {
                my $old_value = $self->check;
                $check{$self} = 1;
                $session{$self}
                    ->client->_do_callback( q[piece_hash_pass],
                                            $self )
                    if not $old_value
                ;    # no point telling us the piece is good twice
                $session{$self}->_check_endgame_status;
                return 1;
            }

            # failed
            # TODO: penalize all peers related to piece
            $session{$self}
                ->client->_do_callback( q[piece_hash_fail], $self );
            return 0;
        }

        sub _unrequested_block {
            my ($self) = @_;
            return if not $working{$self};
            for my $block (
                sort {
                    scalar $a->peers <=> scalar $b->peers
                } values %{ $blocks{$self} }
                )
            {
                if ( $session{$self}->_endgame ) { return $block; }
                if ( not scalar $block->peers ) { return $block; }

    # TODO: check for peer() instead of requested() but remember to...
    #       ...ignore both during endgame.
            }
            return;
        }

        sub as_string {
            my ( $self, $advanced ) = @_;
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

=head1 CONSTRUCTOR

=over 4

=item C<new ( { [ARGS] } )>

Creates a C<Net::BitTorrent::Session::Piece> object.  This constructor
should not be used directly.

=back

=head1 METHODS

=over 4

=item C<as_string ( [ VERBOSE ] )>

Returns a 'ready to print' dump of the
C<Net::BitTorrent::Session::Piece> object's data structure.  If called
in void context, the structure is printed to C<STDERR>.

See also: [id://317520],
L<Net::BitTorrent::as_string()|Net::BitTorrent/as_string ( [ VERBOSE ] )>

=item C<blocks ( )>

Returns a hash of key/value pairs for each
L<Net::BitTorrent::Session::Piece::Block|Net::BitTorrent::Session::Piece::Block>
object related to this piece.  The keys of this hash are
L<offsets|Net::BitTorrent::Session::Piece::Block/offset ( )>.

If this piece is not marked L<working|/working ( )>, C<undef> is the
return value.

=item C<check ( )>

Returns a cached boolean value indicating whether or not this piece
passes hash checking.  This value is cached to save time; to be sure
that this value is accurate, use L<verify ( )|/verify ( )>.

=item C<client ( )>

Returns the L<Net::BitTorrent|Net::BitTorrent> object related to this
file.

=item C<hash ( )>

Returns the 20-byte SHA1 hash used to verify the contents of this
piece.

See also: L<verify ( )|/verify ( )>

=item C<index ( )>

Returns the zero based index of this piece according to the related
L<Net::BitTorrent::Session|Net::BitTorrent::Session> object.

=item C<priority ( [NEWVAL] )>

Mutator to set/get the download priority of this piece.

By default, all pieces begin with a priority of two (C<2>).

See also:
L<Net::BitTorrent::Session::File::priority ( )|Net::BitTorrent::Session::File/priority ( [NEWVAL] )>

=item C<session ( )>

Returns the L<Net::BitTorrent::Session|Net::BitTorrent::Session>
object related to this file.

=item C<size ( )>

Returns the size of the piece represented by this object.

=item C<verify ( )>

Verifies data integrity of this piece by checking against the SHA1
hash.

See also: L<check ( )|/check ( )>, L<hash ( )|/hash ( )>

=item C<working ( )>

Returns a boolean value indicating whether or not we are actively
requesting L<blocks|Net::BitTorrent::Session::Piece::Block> from this
piece.

=back

=head1 AUTHOR

Sanko Robinson <sanko@cpan.org> - L<http://sankorobinson.com/>

CPAN ID: SANKO

ProperNoun on Freenode

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
