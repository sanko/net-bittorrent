{

    package Net::BitTorrent::Session::Piece;

    BEGIN {
        use vars qw[$VERSION];
        use version qw[qv];
        our $SVN
            = q[$Id$];
        our $VERSION = sprintf q[%.3f], version->new(qw$Rev$)->numify / 1000;
    }
    use strict;
    use warnings 'all';
    use Carp qw[carp croak cluck];
    use lib q[../../../];
    use Net::BitTorrent::Session::Piece::Block;
    {    # constructor
        my ( %hash, %index, %session, %blocks, %check, %touch,
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
                $touch{$self}                   = 0;
                $priority{$self}                = 2;
                $previous_incoming_block{$self} = time;    # lies
            }
            return $self;
        }
        sub hash    { $hash{ +shift }; }
        sub index   { $index{ +shift }; }
        sub session { $session{ +shift }; }
        sub client  { $session{ +shift }->client; }
        sub blocks  { $blocks{ +shift }; }
        sub check   { $check{ +shift }; }
        sub touch   { $touch{ +shift }; }

        sub previous_incoming_block {
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

        sub read {
            my ( $self, $offset, $length ) = @_;
            croak(q[Bad length!])
                if defined $length
                    and (    $length !~ m[^\d+$]
                          or $length > $self->session->piece_size );
            croak(q[Bad offset!])
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
                $self->session->files->[$f]->open(q[r]) or return;
                $self->session->files->[$f]->seek($pos) or return;
                $_RETURN
                    .= $self->session->files->[$f]->read($this_read)
                    or return;
                $f++;
                $pos = 0;
                $_LENGTH -= $this_read;
            }
            return $_RETURN;
        }

        sub write {
            my ( $self, $data, $offset ) = @_;
            my $f = 0;
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
                $self->session->files->[$f]->open(q[w]) or return;
                $self->session->files->[$f]->seek($pos) or return;
                $self->session->files->[$f]
                    ->write( substr( $data, 0, $this_write, q[] ) )
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
            if ( Digest::SHA::sha1( $self->read ) eq $self->hash ) {
                my $old_value = $self->check;
                $check{$self} = 1;
                $session{$self}
                    ->client->do_callback( q[piece_hash_pass], $self )
                    if not $old_value
                ;    # no point telling us the piece is good twice
                $session{$self}->check_endgame_status;
                return 1;
            }

            # failed
            # TODO: penalize all peers related to piece
            $session{$self}
                ->client->do_callback( q[piece_hash_fail], $self );
            return 0;
        }

        sub unrequested_block {
            my ($self) = @_;
            return if not $working{$self};
            for my $block (
                sort {
                    scalar $a->peers <=> scalar $b->peers
                } values %{ $blocks{$self} }
                )
            {
                if ( $session{$self}->endgame ) { return $block; }
                if ( not scalar $block->peers ) { return $block; }

    # TODO: check for peer() instead of requested() but remember to...
    #       ...ignore both during endgame.
            }
            return;
        }

        sub as_string {
            my ( $self, $advanced ) = @_;
            my $dump = $$self . q[ [TODO]];
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
            delete $touch{$self};
            delete $session{$self};
            delete $previous_incoming_block{$self};
            return 1;
        }
    }
    1;
}
