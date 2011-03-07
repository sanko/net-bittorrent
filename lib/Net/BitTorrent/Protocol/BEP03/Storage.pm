package Net::BitTorrent::Protocol::BEP03::Storage;
{
    use Moose;
    use Moose::Util::TypeConstraints;
    our $MAJOR = 0; our $MINOR = 74; our $DEV = 14; our $VERSION = sprintf('%0d.%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use lib '../../../../../lib';
    use Net::BitTorrent::Protocol::BEP03::Storage::File;
    use Net::BitTorrent::Protocol::BEP03::Storage::Cache;
    use Net::BitTorrent::Protocol::BEP03::Types qw[:file];

    #
    has key => (is       => 'ro',
                isa      => subtype(as 'Str' => where { length $_ == 8 }),
                required => 1
    );
    has cache => (is  => 'ro',
                  isa => 'Net::BitTorrent::Protocol::BEP03::Storage::Cache',
                  init_arg   => undef,
                  lazy_build => 1,
                  builder    => '_build_cache'
    );

    sub _build_cache {
        my $s = shift;
        Net::BitTorrent::Protocol::BEP03::Storage::Cache->new(
                                              root => $s->root,
                                              path => ['~' . $s->key . '.dat']
        );
    }
    has piece_length => (
        is => 'ro',
        isa =>
            'Net::BitTorrent::Protocol::BEP03::Types::Metadata::Piece_Length',
        required => 1
    );
    has files => (
        is => 'ro',
        isa =>
            'ArrayRef.Net::BitTorrent::Protocol::BEP03::Types::Metadata::File',
        traits   => ['Array'],
        writer   => '_set_files',
        required => 1,
        coerce   => 1,
        handles  => {
            _count_files => 'count',

            #_add_file    => 'push',
            #_file        => 'get'
        }
    );
    has bitfield => (
          is  => 'ro',
          isa => 'Net::BitTorrent::Protocol::BEP03::Types::Torrent::Bitfield',
          lazy_build => 1,
          coerce     => 1,
          builder    => '_build_bitfield',
          init_arg   => undef,
          writer     => '_set_bitfield',
          clearer    => '_clear_bitfield',
          handles    => {
                      _set_piece   => 'Bit_On',
                      _unset_piece => 'Bit_Off',
                      has_piece    => 'bit_test',
                      seed         => 'is_full'
          }
    );
    sub _build_bitfield { '0' x $_[0]->count_pieces }
    after _set_piece => sub {
        my ($s, $i) = @_;

        #$s->trigger_piece_hash_pass($i);
        #$_->_send_have($i) for $s->peers;
    };

    #after _unset_piece => sub { $_[0]->trigger_piece_hash_fail($_[1]) };
    #
    sub wanted {
        my $s      = shift;
        my $b      = $s->bitfield->Shadow;
        my $offset = 0;
        for my $file (grep { $_->priority } @{$s->files}) {
            my $min = $offset / $s->piece_length;
            my $max = ($offset + $file->length) / $s->piece_length;
            $b->Interval_Fill($min, $max);
            $offset += $file->length;
        }
        $b->AndNot($b, $s->bitfield);
        $b;
    }
    sub is_seed { return !shift->wanted->Norm() }

    #
    has root => (
          is => 'ro',
          isa =>
              'Net::BitTorrent::Protocol::BEP03::Types::File::Path::Absolute',
          writer  => '_set_root',
          default => '.',
          coerce  => 1
    );
    after _set_root => sub {
        my $s = shift;
        for my $file (@{$s->files}) {
            my $open_mode = $file->open_mode;
            $file->close;
            $file->open($open_mode, $s->root);
        }
    };

    #
    has size => (is         => 'ro',
                 isa        => 'Int',
                 writer     => '_set_size',
                 lazy_build => 1,
                 builder    => '_build_size'
    );

    sub _build_size {
        my ($s) = @_;
        my $size = 0;
        for my $file (@{$s->files}) { $size += $file->length; }
        return $size;
    }

    sub read {    # Also checks cache
        my ($s, $i, $o, $l) = @_;
        $o //= 0;
        $l //=
            int $i == int $s->count_pieces
            ? $s->size % $s->piece_length
            : $s->piece_length;

        #return () if int $s->size < int (($i * $s->piece_length) + $o + $l);
        my $data = $s->_read($i, $o, $l) || \'';
        my $x = -1;
        my @cache
            = $s->cache->_map_blocks(sub { $x++; $_->[0] == $i ? $x : () });
        for my $i (@cache) {
            my $where = $s->cache->_get_block_info($i);
            my $d     = $s->cache->get_block($i);
            substr $$data, $where->[1], $where->[2], $d;
        }
        return $data;
    }

    sub _read {
        my ($s, $index, $offset, $length) = @_;
        $offset //= 0;
        $length //=
            int $index == int $s->count_pieces
            ? $s->size % $s->piece_length
            : $s->piece_length;
        my $data         = '';
        my $file_index   = 0;
        my $total_offset = int(($index * $s->piece_length) + ($offset || 0));
    SEARCH: while ($total_offset > $s->files->[$file_index]->length) {
            $total_offset -= $s->files->[$file_index]->length;
            $file_index++;
            return () if $file_index == $s->_count_files;
        }
    READ: while ((defined $length) && ($length > 0)) {
            my $this_read
                = (
                ($total_offset + $length) >= $s->files->[$file_index]->length)
                ? ($s->files->[$file_index]->length - $total_offset)
                : $length;
            if ($s->files->[$file_index]->priority == 0) {
                $data .= $s->cache->read_block($index, $total_offset,
                                               $this_read);
            }
            else {
                $s->files->[$file_index]->open('ro', $s->root);
                if (  !$s->files->[$file_index]->open_mode
                    || $s->files->[$file_index]->open_mode ne 'ro')
                {

                    #warn $s->files->[$file_index]->abs_path($s->root);
                    #die $s->files->[$file_index]->open_mode;
                    $data .= "\0" x $this_read;
                }
                else {
                    my $_data = $s->files->[$file_index]
                        ->read($total_offset, $this_read);
                    $data .= $_data if $_data;
                }
            }
            $file_index++;
            $length -= $this_read;
            last READ if $s->_count_files <= $file_index;
            $total_offset = 0;
        }
        return \$data;
    }

    sub write {
        my ($s, $i, $o, $d) = @_;
        my $done         = 0;
        my $file_index   = 0;
        my $total_offset = int(($i * $s->piece_length) + ($o || 0));
        return ()
            if $s->size < ($i * $s->piece_length) + $o + length $d;
    SEARCH:
        while ($total_offset > $s->files->[$file_index]->length) {
            $total_offset -= $s->files->[$file_index]->length;
            $file_index++;
            last SEARCH    # XXX - return?
                if $s->_count_files <= $file_index;
        }
    WRITE: while ((defined $d) && (length $d > 0)) {
            my $this_write
                = (($total_offset + length $d)
                   >= $s->files->[$file_index]->length)
                ? ($s->files->[$file_index]->length - $total_offset)
                : length $d;
            ($done +=
                 $s->cache->modify_block(
                              $i, $total_offset, substr $d, 0, $this_write, ''
                 )
                )
                && next WRITE
                if $s->files->[$file_index]->priority == 0;
            return
                if !((defined $s->files->[$file_index]->open_mode
                      && $s->files->[$file_index]->open_mode eq 'wo'
                     )
                     || $s->files->[$file_index]->open('wo', $s->root)
                );
            $done += $s->files->[$file_index]
                ->write($total_offset, substr $d, 0, $this_write, '');
            $file_index++;
            last WRITE if $s->_count_files <= $file_index;
            $total_offset = 0;
        }
        return $done;
    }

#
# Quick methods
# XXX - Old Version
#my $pieces_per_hashcheck = 10;    # Max block of pieces in single call
#
#sub hash_check {    # Range is split up into $pieces_per_hashcheck blocks
#                    # ??? - Disconnect peers if @$range > 1
#    my ($s, $range) = @_;
#    $range =
#          defined $range
#        ? ref $range
#            ? $range
#            : [$range]
#        : [0 .. $s->count_pieces - 1];
#    if (scalar @$range <= $pieces_per_hashcheck) {
#        $s->_clear_bitfield() if !defined $_[0];    # retain current bitfield
#        for my $index (@$range) {
#            my $piece = $s->read($index);
#            next if !$piece || !$$piece;
#            require Digest::SHA;
#            Digest::SHA::sha1($$piece) eq
#                substr($s->pieces, ($index * 20), 20)
#                ? $s->_set_piece($index)
#                : $s->_unset_piece($index);
#        }
#    }
#    else {
#        my $cv = AnyEvent->condvar;
#        $cv->begin;
#        my (@watchers, @ranges, @this_range, $coderef);
#        push @ranges, [splice(@$range, 0, $pieces_per_hashcheck, ())]
#            while @$range;
#        $coderef = sub {
#            shift @watchers if @watchers;
#            @this_range = shift @ranges;
#            $s->hash_check(@this_range);
#            push @watchers,
#                AE::idle(@ranges ? $coderef : sub { $cv->end });
#        };
#        push @watchers, AE::idle($coderef);
#        $cv->recv;
#        shift @watchers;
#    }
#    return 1;
#}
    sub hash_check {    # XXX - Blocking operation!!!
                        # ??? - Disconnect peers if @$range > 1
        my $s = shift;
        $s->_clear_bitfield() if !defined $_[0];    # retain current bitfield
        require Digest::SHA;
        for my $i (@_ ? @_ : (0 .. $s->count_pieces - 1)) {
            my $piece = $s->read($i);
            $piece // $$piece // ($s->_unset_piece($i) && next);
            lc Digest::SHA::sha1_hex($$piece) eq lc $s->pieces->[$i]->to_Hex
                ? $s->_set_piece($i)
                : $s->_unset_piece($i);
        }
        return 1;
    }
}
1;

=pod

=head1 Author

Sanko Robinson <sanko@cpan.org> - http://sankorobinson.com/

CPAN ID: SANKO

=head1 License and Legal

Copyright (C) 2008-2011 by Sanko Robinson <sanko@cpan.org>

This program is free software; you can redistribute it and/or modify it under
the terms of
L<The Artistic License 2.0|http://www.perlfoundation.org/artistic_license_2_0>.
See the F<LICENSE> file included with this distribution or
L<notes on the Artistic License 2.0|http://www.perlfoundation.org/artistic_2_0_notes>
for clarification.

When separated from the distribution, all original POD documentation is
covered by the
L<Creative Commons Attribution-Share Alike 3.0 License|http://creativecommons.org/licenses/by-sa/3.0/us/legalcode>.
See the
L<clarification of the CCA-SA3.0|http://creativecommons.org/licenses/by-sa/3.0/us/>.

Neither this module nor the L<Author|/Author> is affiliated with BitTorrent,
Inc.

=for rcs $Id$

=cut
