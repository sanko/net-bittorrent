package Net::BitTorrent::Storage;
{
    use Moose;
    use Moose::Util::TypeConstraints;
    our $MAJOR = 0.075; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use lib '../../';
    use Net::BitTorrent::Storage::File;
    use Net::BitTorrent::Storage::Cache;
    use File::Spec::Functions qw[rel2abs];
    has 'cache' => (is         => 'rw',
                    isa        => 'Net::BitTorrent::Storage::Cache',
                    init_arg   => undef,
                    lazy_build => 1,
                    builder    => '_build_cache'
    );

    sub _build_cache {
        Net::BitTorrent::Storage::Cache->new(
               Storage => $_[0],
               Path => ['~' . substr($_[0]->torrent->infohash, 0, 7) . '.dat']
        );
    }
    has 'torrent' => (is       => 'rw',
                      required => 1,
                      isa      => 'Net::BitTorrent::Torrent',
                      init_arg => 'Torrent'
    );
    has 'files' => (is       => 'rw',
                    isa      => 'Torrent::Files',
                    init_arg => 'Files',
                    coerce   => 1,
                    traits   => ['Array'],
                    handles  => {
                                _count    => 'count',
                                _add_file => 'push',
                                _file     => 'get'
                    },
    );
    has 'root' => (
        is       => 'rw',
        isa      => 'Str',
        init_arg => 'Root',    # ??? - Should this be BaseDir/basedir
        trigger  => sub {
            my ($self, $new_root, $old_root) = @_;
            if ($self->_count) {
                for my $file (@{$self->files}) {
                    $file->_shift if defined $old_root;
                    $file->_unshift(rel2abs $new_root);
                }
            }
        }
    );

    #
    has 'size' => (is         => 'ro',
                   isa        => 'Int',
                   writer     => '_size',
                   lazy_build => 1,
                   builder    => '_build_size'
    );

    sub _build_size {
        my ($self) = @_;
        my $size = 0;
        for my $file (@{$self->files}) { $size += $file->length; }
        return $size;
    }

    sub read {
        my ($self, $index, $offset, $length) = @_;
        $offset //= 0;
        $length //=
            int $index == int $self->torrent->piece_count
            ? $self->torrent->size % $self->torrent->piece_length
            : $self->torrent->piece_length;
        my $data       = '';
        my $file_index = 0;
        my $total_offset
            = int(($index * $self->torrent->piece_length) + ($offset || 0));
    SEARCH:
        while ($total_offset > $self->files->[$file_index]->length) {
            $total_offset -= $self->files->[$file_index]->length;
            $file_index++;
            last SEARCH    # XXX - return?
                if not defined $self->files->[$file_index]->length;
        }
    READ: while ((defined $length) && ($length > 0)) {
            my $this_read
                = (($total_offset + $length)
                   >= $self->files->[$file_index]->length)
                ? ($self->files->[$file_index]->length - $total_offset)
                : $length;
            $self->files->[$file_index]->open('ro') or return;
            my $_data = $self->files->[$file_index]
                ->read($total_offset, $this_read);
            $data .= $_data if $_data;
            $file_index++;
            $length -= $this_read;
            last READ if not defined $self->files->[$file_index];
            $total_offset = 0;
        }
        return \$data;
    }
}
1;

=pod

=head1 Author

Sanko Robinson <sanko@cpan.org> - http://sankorobinson.com/

CPAN ID: SANKO

=head1 License and Legal

Copyright (C) 2008-2010 by Sanko Robinson <sanko@cpan.org>

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
