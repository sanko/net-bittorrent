package Net::BitTorrent::Storage::Cache;
{
    use Moose;
    use Moose::Util::TypeConstraints;
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use lib '../../../../lib';
    extends 'Net::BitTorrent::Storage::Node';
    my $blocktype
        = subtype 'NBTypes::Cache::Block' => as 'ArrayRef[Int]' =>
        where { scalar @$_ == 4 } =>
        message {'bad array'};    # i, o, l, file_offset
    has 'blocks' => (traits  => ['Array'],
                     is      => 'ro',
                     isa     => 'ArrayRef[NBTypes::Cache::Block]',
                     default => sub { [] },
                     handles => {_add_block_info => 'push',
                                 _get_block_info => 'get',
                                 _del_block_info => 'delete',
                                 is_empty        => 'is_empty',
                                 size            => 'count',
                                 _clear_blocks   => 'clear',
                                 _grep_blocks    => 'grep',
                                 _map_blocks     => 'map'
                     }
    );
    after '_clear_blocks' => sub {
        my $s = shift;
        $s->close() if defined $s->open;
        unlink $s->path;
    };

    sub add_block ($$$) {
        my ($s, $b, $d) = @_;
        $s->close() if defined $s->open && $s->open ne 'wo';
        return if !$s->open('wo');
        my $_offset = -s $s->filehandle;
        my $header = pack 'N3', $b->index, $b->offset, $b->length;
        warn 'Adding: ' . $header . ' | ' . pack 'L3', $b->index, $b->offset,
            $b->length;
        $s->write($_offset, $header . $d) || die $!;
        $s->_add_block_info([$b->index, $b->offset, $b->length, $_offset]);
    }

    sub get_block ($$) {
        my ($s, $i) = @_;
        my $where = $s->_get_block_info($i);
        return if !defined $where;
        $s->close() if defined $s->open && $s->open ne 'ro';
        $s->open('ro') || return;
        return $s->read($where->[3], $where->[2]);
    }

    sub del_block ($$) {
        my ($s, $i) = @_;
        die '...';    # XXX - I need to double check all offsets after removal
        my $where = $s->_get_block_info($i);
        return if !defined $where;
        $s->close() if defined $s->open && $s->open ne 'ro';
        $s->open('ro') || return;
        my $data = $s->read(0, -s $s->filehandle);
        substr($data, $where->[0], $where->[1], '');
        $s->close();
        rename $s->path, $s->path . '.old';
        $s->open('wo') || return;
        return $s->write(0, $data) && $s->_del_block_info($i);
    }
    sub _resume { return $_->packets }

    sub BUILDALL {
        my ($s, $a) = @_;
        return if !-f $s->path;
        $s->open('ro') || return;

        # XXX - Load index from file
        my ($i, $o, $l) = unpack 'N3', $s->read(12);
        warn join '|', $i, $o, $l;
        die '...';
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
