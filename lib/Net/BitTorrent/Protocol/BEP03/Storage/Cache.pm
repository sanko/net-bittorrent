package Net::BitTorrent::Protocol::BEP03::Storage::Cache;
{
    use Moose;
    use Moose::Util::TypeConstraints;
    our $MAJOR = 0; our $MINOR = 74; our $DEV = 14; our $VERSION = sprintf('%0d.%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use lib '../../../../../../lib';
    use Net::BitTorrent::Protocol::BEP03::Types qw[:file];

    #=cut
    use MIME::Base64;

=cut
    sub encode_base64($;$) { (shift//'') ."\n" }
    sub decode_base64 { (shift//'') =~ m[(.+?)\r?\n$];$1 }
=test
=cut

    use Fcntl qw[/SEEK_/ :flock];
    use File::Spec::Functions qw[splitpath catpath canonpath catfile rel2abs];

    #
    has root => (
          is => 'ro',
          isa =>
              'Net::BitTorrent::Protocol::BEP03::Types::File::Path::Absolute',
          required => 1,
          coerce   => 1
    );
    has path => (
        is      => 'ro',
        isa     => 'ArrayRef[Str]',
        default => sub { [qw[~nb_cache.dat]] },
        handles => {
            abs_path => sub {
                my $s = shift;
                canonpath catfile $s->root, @{$s->path};
                }
        }
    );

    #
    has blocks => (traits  => ['Array'],
                   is      => 'ro',
                   isa     => 'ArrayRef[Int]',
                   default => sub { [] },
                   writer  => '_set_blocks',
                   handles => {_add_block_info => 'push',
                               _get_block_info => 'get',
                               _del_block_info => 'delete',
                               is_empty        => 'is_empty',
                               _count_blocks   => 'count',
                               _clear_blocks   => 'clear',
                               _grep_blocks    => 'grep',
                               _map_blocks     => 'map',
                               _splice_blocks  => 'splice',
                               _first_block    => 'first'
                   }
    );
    after _clear_blocks => sub {
        my $s = shift;
        unlink $s->abs_path;
    };

    sub store_blocks {
        my ($self, %list) = @_;
        open my $FH, -f $self->abs_path ? '+<' : '>', $self->abs_path;
        readline $FH while -s $FH && !eof $FH;
        print $FH "\n" if !-s $FH;
        for my $i (keys %list) {
            print $FH encode_base64 $list{$i};
            $self->_add_block_info($i);
        }
        close $FH;
        return scalar keys %list;
    }

    sub store_block {
        my ($self, $index, $data) = @_;
        if ($self->_grep_blocks(sub { $_ == $index })) {    # Replace
            open my $in,  '<', $self->abs_path;
            open my $out, '>', $self->abs_path . '.new';
            readline $in;                                   # mind the gap
            print $out "\n";
            while (<$in>) {
                print $out $_;
                last
                    if $self->_count_blocks < $.
                        || $self->_get_block_info($. - 1) == $index;
            }
            <$in>;
            print $out encode_base64 $data;
            { local $/ = undef; print $out readline $in; }
            close $_ for $in, $out;
            rename $self->abs_path . 'new', $self->abs_path;
        }
        else {    # Add
            open my $FH, '>>', $self->abs_path;
            seek $FH, SEEK_SET, -s $FH;
            print $FH "\n" if !-s $FH;
            print $FH encode_base64 $data;
            $self->_add_block_info($index);
            close $FH;
        }
        return length $data;
    }

    sub delete_block {
        my ($self, $index) = @_;
        die if !defined $index;
        return if !$self->_first_block(sub { $_ == $index });
        open my $in,  '<', $self->abs_path;
        open my $out, '>', $self->abs_path . '.new';
        readline $in;    # mind the gap
        print $out "\n";
        while (<$in>) {
            print $out $_;
            last
                if $self->_count_blocks < $.
                    || $self->_get_block_info($. - 1) == $index;
        }
        <$in>;
        $self->_splice_blocks($., 1);
        { local $/ = undef; print $out readline $in; }
        close $_ for $in, $out;
        rename $self->abs_path . '.new', $self->abs_path;
    }

    sub modify_block {
        my ($self, $index, $offset, $data) = @_;
        my $original = $self->read_block($index);
        $original .= "\0" x ($offset + length($data) - length($offset))
            if length $original < $offset + length $data;
        substr $original, $offset, length $data, $data;

        #warn sprintf 'Adding %s to index %d', $original, $index;
        return $self->store_block($index, $original) == length($original)
            ? length $data
            : ();
    }

    sub read_block {
        my ($self, $index, $offset, $length) = @_;
        $offset //= 0;
        return if !$self->_grep_blocks(sub { $_ == $index });
        open my $in, '<', $self->abs_path or die "Can't read old file: $!";

        #readline $in;    # mind the gap
        while (<$in>) {
            last
                if $self->_count_blocks < $.
                    || $self->_get_block_info($. - 1) == $index;
        }
        my $data = decode_base64(<$in>);
        $length //= length($data) - $offset;

#warn sprintf 'returning %d bytes at offset %d from %s', $length , $offset, $data;
        return substr($data, $offset, $length);
    }

    sub load_index {
        my $self = shift;
        open my $FH, '<', $self->abs_path;
        my $index = readline $FH;
        chomp $FH;
        $self->_set_blocks(bdecode $index);
        last
            if $self->_count_blocks < $.
                || $self->_get_block_info($. - 1) == $index;
    }

    sub store_index {
        my $self = shift;
        open my $in,  '<', $self->abs_path;
        open my $out, '>', $self->abs_path . '.new';
        readline $in;    # mind the gap
        print $out bencode(\$self->blocks) . "\n";
        { local $/ = undef; print $out readline $in; }
        close $_ for $in, $out;
        rename $self->abs_path . '.new', $self->abs_path;
    }
    sub _resume { return $_->packets }
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
