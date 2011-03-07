package Net::BitTorrent::Protocol::BEP03::Storage::File;
{
    use Moose;
    use Moose::Util::TypeConstraints;
    our $MAJOR = 0; our $MINOR = 74; our $DEV = 14; our $VERSION = sprintf('%0d.%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use lib '../../../../../../lib';
    use Net::BitTorrent::Protocol::BEP03::Types qw[:file];
    use File::Spec::Functions qw[splitpath catpath canonpath catfile rel2abs];
    use File::Path qw[make_path];
    use Fcntl qw[/O_/ /SEEK/ :flock];

    #
    has length => (is       => 'ro',
                   isa      => 'Int',
                   required => 1
    );
    has priority => (is      => 'ro',
                     isa     => subtype(as 'Int' => as enum([0 .. 15])),
                     default => 8
    );
    has path => (
        is       => 'ro',
        isa      => 'ArrayRef[Str]',
        required => 1,
        handles  => {
            abs_path => sub {
                my ($s, $root) = @_;
                canonpath catfile $root, @{$s->path};
                }
        }
    );

    #
    has filehandle => (is       => 'ro',
                       isa      => 'Maybe[GlobRef]',
                       init_arg => undef,
                       writer   => '_set_filehandle'
    );
    has open_mode => (
        is => 'ro',
        isa =>
            'Maybe[Net::BitTorrent::Protocol::BEP03::Types::File::Open::Permission]',
        init_arg => undef,
        writer   => '_set_open_mode'
    );

    #
    sub close {
        my $s = shift;
        if ($s->filehandle) {
            flock $s->filehandle, LOCK_UN;
            close $s->filehandle;
        }
        $s->_set_filehandle(undef);
        $s->_set_open_mode(undef);
    }

    sub open {
        my ($s, $mode, $root) = @_;
        $s->close() if $s->filehandle && $s->open_mode ne $mode;
        my ($vol, $dirs, $file) = splitpath $s->abs_path($root);
        make_path(canonpath catpath $vol, $dirs, '') if $mode =~ m[w];
        my $_mode = $mode eq 'ro' ? O_RDONLY : O_WRONLY;
        sysopen(my ($FH),
                $s->abs_path($root),
                $_mode | (($_mode &= O_WRONLY)
                          ? O_CREAT
                          : 0
                )
            )
            || return !$s->close();
        flock $FH, $mode eq 'ro' ? LOCK_SH : LOCK_EX;
        $s->_set_filehandle($FH);
        $s->_set_open_mode($mode);
    }

    #
    sub read {
        my ($s, $offset, $length) = @_;
        $offset //= 0;
        $length //= $s->length - $offset;
        return if $length + $offset > $s->length;
        return if !$s->open_mode;
        return if $s->open_mode ne 'ro';
        truncate $s->filehandle, $offset
            if $offset + $length > -s $s->filehandle;
        sysseek $s->filehandle, $offset, SEEK_SET;    # Set correct position
        my $real_length = sysread $s->filehandle, my ($data), $length;
        return if $real_length != $length;
        return $data;
    }

    sub write {
        my ($s, $offset, $data) = @_;
        return if length($data) + $offset > $s->length;
        return if !$s->open_mode;
        return if $s->open_mode ne 'wo';
        truncate $s->filehandle, $offset
            if $offset + length $data > -s $s->filehandle;
        sysseek $s->filehandle, $offset, SEEK_SET;    # Set correct position
        return syswrite $s->filehandle, $data, length($data);
    }

    #
    sub DEMOLISH { shift->close; }
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
