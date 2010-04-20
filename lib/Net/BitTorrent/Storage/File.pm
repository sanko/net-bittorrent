package Net::BitTorrent::Storage::File;
{
    use Any::Moose;
    use Any::Moose '::Util::TypeConstraints';
    our $MAJOR = 0.075; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use File::Spec::Functions qw[catfile splitpath catpath];
    use File::Path qw[make_path];
    has 'path' => (is       => 'rw',
                   isa      => 'ArrayRef[Str]',
                   required => 1,
                   init_arg => 'Path',
                   trigger  => sub { $_[0]->close }
    );
    has 'length' => (is       => 'ro',
                     isa      => 'Int',
                     required => 1,
                     init_arg => 'Length'
    );
    has 'offset' => (is       => 'ro',
                     isa      => 'Int',
                     default  => 0,
                     init_arg => 'Offset'
    );

    #
    has 'filehandle' => (is  => 'rw',
                         isa => 'Maybe[GlobRef]');
    enum '_file_perm' => qw[ro wo];
    has 'open' => (
        is      => 'rw',
        isa     => 'Maybe[_file_perm]',
        trigger => sub {
            my ($self, $new_mode, $old_mode) = @_;
            if (defined $new_mode) {
                my $path = catfile @{$self->path};
                my ($vol, $dirs, $file) = splitpath($path);
                make_path(catpath $vol, $dirs, '') if $new_mode ne 'ro';
                open(my ($FH), ($new_mode eq 'ro' ? '<' : '>'), $path)
                    || return !$self->close();
                $self->filehandle($FH);
            }
            else {
                close $self->filehandle if $self->filehandle;
                $self->filehandle(undef);
            }
        }
    );
    sub close () { !shift->open(undef); }

    sub read ($$;$) {
        my ($self, $length, $offset) = @_;
        return if !$self->open;
        return if $self->open ne 'ro';
        $offset //= 0;
        return if $length + $offset > $self->length;
        sysseek $self->filehandle, 0, 1;    # Set position to start of file
        sysread $self->filehandle, my ($data), $length, $offset;
        return $data;
    }

    sub write ($$;$) {
        my ($self, $data, $offset) = @_;
        return if !$self->open;
        return if $self->open ne 'wo';
        $offset //= 0;
        return if length($data) + $offset > $self->length;
        sysseek $self->filehandle, 0, 1;    # Set position to start of file
        return syswrite $self->filehandle, $data, length($data), $offset;
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
