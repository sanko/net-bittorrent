package Net::BitTorrent::Storage;
{
    use Any::Moose;
    use Any::Moose '::Util::TypeConstraints';
    our $MAJOR = 0.075; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use lib '../../';
    use Net::BitTorrent::Storage::File;
    use Net::BitTorrent::Storage::Cache;
    has 'cache' => (is         => 'rw',
                    isa        => 'Net::BitTorrent::Storage::Cache',
                    init_arg   => undef,
                    lazy_build => 1,
                    builder    => '_build_cache'
    );

    sub _build_cache {
        Net::BitTorrent::Storage::Cache->new(Storage => $_[0],
                                             Index   => undef);
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
                    handles  => {file_count => 'count',}
    );
    has 'root' => (
        is       => 'rw',
        isa      => 'Str',
        init_arg => 'Root',    # ??? - Should this be BaseDir/basedir
        trigger  => sub {
            my ($self, $new_root, $old_root) = @_;
            if (@{$self->files} > 1) {
                for my $file (@{$self->files}) {
                    my $path = $file->path;
                    shift @$path if defined $old_root;
                    unshift @$path, $new_root;
                    $file->path($path);
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
