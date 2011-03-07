package t::Net::BitTorrent::Protocol::BEP03::Storage::File;
{
    use strict;
    use warnings;

    # Load standard modules
    use Module::Build;
    use Test::More;
    use parent 'Test::Class';
    use Test::Moose;
    use Test::Fatal;

    # Load locally required modules
    use lib '../../../../../../../.../lib', 'lib';
    use File::Temp qw[tempdir];

    # Load local context
    BEGIN { -d '_build' ? last : chdir '..' for 1 .. 10 }
    my $t_builder = Test::More->builder;
    my $m_builder = Module::Build->current;

    #
    sub class {'Net::BitTorrent::Protocol::BEP03::Storage::File'}

    sub root {
        my $s = shift;
        $s->{'_root'} //= tempdir 'nb_bep03_XXXX', TMPDIR => 1, CLEANUP => 1;
        $s->{'_root'};
    }
    sub init_args { {path => ['test.ext'], length => 20} }

    #
    sub build : Test( startup => 2 ) {
        my $s = shift;
        use_ok $s->class;
        $s->{'m'} = new_ok $s->class, [@_ ? @_ : $s->init_args];
    }

    sub class_can : Test( 4 ) {
        my $s = shift;
        can_ok $s->{'m'}, $_ for qw[open close read write];
    }

    sub class_can_set : Test( 2 ) {
        my $s = shift;
        can_ok $s->{'m'}, "_set_$_" for qw[filehandle open_mode];
    }

    sub moose_does : Test( 0 ) {
        my $s = shift;
    }

    sub moose_attributes : Test( 5 ) {
        my $s = shift;
        has_attribute_ok $s->{'m'}, $_, 'has ' . $_
            for qw[path filehandle open_mode length priority];
    }

    sub moose_meta : Test( 1 ) {
        my $s = shift;
        meta_ok $s->{'m'};
    }

    #
    sub method_001_open : Test( 3 ) {
        my $s = shift;
        ok $s->{'m'}->open('wo', $s->root), sprintf '...->open("wo", "%s")',
            $s->root;
        is $s->{'m'}->open_mode, 'wo', 'open mode is wo';
        ok $s->{'m'}->filehandle, 'filehandle is defined';
    }

    sub method_002_write : Test( 3 ) {
        my $s = shift;
        is $s->{'m'}->write(0, 'XXX'), 3, 'wrote 3 bytes at offset 0';
        is $s->{'m'}->write(6, 'six'), 3, 'wrote 3 bytes at offset 6';
        is $s->{'m'}->write(4, 'eh'),  2, 'wrote 2 bytes at offset 4';
    }

    sub method_003_close : Test( 3 ) {
        my $s = shift;
        ok !$s->{'m'}->close(), '...->close( )';
        is $s->{'m'}->open_mode,  (), 'open mode is undefined';
        is $s->{'m'}->filehandle, (), 'filehandle is undefined';
    }

    sub method_004_open : Test( 3 ) {
        my $s = shift;
        ok $s->{'m'}->open('ro', $s->root), sprintf '...->open("ro", "%s")',
            $s->root;
        is $s->{'m'}->open_mode, 'ro', 'open mode is ro';
        ok $s->{'m'}->filehandle, 'filehandle is defined';
    }

    sub method_005_read : Test( 2 ) {
        my $s = shift;
        is $s->{'m'}->read(0, 5), "XXX\0e",
            '...->read(0, 5) reads five bytes starting from offset 0 (XXX\0e)';
        is $s->{'m'}->read(5, 4), "hsix",
            '...->read(5, 4) reads four bytes starting from offset 5 (hsix)';
    }

    sub teardown_close : Test( shutdown => 1 ) {
        my $s = shift;
        ok !$s->{'m'}->close(), '...->close( ) before we go';
        note 'Absolute path was ' . $s->{'m'}->abs_path($s->root);
        unlink $s->{'m'}->path();
    }

    #
    __PACKAGE__->runtests() if !caller;
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
