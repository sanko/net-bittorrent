package t::Net::BitTorrent::Protocol::BEP03::Storage::Cache;
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
    sub class {'Net::BitTorrent::Protocol::BEP03::Storage::Cache'}

    sub init_args {
        {path    => ['test.ext'],
         root    => tempdir 'nb_bep03_XXXX',
         TMPDIR  => 1,
         CLEANUP => 1
        };
    }

    #
    sub build : Test( startup => 2 ) {
        my $s = shift;
        use_ok $s->class;
        $s->{'m'} = new_ok $s->class, [@_ ? @_ : $s->init_args];
    }

    sub class_can : Test( 4 ) {
        my $s = shift;
        can_ok $s->{'m'}, $_
            for qw[read_block store_block modify_block store_blocks];
    }

    sub class_can_set : Test( 0 ) {
        my $s = shift;

        #can_ok $s->{'m'}, "_set_$_" for qw[filehandle open_mode];
    }

    sub moose_does : Test( 0 ) {
        my $s = shift;
    }

    sub moose_attributes : Test( 2 ) {
        my $s = shift;
        has_attribute_ok $s->{'m'}, $_, 'has ' . $_ for qw[root path];
    }

    sub moose_meta : Test( 1 ) {
        my $s = shift;
        meta_ok $s->{'m'};
    }

    # Work on cache files per-block
    sub method_001_modify_block : Test( 1 ) {
        my $s = shift;
        is $s->{'m'}->modify_block(8, 5, 'Example'), 7,
            'wrote 7 bytes at index 8 offset 5 (Example)';
        note explain $s->{'m'}->blocks;
    }

    sub method_002_store_block : Test( 1 ) {
        my $s = shift;
        is $s->{'m'}->store_block(5, 'Second Example'), 14,
            'wrote 14 bytes at index 5 (Second Example)';
        note explain $s->{'m'}->blocks;
    }

    sub method_003_read_block : Test( 2 ) {
        my $s = shift;
        is $s->{'m'}->read_block(8, 1, 3), "\0" x 3,
            'read 3 bytes from index 8 offset 1 (nulls)';
        is $s->{'m'}->read_block(8, 4, 5), "\0Exam",
            'read 5 bytes from index 8 offset 4 (\0Exam)';
    }

    sub method_004_read_block : Test( 1 ) {
        my $s = shift;
        is $s->{'m'}->read_block(5), 'Second Example',
            'read 14 bytes from index 5 (Second Example)';
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
