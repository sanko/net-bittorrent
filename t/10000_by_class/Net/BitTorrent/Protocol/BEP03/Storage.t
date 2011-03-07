package t::Net::BitTorrent::Protocol::BEP03::Storage;
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
    use lib '../../../../../../../../lib', 'lib';
    use File::Temp qw[tempdir];

    # Load local context
    BEGIN { -d '_build' ? last : chdir '..' for 1 .. 10 }
    my $t_builder = Test::More->builder;
    my $m_builder = Module::Build->current;

    #
    sub class {'Net::BitTorrent::Protocol::BEP03::Storage'}

    sub root {
        my $s = shift;
        $s->{'_root'} //= tempdir 'nb_bep03_XXXX', TMPDIR => 1, CLEANUP => 1;
        $s->{'_root'};
    }

    sub files {
        my $s = shift;

        # coerced into proper objects
        [{path   => [qw[a deep file.ext]],
          length => 32
         },
         {path   => [qw[another deep file.ext]],
          length => 64
         }
        ];
    }

    sub init_args {
        my $s = shift;
        {root         => $s->root,
         piece_length => 14,
         key   => join('', map { ('a' .. 'z')[rand 26] } 1 .. 8),
         files => $s->files
        };
    }

    #
    sub build : Test( startup => 2 ) {
        my $s = shift;
        use_ok $s->class;
        $s->{'m'} = new_ok $s->class, [@_ ? @_ : $s->init_args];
    }

    sub class_can : Test( 6 ) {
        my $s = shift;
        can_ok $s->{'m'}, $_
            for qw[hash_check is_seed wanted size read write];
    }

    sub class_can_set : Test( 3 ) {
        my $s = shift;
        can_ok $s->{'m'}, "_set_$_" for qw[files root size];
    }

    sub moose_does : Test( 0 ) {
        my $s = shift;
    }

    sub moose_attributes : Test( 6 ) {
        my $s = shift;
        has_attribute_ok $s->{'m'}, $_, 'has ' . $_
            for qw[key cache files root size piece_length];
    }

    sub moose_meta : Test( 1 ) {
        my $s = shift;
        meta_ok $s->{'m'};
    }

    #
    sub method_002_write : Test( 4 ) {
        my $s = shift;
        is $s->{'m'}->write(4, 0, 'XXX'), 3,
            'wrote 3 bytes at index 4, offset 0';
        is $s->{'m'}->write(4, 6, 'six'), 3,
            'wrote 3 bytes at index 4, offset 6';
        is $s->{'m'}->write(3, 4, 'eh'), 2,
            'wrote 2 bytes at index 3, offset 4';
        is $s->{'m'}->write(50, 0, 'XXX'), (),
            'fail to write 3 bytes at index 50, offset 0 (beyond size of storage)';
    }

    sub method_005_read : Test( 3 ) {
        my $s = shift;
        is ${$s->{'m'}->read(4, 0, 5)}, "XXX\0\0",
            '...->read(0, 5) reads 5 bytes starting from index 4, offset 0 (XXX\0\0)';
        is ${$s->{'m'}->read(4, 5, 4)}, "\0six",
            '...->read(5, 4) reads 4 bytes starting from index 4, offset 5 (\0six)';
        is ${$s->{'m'}->read(50, 0, 3)}, '',
            'fail to read 3 bytes at index 50, offset 0 (beyond size of storage)';
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

=for rcs $Id$

=cut
