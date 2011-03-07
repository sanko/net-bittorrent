package t::Net::BitTorrent::Protocol::BEP03::Metadata::Piece;
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
    sub class {'Net::BitTorrent::Protocol::BEP03::Metadata::Piece'}

    sub init_args {
        my $s = shift;
        { length => 2**16 };
    }
    sub offsets { [0, 16384, 32768, 49152] }

    #
    sub build : Test( startup => 2 ) {
        my $s = shift;
        use_ok $s->class;
        $s->{'m'} = new_ok $s->class, [@_ ? @_ : $s->init_args];
    }

    sub class_can : Test( 0 ) {
        my $s = shift;
        can_ok $s->{'m'}, $_ for qw[];
    }

    sub class_can_private : Test( 0 ) {
        my $s = shift;
        can_ok $s->{'m'}, "_$_" for qw[];
    }

    sub class_can_set : Test( 0 ) {
        my $s = shift;

        #can_ok $s->{'m'}, "_set_$_" for qw[files root size];
    }

    sub moose_does : Test( 0 ) {
        my $s = shift;
    }

    sub moose_attributes : Test( 4 ) {
        my $s = shift;
        has_attribute_ok $s->{'m'}, $_, 'has ' . $_
            for qw[length priority block_length blocks];
    }

    sub moose_meta : Test( 1 ) {
        my $s = shift;
        meta_ok $s->{'m'};
    }

    # Methods
    sub methods_001_blocks : Test( 1 ) {
        my $s = shift;
        note explain $s->{'m'}->blocks;
        is_deeply [sort(keys %{$s->{'m'}->blocks})], $s->offsets,
            'correct block offsets';
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
