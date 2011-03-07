package t::Net::BitTorrent::Protocol::BEP03::Metadata::Piece_odd_length;
{
    use strict;
    use warnings;

    # Load standard modules
    use Module::Build;
    use Test::More;
    use parent 'Test::Class';
    use Test::Moose;
    use Test::Fatal;

    # Load local context
    BEGIN { -d '_build' ? last : chdir '..' for 1 .. 10 }
    my $t_builder = Test::More->builder;
    my $m_builder = Module::Build->current;

    # Load local modules
    use lib '../../../../../../../.../lib', 'lib';

    BEGIN {
        require
            't\10000_by_class\Net\BitTorrent\Protocol\BEP03\Metadata\Piece.t';
    }
    use parent-norequire,
        't::Net::BitTorrent::Protocol::BEP03::Metadata::Piece';

    #
    sub init_args {
        my $s = shift;
        { length => 1 + (2**16) };
    }
    sub offsets { [0, 16384, 32768, 49152, 65536] }

    #
    sub _last_block_length : Test( 1 ) {
        my $s = shift;
        is $s->{'m'}->blocks->{65536}->length, 1, 'last block length is 1';
    }

    #
    __PACKAGE__->runtests() if !caller;
}
1;

=pod

=head1 Description

This tests pieces which do not break into uniform blocks. This should only
matter for the last block in a torrent.

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
