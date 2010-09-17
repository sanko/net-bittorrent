package t::10000_by_class::Net::BitTorrent::Torrent::Generator_piece_length;
{
    use strict;
    use warnings;
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 12; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use 5.010.000;
    use Test::Most;
    use lib '../', '../../../../../', '../../../../../lib', 'lib';
    use Net::BitTorrent::Torrent::Generator;

    BEGIN {
        require
            't/10000_by_class/Net/BitTorrent/Torrent/Generator_multiple_files.t';
    }
    use parent-norequire,
        't::10000_by_class::Net::BitTorrent::Torrent::Generator_multiple_files';
    $|++;

    sub _200_api : Test( 8 ) {
        my $s = shift;
        is $s->{'torrent'}->piece_length, 262144,
            'default piece_length (2**18)';
        is $s->{'torrent'}->_piece_count, 1, 'current _piece_count == 1';

        #
        note 'setting piece_count to 8';
        $s->{'torrent'}->_set_piece_count(8);
        is $s->{'torrent'}->_piece_count(), 8, 'current _piece_count == 8';
        is $s->{'torrent'}->piece_length(), 6315,
            'current piece_length == 6315';
        is $s->{'torrent'}->info_hash->to_Hex,
            'A23CDB47D65F4716511573F0FCF44F8FC3DDF996',
            'info_hash was automatically regenerated I';

        #
        note 'setting piece_size to 45 bytes';
        $s->{'torrent'}->_set_piece_length(45);
        is $s->{'torrent'}->_piece_count(), 1123,
            'current _piece_count == 1123';
        is $s->{'torrent'}->piece_length(), 45, 'current piece_length == 45';
        is $s->{'torrent'}->info_hash->to_Hex,
            '364CAB7A7E722E0EF01916D387FECEC346639F12',
            'info_hash was automatically regenerated II';
    }

    #
    #$ENV{'TEST_VERBOSE'}++;
    __PACKAGE__->runtests() if !caller;
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
