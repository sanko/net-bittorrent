package t::10000_by_class::Net::BitTorrent::Torrent::Generator_announce_list;
{
    use strict;
    use warnings;
    our $MAJOR = 0; our $MINOR = 74; our $DEV = 13; our $VERSION = sprintf('%0d.%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
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
        ok !$s->{'torrent'}->has_announce_list, 'no default announce-list';
        $s->{'torrent'}->_add_tier(['http://example.com/announce.pl?']);
        is_deeply $s->{'torrent'}->announce_list,
            [['http://example.com/announce.pl?']], 'adding a new tier worked';
        $s->{'torrent'}->_add_tier(['http://example.com/announce.pl?test',
                                    'udp://example.com/announce.php?id=what^'
                                   ]
        );
        is_deeply $s->{'torrent'}->announce_list,
            [['http://example.com/announce.pl?'],
             ['http://example.com/announce.pl?test',
              'udp://example.com/announce.php?id=what^'
             ]
            ],
            'adding a second tier worked';
        is_deeply $s->{'torrent'}->_get_tier(1),
            ['http://example.com/announce.pl?test',
             'udp://example.com/announce.php?id=what^'
            ],
            'getting the second tier worked';
        ok $s->{'torrent'}->_del_tier(0), 'delete tier';
        is_deeply $s->{'torrent'}->announce_list,
            [['http://example.com/announce.pl?test',
              'udp://example.com/announce.php?id=what^'
             ]
            ],
            'first tier was removed';
        ok $s->{'torrent'}->has_announce_list, 'announce-list is now defined';
        is $s->{'torrent'}->info_hash->to_Hex,
            $s->info_hash,
            'info_hash should not have changed';
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
