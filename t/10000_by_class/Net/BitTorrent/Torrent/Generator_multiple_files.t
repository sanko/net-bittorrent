package t::10000_by_class::Net::BitTorrent::Torrent::Generator_multiple_files;
{
    use strict;
    use warnings;
    use 5.010.000;
    use Test::Most;
    use lib '../', '../../../../../', '../../../../../lib', 'lib';
    use Net::BitTorrent::Torrent::Generator;
    BEGIN { require 't/10000_by_class/Net/BitTorrent/Torrent/Generator.t' }
    use parent-norequire,
        't::10000_by_class::Net::BitTorrent::Torrent::Generator';
    $|++;

    # Basic utility functions/methods
    sub files {
        ('./t/90000_data/96000_data/96020_miniswarm_seed/1291672777_30adc6a421_o.jpg',
         './t/90000_data/96000_data/96020_miniswarm_seed/2183742557_5c9a91727d_m.jpg',
         './t/90000_data/96000_data/96020_miniswarm_seed/credit.txt'
        );
    }

    sub new_args {
        my $s = shift;
        -e [$s->files]->[0] ? last : chdir '..' for 0 .. 15;
        (files => [$s->files]);
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
