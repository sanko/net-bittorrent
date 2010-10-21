package t::Net::BitTorrent::Torrent_single_file;
{
    use strict;
    use warnings;

    # Load standard modules
    use Module::Build;
    use Test::More;
    use Test::Moose;
    use Test::Exception;

    # Load local context
    BEGIN { -d '_build' ? last : chdir '..' for 1 .. 10 }
    my $t_builder = Test::More->builder;
    my $m_builder = Module::Build->current;

    # Load local modules
    BEGIN { require 't\1000_basic\Net\BitTorrent\Protocol\BEP03\Metadata.t' }
    use parent-norequire, 't::Net::BitTorrent::Protocol::BEP03::Metadata';
    use Net::BitTorrent::Torrent;

    #
    sub class     {'Net::BitTorrent::Torrent'}
    sub info_hash {'2B3AAF361BD40540BF7E3BFD140B954B90E4DFBC'}

    sub meta_data {
        'd7:comment32:See credit.txt for attributions.10:created by31:Net::Bi'
            . 'tTorrent::GenTorrent 0.113:creation datei1214665975e8:encoding'
            . '5:UTF-84:infod5:filesld6:lengthi28229e4:pathl27:1291672777_30a'
            . 'dc6a421_o.jpgeed6:lengthi21769e4:pathl27:2183742557_5c9a91727d'
            . '_m.jpgeee4:name4:seed12:piece lengthi32768e6:pieces40:'
            . "\x8867\16\x9E \13\xF3\xDE\xE7\37c\2\x96\xC4V\xAF\x8F\xA9k"
            . "\xF07I!\x85=\xBD\xAA8\xA3)\xA8\xFC\xDErNH\x8D\xD4\23ee";
    }
    sub init_args {'t/9000_data/9500_torrents/9503_miniswarm.torrent'}

    sub _files {
        [{length => 28229, path => ['1291672777_30adc6a421_o.jpg']},
         {length => 21769, path => ['2183742557_5c9a91727d_m.jpg']}
        ];
    }

    #
    sub moose_attributes : Test( +0 ) {
        my $s = shift;
        $s->SUPER::moose_attributes;

        #has_attribute_ok $s->{'m'}, 'files',    'has files';
        #has_attribute_ok $s->{'m'}, 'announce', 'has announce';
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
