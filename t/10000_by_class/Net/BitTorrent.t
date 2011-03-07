package t::Net::BitTorrent;
{
    use strict;
    use warnings;
    use lib 'lib';

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
    #BEGIN {
    #    require 't\10000_by_class\Net\BitTorrent\Protocol\BEP03\Storage.t';
    #}
    #use parent-norequire, 't::Net::BitTorrent::Protocol::BEP03::Storage';
    #
    sub class     {'Net::BitTorrent'}
    sub init_args { }

    #
    sub build : Test( startup => 2 ) {
        my $s = shift;
        use_ok $s->class;
        $s->{'m'} = new_ok $s->class, [@_ ? @_ : $s->init_args];
    }

    sub peer_id : Test( 1 ) {
        my $s = shift;
        like $s->{'m'}->peer_id, qr[^NB\d{3}[SU]-.{7}-.{5}$];
    }

    sub torrents : Test( 4 ) {
        my $s = shift;
        is_deeply $s->{'m'}->torrents, [],
            'initial value of ...>torrents is an empty list';
        like exception { $s->{'m'}->add_torrent('Non-existant.torrent') },
            qr[is required],
            '->add_torrent( q[Non-existant.torrent] ) fails';
        isa_ok $s->{'m'}
            ->add_torrent(
                       't/90000_data/95000_torrents/95003_miniswarm.torrent'),
            'Net::BitTorrent::Torrent',
            '->( q[[...]miniswarm.torrent] ) returns new torrent';
        is $s->{'m'}->count_torrents, 1, '->count_torrents( ) is correct';
    }

    sub class_can : Test( 14 ) {
        my $s = shift;
        can_ok $s->{'m'}, $_
            for qw[add_torrent clear_torrents count_torrents filter_torrents
            find_torrent has_torrents info_hashes map_torrents no_torrents
            shuffle_torrents sort_torrents sort_torrents_in_place torrent
            torrents];
    }

    sub moose_does : Test( 0 ) {
        my $s = shift;
    }

    sub moose_attributes : Test( 2 ) {
        my $s = shift;
        has_attribute_ok $s->{'m'}, $_, 'has ' . $_ for qw[peer_id torrents];
    }

    sub moose_meta : Test( 1 ) {
        my $s = shift;
        meta_ok $s->{'m'};
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
