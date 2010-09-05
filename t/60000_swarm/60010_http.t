use strict;
use warnings;
use lib '../../lib';
use Test::More;
use File::Temp;
use AnyEvent;
use lib reverse 'lib', '../lib', '../../lib';
use Net::BitTorrent;
use lib reverse 't/80000_utility', '../t/80000_utility',
    '../../t/80000_utility';
use Net::BitTorrent::Tracker::HTTP;    # Utility
$|++;
my (@cli, @dir);

#
note 'Adding condvar for later use...';
my $cv = AE::cv();
$cv->begin(sub { $cv->send });
note '...which will timeout in 2m';
my $to = AE::timer(120, 0, sub { note 'Timeout!'; $cv->send });
sub _done { $cv->send }

#
my $tracker = Net::BitTorrent::Tracker::HTTP->new(host => '127.0.0.1');
chdir '../..' if !-f 't/90000_data/95000_torrents/95003_miniswarm.torrent';
for my $seed (qw[1 0]) {
    push @cli, Net::BitTorrent->new(
        on_peer_id => sub {
            my ($s, $a) = @_;
            note $a->{'message'};
        },
        on_peer_packet => sub {
            my ($s, $a) = @_;
            note $a->{'message'};
        },
        on_peer_connect => sub {
            my ($s, $a) = @_;
            note 'Connect:    ' . $a->{'message'};
        },
        on_peer_disconnect => sub {
            my ($s, $a) = @_;
            note 'Disconnect: ' . $a->{'message'};
        },
        on_peer_packet_in => sub {
            my ($s, $a) = @_;

            #note explain $a->{'message'}, $a->{'packet'};
        },
        on_piece_hash_pass => sub {
            return if $seed;
            my ($s, $i) = @_;
            pass sprintf 'peer %02d now has piece %02d', scalar(@cli), $i;
            _done() if $s->torrent(0)->seed;
        },
        on_piece_hash_fail => sub { },
        on_peer_bitfield   => sub {
            my ($s, $a) = @_;
            note 'Bitfield packet: ' . $a->{'message'};
        }
    );
    $cli[-1]->add_torrent(
                path => 't\90000_data\95000_torrents\95003_miniswarm.torrent')
        ->tracker->add_tier(
                   [sprintf 'http://%s:%d/announce.pl?%d^', $tracker->host,
                    $tracker->port,                         int rand time
                   ]
        );
    if ($seed) {
        $cli[-1]->torrent(0)
            ->storage->_set_root(
                              't\90000_data\96000_data\96020_miniswarm_seed');
    }
    else {
        push @dir,
            File::Temp->newdir('NBminiswarm_peer_' . scalar(@cli) . '_XXXX',
                               TMPDIR => 1);
        $cli[-1]->torrent(0)->storage->_set_root($dir[-1]->dirname);
    }
    $cli[-1]->torrent(0)->hash_check;
    is $cli[-1]->torrent(0)->have->to_Enum, $seed ? '0,1' : '',
        sprintf '[%02d] hashcheck %s', scalar(@cli),
        ($seed ? 'seed' : 'peer');

    #warn $cli[-1]->torrent(0)->have->to_Enum;
    #warn $cli[-1]->torrent(0)->wanted->to_Enum;
}

#
$cv->recv;

#
done_testing;

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
