# -*- perl -*-
# $Id$
# Miniature swarm of 1 seed, 1 dht tracker, and 5 new peers
#
use strict;
use warnings;
use Module::Build;
use Socket;
use Fcntl qw[:flock];
use Test::More;
use File::Temp qw[];
use IO::Socket qw[SOMAXCONN];
use List::Util qw[sum];
use lib q[../../lib];
use Net::BitTorrent;
use Net::BitTorrent::Util qw[compact];
$|++;
my $test_builder       = Test::More->builder;
my $simple_dot_torrent = q[./t/900_data/950_torrents/953_miniswarm.torrent];
chdir q[../../] if not -f $simple_dot_torrent;
my $build    = Module::Build->current;
my $okay_tcp = $build->notes(q[okay_tcp]);
my $okay_udp = $build->notes(q[okay_udp]);
my $verbose  = $build->notes(q[verbose]);
$SIG{__WARN__} = ($verbose ? sub { diag shift } : sub { });
my $BlockLength = 2**14;
my $Seeds       = 1;
my $Peers_DHT   = 5;
my $Timeout     = 60;
my $sprintf
    = q[%0] . length($Peers_DHT > $Seeds ? $Peers_DHT : $Seeds) . q[d];
my $miniswarm_dot_torrent
    = q[./t/900_data/950_torrents/953_miniswarm.torrent];
my $_infohash = q[2b3aaf361bd40540bf7e3bfd140b954b90e4dfbc];
$|++;
plan tests => int($Seeds + $Peers_DHT + 1) * 2;
SKIP: {
    skip(q[TCP-based tests have been disabled.],
         ($test_builder->{q[Expected_Tests]} - $test_builder->{q[Curr_Test]})
    ) unless $okay_tcp;
    skip(q[UDP-based tests have been disabled.],
         ($test_builder->{q[Expected_Tests]} - $test_builder->{q[Curr_Test]})
    ) unless $okay_udp;

    #
    my %client;

    END {
        for my $client (values %client) {
            next if not defined $client->torrents->{$_infohash};
            for my $file (@{$client->torrents->{$_infohash}->files}) {
                $file->_close;
            }
        }
    }
    my $test_builder = Test::More->builder;
    $client{q[DHT]} = new Net::BitTorrent({LocalAddr => q[127.0.0.1]});
    ok($client{q[DHT]}->isa(q[Net::BitTorrent]), q[DHT (bystander)]);
    ok($client{q[DHT]}->_use_dht, q[DHT (bystander) has enabled dht]);
    for my $chr (1 .. $Seeds) {
        $chr = sprintf $sprintf, $chr;
        $client{q[seed_] . $chr}
            = new Net::BitTorrent({LocalAddr => q[127.0.0.1]});
        skip(sprintf(q[Failed to create seed_%s], $chr),
             $test_builder->{q[Expected_Tests]}
                 - $test_builder->{q[Curr_Test]}
        ) if not $client{q[seed_] . $chr};
        ok($client{q[seed_] . $chr}->_use_dht,
            sprintf q[seed_%s has enabled dht], $chr);
        my $torrent = $client{q[seed_] . $chr}->add_torrent(
                                     {Path    => $miniswarm_dot_torrent,
                                      BaseDir => q[./t/900_data/930_miniswarm]
                                     }
        );
        skip(sprintf(q[Failed to load torrent for seed_%s], $chr),
             $test_builder->{q[Expected_Tests]}
                 - $test_builder->{q[Curr_Test]}
        ) if not $torrent;
        $torrent->hashcheck;
        skip(
            sprintf(
                q[Failed to load torrent for seed_%s: Seed data is missing/corrupt],
                $chr),
            $test_builder->{q[Expected_Tests]} - $test_builder->{q[Curr_Test]}
        ) if not $torrent->is_complete;
        skip(sprintf(q[Failed to open UDP port], $chr),
             $test_builder->{q[Expected_Tests]}
                 - $test_builder->{q[Curr_Test]}
        ) if not $client{q[seed_] . $chr}->_udp_port;
        ok(scalar($torrent->is_complete),
            sprintf(q[seed_%s is seeding], $chr));
        skip(sprintf(q[Failed to load torrent for seed_%s], $chr),
             $test_builder->{q[Expected_Tests]}
                 - $test_builder->{q[Curr_Test]}
        ) if not $torrent->is_complete;
        $client{q[seed_] . $chr}->_dht->_add_node(
                 sprintf(q[%s:%d], q[127.0.0.1], $client{q[DHT]}->_udp_port));
        $client{q[seed_] . $chr}->do_one_loop(0.1);
    }
    for my $chr (1 .. $Peers_DHT) {
        $chr = sprintf $sprintf, $chr;
        $client{$chr} = new Net::BitTorrent({LocalAddr => q[127.0.0.1]});
        skip(sprintf(q[Failed to open UDP port], $chr),
             $test_builder->{q[Expected_Tests]}
                 - $test_builder->{q[Curr_Test]}
        ) if not $client{$chr}->_udp_port;
        skip(sprintf(q[Failed to create dht_%s], $chr),
             $test_builder->{q[Expected_Tests]}
                 - $test_builder->{q[Curr_Test]}
        ) if not $client{$chr};
        ok($client{$chr}->_use_dht, sprintf q[peer_%s has enabled dht], $chr);
        $client{$chr}->on_event(
            q[piece_hash_pass],
            sub {
                my ($self, $args) = @_;
                my $piece
                    = $args->{q[Torrent]}->_piece_by_index($args->{q[Index]});
                my $sum = 0;
                for my $offset (0 .. $args->{q[Torrent]}->_piece_count - 1) {
                    $sum += vec($args->{q[Torrent]}->bitfield, $offset, 1);
                }
                ok($args->{q[Torrent]}->is_complete,
                    sprintf(q[peer_%s is seeding], $chr))
                    if $args->{q[Torrent]}->is_complete;
                return;
            }
        );
        my $torrent =
            $client{$chr}->add_torrent(
                                     {Path => $miniswarm_dot_torrent,
                                      BaseDir =>
                                          File::Temp::tempdir(
                                          sprintf(q[miniswarm_%s_XXXX], $chr),
                                          CLEANUP => 1,
                                          TMPDIR  => 1
                                          ),
                                      BlockLength => $BlockLength
                                     }
            );
        skip(sprintf(q[Failed to load torrent for dht_%s], $chr),
             $test_builder->{q[Expected_Tests]}
                 - $test_builder->{q[Curr_Test]}
        ) if not $torrent;
        $client{$chr}->_dht->_add_node(
                 sprintf(q[%s:%d], q[127.0.0.1], $client{q[DHT]}->_udp_port));
    }
    while ($test_builder->{q[Curr_Test]} < $test_builder->{q[Expected_Tests]})
    {   grep { $_->do_one_loop(0.1); } values %client;
        skip(q[This is taking too long and I have a train to catch.],
             (      $test_builder->{q[Expected_Tests]}
                  - $test_builder->{q[Curr_Test]}
             )
        ) if (int(time - $^T) > $Timeout);
    }
}

=head1 Author

Sanko Robinson <sanko@cpan.org> - http://sankorobinson.com/

CPAN ID: SANKO

=head1 License and Legal

Copyright (C) 2008 by Sanko Robinson E<lt>sanko@cpan.orgE<gt>

This program is free software; you can redistribute it and/or modify
it under the terms of The Artistic License 2.0.  See the F<LICENSE>
file included with this distribution or
http://www.perlfoundation.org/artistic_license_2_0.  For
clarification, see http://www.perlfoundation.org/artistic_2_0_notes.

When separated from the distribution, all POD documentation is covered
by the Creative Commons Attribution-Share Alike 3.0 License.  See
http://creativecommons.org/licenses/by-sa/3.0/us/legalcode.  For
clarification, see http://creativecommons.org/licenses/by-sa/3.0/us/.

Neither this module nor the L<Author|/Author> is affiliated with
BitTorrent, Inc.

=for svn $Id$

=cut
