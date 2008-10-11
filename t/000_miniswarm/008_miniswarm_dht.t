# -*- perl -*-
# $Id$
# Miniature swarm of 1 seed, 1 dht tracker, and 5 new peers
#
use strict;
use warnings;
use Module::Build;
use Socket;
use Fcntl qw[:flock];    # core as of perl 5
use Test::More;
use File::Temp qw[];
use IO::Socket qw[SOMAXCONN];
use List::Util qw[sum];

#
use lib q[../../lib];
use Net::BitTorrent;
use Net::BitTorrent::Util qw[compact];

#
$|++;

# let's keep track of where we are...
my $test_builder = Test::More->builder;

#
my $simple_dot_torrent = q[./t/900_data/950_torrents/953_miniswarm.torrent];

# Make sure the path is correct
chdir q[../../] if not -f $simple_dot_torrent;

#
my $build    = Module::Build->current;
my $okay_tcp = $build->notes(q[okay_tcp]);
my $okay_udp = $build->notes(q[okay_udp]);
my $verbose  = $build->notes(q[verbose]);
$SIG{__WARN__} = ($verbose ? sub { diag shift } : sub { });

#
my $BlockLength = 2**14;
my $Seeds       = 1;
my $Peers_DHT   = 5;
my $Timeout     = 60;      # needs a little extra time to ramp up

#
my $sprintf
    = q[%0] . length($Peers_DHT > $Seeds ? $Peers_DHT : $Seeds) . q[d];

#
my $miniswarm_dot_torrent
    = q[./t/900_data/950_torrents/953_miniswarm.torrent];
my $_infohash = q[2b3aaf361bd40540bf7e3bfd140b954b90e4dfbc];

#
$|++;

#
plan tests => int($Seeds + $Peers_DHT + 1) * 2;

#
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
            next if not defined $client->sessions->{$_infohash};
            for my $file (@{$client->sessions->{$_infohash}->files}) {
                $file->_close;
            }
        }
    }
    my $test_builder = Test::More->builder;
    $client{q[DHT]} = new Net::BitTorrent({LocalAddr => q[127.0.0.1]});
    ok($client{q[DHT]}->isa(q[Net::BitTorrent]), q[DHT (bystander)]);
    $client{q[DHT]}->_use_dht(1);
    ok($client{q[DHT]}->_dht, q[DHT (bystander) has enabled dht]);
    for my $chr (1 .. $Seeds) {
        $chr = sprintf $sprintf, $chr;
        $client{q[seed_] . $chr}
            = new Net::BitTorrent({LocalAddr => q[127.0.0.1]});
        skip(sprintf(q[Failed to create seed_%s], $chr),
             $test_builder->{q[Expected_Tests]}
                 - $test_builder->{q[Curr_Test]}
        ) if not $client{q[seed_] . $chr};
        $client{q[seed_] . $chr}->_use_dht(1);
        ok($client{q[seed_] . $chr}->_dht,
            sprintf q[seed_%s has enabled dht], $chr);
        my $session = $client{q[seed_] . $chr}->add_session(
                                     {Path    => $miniswarm_dot_torrent,
                                      BaseDir => q[./t/900_data/930_miniswarm]
                                     }
        );
        skip(sprintf(q[Failed to load session for seed_%s], $chr),
             $test_builder->{q[Expected_Tests]}
                 - $test_builder->{q[Curr_Test]}
        ) if not $session;
        $session->hashcheck;
        skip(
            sprintf(
                q[Failed to load session for seed_%s: Seed data is missing/corrupt],
                $chr),
            $test_builder->{q[Expected_Tests]} - $test_builder->{q[Curr_Test]}
        ) if not $session->_complete;
        ok(scalar($session->_complete), sprintf(q[seed_%s is seeding], $chr));
        skip(sprintf(q[Failed to load session for seed_%s], $chr),
             $test_builder->{q[Expected_Tests]}
                 - $test_builder->{q[Curr_Test]}
        ) if not $session->_complete;
        $client{q[seed_] . $chr}->_dht->_add_node(
               sprintf(q[%s:%d], q[127.0.0.1], $client{q[DHT]}->_dht->_port));
        $client{q[seed_] . $chr}->do_one_loop(0.1);    # let them announce
    }
    for my $chr (1 .. $Peers_DHT) {
        $chr = sprintf $sprintf, $chr;
        $client{$chr} = new Net::BitTorrent({LocalAddr => q[127.0.0.1]});
        skip(sprintf(q[Failed to create dht_%s], $chr),
             $test_builder->{q[Expected_Tests]}
                 - $test_builder->{q[Curr_Test]}
        ) if not $client{$chr};
        $client{$chr}->_use_dht(1);
        ok($client{$chr}->_dht, sprintf q[peer_%s has enabled dht], $chr);
        $client{$chr}->on_event(
            q[piece_hash_pass],
            sub {
                my ($self, $args) = @_;
                my $piece
                    = $args->{q[Session]}->_piece_by_index($args->{q[Index]});
                my $sum = 0;
                for my $offset (0 .. $args->{q[Session]}->_piece_count - 1) {
                    $sum += vec($args->{q[Session]}->bitfield, $offset, 1);
                }
                ok($args->{q[Session]}->_complete,
                    sprintf(q[peer_%s is seeding], $chr))
                    if $args->{q[Session]}->_complete;
                return;
            }
        );
        my $session = $client{$chr}->add_session(
            {Path => $miniswarm_dot_torrent,
             BaseDir =>
                 File::Temp::tempdir(sprintf(q[miniswarm_%s_XXXX], $chr),
                                     CLEANUP => 1,
                                     TMPDIR  => 1
                 ),
             BlockLength => $BlockLength    # Undocumented
            }
        );
        skip(sprintf(q[Failed to load session for dht_%s], $chr),
             $test_builder->{q[Expected_Tests]}
                 - $test_builder->{q[Curr_Test]}
        ) if not $session;
        $client{$chr}->_dht->_add_node(
               sprintf(q[%s:%d], q[127.0.0.1], $client{q[DHT]}->_dht->_port));
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
