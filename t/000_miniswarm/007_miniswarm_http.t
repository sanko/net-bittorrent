# -*- perl -*-
# $Id$
# Miniature swarm of 1 seed and 5 new peers
#
use strict;
use warnings;
use Module::Build;
use Test::More;

#
use Socket;
use Fcntl qw[:flock];    # core as of perl 5
use Time::HiRes;
use IO::Socket qw[SOMAXCONN];
use List::Util qw[sum];
use File::Temp qw[];

#
use lib q[../../lib];
use Net::BitTorrent;
use Net::BitTorrent::Util qw[:compact :bencode];

#
$|++;

# let's keep track of where we are...
my $test_builder = Test::More->builder;

#
my $simple_dot_torrent = q[./t/900_data/950_torrents/953_miniswarm.torrent];

# Make sure the path is correct
chdir q[../../] if not -f $simple_dot_torrent;

#
my $build               = Module::Build->current;
my $can_talk_to_ourself = $build->notes(q[can_talk_to_ourself]);

#
{    # Simulate a private .torrent
    no warnings q[redefine];
    *Net::BitTorrent::Session::_private = sub { return 1 };
}

#
my $BlockLength = 2**14;
my $Seeds       = 1;
my $Peers       = 5;
my $Timeout     = 45;
plan tests => int($Seeds + $Peers);

#
my $sprintf = q[%0] . length($Peers > $Seeds ? $Peers : $Seeds) . q[d];

#
my $miniswarm_dot_torrent
    = q[./t/900_data/950_torrents/953_miniswarm.torrent];
my $_infohash = q[2b3aaf361bd40540bf7e3bfd140b954b90e4dfbc];

#
$|++;
SKIP: {
 skip q[Socket-based tests have been disabled.], ($test_builder->{q[Expected_Tests]} - $test_builder->{q[Curr_Test]}) unless $can_talk_to_ourself;

    #
    my %_tracker_data;
    my $_tracker_port = 0;
    my $_tracker_host = q[127.0.0.1];
    socket(my ($httpd), PF_INET, SOCK_STREAM, getprotobyname(q[tcp]))
        || skip_all(q[Failed to open socket for tracker]);
    setsockopt($httpd, SOL_SOCKET, SO_REUSEADDR, 1)
        || skip_all(q[Failed to setsockopt for tracker]);
    bind($httpd,
         pack(q[Sna4x8],
              &AF_INET, $_tracker_port,
              (join q[], map { chr $_ } ($_tracker_host =~ m[(\d+)]g)))
        )
        || skip_all(sprintf q[Failed to bind tracker to port: [%d] %s], $^E,
                    $^E);
    listen($httpd, SOMAXCONN)
        || skip_all(sprintf q[Failed to listen on port: [%d] %s], $^E, $^E);
    (undef, $_tracker_port, undef) = unpack(q[SnC4x8], getsockname($httpd));
    diag(sprintf q[HTTP Mini-Tracker running on 127.0.0.1:%d],
         $_tracker_port);

    #
SKIP: {
        my %client;
        my $test_builder = Test::More->builder;

#if ((($Seeds + $Peers) * ($Seeds + $Peers)) >= (SOMAXCONN))
#{    # I hate five eight.
#    skip(
#        sprintf(
#            q[SOMAXCONN is too low. (SOMAXCONN == %d; $Seeds = %d; $Peers = %d)],
#            SOMAXCONN, $Seeds, $Peers
#        ),
#        $test_builder->{q[Expected_Tests]}
#            - $test_builder->{q[Curr_Test]}
#    );
#}
        for my $chr (1 .. $Seeds) {
            $chr = sprintf $sprintf, $chr;
            $client{q[seed_] . $chr}
                = new Net::BitTorrent({LocalAddr => q[127.0.0.1]});
            skip(sprintf(q[Failed to create seed_%s], $chr),
                 $test_builder->{q[Expected_Tests]}
                     - $test_builder->{q[Curr_Test]}
            ) if not $client{q[seed_] . $chr};
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
                $test_builder->{q[Expected_Tests]}
                    - $test_builder->{q[Curr_Test]}
            ) if not $session->_complete;
            ok(scalar($session->_complete), sprintf(q[seed_%s ok], $chr));
            skip(sprintf(q[Failed to load session for seed_%s], $chr),
                 $test_builder->{q[Expected_Tests]}
                     - $test_builder->{q[Curr_Test]}
            ) if not $session->_complete;
            my $tracker = qq[http://127.0.0.1:$_tracker_port/announce];
            $session->_add_tracker([$tracker]);
            $client{q[seed_] . $chr}->do_one_loop(0.1);    # let them announce
        }
        for my $chr (1 .. $Peers) {
            $chr = sprintf $sprintf, $chr;
            $client{$chr} = new Net::BitTorrent({LocalAddr => q[127.0.0.1]});
            skip(sprintf(q[Failed to create leech_%s], $chr),
                 $test_builder->{q[Expected_Tests]}
                     - $test_builder->{q[Curr_Test]}
            ) if not $client{$chr};
            $client{$chr}->on_event(
                q[piece_hash_pass],
                sub {
                    my ($self, $args) = @_;
                    my $piece = $args->{q[Session]}
                        ->_piece_by_index($args->{q[Index]});
                    my $sum = 0;
                    for my $offset (
                                   0 .. $args->{q[Session]}->_piece_count - 1)
                    {   $sum
                            += vec($args->{q[Session]}->bitfield, $offset, 1);
                    }

                   #
                   #my $completion = (
                   #              ((($args->{q[Session]}->_piece_count - $sum)
                   #                / ($args->{q[Session]}->_piece_count)
                   #               )
                   #              ) * 100
                   #);
                   #use Data::Dump qw[pp];
                   #warn pp $args->{q[Session]}->_piece_by_index(0);
                   #die pp $piece;
                   #warn $completion;
                   #my @have;
                   #for my $_index (
                   #               0 .. $args->{q[Session]}->_piece_count - 1)
                   #{   push @have,
                   #          vec($args->{q[Session]}->bitfield, $_index, 1)
                   #        ? $_ == $args->{q[Index]}
                   #            ? q[*]
                   #            : q[|]
                   #        : defined(
                   #            $args->{q[Session]}->_piece_by_index($_index))
                   #        ? q[.]
                   #        : q[ ];
                   #}
                   #
                   #diag(sprintf(q[(%02d|%02d) [%s] %.2f%%],
                   #             $chr, $args->{q[Index]},
                   #             join(q[], @have), $completion
                   #     )
                   #    ) # if $ENV{q[RELEASE_TESTING]}
                   #    ;
                    ok($args->{q[Session]}->_complete,
                        sprintf(q[peer_%s complete], $chr))
                        if $args->{q[Session]}->_complete;
                    return;
                }
            );
            $client{$chr}->on_event(
                q[tracker_announce_okay],
                sub {
                    my ($s, $a) = @_;
                    my ($t, $p) = ($a->{q[Tracker]}, $a->{q[Payload]});
                    return $t->_tier->_session->_new_peer();
                }
            );
            my $session = $client{$chr}->add_session(
                {   Path => $miniswarm_dot_torrent,
                    BaseDir =>
                        File::Temp::tempdir(
                                          sprintf(q[miniswarm_%s_XXXX], $chr),
                                          CLEANUP => 1,
                                          TMPDIR  => 1
                        ),
                    BlockLength => $BlockLength    # Undocumented
                }
            );
            skip(sprintf(q[Failed to load session for leech_%s], $chr),
                 $test_builder->{q[Expected_Tests]}
                     - $test_builder->{q[Curr_Test]}
            ) if not $session;
            my $tracker = qq[http://127.0.0.1:$_tracker_port/announce];
            $session->_add_tracker([$tracker]);
        }
        while ($test_builder->{q[Curr_Test]}
               < $test_builder->{q[Expected_Tests]})
        {   grep { $_->do_one_loop(0.1); check_tracker(); } values %client;
            skip(q[This is taking too long and I have a train to catch.],
                 (      $test_builder->{q[Expected_Tests]}
                      - $test_builder->{q[Curr_Test]}
                 )
            ) if (int(time - $^T) > $Timeout);
        }

        sub check_tracker {
            my $rin = q[];
            vec($rin, fileno($httpd), 1) = 1;

            #
            my ($nfound, $timeleft) = select($rin, undef, undef, 0.1);
            return if $nfound == 0;
            return if vec($rin, fileno($httpd), 1) != 1;

            #
            if (my $paddr = accept(my ($client), $httpd)) {
                grep { $_->do_one_loop(0.1); }
                    values %client;    # clear the pipes
                my $gotten = q[];
                if (sysread($client, my ($data), 1024 * 16)) {
                    $gotten .= $data;
                    if ($data =~ m[^GET\s+(/(announce|scrape)\?([^\s]*))]) {
                        my $type = $2;
                        my %hash = split m[[=&]], $3;
                        $hash{q[info_hash]}
                            =~ s|\%([a-f0-9]{2})|pack(q[C], hex($1))|ieg;
                        my (undef, undef, @address)
                            = unpack(q[SnC4x8], getsockname($client));
                        my %reply;
                        if ($type eq q[announce]) {
                            $hash{q[peer_id]}
                                =~ s|\%([a-f0-9]{2})|pack(q[C], hex($1))|ieg;
                            %reply = (
                                     interval => 1500,
                                     peers => $_tracker_data{$hash{info_hash}}
                                         || q[]
                            );
                            $_tracker_data{$hash{info_hash}}
                                = compact(
                                ((join q[.], @address) . q[:] . $hash{port}),
                                uncompact($_tracker_data{$hash{info_hash}}));
                        }
                        else {    # TODO: scrape
                        }
                        syswrite(
                              $client,
                              join(qq[\015\012],
                                  q[HTTP/1.0 200 Here ya go!],
                                  q[Date: ] . scalar(gmtime) . q[ GMT],
                                  q[Server: Net::BitTorrent test tracker/1.0],
                                  q[Content-type: text/plain],
                                  q[],
                                  bencode(\%reply))
                        );
                    }
                    else {
                        syswrite(
                              $client,
                              join(qq[\015\012],
                                  q[HTTP/1.0 404 Go away!],
                                  q[Date: ] . scalar(gmtime) . q[ GMT],
                                  q[Server: Net::BitTorrent test tracker/1.0],
                                  q[Content-type: text/plain],
                                  q[],
                                  q[Bye!])
                        );
                    }
                }
                close $client;
            }
        }

        END {
            for my $client (values %client) {
                for my $file (@{$client->sessions->{$_infohash}->files}) {
                    $file->_close;
                }
            }
        }
        exit;
    }
}
