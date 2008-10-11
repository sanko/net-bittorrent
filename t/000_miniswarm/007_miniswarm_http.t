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
my $build    = Module::Build->current;
my $okay_tcp = $build->notes(q[okay_tcp]);
my $verbose  = $build->notes(q[verbose]);
$SIG{__WARN__} = ($verbose ? sub { diag shift } : sub { });

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

#
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
    skip(q[TCP-based tests have been disabled.],
         ($test_builder->{q[Expected_Tests]} - $test_builder->{q[Curr_Test]})
    ) unless $okay_tcp;

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
    warn(sprintf q[HTTP Mini-Tracker running on 127.0.0.1:%d],
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
                   #warn(sprintf(q[(%02d|%02d) [%s] %.2f%%],
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


__END__

1..6
ok 1 - seed_1 ok
ok 2 - peer_1 complete
ok 3 - peer_3 complete
ok 4 - peer_2 complete
ok 5 - peer_4 complete
ok 6 - peer_5 complete
Total Elapsed Time = 43.06840 Seconds
         Real Time = 43.06840 Seconds
Exclusive Times
%Time ExclSec CumulS #Calls sec/call Csec/c  Name
 66.6   28.68 30.553    288   0.0996 0.1061  main::check_tracker
 27.5   11.84 13.395    325   0.0364 0.0412  Net::BitTorrent::do_one_loop
 0.97   0.418  0.764    557   0.0008 0.0014  Net::BitTorrent::Peer::_rw
 0.93   0.401  0.483    106   0.0038 0.0046  Net::BitTorrent::Peer::new
 0.58   0.251  0.251      6   0.0418 0.0418  Net::BitTorrent::__socket_open
 0.47   0.203  1.031    208   0.0010 0.0050  Net::BitTorrent::_process_connecti
                                             ons
 0.33   0.141  0.141      2   0.0705 0.0705  Module::Build::Base::_backticks
 0.33   0.140  0.308      8   0.0175 0.0385  Module::Build::BEGIN
 0.26   0.110  0.110      6   0.0183 0.0183  IO::Socket::BEGIN
 0.25   0.109  0.774     14   0.0078 0.0553  main::BEGIN
 0.22   0.094  0.140     26   0.0036 0.0054  Module::Build::Base::BEGIN
 0.15   0.063  0.063     21   0.0030 0.0030  Exporter::export
 0.13   0.056  0.113   1589   0.0000 0.0001  Net::BitTorrent::_event
 0.11   0.046  0.046     22   0.0021 0.0021  Net::BitTorrent::Session::File::_s
                                             ysopen
 0.11   0.046  0.046    520   0.0001 0.0001  Net::BitTorrent::Protocol::HANDSHA
                                             KE
>Exit code: 0    Time: 47.744

------------------------------------------------------------------------------
1..6
ok 1 - seed_1 ok
ok 2 # skip This is taking too long and I have a train to catch.
ok 3 # skip This is taking too long and I have a train to catch.
ok 4 # skip This is taking too long and I have a train to catch.
ok 5 # skip This is taking too long and I have a train to catch.
ok 6 # skip This is taking too long and I have a train to catch.
panic: Devel::DProf inconsistent subroutine return.
# Looks like your test died just after 6.
Total Elapsed Time = 44.73920 Seconds
         Real Time = 44.73920 Seconds
Exclusive Times
%Time ExclSec CumulS #Calls sec/call Csec/c  Name
 37.2   16.66 16.704    175   0.0952 0.0955  Net::BitTorrent::do_one_loop
 30.1   13.50 16.449    138   0.0978 0.1192  main::check_tracker
 4.30   1.924 10.339     14   0.1374 0.7385  main::BEGIN
 3.67   1.641  1.641      6   0.2735 0.2735  Net::BitTorrent::__socket_open
 2.79   1.250  4.701      8   0.1562 0.5876  Module::Build::BEGIN
 2.41   1.077  1.077     12   0.0897 0.0897  DynaLoader::dl_load_file
 2.24   1.000  2.734     26   0.0385 0.1052  Module::Build::Base::BEGIN
 2.09   0.937  0.937      2   0.4685 0.4685  Module::Build::Base::_backticks
 1.61   0.719  1.280     15   0.0479 0.0854  Net::BitTorrent::Session::BEGIN
 0.84   0.376  2.452     13   0.0289 0.1886  Net::BitTorrent::BEGIN
 0.70   0.312  0.312      6   0.0520 0.0520  IO::Socket::BEGIN
 0.66   0.297  0.313      3   0.0990 0.1043  POSIX::SigRt::BEGIN
 0.63   0.280  1.296      9   0.0311 0.1440  XSLoader::load
 0.59   0.266  0.390      3   0.0887 0.1300  Module::Build::Dumper::BEGIN
 0.56   0.250  0.293      6   0.0416 0.0488  Net::BitTorrent::Session::new
>Exit code: 0    Time: 55.182

