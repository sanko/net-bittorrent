# -*- perl -*-
# t/data/etc/http_miniswarm.pl - Used by new 500 tests
# $Id$
use strict;
use warnings;
use Socket;
my ($seeds, $leeches, $timeout) = @ARGV;
$seeds   ||= 1;
$leeches ||= 1;
$timeout ||= 120;
my $sprintf = q[%0] . length($leeches > $seeds ? $leeches : $seeds) . q[d];
socketpair(my ($CHILD), my ($PARENT), AF_UNIX, SOCK_STREAM, PF_UNSPEC)
    or die "socketpair: $!";

if (my $pid = fork) {
    my $port = undef;
    close $PARENT;
    chomp(my $line = readline $CHILD);
    if ($line =~ m[p:(\d+)]) { $port = $1 }
    else                     { die; }    # XXX - ...um, I mean skip.
    close $CHILD;
SKIP: {
        use Test::More;
        use File::Temp qw[];
        use lib q[../../lib];
        use lib q[../../../lib];
        use Net::BitTorrent;
        use Net::BitTorrent::Util qw[compact];
        plan tests => int($seeds + $leeches);
        my %client;
        my $test_builder = Test::More->builder;

        for my $chr (1 .. $seeds) {
            $chr = sprintf $sprintf, $chr;
            $client{q[seed_] . $chr}
                = new Net::BitTorrent({LocalAddr => q[127.0.0.1]});
            skip_all(sprintf(q[Failed to create seed_%s], $chr),
                     $test_builder->{q[Expected_Tests]}
                         - $test_builder->{q[Curr_Test]}
            ) if not $client{q[seed_] . $chr};

         #{    # DEBUG
         #$client{q[seed_] . $chr}->set_callback(q[peer_connect],
         #                           sub { shift; warn shift; });
         #$client{q[seed_] . $chr}->set_callback(q[peer_disconnect],
         #                           sub { shift; shift; warn shift; });
         #$client{q[seed_] . $chr}->set_callback(q[tracker_error],
         #                           sub { shift; shift; warn shift; });
         #$client{q[seed_] .$chr}->set_callback(q[peer_incoming_packet], sub {
         #    shift; shift;
         #    use Data::Dump qw[pp];
         #warn q[Packet!] . pp shift});
         #$client{q[seed_] . $chr}
         #    ->set_callback(q[log], sub { shift; shift; warn shift; });
         #$client{q[seed_] . $chr}->set_debug_level(1000);
         #}
            my $session = $client{q[seed_] . $chr}->add_session(
                           {path => q[./t/data/torrents/miniswarm.torrent],
                            base_dir => q[./t/data/miniswarm/]
                           }
            );
            skip(sprintf(q[Failed to load session for seed_%s], $chr),
                 $test_builder->{q[Expected_Tests]}
                     - $test_builder->{q[Curr_Test]}
            ) if not $session;
            ok(scalar($session->get_complete), sprintf(q[seed_%s ok], $chr));
            skip(sprintf(q[Failed to load session for seed_%s], $chr),
                 $test_builder->{q[Expected_Tests]}
                     - $test_builder->{q[Curr_Test]}
            ) if not $session->get_complete;
            my $tracker = qq[http://127.0.0.1:$port/announce];
            $session->add_tracker([$tracker]);
            $client{q[seed_] . $chr}->do_one_loop(0.1);    # let them announce
        }
        for my $chr (1 .. $leeches) {
            $chr = sprintf $sprintf, $chr;
            $client{$chr} = new Net::BitTorrent({LocalAddr => q[127.0.0.1]});
            skip(sprintf(q[Failed to create leech_%s], $chr),
                 $test_builder->{q[Expected_Tests]}
                     - $test_builder->{q[Curr_Test]}
            ) if not $client{$chr};

#{    # DEBUG
#$client{$chr}->set_callback(q[peer_connect],
#                           sub { shift;  warn shift; });
#$client{$chr}->set_callback(q[peer_disconnect],
#                           sub { shift; shift; warn shift; });
#$client{$chr}->set_callback(q[tracker_error],
#                           sub { shift; shift; warn shift; });
#$client{$chr}->set_callback(q[peer_outgoing_request], sub {warn q[Request!]});
#$client{$chr}->set_callback(q[peer_incoming_packet], sub {
#    shift; shift;
#    use Data::Dump qw[pp];
#    warn q[Packet!] . pp shift
#    });
#$client{$chr}
#    ->set_callback(q[log], sub { shift; shift; warn shift; });
#$client{$chr}->set_debug_level(1000);
#}
            $client{$chr}->set_callback(
                q[piece_hash_pass],
                sub {
                    my ($self, $piece) = @_;
                    my $session = $piece->get_session;
                    my $completion = (
                        (((scalar grep {
                                       $_->get_priority
                                   and $_->get_cached_integrity
                               } @{$session->get_pieces}
                          ) / (scalar @{$session->get_pieces})
                         )
                        ) * 100
                    );
                    diag(sprintf(
                               q[(%02d|%02d) [%s] %.2f%%],
                               $chr,
                               $piece->get_index,
                               join(
                                   q[],
                                   map ((  $_->get_cached_integrity
                                         ? $piece->get_index == $_->get_index
                                                 ? q[*]
                                                 : q[|]
                                         : scalar $_->get_working ? q[.]
                                         : q[ ]
                                       ),
                                       @{$session->get_pieces})
                               ),
                               $completion
                         )
                        )
                        if $ENV{q[RELEASE_TESTING]};
                    ok($session->get_complete,
                        sprintf(q[peer_%s complete], $chr))
                        if $session->get_complete;
                    return;
                }
            );
            my $session =
                $client{$chr}->add_session(
                           {path => q[./t/data/torrents/miniswarm.torrent],
                            base_dir =>
                                File::Temp::tempdir(
                                          sprintf(q[miniswarm_%s_XXXX], $chr),
                                          CLEANUP => 1,
                                          TMPDIR  => 1
                                ),
                            skip_hashcheck => 1
                           }
                );
            skip(sprintf(q[Failed to load session for leech_%s], $chr),
                 $test_builder->{q[Expected_Tests]}
                     - $test_builder->{q[Curr_Test]}
            ) if not $session;
            my $tracker = qq[http://127.0.0.1:$port/announce];
            $session->add_tracker([$tracker]);
        }
        while ($test_builder->{q[Curr_Test]}
               < $test_builder->{q[Expected_Tests]})
        {   grep { $_->do_one_loop(0.1); } values %client;
            skip(q[This is taking too long and I have a train to catch.],
                 (      $test_builder->{q[Expected_Tests]}
                      - $test_builder->{q[Curr_Test]}
                 )
            ) if (int(time - $^T) > $timeout);
        }

        END {
            kill 9, $pid if defined $pid;
            grep {
                $_->remove_session($_->get_sessions->[0])
                    if scalar @{$_->get_sessions}
            } values %client;
        }
        exit;
    }
}
else {
    die "cannot fork: $!" unless defined $pid;
    close $CHILD;
    {
        use Net::BitTorrent::Util qw[:compact :bencode];
        my %tracker_data;
        my $port = 0;
        my $host = q[127.0.0.1];
        socket(my ($httpd), PF_INET, SOCK_STREAM, getprotobyname(q[tcp]))
            || die(q[socket]);
        setsockopt($httpd, SOL_SOCKET, SO_REUSEADDR, 1)
            || die(q[setsockopt]);
        bind($httpd,
             pack(q[Sna4x8],
                  &AF_INET, $port,
                  (join q[], map { chr $_ } ($host =~ m[(\d+)]g)))
        ) || die(q[bind]);
        listen($httpd, SOMAXCONN) || die(q[listen]);
        (undef, $port, undef) = unpack(q[SnC4x8], getsockname($httpd));
        syswrite $PARENT, qq[p:$port];
        close $PARENT;

        while (my $paddr = accept(my ($client), $httpd)) {
            my $gotten = q[];
            while (sysread($client, my ($data), 1024)) {
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
                               peers => $tracker_data{$hash{info_hash}} || q[]
                        );
                        $tracker_data{$hash{info_hash}}
                            = compact(((join q[.], @address) . q[:]
                                       . $hash{port}),
                                      uncompact(
                                               $tracker_data{$hash{info_hash}}
                                      )
                            );
                    }
                    else {    # TODO: scrape
                    }
                    syswrite($client,
                             sprintf
                                 <<END, scalar gmtime, bencode(\%reply)); }
HTTP/1.0 200 Here ya go!
Date: %s GMT
Server: Net::BitTorrent test tracker/1.0
Content-type: text/plain

%s
END
                else {
                    syswrite($client, sprintf <<END, scalar gmtime); }
HTTP/1.0 404 Go away
Date: %s GMT
Server: Net::BitTorrent test tracker/1.0
Content-type: text/plain

Bye.
END
                last if $data =~ m[\015?\012\015?\012];
            }
            close $client;
        }
    }
}
