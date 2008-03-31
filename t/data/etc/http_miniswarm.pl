# -*- perl -*-

# t/data/etc/http_miniswarm.pl - Used by new 500 tests
# $Id$

use strict;
use warnings;
use Socket;

my ( $seeds, $leeches ) = @ARGV;

socketpair( my ($CHILD), my ($PARENT), AF_UNIX,
            SOCK_STREAM, PF_UNSPEC
) or die "socketpair: $!";

if ( my $pid = fork ) {
    my $port = undef;
    close $PARENT;
    chomp( my $line = readline $CHILD );
    if ( $line =~ m[p:(\d+)] ) { $port = $1 }
    else                       { die; }    # ...um, I mean skip.
    close $CHILD;
    {
        use Test::More;
        use File::Temp qw[];

        use lib q[../../lib];

        use Net::BitTorrent;
        use Net::BitTorrent::Util qw[compact];

        plan tests => $seeds + $leeches;

        my %client;
        my $test_builder = Test::More->builder;

        for my $chr ( 1 .. $seeds ) {
            $client{ q[seed_] . $chr } = new Net::BitTorrent(
                      { LocalAddr => q[127.0.0.1], Timeout => 0.1 } );

            skip( sprintf( q[Failed to create seed_%s], $chr ),
                  $test_builder->{Expected_Tests}
                      - $test_builder->{Curr_Test}
            ) if not $client{ q[seed_] . $chr };

            my $session = $client{ q[seed_] . $chr }->add_session(
                    { path => q[./t/data/torrents/miniswarm.torrent],
                      base_dir => q[./t/data/miniswarm/]
                    }
            );
            skip( sprintf( q[Failed to load session for seed_%s],
                           $chr ),
                  $test_builder->{Expected_Tests}
                      - $test_builder->{Curr_Test}
            ) if not $session;

            ok( scalar( $session->complete ),
                sprintf( q[seed_%s ok], $chr ) );
            my $tracker = qq[http://127.0.0.1:$port/announce];
            $session->add_tracker( [$tracker] );
        }

        for my $chr ( 1 .. $leeches ) {
            $client{$chr} = new Net::BitTorrent(
                      { LocalAddr => q[127.0.0.1], Timeout => 0.1 } );

            skip( sprintf( q[Failed to create leech_%s], $chr ),
                  $test_builder->{Expected_Tests}
                      - $test_builder->{Curr_Test}
            ) if not $client{$chr};

            $client{$chr}->set_callback_on_piece_hash_pass(
                sub {
                    my ( $self, $piece ) = @_;
                    my $session = $piece->session;
                    my $completion = (
                        (  (  (  scalar grep {
                                             $_->priority
                                         and $_->check
                                     } @{ $session->pieces }
                              ) / ( scalar @{ $session->pieces } )
                           )
                        ) * 100
                    );
                    my $line =
                        sprintf(
                            q[(%02d|%02d) [%s] %.2f%%],
                            $chr,
                            $piece->index,
                            join(
                                q[],
                                map ( (  $_->check
                                       ? $piece->index == $_->index
                                               ? q[*]
                                               : q[|]
                                       : scalar $_->working ? q[.]
                                       : q[ ]
                                    ),
                                    @{ $session->pieces } )
                            ),
                            $completion
                        );

                    #diag($line);
                    ok( $session->complete ) if $session->complete;
                    return;
                }
            );
            my $session =
                $client{$chr}->add_session(
                    { path => q[./t/data/torrents/miniswarm.torrent],
                      base_dir =>
                          File::Temp::tempdir(
                                sprintf( q[miniswarm_%s_XXXX], $chr ),
                                CLEANUP => 1,
                                TMPDIR  => 1
                          ),
                      skip_hashcheck => 1
                    }
                );

            skip( sprintf( q[Failed to load session for leech_%s],
                           $chr ),
                  $test_builder->{Expected_Tests}
                      - $test_builder->{Curr_Test}
            ) if not $session;
            my $tracker = qq[http://127.0.0.1:$port/announce];
            $session->add_tracker( [$tracker] );
        }
        while ( $test_builder->{Curr_Test}
                < $test_builder->{Expected_Tests} )
        {
            grep { $_->do_one_loop } values %client;
        }
        grep {
            $_->remove_session( $_->sessions->[0] )
                if scalar @{ $_->sessions }
        } values %client;
        kill 9, $pid;
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

        socket( my ($httpd),
                PF_INET, SOCK_STREAM, getprotobyname(q[tcp]) )
            || die(q[socket]);
        setsockopt( $httpd, SOL_SOCKET, SO_REUSEADDR, 1 )
            || die(q[setsockopt]);

        bind( $httpd,
              pack( q[Sna4x8],
                    &AF_INET, $port,
                    (  join q[], map { chr $_ } ( $host =~ m[(\d+)]g )
                    )
              )
        ) || die(q[bind]);
        listen( $httpd, SOMAXCONN ) || die(q[listen]);

        ( undef, $port, undef )
            = unpack( q[SnC4x8], getsockname($httpd) );
        syswrite $PARENT, qq[p:$port];
        close $PARENT;

        while ( my $paddr = accept( my ($client), $httpd ) ) {
            my $gotten = q[];
            while ( sysread( $client, my ($data), 1024 ) ) {
                $gotten .= $data;
                if ( $data
                     =~ m[^GET\s+(/(announce|scrape)\?([^\s]*))] )
                {
                    my $type = $2;
                    my %hash = split m[[=&]], $3;
                    $hash{q[info_hash]}
                        =~ s|\%([a-f0-9]{2})|pack(q[C], hex($1))|ieg;
                    my ( undef, undef, @address )
                        = unpack( q[SnC4x8], getsockname($client) );
                    my %reply;
                    if ( $type eq q[announce] ) {
                        $hash{q[peer_id]}
                            =~ s|\%([a-f0-9]{2})|pack(q[C], hex($1))|ieg;
                        %reply = (
                            interval => 1500,
                            peers => $tracker_data{ $hash{info_hash} }
                                || q[]
                        );
                        $tracker_data{ $hash{info_hash} }
                            = compact(
                                 ( ( join q[.], @address ) . q[:]
                                       . $hash{port}
                                 ),
                                 uncompact(
                                     $tracker_data{ $hash{info_hash} }
                                 )
                            );
                    }
                    else {    # TODO: scrape
                    }
                    syswrite( $client,
                              sprintf
                                  <<END, scalar gmtime, bencode( \%reply ) ); }
HTTP/1.0 200 Here ya go!
Date: %s GMT
Server: Net::BitTorrent test tracker/1.0
Content-type: text/plain

%s
END
                else {
                    syswrite( $client,
                              sprintf <<END, scalar gmtime ); }
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
