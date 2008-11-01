#!C:\perl\bin\perl.exe -w
use strict;
use warnings;
use Test::More;
use Module::Build;

#
use lib q[../../../../../lib];
$|++;

# let's keep track of where we are...
my $test_builder = Test::More->builder;

#
my $simple_dot_torrent = q[./t/900_data/950_torrents/953_miniswarm.torrent];

# Make sure the path is correct
chdir q[../../../../../] if not -f $simple_dot_torrent;

#
my $build           = Module::Build->current;
my $okay_tcp        = $build->notes(q[okay_tcp]);
my $release_testing = $build->notes(q[release_testing]);
my $verbose         = $build->notes(q[verbose]);
$SIG{__WARN__} = ($verbose ? sub { diag shift } : sub { });

#
$|++;

#
my $multi_dot_torrent  = q[./t/900_data/950_torrents/952_multi.torrent];
my $single_dot_torrent = q[./t/900_data/950_torrents/951_single.torrent];

#
BEGIN {
    plan tests => 21;
    use_ok(q[Net::BitTorrent::Torrent::Tracker]);
    use_ok(q[Net::BitTorrent]);
    use_ok(q[Net::BitTorrent::Torrent]);
}

#
SKIP: {

#     skip(
#~         q[Fine grained regression tests skipped; turn on $ENV{RELESE_TESTING} to enable],
#~         ($test_builder->{q[Expected_Tests]} - $test_builder->{q[Curr_Test]})
#~     ) if not $release_testing;
#
#
    my $client = Net::BitTorrent->new();
    my $torrent =
        Net::BitTorrent::Torrent->new({Client => $client,
                                       Path   => $single_dot_torrent
                                      }
        );
    $client->on_event(q[tracker_announce],
                      sub { my ($client, $args) = @_; warn q[Announce]; });
    $client->on_event(q[tracker_connect],
                      sub { my ($client, $args) = @_; warn q[Connect] });
    $client->on_event(q[tracker_disconnect],
                      sub { my ($client, $args) = @_; warn q[Disconnect] });
    $client->on_event(q[tracker_failure],
                      sub { my ($client, $args) = @_; warn q[Failure] });
    $client->on_event(q[tracker_incoming_data],
                      sub { my ($client, $args) = @_; warn q[Read] });
    $client->on_event(q[tracker_outgoing_data],
                      sub { my ($client, $args) = @_; warn q[Wrote] });

    #
    warn(q[Testing Net::BitTorrent::Torrent::Tracker]);
    warn(q[new() requires parameters...]);
    is(Net::BitTorrent::Torrent::Tracker->new(), undef, q[new( )]);
    is( Net::BitTorrent::Torrent::Tracker->new(
                                        URLs => q[http://example.com/announce]
        ),
        undef,
        q[new(URLs => q[http://example.com/announce])],
    );
    is(Net::BitTorrent::Torrent::Tracker->new({}), undef, q[new({ })]);
    is( Net::BitTorrent::Torrent::Tracker->new(
                                      {URLs => q[http://example.com/announce]}
        ),
        undef,
        q[new({URLs => q[http://example.com/announce/]})]
    );
    is(Net::BitTorrent::Torrent::Tracker->new({URLs => undef}),
        undef, q[new({URLs => undef})],
    );
    is(Net::BitTorrent::Torrent::Tracker->new({URLs => []}),
        undef, q[new({URLs => qw[]})]);
    is( Net::BitTorrent::Torrent::Tracker->new(
                                   {URLs => [qw[http://example.com/announce]]}
        ),
        undef,
        q[new({URLs   => [qw[http://example.com/announce]]})]
    );
    is( Net::BitTorrent::Torrent::Tracker->new(
                     {URLs => [qw[http://example.com/announce]], Torrent => 0}
        ),
        undef,
        q[new({URLs   => [qw[http://example.com/announce]], Torrent => 0 })]
    );
    is( Net::BitTorrent::Torrent::Tracker->new(
                        {URLs    => [],
                         Torrent => (bless {}, q[Net::BitTorrent::Torrent])
                        }
        ),
        undef,
        q[new({URLs => [], Torrent => bless({},q[Net::BitTorrent::Torrent])})]
    );
    is( Net::BitTorrent::Torrent::Tracker->new(
                                {URLs    => [qw[http://example.com/announce]],
                                 Torrent => (bless {}, q[Net::A::Torrent])
                                }
        ),
        undef,
        q[new({URLs => [...], Torrent => bless({},q[Net::A::Torrent])})]
    );

    #
    isa_ok(
        Net::BitTorrent::Torrent::Tracker->new(
            {   URLs => [
                    qw[http://example.com/announce
                        udp://example.com/announce/]
                ],
                Torrent => $torrent
            }
        ),
        q[Net::BitTorrent::Torrent::Tracker],
        q[new({URLs => [qw[http... udp...]], Torrent => bless({},q[Net::BitTorrent::Torrent])})]
    );

    #
    warn(q[ Accessors]);
    my $tracker =
        Net::BitTorrent::Torrent::Tracker->new(
                                {URLs    => [qw[http://example.com/announce]],
                                 Torrent => $torrent
                                }
        );
    ok($tracker->_set_complete(30),   q[Set number of seeds]);
    ok($tracker->_set_incomplete(50), q[Set number of peers]);
    is($tracker->complete(),   30, q[Get number of seeds]);
    is($tracker->incomplete(), 50, q[Get number of seeds]);
    is_deeply($tracker->_torrent, $torrent,
              q[Get related N::B::Torrent object]);
    is($tracker->_client->isa(q[Net::BitTorrent]),
        1, q[Get related Net::BitTorrent object]);
    is_deeply($tracker->_urls,
              [bless(do { \(my $o = "http://example.com/announce") },
                     "Net::BitTorrent::Torrent::Tracker::HTTP"
               )
              ],
              q[_urls]
    );

    #
    warn(q[TODO: create a fake tracker and connect to it]);
}

# $Id$
