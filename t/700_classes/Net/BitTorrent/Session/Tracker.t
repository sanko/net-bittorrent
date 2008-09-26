#!/usr/bin/perl -w
use strict;
use warnings;
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

my $build = Module::Build->current;
my $can_talk_to_ourself = $build->notes(q[can_talk_to_ourself]);

#
$|++;

#
my $multi_dot_torrent  = q[./t/900_data/950_torrents/952_multi.torrent];
my $single_dot_torrent = q[./t/900_data/950_torrents/951_single.torrent];

# Make sure the path is correct
chdir q[../../../../../] if not -f $multi_dot_torrent;

#
BEGIN {
    use Test::More;
    plan tests => 21;
    $SIG{__WARN__} = sub { diag shift };    # Quiet Carp
    diag(q[Testing Net::BitTorrent::Tracker]);
    use_ok(q[Net::BitTorrent::Session::Tracker]);
    use_ok(q[Net::BitTorrent]);
    use_ok(q[Net::BitTorrent::Session]);
}

#
{
    my $client = Net::BitTorrent->new();
    my $session =
        Net::BitTorrent::Session->new({Client => $client,
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
    diag(q[Testing Net::BitTorrent::Session::Tracker]);
    diag(q[new() requires parameters...]);
    is(Net::BitTorrent::Session::Tracker->new(), undef, q[new( )]);
    is( Net::BitTorrent::Session::Tracker->new(
                                        URLs => q[http://example.com/announce]
        ),
        undef,
        q[new(URLs => q[http://example.com/announce])],
    );
    is(Net::BitTorrent::Session::Tracker->new({}), undef, q[new({ })]);
    is( Net::BitTorrent::Session::Tracker->new(
                                      {URLs => q[http://example.com/announce]}
        ),
        undef,
        q[new({URLs => q[http://example.com/announce/]})]
    );
    is(Net::BitTorrent::Session::Tracker->new({URLs => undef}),
        undef, q[new({URLs => undef})],
    );
    is(Net::BitTorrent::Session::Tracker->new({URLs => []}),
        undef, q[new({URLs => qw[]})]);
    is( Net::BitTorrent::Session::Tracker->new(
                                   {URLs => [qw[http://example.com/announce]]}
        ),
        undef,
        q[new({URLs   => [qw[http://example.com/announce]]})]
    );
    is( Net::BitTorrent::Session::Tracker->new(
                     {URLs => [qw[http://example.com/announce]], Session => 0}
        ),
        undef,
        q[new({URLs   => [qw[http://example.com/announce]], Session => 0 })]
    );
    is( Net::BitTorrent::Session::Tracker->new(
                        {URLs    => [],
                         Session => (bless {}, q[Net::BitTorrent::Session])
                        }
        ),
        undef,
        q[new({URLs => [], Session => bless({},q[Net::BitTorrent::Session])})]
    );
    is( Net::BitTorrent::Session::Tracker->new(
                                {URLs    => [qw[http://example.com/announce]],
                                 Session => (bless {}, q[Net::A::Session])
                                }
        ),
        undef,
        q[new({URLs => [...], Session => bless({},q[Net::A::Session])})]
    );

    #
    isa_ok(
        Net::BitTorrent::Session::Tracker->new(
            {   URLs => [
                    qw[http://example.com/announce
                        udp://example.com/announce/]
                ],
                Session => $session
            }
        ),
        q[Net::BitTorrent::Session::Tracker],
        q[new({URLs => [qw[http... udp...]], Session => bless({},q[Net::BitTorrent::Session])})]
    );

    #
    diag(q[ Accessors]);
    my $tracker =
        Net::BitTorrent::Session::Tracker->new(
                                {URLs    => [qw[http://example.com/announce]],
                                 Session => $session
                                }
        );
    ok($tracker->_set_complete(30),   q[Set number of seeds]);
    ok($tracker->_set_incomplete(50), q[Set number of peers]);
    is($tracker->complete(),   30, q[Get number of seeds]);
    is($tracker->incomplete(), 50, q[Get number of seeds]);
    is_deeply($tracker->_session, $session,
              q[Get related N::B::Session object]);
    is($tracker->_client->isa(q[Net::BitTorrent]),
        1, q[Get related Net::BitTorrent object]);
    is_deeply($tracker->_urls,
              [bless(do { \(my $o = "http://example.com/announce") },
                     "Net::BitTorrent::Session::Tracker::HTTP"
               )
              ],
              q[_urls]
    );

    #
    diag(q[TODO: create a fake tracker and connect to it])
}
