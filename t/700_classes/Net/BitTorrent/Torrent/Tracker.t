#!C:\perl\bin\perl.exe -w
use strict;
use warnings;
use Test::More;
use Module::Build;
use lib q[../../../../../lib];
use Net::BitTorrent::Torrent::Tracker;
use Net::BitTorrent::Torrent;
use Net::BitTorrent;
$|++;
my $test_builder       = Test::More->builder;
my $simple_dot_torrent = q[./t/900_data/950_torrents/953_miniswarm.torrent];
chdir q[../../../../../] if not -f $simple_dot_torrent;
my $build           = Module::Build->current;
my $okay_tcp        = $build->notes(q[okay_tcp]);
my $release_testing = $build->notes(q[release_testing]);
my $verbose         = $build->notes(q[verbose]);
$SIG{__WARN__} = ($verbose ? sub { diag shift } : sub { });
$|++;
my $multi_dot_torrent  = q[./t/900_data/950_torrents/952_multi.torrent];
my $single_dot_torrent = q[./t/900_data/950_torrents/951_single.torrent];
plan tests => 18;
SKIP: {
    my $client = Net::BitTorrent->new();
    my $torrent =
        Net::BitTorrent::Torrent->new({Client => $client,
                                       Path   => $single_dot_torrent
                                      }
        );
    $client->on_event(q[tracker_success],
                      sub { my ($client, $args) = @_; warn q[Announce]; });
    $client->on_event(q[tracker_connect],
                      sub { my ($client, $args) = @_; warn q[Connect] });
    $client->on_event(q[tracker_disconnect],
                      sub { my ($client, $args) = @_; warn q[Disconnect] });
    $client->on_event(q[tracker_failure],
                      sub { my ($client, $args) = @_; warn q[Failure] });
    $client->on_event(q[tracker_read],
                      sub { my ($client, $args) = @_; warn q[Read] });
    $client->on_event(q[tracker_write],
                      sub { my ($client, $args) = @_; warn q[Wrote] });
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
    warn(q[TODO: create a fake tracker and connect to it]);
}
__END__
Copyright (C) 2008 by Sanko Robinson <sanko@cpan.org>

This program is free software; you can redistribute it and/or modify it
under the terms of The Artistic License 2.0.  See the LICENSE file
included with this distribution or
http://www.perlfoundation.org/artistic_license_2_0.  For
clarification, see http://www.perlfoundation.org/artistic_2_0_notes.

When separated from the distribution, all POD documentation is covered by
the Creative Commons Attribution-Share Alike 3.0 License.  See
http://creativecommons.org/licenses/by-sa/3.0/us/legalcode.  For
clarification, see http://creativecommons.org/licenses/by-sa/3.0/us/.

$Id$
