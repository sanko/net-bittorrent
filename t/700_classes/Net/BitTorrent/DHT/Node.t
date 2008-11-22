#!C:\perl\bin\perl.exe -w
use strict;
use warnings;
use Module::Build;
use Test::More;
use File::Temp qw[tempdir];
use Scalar::Util qw[/weak/];
use Socket qw[inet_aton pack_sockaddr_in];
use lib q[../../../../../lib];
use Net::BitTorrent::DHT::Node;
use Net::BitTorrent::DHT;
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
plan tests => 16;
SKIP: {
    my $client = Net::BitTorrent->new({LocalHost => q[127.0.0.1]});
    my $dht = Net::BitTorrent::DHT->new({Client => $client});
    if (!$client || !$dht) {
        skip(q[Failed to create client and/or dht object],
             (      $test_builder->{q[Expected_Tests]}
                  - $test_builder->{q[Curr_Test]}
             )
        );
    }
    is(Net::BitTorrent::DHT::Node->new(),
        undef, q[new( { ... }) requires params]);
    isa_ok(Net::BitTorrent::DHT::Node->new(
                {  DHT        => $dht,
                   PackedHost => pack_sockaddr_in(0, inet_aton(q[127.0.0.1]))
                }
           ),
           q[Net::BitTorrent::DHT::Node],
           q[new( { DHT => [...], PackedHost => [...] }) ]
    );
    is( Net::BitTorrent::DHT::Node->new(
                    DHT        => $dht,
                    PackedHost => pack_sockaddr_in(0, inet_aton(q[127.0.0.1]))
        ),
        undef,
        q[new( DHT => [...], PackedHost => [...] ) fails]
    );
    my $node =
        Net::BitTorrent::DHT::Node->new(
                {DHT        => $dht,
                 PackedHost => pack_sockaddr_in(0, inet_aton(q[127.0.0.1]))
                }
        );
    is($node->_host, q[127.0.0.1], q[_host() is correct]);
    like($node->_port, qr[^\d+$], q[_port() is correct]);
    is($node->_packed_host,
        pack_sockaddr_in($node->_port, inet_aton($node->_host)),
        q[_packed_host() is correct]);
    is($node->node_id, undef, q[node_id() is undef by default]);
    my $node2 =
        Net::BitTorrent::DHT::Node->new(
               {DHT        => $dht,
                PackedHost => pack_sockaddr_in(0, inet_aton(q[127.0.0.1])),
                NodeID     => q[Test]
               }
        );
    isa_ok($node2, q[Net::BitTorrent::DHT::Node],
           q[new( { DHT => [...], PackedHost => [...], NodeID => 'Test' } )]);
    is($node2->node_id, q[Test], q[node_id() is defined and correct]);
    like($node->_last_seen, qr[^\d+$], sprintf q[_last_seen() == %d (%s)],
         $node->_last_seen, scalar localtime($node->_last_seen));
    is_deeply($node->_infohashes, [], q[No infohashes by default]);
    ok($node->_add_infohash(q[A] x 40), q[Add good infohash]);
    ok(!$node->_add_infohash(q[Z] x 3), q[Cannot add bad infohash]);
    is_deeply($node->_infohashes, [q[a] x 40], q[No infohashes by default]);
    ok($node->_as_string(),  q[_as_string( ) | simple]);
    ok($node->_as_string(1), q[_as_string(1) | advanced]);
    warn(q[TODO: Install event handlers]);
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
