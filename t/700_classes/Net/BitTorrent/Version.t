#!C:\perl\bin\perl.exe -w
use strict;
use warnings;
use Test::More;
use Module::Build;

#
use lib q[../../../../lib];
$|++;

# let's keep track of where we are...
my $test_builder = Test::More->builder;

#
my $simple_dot_torrent = q[./t/900_data/950_torrents/953_miniswarm.torrent];

# Make sure the path is correct
chdir q[../../../../] if not -f $simple_dot_torrent;

#
my $build           = Module::Build->current;
my $release_testing = $build->notes(q[release_testing]);

#
$|++;

#
BEGIN {
    plan tests => 5;
    use_ok(q[Net::BitTorrent::Version]);
}
SKIP: {
    skip(
        q[Fine grained regression tests skipped; turn on $ENV{RELESE_TESTING} to enable],
        ($test_builder->{q[Expected_Tests]} - $test_builder->{q[Curr_Test]})
    ) if not $release_testing;
    like(Net::BitTorrent::Version->gen_peerid(),
         qr[^NB\d{3}[CS]-.{8}.{5}$],
         q[Peer ID conforms to spec. (initial test)]
    );
    if ($Net::BitTorrent::Version::VERSION =~ m[_]) {
        like(Net::BitTorrent::Version->gen_peerid(),
             qr[^NB\d{3}S-.{8}.{5}$],
             q[Peer ID conforms to spec. (unstable/SVN build)]
        );
    }
    else {
        like(Net::BitTorrent::Version->gen_peerid(),
             qr[^NB\d{3}C-.{8}.{5}$],
             q[Peer ID conforms to spec. (stable/CPAN build)]
        );
    }
    like(Net::BitTorrent::Version->gen_peerid(),
         qr[^NB\d{3}[CS]-.{8}.{5}$], q[Peer ID conforms to spec.]);
    ok(length(Net::BitTorrent::Version->gen_node_id()) == 20,
        q[DHT node id conforms o spec.]);
}

# $Id$
