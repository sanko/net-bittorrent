package main;
use lib '../lib';
use lib '../../Net-BitTorrent-DHT/lib';
use strict;
use warnings;
use Net::BitTorrent;
use Data::Dump;
$|++;
my $bt = new Net::BitTorrent(
path         => 'legal.torrent',
                             on_hash_pass => sub { warn 'PASS: ' . pop },
                             on_hash_fail => sub { warn 'FAIL: ' . pop }
);
#ddx $bt;
#$bt->hashcheck;
#die $bt->is_private;
#die unpack 'H*', $bt->infohash;

# Let's stay up to date with what's going on in the routing table
my $timer = AE::timer 60 * 1, 60 * 2,
    sub { $bt->dht_node->dump_ipv4_buckets; $bt->dht_node->dump_ipv6_buckets };
END { $bt->dht_node->dump_ipv4_buckets && $bt->dht_node->dump_ipv6_buckets if $bt->dht_node};

sub dht_cb {
    my ($infohash, $node, $peers) = @_;
    printf "We found %d peers for %s from %s:%d via DHT\n\t%s\n====\n",
        scalar(@$peers),
        $infohash->to_Hex, $node->host, $node->port,
        join ', ', map { sprintf '%s:%d', @$_ } @$peers;
}




AE::cv->recv;
