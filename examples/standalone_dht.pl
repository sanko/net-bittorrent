use strict;
use warnings;
use 5.10.1;
use AnyEvent;
use lib '../lib';
use Net::BitTorrent::DHT;
$|++;

# Standalone node with user-defined port and boot_nodes
my $dht = Net::BitTorrent::DHT->new(
          port => 1338,
          boot_nodes =>
              [['router.bittorrent.com', 6881], ['router.utorrent.com', 6881]]
);

# Two 'quests' for peers (these are two popular Ubuntu swarms)
my $quest_A
    = $dht->get_peers('3e16157f0879eb43e9e51f45d485feff90a77283', \&dht_cb);
my $quest_B
    = $dht->get_peers('a1425e0d6630336cdd9fb320f3fff1030098975a', \&dht_cb);

# Let's stay up to date with what's going on in the routing table
my $timer = AE::timer 30, 60, sub { $dht->as_string(1) };
END { $dht->as_string(1) if $dht }

# tick, tick, tick, ...
AnyEvent->condvar->recv;

sub dht_cb {
    my ($infohash, $node, $peers) = @_;
    say sprintf 'We found %d peers for %s from %s:%d via DHT',
        scalar(@$peers),
        $infohash->to_Hex, $node->host, $node->port;
    say join ', ',
        map { Net::BitTorrent::Protocol::BEP23::Compact::uncompact_ipv4($_) }
        @$peers;
}
