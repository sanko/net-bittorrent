use strict;
use warnings;
use lib '../lib';
$|++;
use AnyEvent;
use Net::BitTorrent::DHT;
use 5.10.1;
my $dht = Net::BitTorrent::DHT->new(
          port => 1338,
          boot_nodes =>
              [['router.bittorrent.com', 6881], ['router.utorrent.com', 6881]]
);

#
my $quest_A
    = $dht->get_peers('3e16157f0879eb43e9e51f45d485feff90a77283', \&dht_cb);
my $quest_B
    = $dht->get_peers('a1425e0d6630336cdd9fb320f3fff1030098975a', \&dht_cb);

#
my $timer = AE::timer 30, 60, \&dump_buckets;
END { dump_buckets() if $dht }

#
AnyEvent->condvar->recv;    # tick, tick, tick, ding!

sub dht_cb {
    my ($infohash, $node, $peers) = @_;
    say sprintf 'We found %d peers for %s from %s:%d via DHT',
        scalar(@$peers),
        $infohash->to_Hex, $node->host, $node->port;
    say join ', ',
        map { Net::BitTorrent::Protocol::BEP23::Compact::uncompact_ipv4($_) }
        @$peers;
}

sub dump_buckets {
    say '-' x 40;
    say sprintf 'Num buckets: %d. My DHT ID: %s',
        $dht->routing_table->count_buckets, $dht->nodeid->to_Hex;
    my $x = 0;
    for my $bucket (@{$dht->routing_table->buckets}) {
        say sprintf
            'Bucket %s: %s (replacement cache: %d)',
            $x++, $bucket->floor->to_Hex, $bucket->count_backup_nodes;
        for my $node (@{$bucket->nodes}) {
            say sprintf '    %s %s:%d fail:%d seen:%d age:%s ver:%s',
                $node->nodeid->to_Hex, $node->host,
                $node->port, $node->fail || 0, $node->seen || 0,
                duration(time - $node->birth), $node->v || '?';
        }
    }
}

sub duration {
    my %dhms = (d => int($_[0] / (24 * 60 * 60)),
                h => ($_[0] / (60 * 60)) % 24,
                m => ($_[0] / 60) % 60,
                s => $_[0] % 60
    );
    return join ' ', map { $dhms{$_} ? $dhms{$_} . $_ : () } sort keys %dhms;
}
