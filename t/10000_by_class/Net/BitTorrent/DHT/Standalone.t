package t::10000_by_class::Net::BitTorrent::DHT::Standalone;
{
    use strict;
    use warnings;
    use lib '../', '../../../../../', '../../../../../lib', 'lib';
    require 't/10000_by_class/Net/BitTorrent/DHT.t';
    use parent-norequire, 't::10000_by_class::Net::BitTorrent::DHT';
    use parent 'Test::Class';
    use Test::Moose;
    use Test::More;
    use 5.010;
    require AnyEvent;

    #
    sub new_args {
        [port => 1338,
         boot_nodes =>
             [['router.utorrent.com', 6881], ['router.bittorrent.com', 6881]]
        ];
    }

    sub check_role : Test( 9 ) {
        my $self = shift;
        does_ok $self->{'dht'}, 'Net::BitTorrent::DHT::Standalone';
        has_attribute_ok $self->{'dht'}, $_ for qw[port
            udp6 udp6_sock udp6_host
            udp4 udp4_sock udp4_host ];
        ok !$self->{'dht'}->has_client,
            '... standalone dht nodes have no client';
    }

    sub init : Test( startup ) {
        my $s = shift;
        diag 'Adding condvar for later use...';
        $s->{'cv'} = AE::cv();
        $s->{'cv'}->begin(sub { $s->{'cv'}->send });
        diag '...which will timeout in 2m.';
        $s->{'to'}
            = AE::timer(60 * 2, 0, sub { diag 'Timeout!'; $s->{'cv'}->send });

        #for my $addr ()
        #{   my $node = $s->{'dht'}->ipv4_add_node($addr);
        #    diag sprintf 'Booting with [\'%s\', %d]', @$addr;
        #    meta_ok $node;
        #    isa_ok $node, 'Net::BitTorrent::Protocol::BEP05::Node';
        #}
    }

    sub wait : Test( shutdown ) {
        my $s = shift;
        $s->{'cv'}->end;
        $s->{'cv'}->recv;
    }

    sub quest_find_node : Test( no_plan ) {
        my $s = shift;
        $s->{'cv'}->begin;
        my $l = join '', map { [0 .. 9, 'a' .. 'f']->[int rand(16)] } 1 .. 40;
        diag 'Seeking nodes near ' . $l;
        $s->{'quest'}{'find_node'} = $s->{'dht'}->find_node(
            $l,
            sub {
                &cb_find_node;
                state $done = 0;
                $s->{'cv'}->end if !$done++;
            }
        );
        ok($s->{'quest'}{'find_node'});
        is ref $s->{'quest'}{'find_node'}, 'ARRAY',
            'find_node quest is an array reference';
    }

    sub cb_find_node {
        my ($ih, $nd, $pr) = @_;
        diag sprintf 'We found %d nodes near %s from [\'%s\', %d] via DHT',
            scalar(@$pr),
            $ih->to_Hex, $nd->host, $nd->port;
        diag join ', ', map { sprintf '[\'%s\', %d]', @$_ } @$pr;
    }

    sub quest_announce_peer : Test( no_plan ) {
        my $s = shift;
        $s->{'cv'}->begin;

        #$s->{'ih'} = join '',
        #    map { [0 .. 9, 'a' .. 'f']->[int rand(16)] } 1 .. 40;
        $s->{'ih'} = '6d0f88e9646c0f3a01bc35d0b0845db3247e6260';
        $s->{'po'} = int rand(65000);
        diag sprintf 'Pretending we are serving %s on port %d', $s->{'ih'},
            $s->{'po'};
        $s->{'quest'}{'announce_peer'} = $s->{'dht'}->announce_peer(
            $s->{'ih'},
            $s->{'po'},
            sub {
                &cb_announce_peer;
                state $done = 0;
                $s->{'cv'}->end if !$done++;
            }
        );
        ok($s->{'quest'}{'announce_peer'});
        is ref $s->{'quest'}{'announce_peer'}, 'ARRAY',
            'announce_peer quest is an array reference';
    }

    sub cb_announce_peer {
        my ($infohash, $node, $port) = @_;
        diag sprintf 'Announced %s on port %d with [\'%s\', %d] (%s)',
            $infohash->to_Hex, $port, $node->host, $node->port,
            $node->nodeid->to_Hex;
    }

    sub quest_get_peers : Test( no_plan ) {
        my $s = shift;
        $s->{'cv'}->begin;
        diag 'Seeking peers with ', $s->{'ih'};
        $s->{'quest'}{'get_peers'} = $s->{'dht'}->get_peers(
            $s->{'ih'},
            sub {
                &cb_get_peers;
                state $done = 0;
                $s->{'cv'}->end if !$done++;
            }
        );
        ok($s->{'quest'}{'get_peers'});
        is ref $s->{'quest'}{'get_peers'}, 'ARRAY',
            'get_peers quest is an array reference';
    }

    sub cb_get_peers {
        my ($ih, $nd, $pr) = @_;
        diag sprintf 'We found %d peers for %s from [\'%s\', %d] via DHT',
            scalar(@$pr),
            $ih->to_Hex, $nd->host, $nd->port;
        diag join ', ', map { sprintf '[\'%s\', %d]', @$_ } @$pr;
    }

    #
    __PACKAGE__->runtests() if !caller;
}
1;
