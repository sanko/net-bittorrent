use strict;
use warnings;
use lib '../../lib';
use Test::More;
use File::Temp;
use Net::BitTorrent::Network::Utility qw[server];
use AnyEvent::Handle;
use AnyEvent;
use Net::BitTorrent;
use lib reverse 'lib', '../lib', '../../lib';

$|++;
{

    package t::Net::BitTorrent::Tracker;
    use Moose;
    use Net::BitTorrent::Network::Utility qw[server];
    use Net::BitTorrent::Protocol::BEP03::Bencode qw[:all];
    has 'port' => (isa     => 'Int',
                   is      => 'ro',
                   writer  => '_set_port',
                   default => 0
    );
    has 'host' =>
        (isa => 'Str', is => 'ro', writer => '_set_host', default => '::');
    has '_peers' => (
        isa     => 'HashRef[HashRef]',    # By (key ^ info_hash)
        is      => 'ro',
        default => sub { {} },
        traits  => ['Hash'],
        handles => {
            forget_peer => 'delete',
            add_peer    => 'set',
            peers       => 'values'

              #info_hashes   => ['map', sub { $_->{'info_hash'} } ],
              #peer_ids      => ['map', sub { $_->{'peer_id'} } ],
              #find_info_hash => ['map', sub { $_->{'info_hash'} eq $_[0] } ],
        }
    );
    has 'condvar' => (isa      => 'Defined',
                      is       => 'ro',
                      init_arg => undef,
                      builder  => '_build_condvar',
                      handles  => [qw[wait broadcast]]
    );
    sub _build_condvar { require AnyEvent; AnyEvent->condvar; }
    has 'socket' => (isa      => 'Defined',
                     is       => 'ro',
                     init_arg => undef,
                     builder  => '_build_socket'
    );

    sub _build_socket {
        my $s = shift;
        my $x = server(
            $s->host,
            $s->port,
            sub {
                my ($fh, $paddr, $host, $port) = @_;
                my $hdl;
                $hdl = AnyEvent::Handle->new(
                    fh       => $fh,
                    on_drain => sub {
                        $s->on_drain($hdl, $fh, $paddr, $host, $port, @_);
                    },
                    on_read => sub {
                        $s->on_read($hdl, $fh, $paddr, $host, $port, @_);
                    },
                    on_eof  => sub { warn 'bye!' }
                );
            },
            sub {
                $s->_set_host($_[1]);
                $s->_set_port($_[2]);
                1;
            },
            $s->protocol
        );
    }
    sub BUILD {1}

    sub on_drain {
        my $s = shift;
        $_[0] = undef;
    }
}
{

    package t::Net::BitTorrent::Tracker::HTTP;
    use Net::BitTorrent::Protocol::BEP03::Bencode qw[:all];
    use Net::BitTorrent::Protocol::BEP07::Compact qw[:all];    # IPv4
    use Net::BitTorrent::Protocol::BEP23::Compact qw[:all];    # IPv6
    use Moose;
    extends 't::Net::BitTorrent::Tracker';
    sub protocol {'tcp'}

    sub on_read {
        my ($s, $h, $fh, $paddr, $ip, $port) = @_;
        my ($status, $body) = ('404 EH!?', 'Sorry. Play again.');
        if ($h->rbuf =~ s[^GET (.+?)(?:\?(.+))? HTTP/1\.(\d)\015\012][]) {
            my ($path, $args, $ver) = ($1, $2, $3);
            my %args = map { m[^(.+?)(?:=(.*))?$]; $1 => $2; }
                split qr[[&;]], $args;
            my %headers = map { m[^(.+?)\s*:\s*(.*)?$]; $1 => $2; }
                split qr[\015\012], $h->rbuf;
            if ($path eq '/announce.pl') {
                my $tracker_id = $args{'tracker id'} || pack 'H*',
                    int rand(time);
                my $max_peers = $args{'max_peers'} || 50;
                my $info_hash = $args{'info_hash'};
                $info_hash =~ s[%(..)][chr hex $1]eg;
                $s->add_peer(pack('H*', $args{'key'}) ^ $info_hash ^
                                 pack('B*', $args{'peer_id'}),
                             {address => [
                                    $args{'ip'} || $ip, $args{'port'} || $port
                              ],
                              downloaded => $args{'downloaded'},
                              event      => $args{'event'} || undef,
                              info_hash  => $info_hash,
                              key        => $args{'key'},
                              left       => $args{'left'},
                              peer_id    => $args{'peer_id'},
                              tracker_id => $tracker_id,
                              uploaded   => $args{'uploaded'},
                              touch      => time
                             }
                );
                $status = '200 Alright';
                my $num_peers = 0;
                my @peers     = grep {
                           $_->{'info_hash'} eq $info_hash
                        && $num_peers++ < $max_peers
                } $s->peers;
                $body = {
                    'min interval' => 60 * 5,
                    interval       => 60 * 10,
                    'tracker id'   => $tracker_id,
                    peers          => (
                        $args{'compact'}
                        ? (compact_ipv4 map { $_->{'address'} } @peers)
                        : (map {
                               {peer_id => $_->{'peer_id'},
                                ip      => $_->{'address'}->[0],
                                port    => $_->{'address'}->[1]
                               }
                               } @peers
                        )
                    )
                };
             }

            #warn bencode $s->get_info_hash($info_hash);
            elsif ($path eq '/scrape.pl') { warn 'Scrape!' }
            else                          { warn 'NFI!' }
        }

        #die $info_hash;
        $h->rbuf = '';
        $body = bencode $body if ref $body;
        $h->push_write(sprintf <<'END', $status, length($body), $body) } }
HTTP/1.0 %s
Content-Type: text/plain
Content-Length: %d
Connection: close

%s
END
 my $tracker = t::Net::BitTorrent::Tracker::HTTP->new(
 host => '127.0.0.1'

 );
chdir '../..' if !-f 't/90000_data/95000_torrents/95003_miniswarm.torrent';
my (@cli, @dir);
for my $seed (qw[1 0]) {
    push @cli, Net::BitTorrent->new(
        on_peer_id => sub {
            my ($s, $a) = @_;
            note $a->{'message'};
        },
        on_peer_packet => sub {
            my ($s, $a) = @_;
            note $a->{'message'};
        },
        on_peer_connect => sub {
            my ($s, $a) = @_;
            note 'Connect:    ' . $a->{'message'};
        },
        on_peer_disconnect => sub {
            my ($s, $a) = @_;
            note 'Disconnect: ' . $a->{'message'};
        },
        on_peer_packet_in => sub {
            my ($s, $a) = @_;
            note explain $a->{'message'}, $a->{'packet'};
        },
        on_piece_hash_pass =>  sub {   },
        on_piece_hash_fail =>  sub {   },
        on_peer_bitfield => sub {
            my ($s, $a) = @_;
            note 'Bitfield packet: ' . $a->{'message'};
    }
    );
    $cli[-1]->add_torrent(
                path => 't\90000_data\95000_torrents\95003_miniswarm.torrent')
        ->tracker->add_tier(
                   [sprintf 'http://%s:%d/announce.pl?%d^', $tracker->host,
                    $tracker->port,                         int rand time
                   ]
        );
        if ($seed) {
            $cli[-1]->torrent(0)->storage->_set_root('t\90000_data\96000_data\96020_miniswarm_seed');
        }
        else {
            push @dir, File::Temp->newdir( 'NBminiswarm_peer_'. scalar(@cli) .'_XXXX', TMPDIR=> 1, CLEANUP => 1 );
            $cli[-1]->torrent(0)->storage->_set_root($dir[-1]->dirname);


        }
        $cli[-1]->torrent(0)->hash_check;
        is $cli[-1]->torrent(0)->have->to_Enum, $seed ?  '0,1' : '',
        sprintf '[%02d] hashcheck %s', scalar(@cli),
        ($seed ?  'seed' : 'peer');
        #warn $cli[-1]->torrent(0)->have->to_Enum;
        #warn $cli[-1]->torrent(0)->wanted->to_Enum;
}
$tracker->wait;
