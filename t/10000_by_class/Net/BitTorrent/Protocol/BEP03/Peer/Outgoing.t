package t::10000_by_class::Net::BitTorrent::Protocol::BEP03::Peer::Outgoing;
{
    use strict;
    use warnings;
    use AnyEvent;
    use AnyEvent::Socket qw[tcp_server];
    use AnyEvent::Handle;
    use Test::More;
    use Test::Moose;
    use parent 'Test::Class';
    use lib '../', '../../../../../../../', '../../../../../../../lib', 'lib';
    use Net::BitTorrent::Protocol::BEP03::Packets qw[:all];
    use Net::BitTorrent::Torrent;
    BEGIN { require 't/10000_by_class/Net/BitTorrent/Peer.tm'; }
    use parent-norequire, 't::10000_by_class::Net::BitTorrent::Peer';

    # Handshake data
    my $simple_dot_torrent
        = 't/90000_data/95000_torrents/95003_miniswarm.torrent';
    chdir '../../../../../../../' if not -f $simple_dot_torrent;
    sub info_hash {'2B3AAF361BD40540BF7E3BFD140B954B90E4DFBC'}

    # Basic utility functions/methods
    sub class {'Net::BitTorrent::Protocol::BEP03::Peer::Outgoing'}

    sub _9000_open_socket : Test( startup => 0 ) {
        my $s = shift;
        $s->{'cv'}->begin;
        $s->{'socket'} = tcp_server '127.0.0.1', 0, sub {
            my ($fh, $host, $port) = @_;
            $s->{'fh'} = $fh;
            subtest 'read handshake'  => sub { $s->_read_handshake };
            subtest 'write handshake' => sub { $s->_write_handshake };
            subtest 'read bitfield'   => sub { $s->_read_bitfield };
            subtest 'write bitfield'  => sub { $s->_write_bitfield };
            subtest 'read interested' => sub { $s->_read_interested };
            subtest 'write unchoke'   => sub { $s->_write_unchoke };
            $s->{'cv'}->end;
            }, sub {
            my ($state, $host, $port) = @_;
            $s->{'host'} = $host;
            $s->{'port'} = $port;
            1;
            }
    }

    sub _read_handshake {
        plan tests => 10;
        my $s = shift;
        $s->{'cv'}->begin;

        #
        AnyEvent->one_event for 1 .. 10;
        ok sysread($s->{'fh'}, my ($in), 1024), 'read handshake packet';
        ok length $in >= 68, 'handshake was >= 68 bytes';
        my $p = parse_packet(\$in);
        is ref $p, 'HASH', 'packet parses to hashref';
        is $p->{'type'},           -1, 'fake handshake type';
        is $p->{'packet_length'},  68, 'parsed packet was 68 bytes';
        is $p->{'payload_length'}, 48, 'parsed payload was 48 bytes';
        is scalar @{$p->{'payload'}}, 3, 'parsed payload has 3 elements';
        is length $p->{'payload'}[0], 8, 'reserved is eight bytes';
        like $p->{'payload'}[1], qr[^[A-F\d]{40}$]i,        'info_hash';
        like $p->{'payload'}[2], qr[^NB\d\d\d[SU]-.{13}+$], 'peer_id';

        # Next step
        $s->{'cv'}->end;
    }

    sub _write_handshake {
        plan tests => 5;
        my $s = shift;
        $s->{'cv'}->begin;
        AnyEvent->one_event for 1 .. 10;
        is syswrite($s->{'fh'},
                   build_handshake($s->reserved, $s->info_hash, $s->peer_id)),
            68, 'wrote 68 byte handshake to peer';
        AnyEvent->one_event for 1 .. 10;
        ok $s->{'peer'}->_has_torrent, '...->torrent is defined';
        is $s->{'peer'}->torrent->info_hash->to_Hex, $s->info_hash,
            '...->torrent->info_hash->to_Hex is correct';
        is $s->{'peer'}->peer_id, $s->peer_id, '...->peer_id is correct';
        is $s->{'peer'}->pieces->to_Enum, '',
            'initial value for ...->pieces->to_Enum is correct';
        $s->{'cv'}->end;
    }

    sub _read_bitfield {
        plan tests => 2;
        my $s = shift;
        $s->{'cv'}->begin;
        AnyEvent->one_event for 1 .. 10;
        is sysread($s->{'fh'}, my ($x), 1024), 6, 'read 6 bytes from peer';
        is_deeply parse_packet(\$x),
            {packet_length  => 6,
             payload        => "\0",
             payload_length => 1,
             type           => 5
            },
            'bitfield is correct';
        $s->{'cv'}->end;
    }

    sub _write_bitfield {
        plan tests => 3;
        my $s = shift;
        $s->{'cv'}->begin;
        AnyEvent->one_event for 1 .. 10;
        is syswrite($s->{'fh'}, build_bitfield(pack 'B*', '10')), 6,
            'wrote 6 byte bitfield to peer';
        AnyEvent->one_event for 1 .. 10;
        is $s->{'peer'}->pieces->to_Enum, '0',
            'new value for ...->pieces->to_Enum is correct';
        ok $s->{'peer'}->interesting, 'peer is now interested in us';
        $s->{'cv'}->end;
    }

    sub _read_interested {
        plan tests => 2;
        my $s = shift;
        $s->{'cv'}->begin;
        AnyEvent->one_event for 1 .. 10;
        is sysread($s->{'fh'}, my ($x), 1024), 5, 'read 6 bytes from peer';
        is_deeply parse_packet(\$x),
            {packet_length  => 5,
             payload_length => 0,
             type           => 2
            },
            'interested is correct';
        $s->{'cv'}->end;
    }

    sub _write_unchoke {
        plan tests => 2;
        my $s = shift;
        $s->{'cv'}->begin;
        AnyEvent->one_event for 1 .. 10;
        is syswrite($s->{'fh'}, build_unchoke), 5,
            'wrote 5 byte unchoke to peer';
        AnyEvent->one_event for 1 .. 10;
        ok !$s->{'peer'}->remote_choked, 'peer is now unchoked by us';
        $s->{'cv'}->end;
    }

    #
    #$ENV{'TEST_VERBOSE'}++;
    __PACKAGE__->runtests() if !caller
}
1;
