package t::10000_by_class::Net::BitTorrent::Protocol::BEP03::Peer::Outgoing_class;
{
    use strict;
    use warnings;
    use AnyEvent;
    use AnyEvent::Socket qw[tcp_server];
    use Test::More;
    use Test::Moose;
    use parent 'Test::Class';
    use lib '../', '../../../../../../../', '../../../../../../../lib', 'lib';
    use Net::BitTorrent::Protocol::BEP03::Packets qw[:all];
    use t::80000_mock::Net::BitTorrent::Torrent;

    BEGIN {
        require 't/10000_by_class/Net/BitTorrent/Protocol/BEP03/Peer_class.t';
    }
    use parent-norequire,
        't::10000_by_class::Net::BitTorrent::Protocol::BEP03::Peer_class';
    sub skip_setters {'skipping setters because they screw things up'}

    # Basic utility functions/methods
    sub class {'Net::BitTorrent::Protocol::BEP03::Peer::Outgoing'}

    sub _000_open_socket : Test( startup => 0 ) {
        my $s = shift;
        $s->{'socket'} = tcp_server '127.0.0.1', 0, sub {
            my ($fh, $host, $port) = @_;
            $s->{'fh'} = $fh;
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
            }, sub {
            my ($state, $host, $port) = @_;
            $s->{'host'} = $host;
            $s->{'port'} = $port;
            1;
            };
    }

    sub new_args {
        my $s = shift;
        $s->{'client'} = t::80000_mock::Net::BitTorrent->new();
        $s->{'torrent'} = t::80000_mock::Net::BitTorrent::Torrent->new(
                                                  info_hash => $s->info_hash);
        (torrent => $s->{'torrent'},
         connect => [$s->{'host'}, $s->{'port'}],
         client  => $s->{'client'}
        );
    }

    sub _0020_check_attributes_writer : Test( +1 ) {
        my $s = shift;
        $s->SUPER::_0020_check_attributes_writer;
        can_ok $s->{'peer'}, $_ for sort qw[
            _set_handle
        ];
    }

    # Events
    sub _10000_handshake : Test( no_plan ) {    # Causes incoming handshake
        my $s = shift;
        AnyEvent->one_event for 1 .. 10;
        is syswrite($s->{'fh'},
                   build_handshake($s->reserved, $s->info_hash, $s->peer_id)),
            68, 'wrote 68 byte handshake to peer';
        AnyEvent->one_event for 1 .. 10;

        #sysread $s->{'fh'}, my ($text), 1024;
        #warn $text;
    }

    #
    #$ENV{'TEST_VERBOSE'}++;
    __PACKAGE__->runtests() if !caller;
}
1;
