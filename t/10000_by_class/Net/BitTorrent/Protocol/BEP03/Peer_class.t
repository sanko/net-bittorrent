package t::10000_by_class::Net::BitTorrent::Protocol::BEP03::Peer_class;
{
    use strict;
    use warnings;
    use Test::Most;
    use Test::Moose;
    use Test::Exception;
    use parent 'Test::Class';
    use lib '../../../../../../', '../../../../../../lib';
    BEGIN { require 't/10000_by_class/Net/BitTorrent/Peer_class.t'; }
    use parent-norequire, 't::10000_by_class::Net::BitTorrent::Peer_class';

    # Basic utility functions/methods
    sub class {'Net::BitTorrent::Protocol::BEP03::Peer'}
    sub new_args { my $s = shift;
        $s->{'client'} = t::80000_mock::Net::BitTorrent->new();
        (client => $s->{'client'}); }

    sub _0010_check_public_attributes : Test( +0 ) {
        my $s = shift;
        $s->SUPER::_0010_check_public_attributes;
        has_attribute_ok $s->{'peer'}, $_ for sort qw[

        ];
    }


    sub _0012_check_private_attributes : Test( +1 ) {
        my $s = shift;
        $s->SUPER::_0012_check_private_attributes;
        has_attribute_ok $s->{'peer'}, $_ for sort qw[_handle];
    }

    sub _0015_check_attributes_reader : Test( +1 ) {
        my $s = shift;
        $s->SUPER::_0015_check_attributes_reader;
        can_ok $s->{'peer'}, $_ for sort qw[
            _handle
        ];
    }

    sub _0040_check_attributes_predicate : Test( +1 ) {
        my $s = shift;
        $s->SUPER::_0040_check_attributes_predicate;
        can_ok $s->{'peer'}, $_ for sort qw[
            _has_handle
        ];
    }

    sub _0080_check_attribute_handlers : Test( +6 ) {
        my $s = shift;
        $s->SUPER::_0080_check_attribute_handlers;
        can_ok $s->{'peer'}, $_ for sort qw[
            rbuf push_read push_write fh host port
        ];
    }

    sub _0090_check_public_methods : Test( 0 ) {
        my $s = shift;
        can_ok $s->{'peer'}, $_ for sort qw[
        ];
    }

    sub _0091_check_private_methods : Test( 12 ) {
        my $s = shift;
        can_ok $s->{'peer'}, $_ for sort qw[
            _build_reserved
            _send_handshake
            _handle_packet_handshake
            _handle_packet_choke _handle_packet_unchoke
            _handle_packet_interested _handle_packet_have
            _handle_packet_bitfield _handle_packet_request
            _handle_packet_piece _handle_packet_ext_protocol _handle_packet
        ];
    }


    $ENV{'TEST_VERBOSE'}++;
    __PACKAGE__->runtests() if !caller;
}
1
