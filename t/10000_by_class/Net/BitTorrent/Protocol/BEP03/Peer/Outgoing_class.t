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

    # Handshake data
    my $simple_dot_torrent
        = 't/90000_data/95000_torrents/95003_miniswarm.torrent';
    chdir '../../../../../../../' if not -f $simple_dot_torrent;
    sub info_hash {'2B3AAF361BD40540BF7E3BFD140B954B90E4DFBC'}

    # Basic utility functions/methods
    sub class {'Net::BitTorrent::Protocol::BEP03::Peer::Outgoing'}

    sub new_args {
        my $s = shift;
        $s->{'client'} = t::80000_mock::Net::BitTorrent->new();
        $s->{'torrent'}
            = Net::BitTorrent::Torrent->new(path => $simple_dot_torrent);
        $s->{'client'}->add_torrent($s->{'torrent'});
        (connect => [$s->{'host'}, $s->{'port'}],
         client  => $s->{'client'},
         torrent => $s->{'torrent'}
        );
    }

    sub _0020_check_attributes_writer : Test( +1 ) {
        my $s = shift;
        $s->SUPER::_0020_check_attributes_writer;
        can_ok $s->{'peer'}, $_ for sort qw[
            _set_handle
        ];
    }
    use Module::Build;
    __PACKAGE__->runtests() if !caller
}
1;
