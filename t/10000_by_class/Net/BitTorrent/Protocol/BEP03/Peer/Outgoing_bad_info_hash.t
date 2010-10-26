package t::10000_by_class::Net::BitTorrent::Protocol::BEP03::Peer::Outgoing_class;
{
    use strict;
    use warnings;
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 12; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use AnyEvent::Impl::Perl;   # Timing is different than with EV. Go figure.
    use AnyEvent;
    use AnyEvent::Socket qw[tcp_server];
    use Test::More;
    use Test::Moose;
    use parent 'Test::Class';
    use lib '../', '../../../../../../../', '../../../../../../../lib', 'lib';
    use Net::BitTorrent::Protocol::BEP03::Packets qw[:all];
    use Net::BitTorrent::Torrent;

    BEGIN {
        require
            't/10000_by_class/Net/BitTorrent/Protocol/BEP03/Peer/Outgoing.t';
    }
    use parent-norequire,
        't::10000_by_class::Net::BitTorrent::Protocol::BEP03::Peer::Outgoing';

    sub new_args {
        my $s = shift;
        my $simple_dot_torrent
            = 't/90000_data/95000_torrents/95003_miniswarm.torrent';
        chdir '../../../../../../../' if not -f $simple_dot_torrent;
        $s->{'client'}
            = Net::BitTorrent->new(on_peer_disconnect => sub { die '...' });
        $s->{'torrent'}
            = Net::BitTorrent::Torrent->new(path => $simple_dot_torrent);
        $s->{'client'}->add_torrent($s->{'torrent'});
        (connect => [$s->{'host'}, $s->{'port'}],
         client  => $s->{'client'},
         torrent => $s->{'torrent'}
        );
    }
    __PACKAGE__->runtests() if !caller
}
1;
