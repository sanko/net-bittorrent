package t::10000_by_class::Net::BitTorrent::Protocol::BEP03::Peer::Incoming;
{
    use strict;
    use warnings;
    use AnyEvent;
    use Test::Most;
    use Test::Moose;
    use parent 'Test::Class';
    use lib '../', '../../../../../../../', '../../../../../../../lib', 'lib';
    use Net::BitTorrent::Protocol::BEP03::Packets qw[:all];
    use Net::BitTorrent;
    use Net::BitTorrent::Torrent;
    $|++;

    # Test related
    sub skip_setters {0}

    # Handshake data
    sub reserved  { "\0" x 8 }
    sub torrent   {'t/90000_data/95000_torrents/95003_miniswarm.torrent'}
    sub info_hash {'2B3AAF361BD40540BF7E3BFD140B954B90E4DFBC'}
    sub peer_id   {'This ain\'t a peer_id'}

    # Callbacks
    sub on_peer_disconnect {
        my ($s, $a) = @_;
        use Data::Dump;
        ddx $a;
        is $a->{'peer'}->handshake, 0, 'disconnect mid-handshake';

        # Regression test
        my $match
            = '127\.0\.0\.1:\d+ \('
            . substr(peer_id(), 0, 20)
            . '\) disconnect: Bad info_hash \(We are not serving '
            . sprintf(info_hash, 0, 40) . '\)';
        like $a->{'message'}, qr[^$match],
            'peer disconnected (unknown torrent)';
    }

    # Basic utility functions/methods
    sub class {'Net::BitTorrent::Protocol::BEP03::Peer::Incoming'}

    sub new_args {
        my $s = shift;
        require Socket;
        require AnyEvent::Handle::Throttle;
        socketpair(my ($child),
                   my ($parent),
                   Socket::AF_UNIX(), Socket::SOCK_STREAM(),
                   Socket::PF_UNSPEC())
            or die "socketpair: $!";
        $s->{'fh'} = $parent;
        $s->{'client'} = Net::BitTorrent->new(
            port              => 0,
            on_listen_failure => sub {
                $s->{'listen_failure'}{$_[1]->{'protocol'}}
                    = $_[1]->{'message'};
            },
            on_peer_disconnect =>
                sub { warn $s; ... }    #sub {$s->on_peer_disconnect(@_)}
        );
        (client => $s->{client},
         handle => AnyEvent::Handle::Throttle->new(fh => $child));
    }

    # Events
    sub _100_handshake_ : Test( 3 ) {
        my $s = shift;
        use Data::Dump;
        return 'TODO';
        syswrite $s->{'fh'},
            build_handshake($s->reserved, $s->info_hash, $s->peer_id);
        AnyEvent->one_event for 1 .. 100;
        ddx $s->{'peer'};
        sysread $s->{'fh'}, my ($text), 1024;
        warn $text;
    }

    # AnyEvent
    sub _00000_init : Test( startup ) {
        my $s = shift;
        note 'Adding condvar for later use...';
        $s->{'cv'} = AE::cv();
        $s->{'cv'}->begin(sub { $s->{'cv'}->send });
        note '...which will timeout in 2m.';
        $s->{'to'} = AE::timer(
            60 * 2,
            0,
            sub {
                diag sprintf 'Timeout waiting for %s!', join ', ',
                    keys %{$s->{'todo'}};
                $s->{'cv'}->send;
            }
        );
    }

    sub wait : Test( shutdown => no_plan ) {
        my $s = shift;
        $s->{'cv'}->end;
        $s->{'cv'}->recv;
    }

    # Setup/teardown
    sub startup : Test( startup => 3 ) {
        my $s = shift;
        use_ok $s->class;
        can_ok $s->class, 'new';
        $s->{'peer'} = new_ok $s->class, [$s->new_args];
        explain 'New peer looks like... ', $s->{'peer'};
    }

    sub setup : Test( setup ) {
        my $s = shift;
    }

    sub shutdown : Test( shutdown ) {
    }

    #
    __PACKAGE__->runtests() if !caller;
}
1
