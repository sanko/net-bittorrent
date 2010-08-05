package t::10000_by_class::Net::BitTorrent::Protocol::BEP03::Peer::Incoming;
{
    use strict;
    use warnings;
    use Test::More;
    use Test::Moose;
    use parent 'Test::Class';
    use lib '../', '../../../../../../../', '../../../../../../../lib', 'lib';
    use Net::BitTorrent::Protocol::BEP03::Packets qw[:all];
    BEGIN { require 't/10000_by_class/Net/BitTorrent/Peer.tm'; }
    use parent-norequire, 't::10000_by_class::Net::BitTorrent::Peer';

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
        $s->{'cv'}->end || return 'TODO';
        warn syswrite $s->{'fh'},
            build_handshake($s->reserved, $s->info_hash, $s->peer_id);
        AnyEvent->one_event for 1 .. 10;
        warn sysread $s->{'fh'}, my ($text), 1024;
        warn $text;
    }

    #
    __PACKAGE__->runtests() if !caller;
}
1
