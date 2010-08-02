package t::10000_by_class::Net::BitTorrent::Peer;
{
    use strict;
    use warnings;
    use AnyEvent;
    use Test::Most;
    use Test::Moose;
    use parent 'Test::Class';
    use lib '../../../../lib', 'lib';
    use Net::BitTorrent::Protocol::BEP03::Packets qw[:all];
            use Net::BitTorrent;
        use t::80000_mock::Net::BitTorrent;
        $|++;


    # Basic utility functions/methods
    sub class {'Net::BitTorrent::Peer'}
    sub new_args{ my $s = shift;
        $s->{'client'} = t::80000_mock::Net::BitTorrent->new();
        (client => $s->{'client'});

    }

    # Test related
    sub skip_setters { 0 }

    # Handshake data
    sub reserved  { "\0" x 8 }
    sub info_hash { 'A' x 40 }
    sub peer_id   { 'Peer_ID' x 20 }

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
    sub init : Test( startup ) {
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
        explain 'New peer looks like... ' , $s->{'peer'};
    }

    sub setup : Test( setup ) {
        my $s = shift;
    }

    sub shutdown : Test( shutdown ) {
    }

=pod

    sub _check_public_methods : Test( 25 ) {return 'TODO';
        my $s = shift;
        can_ok $s->{'peer'}, $_ for sort qw[
            timer        run
            add_torrent  clear_torrents count_torrents find_torrent
            has_torrents info_hashes    map_torrents   no_torrents
            shuffle_torrents            sort_torrents  sort_torrents_in_place
            torrent
            peer add_peer del_peer peer_ids has_peer peers clear_peers
            count_peers   no_peers
            trigger_listen_failure trigger_listen_success
        ];
    }

    sub _check_private_methods : Test( 4 ) {return 'TODO';
        my $s = shift;
        can_ok $s->{'peer'}, $_ for sort qw[
            _on_tcp4_in _on_udp4_in
            _on_tcp6_in _on_udp6_in
        ];
    }

    sub _check_attributes : Test( 19 ) {
        return 'TODO';
        my $s = shift;
        has_attribute_ok $s->{'peer'}, $_ for sort qw[
            port
            peer_id
            torrents
            ip_filter
            dht
            tcp4 tcp6
            udp4 udp6
            tcp4_sock tcp6_sock
            udp4_sock udp6_sock
            tcp4_host tcp6_host
            udp4_host udp6_host
            on_listen_failure on_listen_success
        ];
    }

    sub _check_private_attributes : Test( 1 ) {
        return 'TODO';
        my $s = shift;
        has_attribute_ok $s->{'peer'}, $_ for sort qw[
            _peers
        ];
    }

    sub _check_builder_methods : Test( 8 ) {
        return 'TODO';
        my $s = shift;
        can_ok $s->{'peer'}, $_ for sort qw[
            _build_peer_id
            _build_ip_filter
            _build_dht
            _build_tcp4 _build_tcp6
            _build_udp4 _build_udp6
            _build_callback_no_op
        ];
    }

    sub _check_attribute_private_methods : Test( 27 ) {
        return 'TODO';
        my $s = shift;
        can_ok $s->{'peer'}, $_ for sort qw[
            _set_port
            _set_tcp4 _set_tcp6
            _set_udp4 _set_udp6
            _has_tcp4 _has_tcp6
            _has_udp4 _has_udp6
            _set_tcp4_sock _set_tcp6_sock
            _set_udp4_sock _set_udp6_sock
            _has_tcp4_sock _has_tcp6_sock
            _has_udp4_sock _has_udp6_sock
            _set_tcp4_host _set_tcp6_host
            _set_udp4_host _set_udp6_host
            _has_tcp4_host _has_tcp6_host
            _has_udp4_host _has_udp6_host
            _no_listen_failure _no_listen_success
        ];
    }

    sub validate_peer_id : Test( 1 ) {
        my $s = shift;
        like $s->{'peer'}->peer_id, qr[^NB\d\d\d[SU]-([a-zA-Z\d\-._~]){8}.{5}$],
            sprintf '...->peer_id( ) matches spec [%s]', $s->{'peer'}->peer_id;
    }

    sub validate_ip_filter : Test( 1 ) {
        my $s = shift;
        isa_ok $s->{'peer'}->ip_filter, 'Net::BitTorrent::Network::IPFilter',
            '...->ip_filter( )';
    }

    sub validate_dht : Test( 1 ) {
        my $s = shift;
        isa_ok $s->{'peer'}->dht, 'Net::BitTorrent::DHT', '...->dht( )';
    }

    sub validate_port : Test( 2 ) {
        my $s = shift;
        like $s->{'peer'}->port, qr[^\d+$],
            sprintf '...->port( ) is an integer [%d]', $s->{'peer'}->port;
        my $port = ref $s->port ? $s->port : [$s->port];
        return 'expecting a random port' if grep { $_ == 0 } @$port;
        my $range = join ',', @$port;
        $range =~ s[(?<!\d)(\d+)(?:,((??{$++1}))(?!\d))+][$1..$+]g;
        ok( (grep { $_ == $s->{'peer'}->port } @$port),
            sprintf
                '...->port( ) is one of the ports we wanted to open [%d|[%s]]',
            $s->{'peer'}->port,
            $range
        );
    }

    sub validate_peers : Test( 2 ) {
        my $s = shift;
        is ref $s->{'peer'}->_peers, 'HASH',
            '...->_peers( ) are stored as a hash';
        is $s->{'peer'}->count_peers, 0, '...->count_peers( ) is initially 0';
    }

    sub validate_torrents : Test( 1 ) {
        my $s = shift;
        is ref $s->{'peer'}->torrents, 'ARRAY', '...->torrents( ) is an array';
    }

=cut

    #
    __PACKAGE__->runtests() if !caller;
}
1;
