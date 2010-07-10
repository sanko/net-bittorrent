package t::10000_by_class::Net::BitTorrent;
{
    use strict;
    use warnings;
    use Test::More;
    use Test::Moose;
    use parent 'Test::Class';
    use lib '../../../lib', 'lib';

    #
    sub class {'Net::BitTorrent'}

    sub new_args {
        my $s = shift;
        (on_listen_failure => sub {
             $s->{'listen_failure'}{$_[1]->{'protocol'}} = $_[1]->{'message'};
         }
        );
    }

    #
    sub startup : Test( startup => 3 ) {
        my $s = shift;
        use_ok $s->class;
        can_ok $s->class, 'new';
        $s->{'nb'} = new_ok $s->class, [$s->new_args];
    }

    sub setup : Test( setup ) {
        my $s = shift;
    }

    sub _check_public_methods : Test( 25 ) {
        my $s = shift;
        can_ok $s->{'nb'}, $_ for sort qw[
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

    sub _check_private_methods : Test( 4 ) {
        my $s = shift;
        can_ok $s->{'nb'}, $_ for sort qw[
            _on_tcp4_in _on_udp4_in
            _on_tcp6_in _on_udp6_in
        ];
    }

    sub _check_attributes : Test( 19 ) {
        my $s = shift;
        has_attribute_ok $s->{'nb'}, $_ for sort qw[
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
        my $s = shift;
        has_attribute_ok $s->{'nb'}, $_ for sort qw[
            _peers
        ];
    }

    sub _check_builder_methods : Test( 8 ) {
        my $s = shift;
        can_ok $s->{'nb'}, $_ for sort qw[
            _build_peer_id
            _build_ip_filter
            _build_dht
            _build_tcp4 _build_tcp6
            _build_udp4 _build_udp6
            _build_callback_no_op
        ];
    }

    sub _check_attribute_private_methods : Test( 27 ) {
        my $s = shift;
        can_ok $s->{'nb'}, $_ for sort qw[
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
        like $s->{'nb'}->peer_id, qr[^NB\d\d\d[SU]-([a-zA-Z\d\-._~]){8}.{5}$],
            sprintf '...->peer_id( ) matches spec [%s]', $s->{'nb'}->peer_id;
    }

    sub validate_ip_filter : Test( 1 ) {
        my $s = shift;
        isa_ok $s->{'nb'}->ip_filter, 'Net::BitTorrent::Network::IPFilter',
            '...->ip_filter( )';
    }

    sub validate_dht : Test( 1 ) {
        my $s = shift;
        isa_ok $s->{'nb'}->dht, 'Net::BitTorrent::DHT', '...->dht( )';
    }

    sub validate_port : Test( 1 ) {
        my $s = shift;
        like $s->{'nb'}->port, qr[^\d+$],
            sprintf '...->port( ) is an integer [%d]', $s->{'nb'}->port;
    }

    sub validate_peers : Test( 2 ) {
        my $s = shift;
        is ref $s->{'nb'}->_peers, 'HASH',
            '...->_peers( ) are stored as a hash';
        is $s->{'nb'}->count_peers, 0, '...->count_peers( ) is initially 0';
    }

    sub validate_torrents : Test( 1 ) {
        my $s = shift;
        is ref $s->{'nb'}->torrents, 'ARRAY', '...->torrents( ) is an array';
    }

    #
    __PACKAGE__->runtests() if !caller;
}
1;
