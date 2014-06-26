requires 'Net::BitTorrent::Protocol';
requires 'Net::BitTorrent::DHT';
requires 'Net::BitTorrent', 'v0.2.5';
requires 'AnyEvent';
requires 'Moose';
requires 'perl', '5.008001';

on 'test' => sub {
    requires 'Test::More', '0.98';
};

