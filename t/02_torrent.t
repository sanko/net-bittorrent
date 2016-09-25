use strict;
use Test::More 0.98;
use lib '../lib';
use Net::BitTorrent::Torrent;
#
diag 'checking defaults...';
my $client = Net::BitTorrent::Torrent->new();
#is length $client->peerid, 20, 'Generated peer id is the correct length';
#isa_ok $client->loop, 'IO::Async::Loop';

#
done_testing;
