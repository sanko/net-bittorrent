use strict;
use Test::More 0.98;
use lib '../lib';
use Net::BitTorrent;
#
diag 'checking defaults...';
my $client = new_ok 'Net::BitTorrent', [],  'Net::BitTorrent->new()';
is length $client->peerid, 20, 'Generated peer id is the correct length';
isa_ok $client->loop, 'IO::Async::Loop', '$client->loop';

#
done_testing;
