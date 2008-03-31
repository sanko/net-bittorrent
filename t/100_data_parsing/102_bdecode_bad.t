# -*- perl -*-

# t/100_data_parsing/102_bdecode_bad.t - more N::B::Util::bdecode() tests

use strict;
use warnings;

use Test::More tests => 46;

use lib q[../../lib/];

use Net::BitTorrent::Util qw[bdecode];

bdecode(q[0:0:]);
ok $@, q[data past end of first correct bencoded string];

bdecode(q[leanfdldjfh]);
ok $@, q[empty list with trailing garbage];

bdecode(q[i]);
ok $@, q[aborted integer];

bdecode(q[i0]);
ok $@, q[unterminated integer];

bdecode(q[ie]);
ok $@, q[empty integer];

bdecode(q[i341foo382e]);
ok $@, q[malformed integer];

bdecode(q[i123]);
ok $@, q[unterminated integer];

bdecode(q[1:]);
ok $@, q[string longer than data];

bdecode(q[i6easd]);
ok $@, q[integer with trailing garbage];

bdecode(q[35208734823ljdahflajhdf]);
ok $@, q[garbage looking vaguely like a string, with large count];

bdecode(q[2:abfdjslhfld]);
ok $@, q[string with trailing garbage];

bdecode(q[02:xy]);
ok $@, q[string with extra leading zero in count];

bdecode(q[l]);
ok $@, q[unclosed empty list];

bdecode(q[relwjhrlewjh]);
ok $@, q[complete garbage];

bdecode(q[d]);
ok $@, q[unclosed empty dict];

bdecode(q[defoobar]);
ok $@, q[empty dict with trailing garbage];

bdecode(q[d3:fooe]);
ok $@, q[dict with odd number of elements];

bdecode(q[di1e0:e]);
ok $@, q[dict with integer key];

bdecode(q[d1:b0:1:a0:e]);
ok $@, q[missorted keys];

bdecode(q[d1:a0:1:a0:e]);
ok $@, q[duplicate keys];

bdecode(q[l01:ae]);
ok $@, q[list with string with leading zero in count];

bdecode(q[9999:x]);
ok $@, q[string shorter than count];

bdecode(q[l0:]);
ok $@, q[unclosed list with content];

bdecode(q[d0:0:]);
ok $@, q[unclosed dict with content];

bdecode(q[d0:]);
ok $@, q[unclosed dict with odd number of elements];

bdecode(q[l-3:e]);
ok $@, q[list with negative-length string];

bdecode(q[0:0:]);
like $@, qr[Trailing garbage],
    sprintf q[Catch invalid format (%s)], $@;

bdecode(q[ie]);
like $@, qr[Bad bencoded data],
    sprintf q[Catch invalid format (%s)], $@;

bdecode(q[i341foo382e]);
ok $@, sprintf q[Catch invalid format (%s)], $@;

bdecode(q[i123]);
ok $@, sprintf q[Catch invalid format (%s)], $@;

bdecode(q[]);
ok $@, sprintf q[Catch invalid format (%s)], $@;

bdecode(q[i6easd]);
ok $@, sprintf q[Catch invalid format (%s)], $@;

bdecode(q[35208734823ljdahflajhdf]);
ok $@, sprintf q[Catch invalid format (%s)], $@;

bdecode(q[2:abfdjslhfld]);
ok $@, sprintf q[Catch invalid format (%s)], $@;

bdecode(q[9999:x]);
ok $@, sprintf q[Catch invalid format (%s)], $@;

bdecode(q[l]);
ok $@, sprintf q[Catch invalid format (%s)], $@;

bdecode(q[leanfdldjfh]);
ok $@, sprintf q[Catch invalid format (%s)], $@;

bdecode(q[relwjhrlewjh]);
ok $@, sprintf q[Catch invalid format (%s)], $@;

bdecode(q[d]);
ok $@, sprintf q[Catch invalid format (%s)], $@;

bdecode(q[defoobar]);
ok $@, sprintf q[Catch invalid format (%s)], $@;

bdecode(q[d3:fooe]);
ok $@, sprintf q[Catch invalid format (%s)], $@;

bdecode(q[d0:]);
ok $@, sprintf q[Catch invalid format (%s)], $@;

bdecode(q[d0:0:]);
ok $@, sprintf q[Catch invalid format (%s)], $@;

bdecode(q[l0:]);
ok $@, sprintf q[Catch invalid format (%s)], $@;

bdecode(q[2:x]);
ok $@, sprintf q[Catch invalid format (%s)], $@;

bdecode(q[dli1ee4:ROOTe]);
ok $@, sprintf q[Catch invalid format (%s)], $@;

