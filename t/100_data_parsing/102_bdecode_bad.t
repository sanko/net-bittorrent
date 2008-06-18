# -*- perl -*-
# t/100_data_parsing/102_bdecode_bad.t - more N::B::Util::bdecode() tests
use strict;
use warnings;
use Test::More qw[no_plan];
use lib q[../../lib/];
use Net::BitTorrent::Util qw[bdecode];
is_deeply [bdecode(q[0:0:])], [],
    q[trailing junk at end of valid bencoded string];
is_deeply [bdecode(q[leanfdldjfh])], [[], q[anfdldjfh]],
    q[empty list with trailing garbage];
is_deeply [bdecode(q[i])],           [], q[aborted integer];
is_deeply [bdecode(q[i0])],          [], q[unterminated integer];
is_deeply [bdecode(q[ie])],          [], q[empty integer];
is_deeply [bdecode(q[i341foo382e])], [], q[malformed integer];
is_deeply [bdecode(q[i123])],        [], q[unterminated integer];
is_deeply [bdecode(q[1:])],          [], q[string longer than data];
is_deeply [bdecode(q[i6easd])], [6, q[asd]], q[string with trailing junk];
is_deeply [bdecode(q[35208734823ljdahflajhdf])], [],
    q[garbage looking vaguely like a string, with large count];
is_deeply [bdecode(q[2:abfdjslhfld])], [q[ab], q[fdjslhfld]],
    q[string with trailing garbage];
is_deeply [bdecode(q[02:xy])], [], q[string with extra leading zero in count];
is_deeply [bdecode(q[l])], [[], undef], q[unclosed empty list];
is_deeply [bdecode(q[relwjhrlewjh])], [], q[complete garbage];
is_deeply [bdecode(q[d])], [{}, undef], q[unclosed empty dict];

#exit;
#    bdecode(q[defoobar]);
#    is_deeply [], [], ;q[empty dict with trailing garbage];
#exit;
#    bdecode(q[d3:fooe]);
#    is_deeply [], [], ;q[dict with odd number of elements];
#exit;
#    bdecode(q[di1e0:e]);
#    is_deeply [], [], ;q[dict with integer key];
#exit;
#    warn pp bdecode(q[d1:b0:1:a0:e]);
#    is_deeply [], [], ;q[missorted keys]; # perl has trouble with this...
#exit;
#    bdecode(q[d1:a0:1:a0:e]);
#    is_deeply [], [], ;q[duplicate keys];
#exit;
#    bdecode(q[l01:ae]);
#    is_deeply [], [], ;q[list with string with leading zero in count];
#exit;
#    bdecode(q[9999:x]);
#    is_deeply [], [], ;q[string shorter than count];
#exit;
#    bdecode(q[l0:]);
#    is_deeply [], [], ;q[unclosed list with content];
#exit;
#    bdecode(q[d0:0:]);
#    is_deeply [], [], ;q[unclosed dict with content];
#exit;
#    bdecode(q[d0:]);
#    is_deeply [], [], ;q[unclosed dict with odd number of elements];
#exit;
#    bdecode(q[l-3:e]);
#    is_deeply [], [], ;q[list with negative-length string];
#exit;
#    bdecode(q[0:0:]);
#    like $@, qr[Trailing garbage], sprintf q[Catch invalid format (%s)], $@;
#exit;
#    bdecode(q[ie]);
#    like $@, qr[Bad bencoded data], sprintf q[Catch invalid format (%s)], $@;
#exit;
#    bdecode(q[i341foo382e]);
#    is_deeply [], [], ;sprintf q[Catch invalid format (%s)], $@;
#exit;
#    bdecode(q[i123]);
#    is_deeply [], [], ;sprintf q[Catch invalid format (%s)], $@;
#exit;
is_deeply [bdecode(q[])], [], q[Catch invalid format (empty string)];
is_deeply [bdecode(q[i6easd])], [6, q[asd]],
    q[Catch invalid format (trailing garbage)];

#exit;
#warn pp    bdecode(q[35208734823ljdahflajhdf]);
#    is_deeply [], [], ;sprintf q[Catch invalid format (%s)], $@;
#exit;
#warn pp    bdecode(q[2:abfdjslhfld]);
#    is_deeply [], [], ;sprintf q[Catch invalid format (%s)], $@;
#exit;
#warn pp    bdecode(q[9999:x]);
#    is_deeply [], [], ;sprintf q[Catch invalid format (%s)], $@;
#exit;
#warn pp    bdecode(q[l]);
#    is_deeply [], [], ;sprintf q[Catch invalid format (%s)], $@;
#exit;
#warn pp    bdecode(q[leanfdldjfh]);
#    is_deeply [], [], ;sprintf q[Catch invalid format (%s)], $@;
#exit;
#warn pp    bdecode(q[relwjhrlewjh]);
#    is_deeply [], [], ;sprintf q[Catch invalid format (%s)], $@;
#exit;
#warn pp    bdecode(q[d]);
#    is_deeply [], [], ;sprintf q[Catch invalid format (%s)], $@;
is_deeply [bdecode(q[defoobar])], [{}, q[foobar]],
    q[Catch invalid format (empty dictionary w/ trailing garbage)];
is_deeply [bdecode(q[d3:fooe])], [{foo => undef}, undef],
    q[Catch invalid format (dictionary w/ empty key)];

#exit;
#warn pp    bdecode(q[d0:]);
#    is_deeply [], [], ;sprintf q[Catch invalid format (%s)], $@;
#exit;
#warn pp    bdecode(q[d0:0:]);
#    is_deeply [], [], ;sprintf q[Catch invalid format (%s)], $@;
#exit;
#warn pp   bdecode(q[l0:]);
#    is_deeply [], [], ;sprintf q[Catch invalid format (%s)], $@;
#exit;
#warn pp bdecode(q[2:x]);
#    is_deeply [], [], ;sprintf q[Catch invalid format (%s)], $@;
#exit;
#warn pp bdecode(q[dli1ee4:ROOTe]);
#    is_deeply [bdecode(q[dli1ee4:ROOTe])],[{}, undef],  sprintf q[Catch invalid format (%s)], $@;
is_deeply([bdecode(q[d1:ei0e1:mde1:pi48536e1:v15:µTorrent 1.7.7e])],
          [{e => 0, m => {}, p => 48536, v => "\xC2\xB5Torrent 1.7.7"}, undef
          ],
          q[Complex structure (empty dictionary, 'safe' hex chars)]
);
