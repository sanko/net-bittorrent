#!/usr/bin/perl -w
package test;
$|++;
SKIP: {
    use strict;
    use warnings;

    #
    use lib q[../lib];

    # let's keep track of where we are...
    my $test_builder = Test::More->builder;

    #
    BEGIN {
        use Test::More;
        plan tests => 311;
    }
    {
        diag(q[Testing Net::BitTorrent::Util]);
        BEGIN { use_ok(q[Net::BitTorrent::Util], qw[:bencode]); }
        diag(q[  [...]::bdecode]);
        my $test_string
            = q[d7:Integeri42e4:Listl6:item 1i2ei3ee6:String9:The Valuee];
        my ($hashref) = bdecode($test_string);
        ok defined $hashref, q[bdecode() returned something];
        is ref $hashref, q[HASH], q[bdecode() returned a valid hash ref];
        ok defined $hashref->{q[Integer]}, q[Integer key present];
        is $hashref->{q[Integer]}, 42, q[  and its the correct value];
        ok defined $hashref->{q[String]}, q[String key present];
        is ${$hashref;}{q[String]}, q[The Value],
            q[  and its the correct value];
        ok defined $hashref->{q[List]}, q[List key present];
        is @{$hashref->{q[List]}}, 3, q[  list has 3 elements];
        is ${$hashref->{q[List]}}[0], q[item 1], q[    first element correct];
        is ${$hashref->{q[List]}}[1], 2, q[    second element correct];
        is ${$hashref->{q[List]}}[2], 3, q[    third element correct];
        my ($encoded_string) = bencode($hashref);
        ok defined $encoded_string, q[bencode() returned something];
        is $encoded_string, $test_string,
            q[  and it appears to be the correct value];
        is(bdecode(q[i4e]),      4,      q[integer]);
        is(bdecode(q[i0e]),      0,      q[zero]);
        is(bdecode(q[i-0e]) + 0, 0,      q[zero w/ sign]);
        is(bdecode(q[i-10e]),    -10,    q[negative integer]);
        is(bdecode(q[0:]),       undef,  q[zero length string]);
        is(bdecode(q[3:abc]),    q[abc], q[string]);
        is(bdecode(q[10:1234567890]),
            q[1234567890], q[integer cast as string]);
        is(bdecode(q[02:xy]), undef, q[string with leading zero in length]);
        is(bdecode(q[i03e]),  3,     q[integer with leading zero]);
        is_deeply([bdecode(q[0:0:])], [],
                  q[trailing junk at end of valid bencoded string]);
        is_deeply([bdecode(q[leanfdldjfh])],
                  [[], q[anfdldjfh]],
                  q[empty list with trailing garbage]);
        is_deeply([bdecode(q[i])],           [], q[aborted integer]);
        is_deeply([bdecode(q[i0])],          [], q[unterminated integer]);
        is_deeply([bdecode(q[ie])],          [], q[empty integer]);
        is_deeply([bdecode(q[i341foo382e])], [], q[malformed integer]);
        is_deeply([bdecode(q[i123])],        [], q[unterminated integer]);
        is_deeply([bdecode(q[1:])],          [], q[string longer than data]);
        is_deeply([bdecode(q[i6easd])],
                  [6, q[asd]],
                  q[string with trailing junk]);
        is_deeply([bdecode(q[35208734823ljdahflajhdf])],
                  [],
                  q[garbage looking vaguely like a string, with large count]);
        is_deeply([bdecode(q[2:abfdjslhfld])],
                  [q[ab], q[fdjslhfld]],
                  q[string with trailing garbage]
        );
        is_deeply([bdecode(q[02:xy])], [],
                  q[string with extra leading zero in count]);
        is_deeply([bdecode(q[l])], [[], q[]], q[unclosed empty list]);
        is_deeply([bdecode(q[relwjhrlewjh])], [], q[complete garbage]);
        is_deeply([bdecode(q[d])], [{}, q[]], q[unclosed empty dict]);
        is_deeply(
                [bdecode(q[defoobar])],
                [{}, q[foobar]],
                q[Catch invalid format (empty dictionary w/ trailing garbage)]
        );
        is_deeply([bdecode(q[d3:fooe])],
                  [{foo => undef}, undef],
                  q[Catch invalid format (dictionary w/ empty key)]);
        is_deeply([bdecode(q[d1:ei0e1:mde1:pi48536e1:v15:µTorrent 1.7.7e])],
                  [{e => 0,
                    m => {},
                    p => 48536,
                    v => "\xC2\xB5Torrent 1.7.7"
                   },
                   q[]
                  ],
                  q[Complex structure (empty dictionary, 'safe' hex chars)]
        );

        #
        diag(q[  [...]::bencode]);
        is(bencode(4),   q[i4e],   q[integer]);
        is(bencode(0),   q[i0e],   q[zero]);
        is(bencode(-0),  q[i0e],   q[zero w/ sign]);
        is(bencode(-10), q[i-10e], q[negative integer]);
        is(bencode(+10), q[i10e],  q[positive integer]);
        is(bencode(time x 50), q[i] . (time x 50) . q[e], q[large number]);
        is(bencode(q[Perl]), q[4:Perl], q[string]);
        is(bencode(q[]),     q[0:],     q[null string]);
        is(bencode(q[0:0:]), q[4:0:0:],
            q[odd string (malformed bencoded int)]);
        is(bencode([1, 2, 3]), q[li1ei2ei3ee], q[list (integers)]);
        is(bencode([q[one], q[two], q[three]]),
            q[l3:one3:two5:threee], q[list (strings)]);
        is(bencode([q[three], 1, 2, 3, q[one], q[two]]),
            q[l5:threei1ei2ei3e3:one3:twoe],
            q[list (mixed scalars)]);
        is(bencode([]), q[le], q[empty list]);
        is(bencode([[q[Alice], q[Bob]], [2, 3]]),
            q[ll5:Alice3:Bobeli2ei3eee], q[list^list]);
        is(bencode({date => {month => q[January], year => 2009}}),
            q[d4:dated5:month7:January4:yeari2009eee],
            q[dictionary]);
        is(bencode({}), q[de], q[dictionary from empty hash]);
        is(bencode({age => 25, eyes => q[blue]}),
            q[d3:agei25e4:eyes4:bluee], q[dictionary from anon hash]);
        is( length bencode({join(q[], map(chr($_), 0 .. 255)) =>
                                join(q[], map(chr($_), 0 .. 255))
                           }
            ),
            522,
            q[anon hash with long key/value pair]
        );
        bencode(
            {sub => sub { return my $brain; }
            }
        );
    }
    {

        BEGIN {
            use_ok(q[Net::BitTorrent::Util], qw[:compact]);
        }
        diag(q[  [...]::compact]);
        is(compact(qw[127.0.0.1:98]), qq[\x7F\0\0\1\0b], q[localhost]);
        is(compact(qw[127.0.0.1:0]),
            qq[\x7F\0\0\1\0\0], q[port number of zero]);
        is(compact(qw[127.0.0.1:5000]),
            qq[\x7F\0\0\1\23\x88], q[Large port number]);
        is(compact(qw[127.0.0.1:65535]),
            qq[\x7F\0\0\1\xFF\xFF], q[Large port number]);
        is(compact(qw[127.0.0.1:3265 255.25.21.32:0]),
            qq[\x7F\0\0\1\f\xC1\xFF\31\25 \0\0],
            q[short list of peers]);
        diag(q[TODO: invalid IPv4 (ex: 999.999.999.999)]);
        is(compact(qw[127.0.0.1:3265 127.0.0.1:3265]),
            qq[\x7F\0\0\1\f\xC1], q[Filter duplicates]);

        #
        diag(q[   Testing attempts to compact bad data...]);
        is(compact(qw[127.0.0.1:000065]),
            qq[\x7F\0\0\1\0A], q[Port with leading zeros]);
        is(compact(qw[270.0.0.1:0]),
            undef, q[Invalid IP address: 270.0.0.1:0]);
        is(compact(qw[127.0.0.1:5000000]),
            undef, q[Port number beyond ephemeral range: 127.0.0.1:5000000]);
        is(compact(qw[127.0.0.1:500:30]),
            undef, q[Invalid IP address: 127.0.0.1:500:30]);
        is(compact(qw[127.0.0.1.3:50030]),
            undef, q[Invalid IP address: 127.0.0.1.3:50030]);
        is(compact(qw[127.0.0.1:3265 255.25.21.32:0:4554845]),
            qq[\x7F\0\0\1\f\xC1], q[Invalid peer in list]);
        is(compact(qw[]),  undef, q[Empty list]);
        is(compact(q[]),   undef, q[String]);
        is(compact(undef), undef, q[undef]);
        is(compact(),      undef, q[undef]);
        diag(q[   TODO: IPv6 passing itself off as IPv4.]);

        #
        diag(q[  [...]::uncompact]);
        is(uncompact(q[]),   undef, q[Empty string]);
        is(uncompact(undef), undef, q[undef string]);
        is(uncompact(),      undef, q[undef string]);
        is(uncompact(qq[\x7F\0\0\1\xFF\xFF]),
            qw[127.0.0.1:65535], q[Large port number]);
        my @peers = sort(uncompact(qq[\x7F\0\0\1\f\xC1\xFF\31\25 \0\0]));
        is(@peers,    2,                 q[Short list of peers...]);
        is($peers[0], q[127.0.0.1:3265], q[   ...First checks out.]);
        is($peers[1], q[255.25.21.32:0], q[   ...Second is okay too.]);

        #
        diag(q[   TODO: Attempts to uncompact bad data]);

        #is( uncompact(qw[127.0.0.1:98]), qq[\x7F\0\0\1\0b], q[localhost] );
        #is( uncompact(qw[127.0.0.1:0]),
        #    qq[\x7F\0\0\1\0\0], q[port number of zero] );
        #is( uncompact(qw[127.0.0.1:5000]),
        #    qq[\x7F\0\0\1\23\x88], q[large port number] );
    }
    {
        diag(q[Testing Net::BitTorrent::Protocol]);

        BEGIN {
            use lib q[./lib];
            use_ok(q[Net::BitTorrent::Protocol], qw[:build]);
        }
        diag(q[ [...]::build_handshake()]);
        is(build_handshake(), undef, q[   ...requires three params]);
        is(build_handshake(undef, undef, undef),
            undef, q[   ...uh, defined params]);
        is(build_handshake(q[junk], q[junk], q[junk]),
            undef, q[  (...proper defined params, please!)]);
        is(build_handshake(q[junk9565], q[junk], q[junk]),
            undef, q[  (...proper defined params, please!)]);
        is(build_handshake(q[junk9565], q[junk] x 5, q[junk]),
            undef, q[  (...proper defined params, please!)]);
        is(build_handshake(chr(0) x 8, q[junk], q[junk]),
            undef, q[   ...8 reserved bytes,]);
        is(build_handshake(chr(0) x 8, q[01234567890123456789], q[junk]),
            undef, q[   ...a peerid,]);
        is( build_handshake(pack(q[C*], split(q[], q[00000000])),
                            pack(q[H*], q[0123456789] x 4),
                            q[random peer id here!]
            ),
            qq[\23BitTorrent protocol\0\0\0\0\0\0\0\0\1#Eg\x89\1#Eg\x89\1#Eg\x89\1#Eg\x89random peer id here!],
            q[   ...and an infohash.]
        );
        is( build_handshake(chr(0) x 8, q[A] x 20, q[B] x 20),
            qq[\23BitTorrent protocol\0\0\0\0\0\0\0\0AAAAAAAAAAAAAAAAAAAABBBBBBBBBBBBBBBBBBBB],
            q[   Double check.]
        );

        #
        diag(q[ [...]::build_keepalive]);
        is(build_keepalive(), qq[\0\0\0\0],
            q[   ...requires no params and has no payload]);

        #
        diag(q[ [...]::build_choke()]);
        is(build_choke(), qq[\0\0\0\1\0],
            q[   ...requires no params and has no payload]);

        #
        diag(q[ [...]::build_unchoke()]);
        is(build_unchoke(), qq[\0\0\0\1\1],
            q[   ...requires no params and has no payload]);

        #
        diag(q[ [...]::build_interested()]);
        is(build_interested(), qq[\0\0\0\1\2],
            q[   ...requires no params and has no payload]);

        #
        diag(q[ [...]::build_not_interested()]);
        is(build_not_interested(), qq[\0\0\0\1\3],
            q[   ...requires no params and has no payload]);

        #
        diag(q[ [...]::build_have()]);
        is(build_have(),            undef, q[   ...requires a single param]);
        is(build_have(q[1desfdds]), undef, q[   ...an index]);
        is(build_have(9), qq[\0\0\0\5\4\0\0\0\t],
            q[   ...requires a single param]);
        is(build_have(0), qq[\0\0\0\5\4\0\0\0\0], q[   ...an number]);
        is( build_have(999999999999999),
            qq[\0\0\0\5\4\xFF\xFF\xFF\xFF],
            q[   ...even a large one is okay]
        );
        diag(
            q[     (A quadrillion piece torrent? Ha! The .torrent itself would be several GBs)]
        );
        is(build_have(-5), undef, q[   ...as long as it's positive]);

        #
        diag(q[ [...]::build_bitfield]);
        is(build_bitfield(),    undef, q[   ...requires a single param]);
        is(build_bitfield(q[]), undef, q[   ...a packed bitfield]);
        is(build_bitfield(q[abcdefg]),
            qq[\0\0\0\b\5abcdefg],
            q[   ...but what _doesn't_ unpack to binary?]);
        my $tmp = join((join time, keys(%ENV)), %INC);  # fairly random string
        is(build_bitfield($tmp),
            pack(q[N], length($tmp) + 1) . chr(5) . $tmp,
            q[   ...more testing]);

        #
        diag(q[ [...]::build_request]);
        is(build_request(undef, 2,     3), undef, q[   ...requires an index]);
        is(build_request(1,     undef, 3), undef, q[   ...an offset]);
        is(build_request(1, 2, undef), undef, q[   ...and a length.]);
        is(build_request(q[], q[], q[]),
            undef, q[   They should all be positive numbers (A)]);
        is(build_request(-1, q[], q[]),
            undef, q[   They should all be positive numbers (B)]);
        is(build_request(1, q[], q[]),
            undef, q[   They should all be positive numbers (C)]);
        is(build_request(1, -2, q[]),
            undef, q[   They should all be positive numbers (D)]);
        is(build_request(1, 2, q[]),
            undef, q[   They should all be positive numbers (E)]);
        is(build_request(1, 2, -3),
            undef, q[   They should all be positive numbers (F)]);
        is(build_request(1, 2, 3),
            qq[\0\0\0\r\6\0\0\0\1\0\0\0\2\0\0\0\3],
            q[   They should all be positive numbers (G)]);
        is( build_request(999999999999999, 999999999999999, 999999999999999),
            pack("H*", "0000000d06ffffffffffffffffffffffff"),
            q[   They should all be positive numbers (H)]
        );

        #
        diag(q[ [...]::build_piece]);
        is(build_piece(undef, 2, 3), undef, q[   ...requires an index]);
        is(build_piece(1,   undef, q[test]), undef, q[   ...an offset]);
        is(build_piece(1,   2,     undef),   undef, q[   ...and data]);
        is(build_piece(q[], q[],   q[]),     undef, q[   validation (A)]);
        is(build_piece(-1,  q[],   q[]),     undef, q[   validation (B)]);
        is(build_piece(1,   q[],   q[]),     undef, q[   validation (C)]);
        is(build_piece(1,   -2,    q[]),     undef, q[   validation (D)]);
        is(build_piece(1,   2,     \q[XXX]),
            qq[\0\0\0\f\a\0\0\0\1\0\0\0\2XXX],
            q[   validation (E)]);
        is(build_piece(1, 2, \$tmp),
            (pack(q[NcN2a*], length($tmp) + 9, 7, 1, 2, $tmp)),
            q[   validation (F)]);

        #
        diag(q[ [...]::build_cancel]);
        is(build_cancel(undef, 2,     3), undef, q[   ...requires an index]);
        is(build_cancel(1,     undef, 3), undef, q[   ...an offset]);
        is(build_cancel(1, 2, undef), undef, q[   ...and a length.]);
        is(build_cancel(q[], q[], q[]),
            undef, q[   They should all be positive numbers (A)]);
        is(build_cancel(-1, q[], q[]),
            undef, q[   They should all be positive numbers (B)]);
        is(build_cancel(1, q[], q[]),
            undef, q[   They should all be positive numbers (C)]);
        is(build_cancel(1, -2, q[]),
            undef, q[   They should all be positive numbers (D)]);
        is(build_cancel(1, 2, q[]),
            undef, q[   They should all be positive numbers (E)]);
        is(build_cancel(1, 2, -3),
            undef, q[   They should all be positive numbers (F)]);
        is(build_cancel(1, 2, 3),
            qq[\0\0\0\r\b\0\0\0\1\0\0\0\2\0\0\0\3],
            q[   They should all be positive numbers (G)]);
        is( build_cancel(999999999999999, 999999999999999, 999999999999999),
            pack("H*", "0000000d08ffffffffffffffffffffffff"),
            q[   They should all be positive numbers (H)]
        );

        #
        diag(q[ [...]::build_port]);
        is(build_port(),    undef, q[   Requires a port number]);
        is(build_port(-5),  undef, q[   ...and ports are always positive]);
        is(build_port(3.3), undef, q[   ...integers]);
        is(build_port(q[test]), undef, q[   Validation (A)]);
        is(build_port(8555), qq[\0\0\0\5\t\0\0!k], q[   Validation (B)]);
        is(build_port(652145), qq[\0\0\0\a\t\0\t\xF3q], q[   Validation (C)]);

        #
        diag(q[ [...]::build_allowed_fast]);
        is(build_allowed_fast(), undef, q[   Requires a piece index]);
        is(build_allowed_fast(-5), undef,
            q[   ...which is always a positive]);
        is(build_allowed_fast(3.3),     undef, q[   ...integer]);
        is(build_allowed_fast(q[test]), undef, q[   Validation (A)]);
        is(build_allowed_fast(8555),
            qq[\0\0\0\5\21\0\0!k], q[   Validation (B)]);
        is(build_allowed_fast(652145),
            qq[\0\0\0\5\21\0\t\xF3q], q[   Validation (C)]);
        is(build_allowed_fast(0), qq[\0\0\0\5\21\0\0\0\0],
            q[   Validation (D)]);
        diag(q[ [...]::build_reject_request]);
        is(build_reject_request(undef, 2, 3),
            undef, q[   ...requires an index]);
        is(build_reject_request(1, undef, 3), undef, q[   ...an offset]);
        is(build_reject_request(1, 2, undef), undef, q[   ...and a length.]);
        is(build_reject_request(q[], q[], q[]),
            undef, q[   They should all be positive numbers (A)]);
        is(build_reject_request(-1, q[], q[]),
            undef, q[   They should all be positive numbers (B)]);
        is(build_reject_request(1, q[], q[]),
            undef, q[   They should all be positive numbers (C)]);
        is(build_reject_request(1, -2, q[]),
            undef, q[   They should all be positive numbers (D)]);
        is(build_reject_request(1, 2, q[]),
            undef, q[   They should all be positive numbers (E)]);
        is(build_reject_request(1, 2, -3),
            undef, q[   They should all be positive numbers (F)]);
        is(build_reject_request(1, 2, 3),
            qq[\0\0\0\r\20\0\0\0\1\0\0\0\2\0\0\0\3],
            q[   They should all be positive numbers (G)]);
        is( build_reject_request(999999999999999, 999999999999999,
                                 999999999999999
            ),
            pack("H*", "0000000d10ffffffffffffffffffffffff"),
            q[   They should all be positive numbers (H)]
        );

        #
        diag(q[ [...]::build_have_all]);
        is(build_have_all(), qq[\0\0\0\1\16],
            q[   ...requires no params and has no payload]);

        #
        diag(q[ [...]::build_have_none]);
        is(build_have_none(), qq[\0\0\0\1\17],
            q[   ...requires no params and has no payload]);

        #
        diag(q[ [...]::build_suggest_piece]);
        is(build_suggest_piece(), undef, q[   Requires a piece index]);
        is(build_suggest_piece(-5), undef,
            q[   ...which is always a positive]);
        is(build_suggest_piece(3.3),     undef, q[   ...integer]);
        is(build_suggest_piece(q[test]), undef, q[   Validation (A)]);
        is(build_suggest_piece(8555),
            qq[\0\0\0\5\r\0\0!k], q[   Validation (B)]);
        is(build_suggest_piece(652145),
            qq[\0\0\0\5\r\0\t\xF3q], q[   Validation (C)]);
        is(build_suggest_piece(0), qq[\0\0\0\5\r\0\0\0\0],
            q[   Validation (D)]);

        #
        diag(q[TODO: build_ExtProtocol]);
        is(build_ExtProtocol(), undef,
            q[   ...requires a message id and a playload]);
        is(build_ExtProtocol(undef, {}), undef, q[   ...validation (A)]);
        is(build_ExtProtocol(-1,    {}), undef, q[   ...validation (B)]);
        is(build_ExtProtocol(q[],   {}), undef, q[   ...validation (C)]);
        is(build_ExtProtocol(0, undef), undef, q[   ...validation (D)]);
        is(build_ExtProtocol(0, 2),     undef, q[   ...validation (E)]);
        is(build_ExtProtocol(0, -2),    undef, q[   ...validation (F)]);
        is(build_ExtProtocol(0, q[]),   undef, q[   ...validation (G)]);
        is( build_ExtProtocol(0, {}),
            qq[\0\0\0\4\24\0de],
            q[   ...validation (H)]
        );
        is( build_ExtProtocol(
                0,
                {m => {ut_pex => 1, q[µT_PEX] => 2},
                 (    # is incoming ? ():
                    (p => 30)    # port
                 ),
                 v      => q[Net::BitTorrent r0.30],
                 yourip => pack(q[C4], (q[127.0.0.1] =~ m[(\d+)]g)),
                 reqq => 30      # XXX - Lies.  It's on my todo list...
                       # reqq == An integer, the number of outstanding request messages
                       # this client supports without dropping any.  The default in in
                       # libtorrent is 2050.
                }
            ),
            qq[\0\0\0Z\24\0d1:md6:ut_pexi1e7:\xC2\xB5T_PEXi2ee1:pi30e4:reqqi30e1:v21:Net::BitTorrent r0.306:yourip4:\x7F\0\0\1e],
            q[   ...validation (I | initial handshake)]
        );

#
#         use Data::Dump qw[pp];
#~         warn pp build_ExtProtocol(
#~             0,
#~             {m => {ut_pex => 1, q[µT_PEX] => 2},
#~              (         # is incoming ? ():
#~                 (p => 30)    # port
#~              ),
#~              v      => q[Net::BitTorrent r0.30],
#~              yourip => pack(q[C4], (q[127.0.0.1] =~ m[(\d+)]g)),
#~              reqq => 30      # XXX - Lies.  It's on my todo list...
#~                    # reqq == An integer, the number of outstanding request messages
#~                    # this client supports without dropping any.  The default in in
#~                    # libtorrent is 2050.
#~             }
#~         );
#~         die;
#
    }
    {
        diag(q[Testing Net::BitTorrent::Session::Tracker]);
        diag(
            q[Net::BitTorrent::Session::Tracker->new() requires parameters...]
        );
        is(eval { Net::BitTorrent::Session::Tracker->new() },
            undef, q[Net::BitTorrent::Session::Tracker->new( )]);
        is( eval {
                Net::BitTorrent::Session::Tracker->new(
                                      URLs => q[http://example.com/announce]);
            },
            undef,
            q[Net::BitTorrent::Session::Tracker->new(URLs => q[http://example.com/announce])],
        );
        is(eval { Net::BitTorrent::Session::Tracker->new({}) },
            undef, q[Net::BitTorrent::Session::Tracker->new({ })]);
        is( eval {
                Net::BitTorrent::Session::Tracker->new(
                                    {URLs => q[http://example.com/announce]});
            },
            undef,
            q[Net::BitTorrent::Session::Tracker->new({URLs => q[http://example.com/announce/]})]
        );
        is( eval { Net::BitTorrent::Session::Tracker->new({URLs => undef}) },
            undef,
            q[Net::BitTorrent::Session::Tracker->new({URLs => undef})],
        );
        is(eval { Net::BitTorrent::Session::Tracker->new({URLs => []}) },
            undef, q[Net::BitTorrent::Session::Tracker->new({URLs => qw[]})]);
        is( eval {
                Net::BitTorrent::Session::Tracker->new(
                                 {URLs => [qw[http://example.com/announce]]});
            },
            undef,
            q[Net::BitTorrent::Session::Tracker->new({URLs   => [qw[http://example.com/announce]]})]
        );
        is( eval {
                Net::BitTorrent::Session::Tracker->new(
                     {URLs => [qw[http://example.com/announce]], Session => 0}
                );
            },
            undef,
            q[Net::BitTorrent::Session::Tracker->new({URLs   => [qw[http://example.com/announce]], Session => 0 })]
        );
        is( eval {
                Net::BitTorrent::Session::Tracker->new(
                        {URLs    => [],
                         Session => (bless {}, q[Net::BitTorrent::Session])
                        }
                );
            },
            undef,
            q[Net::BitTorrent::Session::Tracker->new({URLs => [], Session => bless({},q[Not::BitTorrent::Session])})]
        );

        #
        isa_ok(
            eval {
                Net::BitTorrent::Session::Tracker->new(
                                {URLs    => [qw[http://example.com/announce]],
                                 Session => Not::BitTorrent::Session->new()
                                }
                );
            },
            q[Net::BitTorrent::Session::Tracker],
            q[Net::BitTorrent::Session::Tracker->new({URLs => [qw[http://example.com/announce]], Session => bless({},q[Net::BitTorrent::Session])})]
        );
        diag(q[TODO: Everything]);
    }
    {
        use File::Temp qw[tempfile];
        my ($filehandle, $filename)
            = tempfile(q[~NBSF_test_XXXXX], OPEN => 1, TMPDIR => 1);
        diag(q[Testing Net::BitTorrent::Session::File]);
        diag(sprintf(q[   File::Temp created '%s' for us to play with],
                     $filename)
        );
        diag(q[Net::BitTorrent::Session::File->new() requires parameters...]);
        is(eval { Net::BitTorrent::Session::File->new() },
            undef, q[Net::BitTorrent::Session::File->new( )]);
        is( eval { Net::BitTorrent::Session::File->new(Path => $filename) },
            undef,
            sprintf(q[Net::BitTorrent::Session::File->new(Path => q[%s])],
                    $filename)
        );
        is(eval { Net::BitTorrent::Session::File->new({}) },
            undef, q[Net::BitTorrent::Session::File->new({ })]);
        is( eval { Net::BitTorrent::Session::File->new({Path => $filename}) },
            undef,
            sprintf(q[Net::BitTorrent::Session::File->new({Path => q[%s]})],
                    $filename)
        );
        is( eval { Net::BitTorrent::Session::File->new({Path => $filename}) },
            undef,
            sprintf(q[Net::BitTorrent::Session::File->new({Path => q[%s]})],
                    $filename)
        );
        is( eval {
                Net::BitTorrent::Session::File->new({Path    => $filename,
                                                     Session => 0
                                                    }
                );
            },
            undef,
            sprintf(
                q[Net::BitTorrent::Session::File->new({Path => q[%s],Session=> 0})],
                $filename)
        );
        is( eval {
                Net::BitTorrent::Session::File->new(
                         {Path    => $filename,
                          Session => bless({}, q[Not::BitTorrent::Session])
                         }
                );
            },
            undef,
            sprintf(
                q[Net::BitTorrent::Session::File->new({Path => q[%s],Session=> bless {}, q[Not::BitTorrent::Session]})],
                $filename)
        );
        is( eval {
                Net::BitTorrent::Session::File->new(
                        {Path    => $filename,
                         Session => bless({}, q[Net::BitTorrent::Session]),
                         Size    => undef
                        }
                );
            },
            undef,
            sprintf(
                q[Net::BitTorrent::Session::File->new({Path => q[%s],Session=> bless({}, q[Net::BitTorrent::Session]),Size=>undef})],
                $filename)
        );
        is( eval {
                Net::BitTorrent::Session::File->new(
                        {Path    => $filename,
                         Session => bless({}, q[Net::BitTorrent::Session]),
                         Size    => q[QQQ]
                        }
                );
            },
            undef,
            sprintf(
                q[Net::BitTorrent::Session::File->new({Path => q[%s],Session=> bless({}, q[Net::BitTorrent::Session]),Size=> q[QQQ]})],
                $filename)
        );
        is( eval {
                Net::BitTorrent::Session::File->new(
                        {Path    => $filename,
                         Session => bless({}, q[Net::BitTorrent::Session]),
                         Size    => -1024
                        }
                );
            },
            undef,
            sprintf(
                q[Net::BitTorrent::Session::File->new({Path => q[%s],Session=> bless({}, q[Net::BitTorrent::Session]),Size=> -1024})],
                $filename)
        );
        is( eval {
                Net::BitTorrent::Session::File->new(
                        {Path    => $filename,
                         Session => bless({}, q[Net::BitTorrent::Session]),
                         Size    => 1024
                        }
                );
            },
            undef,
            sprintf(
                q[Net::BitTorrent::Session::File->new({Path => q[%s],Session=> bless({}, q[Net::BitTorrent::Session]),Size=> 1024})],
                $filename)
        );
        is( eval {
                Net::BitTorrent::Session::File->new(
                        {Path    => $filename,
                         Session => bless({}, q[Net::BitTorrent::Session]),
                         Size    => 1024,
                         Index   => undef,
                        }
                );
            },
            undef,
            sprintf(
                q[Net::BitTorrent::Session::File->new({Path => q[%s],Session=> bless({}, q[Net::BitTorrent::Session]),Size=> 1024, Index=>undef})],
                $filename)
        );
        is( eval {
                Net::BitTorrent::Session::File->new(
                        {Path    => $filename,
                         Session => bless({}, q[Net::BitTorrent::Session]),
                         Size    => 1024,
                         Index   => -1
                        }
                );
            },
            undef,
            sprintf(
                q[Net::BitTorrent::Session::File->new({Path => q[%s],Session=> bless({}, q[Net::BitTorrent::Session]),Size=> 1024, Index=>-1})],
                $filename)
        );
        is( eval {
                Net::BitTorrent::Session::File->new(
                        {Path    => $filename,
                         Session => bless({}, q[Net::BitTorrent::Session]),
                         Size    => 1024,
                         Index   => q[AAA]
                        }
                );
            },
            undef,
            sprintf(
                q[Net::BitTorrent::Session::File->new({Path => q[%s],Session=> bless({}, q[Net::BitTorrent::Session]),Size=> 1024, Index=> 'AAA'})],
                $filename)
        );
        is( eval {
                Net::BitTorrent::Session::File->new(
                        {Path    => $filename,
                         Session => bless({}, q[Net::BitTorrent::Session]),
                         Size    => 1024,
                         Index   => \0
                        }
                );
            },
            undef,
            sprintf(
                q[Net::BitTorrent::Session::File->new({Path => q[%s],Session=> bless({}, q[Net::BitTorrent::Session]),Size=> 1024, Index=>1})],
                $filename)
        );

        #
        my $file_object =
            Net::BitTorrent::Session::File->new(
                        {Path    => $filename,
                         Session => bless({}, q[Net::BitTorrent::Session]),
                         Size    => 1024,
                         Index   => 1
                        }
            );
        isa_ok(
            $file_object,
            q[Net::BitTorrent::Session::File],
            sprintf(
                q[Net::BitTorrent::Session::File->new({Path => q[%s],Session=> bless({}, q[Not::BetTorrent::Session), Size=> 1024]})],
                $filename)
        );
        diag(q[Check all sorts of stuff...]);
        is($file_object->priority, 2, q[   ...priority() defaults to 2]);
        is(eval { $file_object->set_priority() },
            undef, q[   ...set_priority() requires a parameter]);
        ok($file_object->set_priority(3), q[   ...set_priority(3) works]);
        is(eval { $file_object->set_priority(-3) },
            undef, q[   ...set_priority(-3) does not]);
        is(eval { $file_object->set_priority(q[random]) },
            undef, q[   ...set_priority('random') doesn't either]);
        is($file_object->priority, 3, q[   ...priority() is now 3]);

#is( $single_file_torrent->infohash,
#    q[98be9ffbf2876fee212607faefd91705fc047581],
#    q[Infohash was generated correctly.]
#);
#is(Net::BitTorrent::Session::File->new({Path => q[blah.torrent]}),
#    undef,
#    q[Net::BitTorrent::Session::File->new({Path => q[blah.torrent]})]);
#
#~         is($Session_even_sized_list_params, undef,
#~             q[Net::BitTorrent->new(LocalPort => [20502..20505]) returns undef]);
#~         my $Session_list_params = Net::BitTorrent->new([20502 .. 20505]);
#~         is($Session_list_params, undef,
#~             q[Net::BitTorrent->new([20502..20505]) returns undef]);
#~         my $Session_singal_param = Net::BitTorrent->new(q[0.0.0.0:20502]);
#~         is($Session_singal_param, undef,
#~             q[Net::BitTorrent->new(q[0.0.0.0:20502]) returns undef]);
#~
#~         #
#~         my $Session_default_port = Net::BitTorrent->new({});
#~         isa_ok($Session_default_port, q[Net::BitTorrent],
#~                q[Net::BitTorrent->new({ })]);
#~         my $Session_single_port = Net::BitTorrent->new({LocalPort => 20502});
#~         isa_ok($Session_single_port, q[Net::BitTorrent],
#~                q[Net::BitTorrent->new({LocalPort => 20502})]);
#~         my $Session_range_port
#~             = Net::BitTorrent->new({LocalPort => [20502 .. 20505]});
#~         isa_ok($Session_range_port, q[Net::BitTorrent],
#~                q[Net::BitTorrent->new({LocalPort => [20502 .. 20505]})]);
#~         my $Session_list_port
#~             = Net::BitTorrent->new({LocalPort => [20502, 20505]});
#~         isa_ok($Session_list_port, q[Net::BitTorrent],
#~                q[Net::BitTorrent->new({LocalPort => [20502, 20505]})]);
#~
#~         #
#~         my $socket = $Session_list_port->_get_socket;
#~         isa_ok($Session_list_port->_get_socket, q[GLOB], q[Socket is valid.]);
#~         my ($port, $packed_ip)
#~             = unpack_sockaddr_in(getsockname($Session_list_port->_get_socket));
#~         is($port, 20505, q[Correct port was opened (20505).]);
#~
#~         #
#~         like($Session_list_port->get_peerid,
#~              qr[^NB\d{3}[CS]-.{8}.{5}$], q[Peer ID conforms to spec.]);
#
    }
    {

        BEGIN {
            use_ok(q[Net::BitTorrent::Peer]);
        }

        #
        diag(q[Testing Net::BitTorrent::Peer]);
        diag(q[Net::BitTorrent::Peer->new() requires parameters...]);
        is(eval { Net::BitTorrent::Peer->new() },
            undef, q[Net::BitTorrent::Peer->new( )]);
        is(eval { Net::BitTorrent::Peer->new({}) },
            undef, q[Net::BitTorrent::Peer->new({ })]);
        is(eval { Net::BitTorrent::Peer->new({Client => 0}) },
            undef, q[Net::BitTorrent::Peer->new( { Client=> 0 } )]);
        is( eval {
                Net::BitTorrent::Peer->new(
                                  {Client => (bless {}, q[Not::BitTorrent])});
            },
            undef,
            q[Net::BitTorrent::Peer->new({Client=> bless ({}, q[Not::BitTorrent])})]
        );

        #
        BEGIN {
            use_ok(
                q[Socket], qw[AF_INET SOCK_STREAM INADDR_LOOPBACK
                    SOL_SOCKET SO_REUSEADDR sockaddr_in
                    ]
            );
        }

        #
        diag(q[For this next bit, we're testing incoming peers...]);
    SKIP: {
            socket(my ($newsock), AF_INET, SOCK_STREAM, getprotobyname('tcp'))
                ? pass(q[Created $newsock])
                : skip(q[Failed to create $newsock], 8);
            socket(my ($listen), AF_INET, SOCK_STREAM, getprotobyname('tcp'))
                ? pass(q[Created $listen])
                : skip(q[Failed to create $listen], 7);
            setsockopt($listen, SOL_SOCKET, SO_REUSEADDR, pack(q[l], 1))
                ? pass(q[setsockopt SO_REUSEADDR])
                : skip(q[Failed to setsockop SO_REUSEADDR], 6);
            bind($listen, sockaddr_in(0, INADDR_LOOPBACK))
                ? pass(q[Bound $listen to any open port on any local address])
                : skip(q[Failed to bind $listen], 4);

            # diag
            my ($port, $packed_ip) = unpack_sockaddr_in(getsockname($listen));
            listen($listen, 5)
                ? pass(sprintf(q[Socket listening on %s:%d],
                               inet_ntoa($packed_ip), $port
                       )
                )
                : skip(sprintf(q[Failed to listen on %s:%d],
                               inet_ntoa($packed_ip), $port
                       ),
                       3
                );
            connect($newsock, getsockname($listen))
                ? pass(q[Connecting...])
                : skip(q[Failed to connect loopback], 2);
            accept(my ($accept), $listen)
                ? pass(q[Socket accepted])
                : skip(q[Failed to accept new connection], 1);
            my $peer_A =
                Net::BitTorrent::Peer->new(
                                  {Client => bless({}, q[Net::BitTorrent]),
                                   Socket => $accept
                                  }
                );
            isa_ok(
                  $peer_A,
                  q[Net::BitTorrent::Peer],
                  q[Net::BitTorrent::Peer->new({Client=>[...],Socket=>[...]})]
            );
        }
        diag(q[For this next bit, we're testing outgoing peers...]);
        {
            my $peer_B =
                Net::BitTorrent::Peer->new(
                               {Client  => Not::BitTorrent->new(),
                                Session => Not::BitTorrent::Session->new(),
                                Address => q[127.0.0.1:1995]
                               }
                );
            isa_ok(
                $peer_B,
                q[Net::BitTorrent::Peer],
                q[Net::BitTorrent::Peer->new({Client=>[...],Session=>[...],Address=>[...]})]
            );
        }
        diag(q[TODO: Check all sorts of socket related stuff...]);
    }
    {

        BEGIN {
            use_ok(q[Net::BitTorrent::Session]);
        }

        #
        my $multi_dot_torrent
            = q[./t/900_data/950_torrents/952_multi.torrent];
        my $single_dot_torrent
            = q[./t/900_data/950_torrents/951_single.torrent];

        # Make sure the path is correct
        chdir q[../] if not -f $multi_dot_torrent;

        #
        diag(q[Testing Net::BitTorrent::Session]);

        #
        skip(
            q[Cannot find required .torrent files],
            (      $test_builder->{q[Expected_Tests]}
                 - $test_builder->{q[Curr_Test]}
                )    # XXX - This should only skip this section
        ) if !-f $single_dot_torrent or !-f $multi_dot_torrent;
        ok(-f $single_dot_torrent and -f $multi_dot_torrent,
            q[Required .torrent files were found]);

        #
        diag(q[Net::BitTorrent::Session->new() requires parameters...]);
        is(eval { Net::BitTorrent::Session->new() },
            undef, q[Net::BitTorrent::Session->new( )]);
        is( eval { Net::BitTorrent::Session->new(Path => q[blah.torrent]) },
            undef,
            q[Net::BitTorrent::Session->new(Path => q[blah.torrent])]
        );
        is(eval { Net::BitTorrent::Session->new({}) },
            undef, q[Net::BitTorrent::Session->new({ })]);
        is( eval { Net::BitTorrent::Session->new({Path => q[blah.torrent]}) },
            undef,
            q[Net::BitTorrent::Session->new({Path => q[blah.torrent]})]
        );
        is( eval {
                Net::BitTorrent::Session->new({Path => $multi_dot_torrent});
            },
            undef,
            q[Net::BitTorrent::Session->new({Path => q[./t/900_[...]multi.torrent]})]
        );
        is( eval {
                Net::BitTorrent::Session->new({Path   => $multi_dot_torrent,
                                               Client => 0
                                              }
                );
            },
            undef,
            q[Net::BitTorrent::Session->new({Path => q[./t/900_[...]multi.torrent],Client=> 0})]
        );
        is( eval {
                Net::BitTorrent::Session->new({Path   => $multi_dot_torrent,
                                               Client => bless {},
                                               q[Not::A::BitTorrent]
                                              }
                );
            },
            undef,
            q[Net::BitTorrent::Session->new({Path => q[./t/900_[...]multi.torrent],Client=> bless {}, q[Not::BitTorrent]})]
        );

        #
        #
        my $single_file_torrent =
            Net::BitTorrent::Session->new({Path   => $single_dot_torrent,
                                           Client => bless {},
                                           q[Net::BitTorrent]
                                          }
            );
        isa_ok(
            $single_file_torrent,
            q[Net::BitTorrent::Session],
            q[Net::BitTorrent::Session->new({Path => q[./t/900_[...]single.torrent],Client=> bless {}, q[Not::BetTorrent]})]
        );
        diag(q[Check all sorts of file/hash related stuff...]);
        is( $single_file_torrent->infohash,
            q[98be9ffbf2876fee212607faefd91705fc047581],
            q[Infohash was generated correctly.]
        );

        #
        my $multi_file_torrent =
            Net::BitTorrent::Session->new(
                   {Path   => q[./t/900_data/950_torrents/952_multi.torrent],
                    Client => bless {},
                    q[Net::BitTorrent]
                   }
            );
        isa_ok(
            $multi_file_torrent,
            q[Net::BitTorrent::Session],
            q[Net::BitTorrent::Session->new({Path => q[./t/900_[...]multi.torrent],Client=> bless {}, q[Not::BetTorrent]})]
        );
        diag(q[Check all sorts of file/hash related stuff...]);
        is($multi_file_torrent->infohash,
            q[46ece60594afc29a0138a255660fe47521bb2966],
            q[Infohash was generated correctly.]);

#is(Net::BitTorrent::Session->new({Path => q[blah.torrent]}),
#    undef,
#    q[Net::BitTorrent::Session->new({Path => q[blah.torrent]})]);
#
#~         is($client_even_sized_list_params, undef,
#~             q[Net::BitTorrent->new(LocalPort => [20502..20505]) returns undef]);
#~         my $client_list_params = Net::BitTorrent->new([20502 .. 20505]);
#~         is($client_list_params, undef,
#~             q[Net::BitTorrent->new([20502..20505]) returns undef]);
#~         my $client_singal_param = Net::BitTorrent->new(q[0.0.0.0:20502]);
#~         is($client_singal_param, undef,
#~             q[Net::BitTorrent->new(q[0.0.0.0:20502]) returns undef]);
#~
#~         #
#~         my $client_default_port = Net::BitTorrent->new({});
#~         isa_ok($client_default_port, q[Net::BitTorrent],
#~                q[Net::BitTorrent->new({ })]);
#~         my $client_single_port = Net::BitTorrent->new({LocalPort => 20502});
#~         isa_ok($client_single_port, q[Net::BitTorrent],
#~                q[Net::BitTorrent->new({LocalPort => 20502})]);
#~         my $client_range_port
#~             = Net::BitTorrent->new({LocalPort => [20502 .. 20505]});
#~         isa_ok($client_range_port, q[Net::BitTorrent],
#~                q[Net::BitTorrent->new({LocalPort => [20502 .. 20505]})]);
#~         my $client_list_port
#~             = Net::BitTorrent->new({LocalPort => [20502, 20505]});
#~         isa_ok($client_list_port, q[Net::BitTorrent],
#~                q[Net::BitTorrent->new({LocalPort => [20502, 20505]})]);
#~
#~         #
#~         my $socket = $client_list_port->_get_socket;
#~         isa_ok($client_list_port->_get_socket, q[GLOB], q[Socket is valid.]);
#~         my ($port, $packed_ip)
#~             = unpack_sockaddr_in(getsockname($client_list_port->_get_socket));
#~         is($port, 20505, q[Correct port was opened (20505).]);
#~
#~         #
#~         like($client_list_port->get_peerid,
#~              qr[^NB\d{3}[CS]-.{8}.{5}$], q[Peer ID conforms to spec.]);
#
    }
    {

        #
        BEGIN {
            use_ok(q[Net::BitTorrent]);
        }

        #
        diag(q[Testing (private) Net::BitTorrent::__build_reserved()]);
        is(Net::BitTorrent::__build_reserved(), qq[\0\0\0\0\0\20\0\0],
            q[Net::BitTorrent::__build_reserved() currently only indicates that we support the ExtProtocol]
        );

        #
        BEGIN {
            use_ok(q[Socket], qw[/pack_sockaddr_in/ /inet_/]);
        }
        diag(q[Testing (private) Net::BitTorrent::__socket_open()]);
        is(eval { Net::BitTorrent::__socket_open() },
            undef, q[Net::BitTorrent::__socket_open() returns undef]);
        is(eval { Net::BitTorrent::__socket_open(2200) },
            undef, q[Net::BitTorrent::__socket_open(2200) returns undef]);
        is(eval { Net::BitTorrent::__socket_open(undef, 3400) },
            undef,
            q[Net::BitTorrent::__socket_open(undef, 3400) returns undef]);
        is(eval { Net::BitTorrent::__socket_open(undef, undef) },
            undef,
            q[Net::BitTorrent::__socket_open(undef, undef) returns undef]);
        is( eval {
                Net::BitTorrent::__socket_open(inet_aton(q[127.0.0.1]),
                                               q[test]);
            },
            undef,
            q[Net::BitTorrent::__socket_open(inet_aton(q[127.0.0.1]), q[test]) returns undef]
        );
        is(eval { Net::BitTorrent::__socket_open({}) },
            undef, q[Net::BitTorrent::__socket_open({}) returns undef]);
        is( eval { Net::BitTorrent::__socket_open(q[127.0.0.1:5500]) },
            undef,
            q[Net::BitTorrent::__socket_open(q[127.0.0.1:5500]) returns undef]
        );

        #
        my $socket_one = Net::BitTorrent::__socket_open(q[127.0.0.1], 5500);
        isa_ok(
            $socket_one,
            q[GLOB],
            q[Net::BitTorrent::__socket_open(q[127.0.0.1], 5500) returns a socket...]
        );
        my ($port_one, $packed_ip_one)
            = unpack_sockaddr_in(getsockname($socket_one));
        is($port_one, 5500,
            q[   ...which would accept connections on port 5500...]);
        is($packed_ip_one, inet_aton(q[127.0.0.1]),
            q[   ...if it were open to the outside world.]);

        #
        my $socket_two = Net::BitTorrent::__socket_open(q[127.0.0.1], 5500);
        is($socket_two, undef,
            q[Retrying Net::BitTorrent::__socket_open(q[127.0.0.1], 5500) returns undef...]
        );
        $socket_two
            = Net::BitTorrent::__socket_open(q[127.0.0.1], 5500, 1, 1);
        isa_ok(
            $socket_two,
            q[GLOB],
            q[   ...unless we ask to reuse the address.  In which case... [Undocumented]]
        );
        my ($port_two, $packed_ip_two)
            = unpack_sockaddr_in(getsockname($socket_two));
        is($port_two, 5500,
            q[   ...we could accept connections on port 5500...]);
        is($packed_ip_two, inet_aton(q[127.0.0.1]),
            q[   ...if we were open to the outside world.]);
        is( eval {
                Net::BitTorrent::__socket_open(q[127.0.0.1], 5500, q[fdsa]);
            },
            undef,
            q[ReuseAddr requires a bool value...]
        );
        is( eval { Net::BitTorrent::__socket_open(q[127.0.0.1], 5500, 100) },
            undef,
            q[   ...take two.]
        );
        is( eval {
                Net::BitTorrent::__socket_open(q[127.0.0.1], 5500, 1,
                                               q[fdsa]);
            },
            undef,
            q[ReusePort requires a bool value... [Disabled]]
        );
        is( eval {
                Net::BitTorrent::__socket_open(q[127.0.0.1], 5500, 1, 100);
            },
            undef,
            q[   ...take two.]
        );
        diag(q[ [Alpha] __socket_open() and new() accept textual]);
        diag(q[         hostnames (localhost, ganchan.somewhere.net, etc.)]);
        diag(q[         which are automatically resolved.]);
        isa_ok(Net::BitTorrent::__socket_open(q[localhost], 5500, 1, 1),
               q[GLOB],
               q[Net::BitTorrent::__socket_open(q[localhost], 5500, 1, 1) [Undocumented]]
        );
    }
    {

        #
        diag(q[Testing Net::BitTorrent->_add_connection()]);

        #
        my $fake_client = Not::BitTorrent->new();
        my $bt_ro       = Not::BitTorrent->new();
        my $bt_rw       = Not::BitTorrent->new();
        my $bt_wo       = Not::BitTorrent->new();
        my $bt_extra    = Not::BitTorrent->new();

        #
        is(eval { $fake_client->_add_connection() },
            undef, q[_add_connection requires parameters]);
        is(eval { $fake_client->_add_connection(undef, undef) },
            undef, q[   Two, actually]);
        is(eval { $fake_client->_add_connection(1, 2) },
            undef, q[   Two, actually (take two)]);
        is(eval { $fake_client->_add_connection(undef, 2) },
            undef, q[   ...first a socket containing object]);
        is( eval { $fake_client->_add_connection(Not::BitTorrent->new(), 2) },
            undef,
            q[   ...first a socket containing object (take two)]
        );
        is( eval { $fake_client->_add_connection(Not::BitTorrent->new(), 2) },
            undef,
            q[   ...first a socket]
        );
        is( eval {
                $fake_client->_add_connection(Not::BitTorrent->new(), undef);
            },
            undef,
            q[   ...a mode]
        );
        is( eval {
                $fake_client->_add_connection(Not::BitTorrent->new(), q[ddd]);
            },
            undef,
            q[   ...a mode (take two: 'ddd')]
        );
        is( eval {
                $fake_client->_add_connection(Not::BitTorrent->new(),
                                              q[road]);
            },
            undef,
            q[   ...a mode (take three: 'road')]
        );
        is( eval {
                $fake_client->_add_connection(Not::BitTorrent->new(),
                                              q[read]);
            },
            undef,
            q[   ...a mode (take four: 'read')]
        );
        is( eval {
                $fake_client->_add_connection(Not::BitTorrent->new(),
                                              q[write]);
            },
            undef,
            q[   ...a mode (take five: 'write')]
        );
        ok($fake_client->_add_connection($bt_rw, q[rw]),
            q[   ...a mode (take six: 'rw')]);
        ok($fake_client->_add_connection($bt_ro, q[ro]),
            q[   ...a mode (take seven: 'ro')]);
        ok($fake_client->_add_connection($bt_wo, q[wo]),
            q[   ...a mode (take eight: 'wo')]);

        #
        is(eval { $fake_client->_add_connection($bt_wo, q[wo]) },
            undef, q[BTW, we can only add a socket once]);

        #
        diag(q[TODO: Check list of _sockets()]);
        diag(q[Note: In reality, $client->_connections() would contain]);
        diag(q[  a weak ref to $client itself; but this is a fake client.]);

        #use Data::Dump qw[pp];
        #warn pp $fake_client->_connections;
        is(scalar(keys %{$fake_client->_connections}),
            3, q[Check list of _connections() == 3]);

        #
        diag(q[Testing Net::BitTorrent->_del_connection()]);
        is(eval { $fake_client->_del_connection() },
            undef, q[_del_connection requires one parameter:]);
        is(eval { $fake_client->_del_connection(0) }, undef, q[   a socket.]);
        ok($fake_client->_del_connection($bt_ro),
            q[Read only socket removed]);
        ok($fake_client->_del_connection($bt_rw),
            q[Read-write socket removed]);
        ok($fake_client->_del_connection($bt_wo),
            q[Write-only socket removed]);
        is($fake_client->_del_connection($bt_extra),
            undef, q[We can only remove sockets we've added]);

        # In reality, $fake_client->_connections() would contain a weak ref to
        # $fake_client itself... but this is a fake client.
        diag(q[Checking removal of all sockets...]);
        is_deeply($fake_client->_connections, {},
                  q[_sockets() returned an empty hashref]);

        #
        diag(q[Testing Net::BitTorrent->do_one_loop()]);
        ok($fake_client->do_one_loop(),
            q[   Net::BitTorrent->do_one_loop() accepts an optional timeout parameter...]
        );
        ok($fake_client->do_one_loop(1),
            q[   Timeout, if defined, must be an integer...]);
        ok($fake_client->do_one_loop(1.25), q[   ...or a float...]);
        is($fake_client->do_one_loop(q[test]),
            undef, q[   ...but not random junk.]);
        is($fake_client->do_one_loop(-3),
            undef, q[   ...or negative numbers.]);

        #
        diag(
            q[Reloading the sockets to test select() (We don't actually use these)]
        );
        ok($fake_client->_add_connection($bt_rw, q[rw]),
            q[   RW socket added)]);
        ok($fake_client->_add_connection($bt_ro, q[ro]),
            q[   RO socket added)]);
        ok($fake_client->_add_connection($bt_wo, q[wo]),
            q[   WO socket added]);

        #
        diag(  q[This next bit (tries) to create a server, client, and ]
             . q[the accepted loopback...]);
        diag(q[Think happy thoughts.]);
    }
    {    #
        diag(q[Testing Net::BitTorrent->new()]);
        my $client_no_params = Net::BitTorrent->new();
        isa_ok($client_no_params, q[Net::BitTorrent],
               q[Net::BitTorrent->new( )]);

        #
        is( eval { Net::BitTorrent->new(LocalPort => [20502 .. 20505]) },
            undef,
            q[Net::BitTorrent->new(LocalPort => [20502..20505]) returns undef]
        );
        is(eval { Net::BitTorrent->new([20502 .. 20505]) },
            undef, q[Net::BitTorrent->new([20502..20505]) returns undef]);
        is(eval { Net::BitTorrent->new(q[0.0.0.0:20502]) },
            undef, q[Net::BitTorrent->new(q[0.0.0.0:20502]) returns undef]);

        #
        isa_ok(Net::BitTorrent->new({}),
               q[Net::BitTorrent], q[Net::BitTorrent->new({ })]);
        isa_ok(Net::BitTorrent->new({LocalPort => 20502}),
               q[Net::BitTorrent],
               q[Net::BitTorrent->new({LocalPort => 20502})]
        );

        # Uses 20502 so $client_list_port is forced to use 20505
        my $client_range_port
            = Net::BitTorrent->new({LocalPort => [20502 .. 20505]});
        isa_ok($client_range_port, q[Net::BitTorrent],
               q[Net::BitTorrent->new({LocalPort => [20502 .. 20505]})]);
        my $client_list_port
            = Net::BitTorrent->new({LocalPort => [20502, 20505]});
        isa_ok($client_list_port, q[Net::BitTorrent],
               q[Net::BitTorrent->new({LocalPort => [20502, 20505]})]);

        #
        my $socket = $client_list_port->_socket;
        isa_ok($client_list_port->_socket, q[GLOB], q[Socket is valid.]);
        my ($port, $packed_ip)
            = unpack_sockaddr_in(getsockname($client_list_port->_socket));
        is($port, 20505, q[Correct port was opened (20505).]);

        #
        like($client_list_port->peerid, qr[^NB\d{3}[CS]-.{8}.{5}$],
             q[Peer ID conforms to spec.]);
    }
    {
        diag(q[Testing Net::BitTorrent->add_session()]);
        my $client = Net::BitTorrent->new();
        is( eval {
                $client->add_session(
                              q[./t/900_data/950_torrents/952_multi.torrent]);
            },
            undef,
            q[Needs hash ref params]
        );
        my $session = $client->add_session(
                    {Path => q[./t/900_data/950_torrents/952_multi.torrent]});
        isa_ok($session, q[Net::BitTorrent::Session], q[Added session]);
        is_deeply($client->sessions,
                  {$$session => $session},
                  q[Net::BitTorrent correctly stores sessions]);
        is( $client->add_session(
                      {Path => q[./t/900_data/950_torrents/952_multi.torrent]}
            ),
            undef,
            q[   ...but only once.]
        );
        is_deeply($client->sessions,
                  {$$session => $session},
                  q[   (Double check that to be sure)]);

        #use Data::Dump qw[pp];
        #warn pp $client->sessions();
        #warn pp $client->_connections;
    }

    #
    1;
}
{
    {

        package Not::BitTorrent::Peer;
        use strict;
        use warnings;
        our @ISA;
        BEGIN { @ISA = qw[Net::BitTorrent::Peer]; }
        my $client;

        sub new {
            $client = Not::BitTorrent->new();
            return bless \{}, q[Not::BitTorrent::Peer];
        }
        sub _client  { return $client }
        sub session  { return Not::BitTorrent::Session->new }
        sub bitfield { return pack q[B*], q[11001100110011001100] }

        sub _socket {
            use Data::Dump qw[pp];
            my ($self) = @_;
            $$self->{q[socket]}
                ||= Net::BitTorrent::__socket_open(q[127.0.0.1], 0);
            return $$self->{q[socket]};
        }

        sub _rw {
            my ($self, $r, $w, $e) = @_;
            $self->{q[times]}
                = defind $self->{q[times]}
                ? $self->{q[times]}
                : [qw[1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th]];
            Test::More::ok($w,
                           sprintf(q[   write callback called (%s time)],
                                   shift @{$self->{q[times]}})
            );
            return (0, 0);
        }
    }
    {

        package Not::BitTorrent::Session;
        use strict;
        use warnings;
        our @ISA;
        BEGIN { @ISA = qw[Net::BitTorrent::Session]; }
        my $client;

        sub new {
            $client = Not::BitTorrent->new();
            return bless \{}, q[Not::BitTorrent::Session];
        }
        sub _client      { return $client }
        sub _piece_count { return 42 }
        sub infohash     { return q[ABCDEF0123] x 4 }
        sub bitfield     { return pack q[B*], q[11001100110011001100] }
    }
    {

        package Not::BitTorrent;
        use strict;
        use warnings;
        use Socket;
        our @ISA;
        BEGIN { @ISA = qw[Net::BitTorrent]; }
        sub new { return bless \{}, q[Not::BitTorrent]; }
        sub schedule { Test::More::diag(q[Fake N::B object works]); }
        sub peerid   { return q[This is only a test.]; }

        sub _socket {
            use Data::Dump qw[pp];
            my ($self) = @_;
            $$self->{q[socket]}
                ||= Net::BitTorrent::__socket_open(q[127.0.0.1], 0);
            return $$self->{q[socket]};
        }

        sub _rw {
            my ($self, $r, $w, $e) = @_;
            Test::More::ok($r,
                 q[   accept callback called (This should only happen once)]);
            Test::More::ok(accept(my ($socket_accepted), $self->_socket),
                           q[accept()'d connection]);
            my @times = qw[1st 2nd 3rd 4th 5th 6th 7th 8th 9th 10th 11th];
            die;
            Test::More::ok($self->_add_connection($socket_accepted, q[rw]),
                           q[added new connection to fake client]);
        }
    }
}
