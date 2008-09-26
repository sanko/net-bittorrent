#!/usr/bin/perl -w
use strict;
use warnings;
use Module::Build;
#
use lib q[../../../../lib];
$|++;

# let's keep track of where we are...
my $test_builder = Test::More->builder;

#
my $simple_dot_torrent = q[./t/900_data/950_torrents/953_miniswarm.torrent];

# Make sure the path is correct
chdir q[../../../../] if not -f $simple_dot_torrent;
#

my $build = Module::Build->current;
my $can_talk_to_ourself = $build->notes(q[can_talk_to_ourself]);

#
$|++;

#
BEGIN {
    use Test::More;
    plan tests => 267;
    $SIG{__WARN__} = sub { diag shift };    # Quiet Carp
    use_ok(q[Net::BitTorrent::Protocol], qw[:all]);
}
{                                           #
    diag(q[ Message types...]);
    is(HANDSHAKE,      -1,  q[Handshake]);
    is(KEEPALIVE,      q[], q[Keepalive]);
    is(CHOKE,          0,   q[Choke]);
    is(UNCHOKE,        1,   q[Unchoke]);
    is(INTERESTED,     2,   q[Interested]);
    is(NOT_INTERESTED, 3,   q[Not Interested]);
    is(HAVE,           4,   q[Have]);
    is(BITFIELD,       5,   q[Bitfield]);
    is(REQUEST,        6,   q[Request]);
    is(PIECE,          7,   q[Piece]);
    is(CANCEL,         8,   q[Cancel]);
    is(PORT,           9,   q[Port]);
    is(SUGGEST,        13,  q[Suggest]);
    is(HAVE_ALL,       14,  q[Have All]);
    is(HAVE_NONE,      15,  q[Have None]);
    is(REJECT,         16,  q[Reject]);
    is(ALLOWED_FAST,   17,  q[Allowed Fast]);
    is(EXTPROTOCOL,    20,  q[Extended]);

    #
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
        qq[\23BitTorrent protocol\0\0\0\0\0\0\0\0\1#Eg\x89]
            . qq[\1#Eg\x89\1#Eg\x89\1#Eg\x89random peer id here!],
        q[   ...and an infohash.]
    );
    is( build_handshake(chr(0) x 8, q[A] x 20, q[B] x 20),
        qq[\23BitTorrent protocol\0\0\0\0\0\0\0\0]
            . q[AAAAAAAAAAAAAAAAAAAABBBBBBBBBBBBBBBBBBBB],
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
    is(build_have(0), qq[\0\0\0\5\4\0\0\0\0], q[   ...a number]);
    is(build_have(999999999999999),
        qq[\0\0\0\5\4\xFF\xFF\xFF\xFF],
        q[   ...even a large one is okay]);
    diag(
        q[     (A quadrillion piece torrent? Ha! The .torrent itself would be several GBs)]
    );
    is(build_have(-5), undef, q[   ...as long as it's positive]);

    #
    diag(q[ [...]::build_bitfield]);
    is(build_bitfield(),    undef, q[   ...requires a single param]);
    is(build_bitfield(q[]), undef, q[   ...a packed bitfield]);
    is(build_bitfield(q[abcdefg]),
        qq[\0\0\0\b\5abcdefg], q[   ...but what _doesn't_ unpack to binary?]);
    my $tmp = join((join time, keys(%ENV)), %INC);    # fairly random string
    is(build_bitfield($tmp),
        pack(q[N], length($tmp) + 1) . chr(5) . $tmp,
        q[   ...more testing]);

    #
    diag(q[ [...]::build_request]);
    is(build_request(undef, 2,     3),     undef, q[   ...requires an index]);
    is(build_request(1,     undef, 3),     undef, q[   ...an offset]);
    is(build_request(1,     2,     undef), undef, q[   ...and a length.]);
    is(build_request(q[],   q[],   q[]),
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
    is(build_piece(undef, 2,     3),       undef, q[   ...requires an index]);
    is(build_piece(1,     undef, q[test]), undef, q[   ...an offset]);
    is(build_piece(1,     2,     undef),   undef, q[   ...and data]);
    is(build_piece(q[],   q[],   q[]),     undef, q[   validation (A)]);
    is(build_piece(-1,    q[],   q[]),     undef, q[   validation (B)]);
    is(build_piece(1,     q[],   q[]),     undef, q[   validation (C)]);
    is(build_piece(1,     -2,    q[]),     undef, q[   validation (D)]);
    is(build_piece(1,     2,     \q[XXX]),
        qq[\0\0\0\f\a\0\0\0\1\0\0\0\2XXX],
        q[   validation (E)]);
    is(build_piece(1, 2, \$tmp),
        (pack(q[NcN2a*], length($tmp) + 9, 7, 1, 2, $tmp)),
        q[   validation (F)]);

    #
    diag(q[ [...]::build_cancel]);
    is(build_cancel(undef, 2,     3),     undef, q[   ...requires an index]);
    is(build_cancel(1,     undef, 3),     undef, q[   ...an offset]);
    is(build_cancel(1,     2,     undef), undef, q[   ...and a length.]);
    is(build_cancel(q[],   q[],   q[]),
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
    is(build_port(),        undef, q[   Requires a port number]);
    is(build_port(-5),      undef, q[   ...and ports are always positive]);
    is(build_port(3.3),     undef, q[   ...integers]);
    is(build_port(q[test]), undef, q[   Validation (A)]);
    is(build_port(8555),   qq[\0\0\0\5\t\0\0!k],    q[   Validation (B)]);
    is(build_port(652145), qq[\0\0\0\a\t\0\t\xF3q], q[   Validation (C)]);

    #
    diag(q[ [...]::build_allowed_fast]);
    is(build_allowed_fast(),    undef, q[   Requires a piece index]);
    is(build_allowed_fast(-5),  undef, q[   ...which is always a positive]);
    is(build_allowed_fast(3.3), undef, q[   ...integer]);
    is(build_allowed_fast(q[test]), undef, q[   Validation (A)]);
    is(build_allowed_fast(8555), qq[\0\0\0\5\21\0\0!k], q[   Validation (B)]);
    is(build_allowed_fast(652145),
        qq[\0\0\0\5\21\0\t\xF3q], q[   Validation (C)]);
    is(build_allowed_fast(0), qq[\0\0\0\5\21\0\0\0\0], q[   Validation (D)]);
    diag(q[ [...]::build_reject]);
    is(build_reject(undef, 2,     3),     undef, q[   ...requires an index]);
    is(build_reject(1,     undef, 3),     undef, q[   ...an offset]);
    is(build_reject(1,     2,     undef), undef, q[   ...and a length.]);
    is(build_reject(q[],   q[],   q[]),
        undef, q[   They should all be positive numbers (A)]);
    is(build_reject(-1, q[], q[]),
        undef, q[   They should all be positive numbers (B)]);
    is(build_reject(1, q[], q[]),
        undef, q[   They should all be positive numbers (C)]);
    is(build_reject(1, -2, q[]),
        undef, q[   They should all be positive numbers (D)]);
    is(build_reject(1, 2, q[]),
        undef, q[   They should all be positive numbers (E)]);
    is(build_reject(1, 2, -3),
        undef, q[   They should all be positive numbers (F)]);
    is(build_reject(1, 2, 3),
        qq[\0\0\0\r\20\0\0\0\1\0\0\0\2\0\0\0\3],
        q[   They should all be positive numbers (G)]);
    is( build_reject(999999999999999, 999999999999999, 999999999999999),
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
    diag(q[ [...]::build_suggest]);
    is(build_suggest(),        undef, q[   Requires a piece index]);
    is(build_suggest(-5),      undef, q[   ...which is always a positive]);
    is(build_suggest(3.3),     undef, q[   ...integer]);
    is(build_suggest(q[test]), undef, q[   Validation (A)]);
    is(build_suggest(8555),   qq[\0\0\0\5\r\0\0!k],    q[   Validation (B)]);
    is(build_suggest(652145), qq[\0\0\0\5\r\0\t\xF3q], q[   Validation (C)]);
    is(build_suggest(0),      qq[\0\0\0\5\r\0\0\0\0],  q[   Validation (D)]);

    #
    diag(q[TODO: build_extended]);
    is(build_extended(), undef,
        q[   ...requires a message id and a playload]);
    is(build_extended(undef, {}), undef, q[   ...validation (A)]);
    is(build_extended(-1,    {}), undef, q[   ...validation (B)]);
    is(build_extended(q[],   {}), undef, q[   ...validation (C)]);
    is(build_extended(0, undef), undef, q[   ...validation (D)]);
    is(build_extended(0, 2),     undef, q[   ...validation (E)]);
    is(build_extended(0, -2),    undef, q[   ...validation (F)]);
    is(build_extended(0, q[]),   undef, q[   ...validation (G)]);
    is(build_extended(0, {}), qq[\0\0\0\4\24\0de], q[   ...validation (H)]);
    is( build_extended(
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
    diag(q[ [...]:: _parse_handshake]);
    is(_parse_handshake(),          undef, q[Undef]);
    is(_parse_handshake(q[]),       undef, q[Empty]);
    is(_parse_handshake(q[Hahaha]), undef, q[Not enough data]);
    is( _parse_handshake(      qq[\23NotTorrent protocol\0\0\0\0\0\0\0\0]
                             . q[AAAAAAAAAAAAAAAAAAAABBBBBBBBBBBBBBBBBBBB]
        ),
        undef,
        q[Bad protocol name]
    );
    is_deeply(_parse_handshake(
                                   qq[\23BitTorrent protocol\0\0\0\0\0\0\0\0]
                                 . q[AAAAAAAAAAAAAAAAAAAABBBBBBBBBBBBBBBBBBBB]
              ),
              [qq[\0] x 8, q[A] x 20, q[B] x 20],
              q[Correct handshake]
    );

    #
    diag(q[ [...]:: _parse_keepalive]);
    is(_parse_keepalive(), undef, q[  ...has no payload and nothing to test]);

    #
    diag(q[ [...]:: _parse_choke]);
    is(_parse_choke(), undef, q[  ...has no payload and nothing to test]);

    #
    diag(q[ [...]:: _parse_unchoke]);
    is(_parse_unchoke(), undef, q[  ...has no payload and nothing to test]);

    #
    diag(q[ [...]:: _parse_interested]);
    is(_parse_interested(), undef,
        q[  ...has no payload and nothing to test]);

    #
    diag(q[ [...]:: _parse_not_interested]);
    is(_parse_not_interested(), undef,
        q[  ...has no payload and nothing to test]);

    #
    diag(q[ [...]:: _parse_have]);
    is(_parse_have(),             undef,     q[Undef]);
    is(_parse_have(q[]),          undef,     q[Empty]);
    is(_parse_have(qq[\0\0\0d]),  100,       q[ ...100]);
    is(_parse_have(qq[\0\0\0\0]), 0,         q[ ...0]);
    is(_parse_have(qq[\0\0\4\0]), 1024,      q[ ...1024]);
    is(_parse_have(qq[\f\f\f\f]), 202116108, q[ ...202116108]);

    #
    diag(q[ [...]:: _parse_bitfield]);
    is(_parse_bitfield(),    undef, q[Undef]);
    is(_parse_bitfield(q[]), undef, q[Empty]);
    is(_parse_bitfield(pack q[B*], q[1110010100010]),
        qq[\xA7\b], q[ ...1110010100010]);
    is(_parse_bitfield(pack q[B*], q[00]),    qq[\0],  q[ ..00]);
    is(_parse_bitfield(pack q[B*], q[00001]), qq[\20], q[ ...00001]);
    is(_parse_bitfield(pack q[B*], q[1111111111111]),
        qq[\xFF\37], q[ ...1111111111111]);

    #
    diag(q[ [...]:: _parse_request]);
    is(_parse_request(),    undef, q[Undef]);
    is(_parse_request(q[]), undef, q[Empty]);
    is_deeply(_parse_request(qq[\0\0\0\0\0\0\0\0\0\0\0\0]),
              [0, 0, 0],
              q[ ...i:0 o:0 l:0]);
    is_deeply(_parse_request(qq[\0\0\0\0\0\0\0\0\0\2\0\0]),
              [0, 0, 2**17],
              q[ ...i:0 o:0 l:2**17]);
    is_deeply(_parse_request(qq[\0\0\0d\0\0\@\0\0\2\0\0]),
              [100, 2**14, 2**17],
              q[ ...i:100 o:2**14 l:2**17]);
    is_deeply(_parse_request(qq[\0\20\0\0\0\0\@\0\0\2\0\0]),
              [2**20, 2**14, 2**17],
              q[ ...i:2**20 o:2**14 l:2**17]);

    #
    diag(q[ [...]:: _parse_piece]);
    is(_parse_piece(),    undef, q[Undef]);
    is(_parse_piece(q[]), undef, q[Empty]);
    is_deeply(_parse_piece(qq[\0\0\0\0\0\0\0\0TEST]),
              [0, 0, q[TEST]],
              q[ ...i:0 o:0 d:'TEST']);
    is_deeply(_parse_piece(qq[\0\0\0d\0\0\@\0TEST]),
              [100, 2**14, q[TEST]],
              q[ ...i:100 o:2**14 d:'TEST']);
    is_deeply(_parse_piece(qq[\0\20\0\0\0\0\@\0TEST]),
              [2**20, 2**14, q[TEST]],
              q[ ...i:2**20 o:2**14 d:'TEST']);
    is_deeply([_parse_piece(qq[\0\20\0\0\0\0\@\0])],
              [], q[ ...i:2**20 o:2**14 d:'TEST']);

    #
    diag(q[ [...]:: _parse_cancel]);
    is(_parse_cancel(),    undef, q[Undef]);
    is(_parse_cancel(q[]), undef, q[Empty]);
    is_deeply(_parse_cancel(qq[\0\0\0\0\0\0\0\0\0\0\0\0]),
              [0, 0, 0],
              q[ ...i:0 o:0 l:0]);
    is_deeply(_parse_cancel(qq[\0\0\0\0\0\0\0\0\0\2\0\0]),
              [0, 0, 2**17],
              q[ ...i:0 o:0 l:2**17]);
    is_deeply(_parse_cancel(qq[\0\0\0d\0\0\@\0\0\2\0\0]),
              [100, 2**14, 2**17],
              q[ ...i:100 o:2**14 l:2**17]);
    is_deeply(_parse_cancel(qq[\0\20\0\0\0\0\@\0\0\2\0\0]),
              [2**20, 2**14, 2**17],
              q[ ...i:2**20 o:2**14 l:2**17]);

    #
    diag(q[ [...]:: _parse_port]);
    is(_parse_port(),             undef,     q[Undef]);
    is(_parse_port(q[]),          undef,     q[Empty]);
    is(_parse_port(qq[\0\0\0d]),  100,       q[ ...100]);
    is(_parse_port(qq[\0\0\0\0]), 0,         q[ ...0]);
    is(_parse_port(qq[\0\0\4\0]), 1024,      q[ ...1024]);
    is(_parse_port(qq[\f\f\f\f]), 202116108, q[ ...202116108]);

    #
    diag(q[ [...]:: _parse_suggest]);
    is(_parse_suggest(),             undef,     q[Undef]);
    is(_parse_suggest(q[]),          undef,     q[Empty]);
    is(_parse_suggest(qq[\0\0\0d]),  100,       q[ ...100]);
    is(_parse_suggest(qq[\0\0\0\0]), 0,         q[ ...0]);
    is(_parse_suggest(qq[\0\0\4\0]), 1024,      q[ ...1024]);
    is(_parse_suggest(qq[\f\f\f\f]), 202116108, q[ ...202116108]);

    #
    diag(q[ [...]:: _parse_have_all]);
    is(_parse_have_all(), undef, q[  ...has no payload and nothing to test]);

    #
    diag(q[ [...]:: _parse_have_none]);
    is(_parse_have_none(), undef, q[  ...has no payload and nothing to test]);

    #
    diag(q[ [...]:: _parse_reject]);
    is(_parse_reject(),    undef, q[Undef]);
    is(_parse_reject(q[]), undef, q[Empty]);
    is_deeply(_parse_reject(qq[\0\0\0\0\0\0\0\0\0\0\0\0]),
              [0, 0, 0],
              q[ ...i:0 o:0 l:0]);
    is_deeply(_parse_reject(qq[\0\0\0\0\0\0\0\0\0\2\0\0]),
              [0, 0, 2**17],
              q[ ...i:0 o:0 l:2**17]);
    is_deeply(_parse_reject(qq[\0\0\0d\0\0\@\0\0\2\0\0]),
              [100, 2**14, 2**17],
              q[ ...i:100 o:2**14 l:2**17]);
    is_deeply(_parse_reject(qq[\0\20\0\0\0\0\@\0\0\2\0\0]),
              [2**20, 2**14, 2**17],
              q[ ...i:2**20 o:2**14 l:2**17]);

    #
    diag(q[ [...]:: _parse_allowed_fast]);
    is(_parse_allowed_fast(),             undef,     q[Undef]);
    is(_parse_allowed_fast(q[]),          undef,     q[Empty]);
    is(_parse_allowed_fast(qq[\0\0\0d]),  100,       q[ ...100]);
    is(_parse_allowed_fast(qq[\0\0\0\0]), 0,         q[ ...0]);
    is(_parse_allowed_fast(qq[\0\0\4\0]), 1024,      q[ ...1024]);
    is(_parse_allowed_fast(qq[\f\f\f\f]), 202116108, q[ ...202116108]);

    #
    diag(q[ [...]:: _parse_extended]);
    is(_parse_extended(),    undef, q[Undef]);
    is(_parse_extended(q[]), undef, q[Empty]);
    is_deeply(
        _parse_extended(
            qq[\0d1:md6:ut_pexi1e7:\xC2\xB5T_PEXi2ee1:pi30e4:reqqi30e1:v21:Net::BitTorrent r0.306:yourip4:\x7F\0\0\1e]
        ),
        [   0,
            {   m => {ut_pex => 1, q[µT_PEX] => 2},
                ((p => 30)),
                v      => q[Net::BitTorrent r0.30],
                yourip => pack(q[C4], (q[127.0.0.1] =~ m[(\d+)]g)),
                reqq   => 30
            }
        ],
        ,
        q[Extended Protocol...]
    );

    #
    diag(q[  [...]::parse_packet]);
    is(parse_packet(),    undef, q[Undef]);
    is(parse_packet(q[]), undef, q[Empty]);
    is(parse_packet(\{}), undef, q[Hashref]);
    my $packet = q[Testing];
    is(parse_packet(\$packet), undef, q[Random string]);
    $packet = qq[\0\0\0\5\40\0\0\0F];
    is(parse_packet(\$packet), undef, q[Bad packet]);

    #
    diag(q[Here we simulate a 'real' P2P session to check packet parsing]);
    my @original_data = (
        build_handshake(pack(q[C*], split(q[], q[00000000])),
                        pack(q[H*], q[0123456789] x 4),
                        q[random peer id here!]
        ),
        build_bitfield(q[11100010]),
        build_extended(
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
        build_keepalive(),
        build_keepalive(),
        build_keepalive(),
        build_keepalive(),
        build_keepalive(),
        build_interested(),
        build_keepalive(),
        build_not_interested(),
        build_unchoke(),
        build_choke(),
        build_keepalive(),
        build_interested(),
        build_unchoke(),
        build_keepalive(),
        build_have(75),
        build_have(0),
        build_keepalive(),
        build_port(1024),
        build_request(0,     0,     2**15),
        build_request(99999, 2**17, 2**15),
        build_cancel(99999, 2**17, 2**15),
        build_piece(1,     2,  \q[XXX]),
        build_piece(0,     6,  \q[XXX]),
        build_piece(99999, 12, \q[XXX]),
        build_suggest(0),
        build_suggest(2**14),
        build_have_all(),           # this would never come this late...
        build_have_none(),          # this would never come this late...
        build_allowed_fast(0),
        build_allowed_fast(1024),
        build_reject(10),
        build_reject(0,    0,     1024),
        build_reject(1024, 2**18, 2**16),
    );
    my $data = join q[], @original_data;
    is(parse_packet($data), undef, q[Requires a ref]);
    is($data, join(q[], @original_data), q[   ...left data alone.]);

    #
    is_deeply(parse_packet(\$data),
              {Payload => ["\0\0\0\0\0\0\0\0",
                           "\1#Eg\x89\1#Eg\x89\1#Eg\x89\1#Eg\x89",
                           "random peer id here!",
               ],
               Type => HANDSHAKE
              },
              q[Handshake...]
    );
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data),
              {Payload => qq[\x8C\x8C\x8C\f\f\f\x8C\f],
               Type    => BITFIELD
              },
              q[Bitfield...]
    );
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data),
              {Payload => [0,
                           {m => {ut_pex => 1, q[µT_PEX] => 2},
                            ((p => 30)),
                            v => q[Net::BitTorrent r0.30],
                            yourip =>
                                pack(q[C4], (q[127.0.0.1] =~ m[(\d+)]g)),
                            reqq => 30
                           }
               ],
               Type => EXTPROTOCOL
              },
              q[Extended Protocol...]
    );
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data), {Type => KEEPALIVE}, q[Keepalive...]);
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data), {Type => KEEPALIVE}, q[Keepalive...]);
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data), {Type => KEEPALIVE}, q[Keepalive...]);
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data), {Type => KEEPALIVE}, q[Keepalive...]);
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data), {Type => KEEPALIVE}, q[Keepalive...]);
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data), {Type => INTERESTED}, q[Interested...]);
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data), {Type => KEEPALIVE}, q[Keepalive...]);
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data),
              {Type => NOT_INTERESTED},
              q[Not interested...]
    );
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data), {Type => UNCHOKE}, q[Unchoke...]);
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data), {Type => CHOKE}, q[Choke...]);
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data), {Type => KEEPALIVE}, q[Keepalive...]);
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data), {Type => INTERESTED}, q[Interested...]);
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data), {Type => UNCHOKE}, q[Unchoke...]);
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data), {Type => KEEPALIVE}, q[Keepalive...]);
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data), {Payload => 75, Type => HAVE},
              q[Have...]);
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data), {Payload => 0, Type => HAVE}, q[Have...]);
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data), {Type => KEEPALIVE}, q[Keepalive...]);
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data), {Payload => 1024, Type => PORT},
              q[Port...]);
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data),
              {Payload => [0, 0, 2**15], Type => REQUEST},
              q[Request...]);
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data),
              {Payload => [99999, 2**17, 2**15], Type => REQUEST},
              q[Request...]);
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data),
              {Payload => [99999, 2**17, 2**15], Type => CANCEL},
              q[Cancel...]);
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data),
              {Payload => [1, 2, q[XXX]], Type => PIECE}, q[Piece...]);
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data),
              {Payload => [0, 6, q[XXX]], Type => PIECE}, q[Piece...]);
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data),
              {Payload => [99999, 12, q[XXX]], Type => PIECE}, q[Piece...]);
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data), {Payload => 0, Type => SUGGEST},
              q[Suggestion...]);
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data), {Payload => 2**14, Type => SUGGEST},
              q[Suggestion...]);
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data), {Type => HAVE_ALL}, q[Have All...]);
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data), {Type => HAVE_NONE}, q[Have None...]);
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data),
              {Payload => 0, Type => ALLOWED_FAST},
              q[Allowed Fast...]);
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data),
              {Payload => 1024, Type => ALLOWED_FAST},
              q[Allowed Fast...]);
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data), {Payload => [0, 0, 1024], Type => REJECT},
              q[Reject...]);
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);
    is_deeply(parse_packet(\$data),
              {Payload => [1024, 2**18, 2**16], Type => REJECT},
              q[Reject...]);
    shift @original_data;
    is($data, join(q[], @original_data), q[   ...was shifted from data.]);

    #
    is_deeply(\@original_data, [], q[Looks like we're done.]);

    #
    my $blah_1;
    is(parse_packet(\$blah_1), undef, q[Undef]);
    my $blah_2 = q[];
    is(parse_packet(\$blah_2), undef, q[Empty string]);
    my $blah_3 = qq[\0\0\0\r\25\0\0\4\0\0\4\0\0\0\1\0\0];
    is(parse_packet(\$blah_3), undef, q[Bad/unknown packet]);
}
