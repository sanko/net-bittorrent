# -*- perl -*-

# t/003_NBU_parse_packet.t - If these fail, _nothing_ will work.
# $Id$

use strict;
use warnings;

use Test::More tests => 15;

use lib q[../lib];

BEGIN {
    use_ok(q[Net::BitTorrent::Session::Peer]);
}

{

    package Fake::BitTorrent;    # Real N::B will try to open a port
    sub new { return bless [q[Fake Net::BitTorrent object!]]; }
    sub callback_log { diag($_[1]->{q[message]}); }
    sub _do_callback { }
}

#my $client = new Fake::BitTorrent;

#my $peer=new Net::BitTorrent::Peer({client=>$client});

SKIP: {
    skip q[Internal changes not yet reflected in tests], 14;

    is_deeply(    # this is a malformed choke message, btw
        [_parse_packet(qq[\0\0\3\xE8\0\0\0\3\xE8\0])],
        [{}, qq[\0\0\3\xE8\0\0\0\3\xE8\0]],
        q[handle incomplete packet]
    );

    is_deeply(    # malformed interested packet
        [_parse_packet(qq[\0\0\0\5\3\0\0\0\5\3])],
        [   {   data   => {},
                error  => q[Incorrect packet length for not interested],
                length => 5,
                type   => 3,
            },
            qq[\3],
        ],
        q[handle malformed packet]
    );

    is_deeply(
        [   _parse_packet(
                pack(q[c/a a8 a20 a20],
                    q[BitTorrent protocol],
                    q[0] x 8,
                    pack(q[H*], q[TESTING! TESTING! TESTING!]),
                    q[This is a test. Blah, blah, blah]) x 2
            )
        ],
        [   {   data => {
                    info_hash => q[decd27010decd27010decd270100000000000000],
                    parsed_reserved => {
                        BitCometext => 0,
                        dht         => 0,
                        extprotocol => 1,
                        fastpeers   => 0
                    },
                    peer_id  => q[This is a test. Blah],
                    protocol => q[BitTorrent protocol],
                    reserved => q[00000000]
                },
                error  => undef,
                length => 68,
                type   => undef
            },
            qq[\23BitTorrent protocol00000000\xDE\xCD'\1\r\xEC\xD2p\20\xDE\xCD']
              . qq[\1\0\0\0\0\0\0\0This is a test. Blah]
        ],
        q[parse handshake packet]
    );

    is_deeply(
        [_parse_packet(qq[\0\0\0\0\1])],
        [{data => {}, error => undef, length => 0, type => -1}, qq[\1]],
        q[parse keepalive packet]
    );

    is_deeply(
        [_parse_packet(qq[\0\0\0\1\0\0\0\0\1\0])],
        [{data => {}, error => undef, length => 1, type => 0}, qq[\0\0\0\1\0]],
        q[parse choke packet]
    );

    is_deeply(
        [_parse_packet(qq[\0\0\0\1\1\0\0\0\1\1])],
        [{data => {}, error => undef, length => 1, type => 1}, qq[\0\0\0\1\1]],
        q[parse unchoke packet]
    );

    is_deeply(
        [_parse_packet(qq[\0\0\0\1\2\0\0\0\1\2])],
        [{data => {}, error => undef, length => 1, type => 2}, qq[\0\0\0\1\2]],
        q[parse interested packet]
    );

    is_deeply(
        [_parse_packet(qq[\0\0\0\1\3\0\0\0\1\3])],
        [{data => {}, error => undef, length => 1, type => 3}, qq[\0\0\0\1\3]],
        q[parse uninterested packet]
    );

    is_deeply(
        [_parse_packet(qq[\0\0\0\5\4\0\0\2{\0\0\0\5\4\0\0\2{])],
        [   {   data   => {index => 635},
                error  => undef,
                length => 5,
                type   => 4
            },
            qq[\0\0\0\5\4\0\0\2{]
        ],
        q[parse have packet]
    );

    is_deeply(
        [   _parse_packet(
                pack(q[H*],
                        q[0000001405ffffffffffffffffffffff]
                      . q[fffffffffffffffc0000001405ffffff]
                      . q[fffffffffffffffffffffffffffffffc])
            )
        ],
        [   {   data => {
                    bitfield =>
                      pack("H*", "fffffffffffffffffffffffffffffffffffffc")
                },
                error    => undef,
                "length" => 20,
                type     => 5,
            },
            pack("H*", "0000001405fffffffffffffffffffffffffffffffffffffc"),

        ],
        q[parse bitfield packet]
    );

    is_deeply(
        [   _parse_packet(
                pack(q[H*], q[0000000d060000001e00000400000024f0] x 2)
            )
        ],
        [   {   data   => {index => 30, length => 9456, offset => 1024},
                error  => undef,
                length => 13,
                type   => 6,
            },
            qq[\0\0\0\r\6\0\0\0\36\0\0\4\0\0\0\$\xF0]
        ],
        q[parse request packet]
    );

    is_deeply(
        [   _parse_packet(
                pack(
                    q[NCa*], 26, 7,
                    pack(q[H*],
                            q[0000001c0000001e0000000d0]
                          . q[60000001e00000400000024f0])
                  ) x 2
            )
        ],
        [   {   data => {
                    block  => qq[\0\0\0\r\6\0\0\0\36\0\0\4\0\0\0\$\xF0],
                    index  => 28,
                    length => 17,
                    offset => 30,
                },
                error  => undef,
                length => 26,
                type   => 7,
            },
            pack(q[H*],
                    q[0000001a070000001c0000001e0000]
                  . q[000d060000001e00000400000024f0])
        ],
        q[parse piece packet]
    );

    is_deeply(
        [   _parse_packet(
                pack(q[H*], q[0000000d060000001e00000400000024f0] x 2)
            )
        ],
        [   {   data   => {index => 30, length => 9456, offset => 1024},
                error  => undef,
                length => 13,
                type   => 6,
            },
            qq[\0\0\0\r\6\0\0\0\36\0\0\4\0\0\0\$\xF0]
        ],
        q[parse request packet]
    );
    is_deeply(
        [   _parse_packet(
                pack(q[H*], q[0000000d080000001e00000400000024f0] x 2)
            )
        ],
        [   {   data   => {index => 30, length => 9456, offset => 1024},
                error  => undef,
                length => 13,
                type   => 8,
            },
            qq[\0\0\0\r\b\0\0\0\36\0\0\4\0\0\0\$\xF0]
        ],
        q[parse cancel packet]
    );
  TODO: {
        local $TODO = q[_parse_packet is incomplete.];

        #09: $__MSG_PORT  #10: $__MSG_INFO_PAYLOAD  #20: $__MSG_EXTENDED
    }
}
1;
