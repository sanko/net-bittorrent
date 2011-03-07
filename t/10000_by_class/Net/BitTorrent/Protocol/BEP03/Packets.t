package t::Net::BitTorrent::Protocol::BEP03::Packets;
{
    use strict;
    use warnings;

    # Load standard modules
    use Module::Build;
    use Test::More;
    use Test::Fatal;

    # Load local context
    BEGIN { -d '_build' ? last : chdir '..' for 1 .. 10 }
    my $t_builder = Test::More->builder;
    my $m_builder = Module::Build->current;

    # Load locally required modules
    use lib '../../../../../../../../lib', 'lib';
    use Net::BitTorrent::Protocol::BEP03::Packets qw[:all];

    # Tests!
    is $HANDSHAKE,      -1, '$Handshake      == -1 (pseudo-type)';
    is $KEEPALIVE,      '', '$Keepalive      == \'\' (pseudo-type)';
    is $CHOKE,          0,  '$Choke          == 0';
    is $UNCHOKE,        1,  '$Unchoke        == 1';
    is $INTERESTED,     2,  '$Interested     == 2';
    is $NOT_INTERESTED, 3,  '$Not Interested == 3';
    is $HAVE,           4,  '$Have           == 4';
    is $BITFIELD,       5,  '$Bitfield       == 5';
    is $REQUEST,        6,  '$Request        == 6';
    is $PIECE,          7,  '$Piece          == 7';
    is $CANCEL,         8,  '$Cancel         == 8';
    is $PORT,           9,  '$Port           == 9';

    #
    isn't exception { build_handshake() }, undef,
        'build_handshake() is a fatal error';
    isn't exception { build_handshake(undef, undef, undef) },
        undef, 'build_handshake(undef, undef, undef) is a fatal error';
    isn't exception { build_handshake('junk', 'junk', 'junk') },
        undef,
        'build_handshake(\'junk\', \'junk\', \'junk\') is a fatal error';
    isn't exception { build_handshake('junk9565', 'junk', 'junk') },
        undef,
        'build_handshake(\'junk9565\', \'junk\', \'junk\') is a fatal error';
    isn't exception { build_handshake('junk9565', 'junk' x 5, 'junk') },
        undef,
        'build_handshake(\'junk9565\', \'junk\' x 5, \'junk\') is a fatal error';
    isn't exception { build_handshake("\0" x 8, 'junk', 'junk') },
        undef,
        'build_handshake("\0" x 8, \'junk\', \'junk\') is a fatal error';
    isn't exception {
        build_handshake("\0" x 8, '01234567890123456789', 'junk');
    },
        undef,
        'build_handshake("\0" x 8, \'01234567890123456789\', \'junk\') is a fatal error';
    is build_handshake(chr(0) x 8, 'A' x 20, 'B' x 20),
        "\23BitTorrent protocol\0\0\0\0\0\0\0\0"
        . 'AAAAAAAAAAAAAAAAAAAABBBBBBBBBBBBBBBBBBBB',
        'build_handshake(chr(0) x 8, \'A\' x 20, \'B\' x 20) == okay';
    is build_handshake(pack('C*', split('', '00000000')),
                       pack('H*', '0123456789' x 4),
                       'random peer id here!'),
        "\23BitTorrent protocol\0\0\0\0\0\0\0\0\1#Eg\x89"
        . "\1#Eg\x89\1#Eg\x89\1#Eg\x89random peer id here!",
        'build_handshake( [...] )                           == okay';

    #
    is build_keepalive(), "\0\0\0\0",
        'build_keepalive() == "\0\0\0\0" (has no payload)';
    is build_choke(), "\0\0\0\1\0",
        'build_choke() == "\0\0\0\1\0" (has no payload)';
    is build_unchoke(), "\0\0\0\1\1",
        'build_unchoke() == "\0\0\0\1\1" (has no payload)';
    is build_interested(), "\0\0\0\1\2",
        'build_interested() == "\0\0\0\1\2" (has no payload)';
    is build_not_interested(), "\0\0\0\1\3",
        'build_not_interested() == "\0\0\0\1\3" (has no payload)';

    #
    isn't exception { build_have() }, undef,
        'build_have()           is a fatal error';
    isn't exception { build_have('1desfdds') }, undef,
        'build_have(\'1desfdds\') is a fatal error';
    is build_have(9), "\0\0\0\5\4\0\0\0\t",
        'build_have(9)          == "\0\0\0\5\4\0\0\0\t"';
    is build_have(0), "\0\0\0\5\4\0\0\0\0",
        'build_have(0)          == "\0\0\0\5\4\0\0\0\0"';
    is build_have(4294967295), "\0\0\0\5\4\xFF\xFF\xFF\xFF",
        'build_have(4294967295) == "\0\0\0\5\4\xFF\xFF\xFF\xFF" (32bit math limit)';
    isn't exception { build_have(-5) }, undef,
        'build_have(-5)         is a fatal error (negative index == bad index)';

    #
    isn't exception { build_bitfield() }, undef,
        'build_bitfield()           is a fatal error';
    isn't exception { build_bitfield('') }, undef,
        'build_bitfield(\'\')         is a fatal error';
    is build_bitfield('abcdefg'),
        "\0\0\0\b\5abcdefg",
        'build_bitfield(\'abcdefg\') == "\0\0\0\b\5abcdefg"';
    my $tmp = join((join time, keys(%ENV)), %INC);    # fairly random string
    is build_bitfield($tmp),
        pack('N', length($tmp) + 1) . chr(5) . $tmp,
        sprintf
        'build_bitfield($tmp)       == okay (where $tmp =~ qr[^.{%d}$\')]',
        length $tmp;

    #
    isn't exception { build_request() }, undef,
        'build_request()            is a fatal error';
    isn't exception { build_request(undef, 2, 3) },
        undef, 'build_request(undef, 2, 3) is a fatal error';
    isn't exception { build_request(1, undef, 3) },
        undef, 'build_request(1, undef, 3) is a fatal error';
    isn't exception { build_request(1, 2, undef) },
        undef, 'build_request(1, 2, undef) is a fatal error';
    isn't exception { build_request(q[], '', '') },
        undef, 'build_request(\'\', \'\', \'\')  is a fatal error';
    isn't exception { build_request(-1, '', '') },
        undef, 'build_request(-1, \'\', \'\')  is a fatal error';
    isn't exception { build_request(1, '', '') },
        undef, 'build_request(1, \'\', \'\')   is a fatal error';
    isn't exception { build_request(1, -2, '') },
        undef, 'build_request(1, -2, \'\')   is a fatal error';
    isn't exception { build_request(1, 2, '') },
        undef, 'build_request(1, 2, \'\')    is a fatal error';
    isn't exception { build_request(1, 2, -3) }, undef,
        'build_request(1, 2, -3)    is a fatal error';
    is build_request(1, 2, 3),
        "\0\0\0\r\6\0\0\0\1\0\0\0\2\0\0\0\3",
        'build_request(1, 2, 3)     == "\0\0\0\r\6\0\0\0\1\0\0\0\2\0\0\0\3"';
    is build_request(4294967295, 4294967295, 4294967295),
        pack('H*', '0000000d06ffffffffffffffffffffffff'),
        'build_request(4294967295, 4294967295, 4294967295) == pack(\'H*\', \'0000000d06ffffffffffffffffffffffff\')';

    #
    isn't exception { build_piece(undef, 2, 3) },
        undef,
        'build_piece(undef, 2, 3)      is a fatal error (requires an index)';
    isn't exception { build_piece(1, undef, 'test') },
        undef,
        'build_piece(1, undef, \'test\') is a fatal error (requires an offset)';
    isn't exception { build_piece(1, 2, undef) },
        undef,
        'build_piece(1, 2,     undef)  is a fatal error (requires a block of data)';
    isn't exception { build_piece('', '', 0, '') },
        undef, 'build_piece(\'\', \'\', 0, \'\')   is a fatal error';
    isn't exception { build_piece(-1, '', 0, '') },
        undef, 'build_piece(-1, \'\', 0, \'\')   is a fatal error';
    isn't exception { build_piece(1, '', 0, '') }, undef,
        'build_piece( 1, \'\', 0, \'\')   is a fatal error';
    isn't exception { build_piece(1, -2, 0, '') }, undef,
        'build_piece( 1, -2, 0, \'\')   is a fatal error';
    is build_piece(1, 2, 3, \'XXX'),
        "\0\0\0\f\a\0\0\0\1\0\0\0\2XXX",
        'build_piece(1, 2, 3, \\\'XXX\') == "\0\0\0\f\a\0\0\0\1\0\0\0\2XXX"';
    is build_piece(1, 2, length($tmp), \$tmp),
        pack('NcN2a*', length($tmp) + 9, 7, 1, 2, $tmp),
        'build_piece(1, 2, length($tmp), \$tmp)  == okay';

    #
    isn't exception { build_cancel() }, undef,
        'build_cancel()            is a fatal error';
    isn't exception { build_cancel(undef, 2, 3) },
        undef, 'build_cancel(undef, 2, 3) is a fatal error';
    isn't exception { build_cancel(1, undef, 3) },
        undef, 'build_cancel(1, undef, 3) is a fatal error';
    isn't exception { build_cancel(1, 2, undef) },
        undef, 'build_cancel(1, 2, undef) is a fatal error';
    isn't exception { build_cancel('', '', '') },
        undef, 'build_cancel(\'\', \'\', \'\')  is a fatal error';
    isn't exception { build_cancel(-1, '', '') },
        undef, 'build_cancel(-1, \'\', \'\')  is a fatal error';
    isn't exception { build_cancel(1, '', '') },
        undef, 'build_cancel(1, \'\', \'\')   is a fatal error';
    isn't exception { build_cancel(1, -2, '') }, undef,
        'build_cancel(1, -2, \'\')   is a fatal error';
    isn't exception { build_cancel(1, 2, '') }, undef,
        'build_cancel(1, 2, \'\')    is a fatal error';
    isn't exception { build_cancel(1, 2, -3) }, undef,
        'build_cancel(1, 2, -3)    is a fatal error';
    is build_cancel(1, 2, 3), "\0\0\0\r\b\0\0\0\1\0\0\0\2\0\0\0\3",
        'build_cancel(1, 2, 3)     == "\0\0\0\r\b\0\0\0\1\0\0\0\2\0\0\0\3"';
    is build_cancel(4294967295, 4294967295, 4294967295),
        pack('H*', '0000000d08ffffffffffffffffffffffff'),
        'build_cancel(4294967295, 4294967295, 4294967295) == pack(\'H*\', \'0000000d08ffffffffffffffffffffffff\')';

    #
    isn't exception { build_port() }, undef,
        'build_port()       is a fatal error';
    isn't exception { build_port(-5) }, undef,
        'build_port(-5)     is a fatal error';
    isn't exception { build_port(3.3) }, undef,
        'build_port(3.3)    is a fatal error';
    isn't exception { build_port('test') }, undef,
        'build_port(\'test\') is a fatal error';
    is build_port(8555), "\0\0\0\5\t\0\0!k",
        'build_port(8555)   == "\0\0\0\5\t\0\0!k"';
    is build_port(652145), "\0\0\0\a\t\0\t\xF3q",
        'build_port(652145) == "\0\0\0\a\t\0\t\xF3q"';

    #
    is _parse_handshake(), undef,
        '_parse_handshake()    is a fatal error (no data)';
    is _parse_handshake(''), undef,
        '_parse_handshake(\'\') is a fatal error (no/not enough data)';
    is _parse_handshake('Hahaha'),
        undef,
        '_parse_handshake(\'Hahaha\') is a fatal error (Not enough data)';
    is _parse_handshake(  "\23NotTorrent protocol\0\0\0\0\0\0\0\0"
                        . 'AAAAAAAAAAAAAAAAAAAABBBBBBBBBBBBBBBBBBBB'),
        undef,
        '_parse_handshake("\23NotTorrent protocol[...]") is a fatal error (Bad protocol name)';
    is_deeply _parse_handshake(  "\23BitTorrent protocol\0\0\0\0\0\0\0\0"
                               . 'AAAAAAAAAAAAAAAAAAAABBBBBBBBBBBBBBBBBBBB'),
        ["\0" x 8, '41' x 20, 'B' x 20],
        '_parse_handshake([...]) == [packet] (Correct handshake)';

    #
    is _parse_keepalive(), undef,
        '_parse_keepalive() has no payload and nothing to test';
    is _parse_choke(), undef,
        '_parse_choke() has no payload and nothing to test';
    is _parse_unchoke(), undef,
        '_parse_unchoke() has no payload and nothing to test';
    is _parse_interested(), undef,
        '_parse_interested() has no payload and nothing to test';
    is _parse_not_interested(), undef,
        '_parse_not_interested() has no payload and nothing to test';
    isn't exception { _parse_have() }, undef,
        '_parse_have() is a fatal error (no packed index)';
    isn't exception { _parse_have('') }, undef,
        '_parse_have(\'\') is a fatal error (no packed index)';
    is _parse_have("\0\0\0d"),  100,  '_parse_have("\0\0\0d") == 100';
    is _parse_have("\0\0\0\0"), 0,    '_parse_have("\0\0\0\0") == 0';
    is _parse_have("\0\0\4\0"), 1024, '_parse_have("\0\0\4\0") == 1024';
    is _parse_have("\f\f\f\f"),
        202116108, '_parse_have("\f\f\f\f") == 202116108';
    is _parse_have("\x0f\x0f\x0f\x0f"),
        252645135, '_parse_have("\x0f\x0f\x0f\x0f") == 252645135';
    is _parse_have("\xff\xff\xff\xff"), 4294967295,
        '_parse_have("\xff\xff\xff\xff") == 4294967295 (upper limit for 32-bit math)';

    #
    isn't exception { _parse_bitfield() }, undef,
        '_parse_bitfield() is a fatal error (no data)';
    isn't exception { _parse_bitfield('') }, undef,
        '_parse_bitfield(\'\') is a fatal error (no data)';
    is _parse_bitfield(pack 'B*', '1110010100010'),
        "\xA7\b", '_parse_bitfield([...], \'1110010100010\') == "\xA7\b"';
    is _parse_bitfield(pack 'B*', '00'),
        "\0", '_parse_bitfield([...], \'00\') == "\0"';
    is _parse_bitfield(pack 'B*', '00001'),
        "\20", '_parse_bitfield([...], \'00001\') == "\20"';
    is _parse_bitfield(pack 'B*', '1111111111111'),
        "\xFF\37", '_parse_bitfield([...], \'1111111111111\') == "\xFF\37"';

    #
    is _parse_request(), (),
        '_parse_request() is note a fatal error but returns undef';
    is _parse_request(''), (),
        '_parse_request(\'\') is not a fatal error but returns no data';
    is_deeply _parse_request("\0\0\0\0\0\0\0\0\0\0\0\0"),
        [0, 0, 0],
        '_parse_request("\0\0\0\0\0\0\0\0\0\0\0\0")  == [0, 0, 0]';
    is_deeply _parse_request("\0\0\0\0\0\0\0\0\0\2\0\0"),
        [0, 0, 2**17],
        '_parse_request("\0\0\0\0\0\0\0\0\0\2\0\0")  == [0, 0, 2**17]';
    is_deeply _parse_request("\0\0\0d\0\0\@\0\0\2\0\0"),
        [100, 2**14, 2**17],
        '_parse_request("\0\0\0d\0\0\@\0\0\2\0\0")   == [100, 2**14, 2**17]';
    is_deeply _parse_request("\0\20\0\0\0\0\@\0\0\2\0\0"),
        [2**20, 2**14, 2**17],
        '_parse_request("\0\20\0\0\0\0\@\0\0\2\0\0") == [2**20, 2**14, 2**17]';

    #
    is _parse_piece(), undef, '_parse_piece() is a fatal error';
    is _parse_piece(''), undef, '_parse_piece(\'\') is a fatal error';
    is_deeply _parse_piece("\0\0\0\0\0\0\0\0TEST"),
        [0, 0, 'TEST'],
        '_parse_piece("\0\0\0\0\0\0\0\0TEST")  == [0, 0, \'TEST\']';
    is_deeply _parse_piece("\0\0\0d\0\0\@\0TEST"),
        [100, 2**14, 'TEST'],
        '_parse_piece("\0\0\0d\0\0\@\0TEST")   == [100, 2**14, \'TEST\']';
    is_deeply
        _parse_piece("\0\20\0\0\0\0\@\0TEST"),
        [2**20, 2**14, 'TEST'],
        '_parse_piece("\0\20\0\0\0\0\@\0TEST") == [2**20, 2**14, \'TEST\']';
    is_deeply [_parse_piece("\0\20\0\0\0\0\@\0")],
        [],
        '_parse_piece("\0\20\0\0\0\0\@\0")     == \'\' (empty pieces should be considered bad packets)';

    #
    is _parse_cancel(), undef, '_parse_cancel() is a fatal error';
    is _parse_cancel(''), undef, '_parse_cancel(\'\') is a fatal error';
    is_deeply _parse_cancel("\0\0\0\0\0\0\0\0\0\0\0\0"),
        [0, 0, 0],
        '_parse_cancel("\0\0\0\0\0\0\0\0\0\0\0\0")  == [0, 0, 0]';
    is_deeply _parse_cancel("\0\0\0\0\0\0\0\0\0\2\0\0"),
        [0, 0, 2**17],
        '_parse_cancel("\0\0\0\0\0\0\0\0\0\2\0\0")  == [0, 0, 2**17]';
    is_deeply _parse_cancel("\0\0\0d\0\0\@\0\0\2\0\0"),
        [100, 2**14, 2**17],
        '_parse_cancel("\0\0\0d\0\0\@\0\0\2\0\0")   == [100, 2**14, 2**17]';
    is_deeply _parse_cancel("\0\20\0\0\0\0\@\0\0\2\0\0"),
        [2**20, 2**14, 2**17],
        '_parse_cancel("\0\20\0\0\0\0\@\0\0\2\0\0") == [2**20, 2**14, 2**17]';

    #
    is _parse_port(), undef, '_parse_port() is a fatal error';
    is _parse_port(''),         undef, '_parse_port(\'\') is a fatal error';
    is _parse_port("\0\0\0d"),  100,   '_parse_port("\0\0\0d")  == 100';
    is _parse_port("\0\0\0\0"), 0,     '_parse_port("0\0\0\0")  == 0';
    is _parse_port("\0\0\4\0"), 1024,  '_parse_port("\0\0\4\0") == 1024';
    is _parse_port("\f\f\f\f"),
        202116108, '_parse_port("\f\f\f\f") == 202116108';
    is _parse_port("\x0f\x0f\x0f\x0f"),
        252645135, '_parse_port("\x0f\x0f\x0f\x0f") == 252645135';
    is _parse_port("\xf0\xf0\xf0\xf0"),
        4042322160, '_parse_port("\xf0\xf0\xf0\xf0") == 4042322160';
    is _parse_port("\xff\xff\xff\xff"),
        4294967295, '_parse_port("\xff\xff\xff\xff") == 4294967295';

    #
    note 'Here, we simulate a "real" P2P session to check packet parsing';
    my @original_data = (build_handshake(pack('C*', split '', '00000000'),
                                         '0123456789' x 4,
                                         'random peer id here!'
                         ),
                         build_bitfield('11100010'),
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
                         build_piece(1,     2,  3, \'XXX'),
                         build_piece(0,     6,  3, \'XXX'),
                         build_piece(99999, 12, 3, \'XXX')
    );
    my $data = join '', @original_data;
    isn't exception { parse_packet($data) }, undef,
        'parse_packet($data) is a fatal error (Requires a SCALAR reference)';
    is $data, join('', @original_data), '   ...left data alone.';

    #
    is_deeply parse_packet(\$data),
        {payload => [pack('C*', split '', '00000000'),
                     '0123456789' x 4,
                     'random peer id here!'
         ],
         payload_length => 48,
         packet_length  => 68,
         type           => $HANDSHAKE
        },
        'Handshake...';
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf('   ...was shifted from data. (line %d)', __LINE__);
    is_deeply parse_packet(\$data),
        {payload        => "\x8C\x8C\x8C\f\f\f\x8C\f",
         payload_length => 8,
         packet_length  => 13,
         type           => $BITFIELD
        },
        'Bitfield...';
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;
    is_deeply parse_packet(\$data),
        {packet_length => 4, payload_length => 0, type => $KEEPALIVE},
        'Keepalive...';
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;
    is_deeply parse_packet(\$data),
        {packet_length => 4, payload_length => 0, type => $KEEPALIVE},
        'Keepalive...';
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;
    is_deeply(parse_packet(\$data),
              {packet_length => 4, payload_length => 0, type => $KEEPALIVE},
              'Keepalive...');
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;
    is_deeply parse_packet(\$data),
        {packet_length => 4, payload_length => 0, type => $KEEPALIVE},
        'Keepalive...';
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;
    is_deeply parse_packet(\$data),
        {packet_length => 4, payload_length => 0, type => $KEEPALIVE},
        'Keepalive...';
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;
    is_deeply parse_packet(\$data),
        {packet_length => 5, payload_length => 0, type => $INTERESTED},
        'Interested...';
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;
    is_deeply parse_packet(\$data),
        {packet_length => 4, payload_length => 0, type => $KEEPALIVE},
        'Keepalive...';
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;
    is_deeply parse_packet(\$data),
        {packet_length => 5, payload_length => 0, type => $NOT_INTERESTED},
        'Not interested...';
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;
    is_deeply parse_packet(\$data),
        {packet_length => 5, payload_length => 0, type => $UNCHOKE},
        'Unchoke...';
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;
    is_deeply parse_packet(\$data),
        {packet_length => 5, payload_length => 0, type => $CHOKE}, 'Choke...';
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;
    is_deeply parse_packet(\$data),
        {packet_length => 4, payload_length => 0, type => $KEEPALIVE},
        'Keepalive...';
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;
    is_deeply parse_packet(\$data),
        {packet_length => 5, payload_length => 0, type => $INTERESTED},
        'Interested...';
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;
    is_deeply parse_packet(\$data),
        {packet_length => 5, payload_length => 0, type => $UNCHOKE},
        'Unchoke...';
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;
    is_deeply parse_packet(\$data),
        {packet_length => 4, payload_length => 0, type => $KEEPALIVE},
        'Keepalive...';
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;
    is_deeply parse_packet(\$data),
        {payload        => 75,
         packet_length  => 9,
         payload_length => 4,
         type           => $HAVE
        },
        'Have...';
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;
    is_deeply parse_packet(\$data),
        {payload        => 0,
         packet_length  => 9,
         payload_length => 4,
         type           => $HAVE
        },
        'Have...';
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;
    is_deeply parse_packet(\$data),
        {packet_length => 4, payload_length => 0, type => $KEEPALIVE},
        'Keepalive...';
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;
    is_deeply parse_packet(\$data),
        {payload        => 1024,
         packet_length  => 9,
         payload_length => 4,
         type           => $PORT
        },
        'Port...';
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;
    is_deeply parse_packet(\$data),
        {payload        => [0, 0, 2**15],
         packet_length  => 17,
         payload_length => 12,
         type           => $REQUEST
        },
        'Request...';
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;
    is_deeply parse_packet(\$data),
        {payload        => [99999, 2**17, 2**15],
         packet_length  => 17,
         payload_length => 12,
         type           => $REQUEST
        },
        'Request...';
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;
    is_deeply parse_packet(\$data),
        {payload        => [99999, 2**17, 2**15],
         packet_length  => 17,
         payload_length => 12,
         type           => $CANCEL
        },
        'Cancel...';
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;
    is_deeply parse_packet(\$data),
        {payload        => [1, 2, 'XXX'],
         packet_length  => 16,
         payload_length => 11,
         type           => $PIECE
        },
        'Piece...';
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;
    is_deeply parse_packet(\$data),
        {payload        => [0, 6, 'XXX'],
         packet_length  => 16,
         payload_length => 11,
         type           => $PIECE
        },
        'Piece...';
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;
    is_deeply parse_packet(\$data),
        {payload        => [99999, 12, 'XXX'],
         packet_length  => 16,
         payload_length => 11,
         type           => $PIECE
        },
        'Piece...';
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;

    #
    is_deeply \@original_data, [], 'Looks like we got them all!';

    #
    note 'TODO: DHT packets!';

    #
    done_testing;
}
1;

=pod

=head1 Author

Sanko Robinson <sanko@cpan.org> - http://sankorobinson.com/

CPAN ID: SANKO

=head1 License and Legal

Copyright (C) 2008-2011 by Sanko Robinson <sanko@cpan.org>

This program is free software; you can redistribute it and/or modify it under
the terms of
L<The Artistic License 2.0|http://www.perlfoundation.org/artistic_license_2_0>.
See the F<LICENSE> file included with this distribution or
L<notes on the Artistic License 2.0|http://www.perlfoundation.org/artistic_2_0_notes>
for clarification.

=for rcs $Id$

=cut
