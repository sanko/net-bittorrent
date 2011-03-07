package t::Net::BitTorrent::Protocol::BEP06::Packets;
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
    use Net::BitTorrent::Protocol::BEP06::Packets qw[:all];

    # Tests!
    is $SUGGEST,      13, '$Suggest        == 13';
    is $HAVE_ALL,     14, '$Have All       == 14';
    is $HAVE_NONE,    15, '$Have None      == 15';
    is $REJECT,       16, '$Reject         == 16';
    is $ALLOWED_FAST, 17, '$Allowed Fast   == 17';

    #
    is _parse_suggest(), undef, '_parse_suggest() is a fatal error';
    is _parse_suggest(''), undef, '_parse_suggest(\'\') is a fatal error';
    is _parse_suggest("\0\0\0d"),  100,  '_parse_suggest("\0\0\0d")  == 100';
    is _parse_suggest("\0\0\0\0"), 0,    '_parse_suggest("\0\0\0\0") == 0';
    is _parse_suggest("\0\0\4\0"), 1024, '_parse_suggest("\0\0\4\0") == 1024';
    is _parse_suggest("\f\f\f\f"),
        202116108, '_parse_suggest("\f\f\f\f") == 202116108';
    is _parse_suggest("\x0f\x0f\x0f\x0f"),
        252645135, '_parse_suggest("\x0f\x0f\x0f\x0f") == 252645135';
    is _parse_suggest("\xf0\xf0\xf0\xf0"),
        4042322160, '_parse_suggest("\xf0\xf0\xf0\xf0") == 4042322160';
    is _parse_suggest("\xff\xff\xff\xff"),
        4294967295, '_parse_suggest("\xff\xff\xff\xff") == 4294967295';

    #
    is _parse_have_all(), undef, '_parse_have_all() is a fatal error';

    #
    is _parse_have_none(), undef, '_parse_have_none() is a fatal error';

    #
    is _parse_reject(), undef, '_parse_reject() is a fatal error';
    is _parse_reject(''), undef, '_parse_reject(\'\') is a fatal error';
    is_deeply _parse_reject("\0\0\0\0\0\0\0\0\0\0\0\0"),
        [0, 0, 0],
        '_parse_reject("\0\0\0\0\0\0\0\0\0\0\0\0")  == [0, 0, 0]';
    is_deeply _parse_reject("\0\0\0\0\0\0\0\0\0\2\0\0"),
        [0, 0, 2**17],
        '_parse_reject("\0\0\0\0\0\0\0\0\0\2\0\0")  == [0, 0, 2**17]';
    is_deeply _parse_reject("\0\0\0d\0\0\@\0\0\2\0\0"),
        [100, 2**14, 2**17],
        '_parse_reject("\0\0\0d\0\0\@\0\0\2\0\0")   == [100, 2**14, 2**17]';
    is_deeply _parse_reject("\0\20\0\0\0\0\@\0\0\2\0\0"),
        [2**20, 2**14, 2**17],
        '_parse_reject("\0\20\0\0\0\0\@\0\0\2\0\0") == [2**20, 2**14, 2**17]';

    #
    is _parse_allowed_fast(), undef, '_parse_allowed_fast() is a fatal error';
    is _parse_allowed_fast(''),
        undef, '_parse_allowed_fast(\'\') is a fatal error';
    is _parse_allowed_fast("\0\0\0d"),
        100, '_parse_allowed_fast("\0\0\0d")  == 100';
    is _parse_allowed_fast("\0\0\0\0"),
        0, '_parse_allowed_fast("\0\0\0\0") == 0';
    is _parse_allowed_fast("\0\0\4\0"),
        1024, '_parse_allowed_fast("\0\0\4\0") == 1024';
    is _parse_allowed_fast("\f\f\f\f"),
        202116108, '_parse_allowed_fast("\f\f\f\f") == 202116108';
    is _parse_allowed_fast("\x0f\x0f\x0f\x0f"),
        252645135, '_parse_allowed_fast("\x0f\x0f\x0f\x0f") == 252645135';
    is _parse_allowed_fast("\xf0\xf0\xf0\xf0"),
        4042322160, '_parse_allowed_fast("\xf0\xf0\xf0\xf0") == 4042322160';
    is _parse_allowed_fast("\xff\xff\xff\xff"),
        4294967295, '_parse_allowed_fast("\xff\xff\xff\xff") == 4294967295';

    #
    isn't exception { parse_packet() }, undef,
        'parse_packet() is a fatal error';
    isn't exception { parse_packet('') }, undef,
        'parse_packet(\'\') is a fatal error';
    isn't exception { parse_packet(\{}) }, undef,
        'parse_packet(\{ }) is a fatal error (requires SCALAR ref)';
    my $packet = 'Testing';
    is parse_packet(\$packet), (),
        'parse_packet(\$packet) is not a fatal error but returns undef (where $packet == \'Testing\')';
    $packet = "\0\0\0\5\40\0\0\0F";
    isn't exception { parse_packet(\$packet) }, undef,
        'parse_packet(\$packet) is a fatal error (where $packet == "\0\0\0\5\40\0\0\0F")';
    $packet = undef;
    isn't exception { parse_packet(\$packet) }, undef,
        'parse_packet(\$packet) is a fatal error (where $packet is a fatal error)';
    $packet = '';
    isn't exception { parse_packet(\$packet) },
        undef,
        'parse_packet(\$packet) is a fatal error (where $packet == "")';
    $packet = "\0\0\0\r\25\0\0\4\0\0\4\0\0\0\1\0\0";
    isn't exception { parse_packet(\$packet) }, undef,
        'parse_packet(\$packet) is a fatal error (where $packet == "\0\0\0\r\25\0\0\4\0\0\4\0\0\0\1\0\0")';

    #
    note 'Here, we simulate a "real" P2P session to check packet parsing';
    my @original_data = (build_suggest(0),
                         build_suggest(2**14),
                         build_have_all(),
                         build_have_none(),
                         build_allowed_fast(0),
                         build_allowed_fast(1024),
                         build_reject(0,    0,     1024),
                         build_reject(1024, 2**18, 2**16),
    );
    my $data = join '', @original_data;
    isn't exception { parse_packet($data) }, undef,
        'parse_packet($data) is a fatal error (Requires a SCALAR reference)';
    is $data, join('', @original_data), '   ...left data alone.';

    #
    is_deeply parse_packet(\$data),
        {payload        => 0,
         packet_length  => 9,
         payload_length => 4,
         type           => $SUGGEST
        },
        'Suggestion...';
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;
    is_deeply parse_packet(\$data),
        {payload        => 2**14,
         packet_length  => 9,
         payload_length => 4,
         type           => $SUGGEST
        },
        'Suggestion...';
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;
    is_deeply parse_packet(\$data),
        {packet_length => 5, payload_length => 0, type => $HAVE_ALL},
        'Have All...';
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;
    is_deeply parse_packet(\$data),
        {packet_length => 5, payload_length => 0, type => $HAVE_NONE},
        'Have None...';
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;
    is_deeply parse_packet(\$data),
        {payload        => 0,
         payload_length => 4,
         packet_length  => 9,
         type           => $ALLOWED_FAST
        },
        'Allowed Fast...';
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;
    is_deeply parse_packet(\$data),
        {payload        => 1024,
         payload_length => 4,
         packet_length  => 9,
         type           => $ALLOWED_FAST
        },
        'Allowed Fast...';
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;
    is_deeply parse_packet(\$data),
        {payload        => [0, 0, 1024],
         payload_length => 12,
         packet_length  => 17,
         type           => $REJECT
        },
        'Reject...';
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;
    is_deeply parse_packet(\$data),
        {payload        => [1024, 2**18, 2**16],
         packet_length  => 17,
         payload_length => 12,
         type           => $REJECT
        },
        'Reject...';
    note explain $original_data[0];
    note explain parse_packet(\shift @original_data);
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;
    is_deeply \@original_data, [], 'Looks like we got them all!';

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
