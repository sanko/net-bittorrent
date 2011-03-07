package t::Net::BitTorrent::Protocol::BEP10::Packets;
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
    use Net::BitTorrent::Protocol::BEP10::Packets qw[:all];

    # Tests!
    is $EXTPROTOCOL, 20, '$Extended       == 20';

    #
    isn't exception { build_extended() }, undef,
        'build_extended() is a fatal error';
    isn't exception { build_extended(undef, {}) },
        undef, 'build_extended(undef, { }) is a fatal error';
    isn't exception { build_extended(-1, {}) }, undef,
        'build_extended(-1, { })    is a fatal error';
    isn't exception { build_extended('', {}) }, undef,
        'build_extended(\'\', { })    is a fatal error';
    isn't exception { build_extended(0, undef) },
        undef, 'build_extended(0, undef)   is a fatal error';
    isn't exception { build_extended(0, 2) }, undef,
        'build_extended(0, 2)       is a fatal error';
    isn't exception { build_extended(0, -2) }, undef,
        'build_extended(0, -2)      is a fatal error';
    isn't exception { build_extended(0, '') }, undef,
        'build_extended(0, \'\')      is a fatal error';
    is build_extended(0, {}), "\0\0\0\4\24\0de",
        'build_extended(0, { })     == "\0\0\0\4\24\0de"';
    is build_extended(0,
                      {m => {ut_pex => 1, 'µT_PEX' => 2},
                       ((p => 30)),
                       v      => 'Net::BitTorrent r0.30',
                       yourip => pack('C4', ('127.0.0.1' =~ m[(\d+)]g)),
                       reqq   => 30
                      }
        ),
        "\0\0\0Z\24\0d1:md6:ut_pexi1e7:\xC2\xB5T_PEXi2ee1:pi30e4:reqqi30e1:v21:Net::BitTorrent r0.306:yourip4:\x7F\0\0\1e",
        'build_extended(0, { .. }   == "\0\0\0Z\24\0d[...]e" (id == 0 | initial ext handshake is bencoded dict)';

    #
    is _parse_extended(), undef, '_parse_extended() is a fatal error';
    is _parse_extended(''), undef, '_parse_extended(\'\') is a fatal error';
    is_deeply _parse_extended(
        "\0d1:md6:ut_pexi1e7:\xC2\xB5T_PEXi2ee1:pi30e4:reqqi30e1:v21:Net::BitTorrent r0.306:yourip4:\x7F\0\0\1e"
        ),
        [0,
         {m => {ut_pex => 1, 'µT_PEX' => 2},
          ((p => 30)),
          v      => 'Net::BitTorrent r0.30',
          yourip => pack('C4', ('127.0.0.1' =~ m[(\d+)]g)),
          reqq   => 30
         }
        ],
        '_parse_extended([...]) == [0, { ... }] (packet ID and content)';

    #
    note 'Here, we simulate a "real" P2P session to check packet parsing';
    my @original_data = (
                        build_extended(
                            0,
                            {m => {ut_pex    => 1,
                                   'µT_PEX' => 2
                             },
                             ((p => 30)),
                             v      => 'Net::BitTorrent r0.30',
                             yourip => pack('C4', ('127.0.0.1' =~ m[(\d+)]g)),
                             reqq   => 30
                            }
                        )
    );
    my $data = join '', @original_data;
    isn't exception { parse_packet($data) }, undef,
        'parse_packet($data) is a fatal error (Requires a SCALAR reference)';
    is $data, join('', @original_data), '   ...left data alone.';

    #
    is_deeply parse_packet(\$data),
        {payload => [0,
                     {m => {ut_pex => 1, 'µT_PEX' => 2},
                      ((p => 30)),
                      v      => 'Net::BitTorrent r0.30',
                      yourip => pack('C4', ('127.0.0.1' =~ m[(\d+)]g)),
                      reqq   => 30
                     }
         ],
         payload_length => 89,
         packet_length  => 94,
         type           => $EXTPROTOCOL
        },
        'Extended Protocol...';
    note explain shift @original_data;
    is $data,
        join('', @original_data),
        sprintf '   ...was shifted from data. (line %d)', __LINE__;

    #
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
