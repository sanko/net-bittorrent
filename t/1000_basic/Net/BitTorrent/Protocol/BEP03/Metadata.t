package t::Net::BitTorrent::Protocol::BEP03::Metadata;
{
    use strict;
    use warnings;

    # Load standard modules
    use Module::Build;
    use Test::More;
    use parent 'Test::Class';
    use Test::Moose;
    use Test::Exception;

    # Load local context
    BEGIN { -d '_build' ? last : chdir '..' for 1 .. 10 }
    my $t_builder = Test::More->builder;
    my $m_builder = Module::Build->current;

    # Load local modules
    use lib '../../../../../../lib', 'lib';
    use Net::BitTorrent::Protocol::BEP03::Metadata;

    #
    sub class     {'Net::BitTorrent::Protocol::BEP03::Metadata'}
    sub init_args { () }
    sub info_hash {'E55BD8B859C8E835234EF6801B56E8A6F730BB6C'}
    sub stringish {'d4:infod5:filesle12:piece_lengthi262144e6:pieces0:ee'}

    #
    sub build : Test( startup => 1 ) {
        my $s = shift;
        $s->{'m'} = new_ok $s->class, [@_ ? @_ : $s->init_args];
    }

    sub files : Test( 1 ) {
        my $s = shift;
        is_deeply [$s->{'m'}->files],
            [$s->init_args ? $s->init_args->{'files'} : []],
            '...->files are correct';
    }

    sub new_announce : Test( 6 ) {
        my $s = shift;
        throws_ok sub { $s->class->new(announce => 'test') },
            qr[Validation failed], 'test is not a valid announce URL';
        throws_ok sub { $s->class->new(announce => '') },
            qr[Validation failed],
            'empty string is not a valid announce URL';
        throws_ok sub { $s->class->new(announce => 'fail://example.com/') },
            qr[Validation failed],
            'fail://example.com/ is not a valid announce URL';
        lives_ok sub { $s->class->new(announce => 'udp://example.com/') },
            'udp://example.com/ is a valid announce URL';
        lives_ok sub { $s->class->new(announce => 'http://example.com/') },
            'http://example.com/ is a valid announce URL';
        lives_ok sub { $s->class->new(announce => 'https://example.com/') },
            'https://example.com/ is a valid announce URL';
    }

    sub new_files : Test( 14 ) {
        my $s = shift;
        for my $files ([{path   => [qw[deep deeper deepest.ext]],
                         length => 1024
                        }
                       ],
                       [{path   => [qw[deep deeper deepest.ext]],
                         length => 1024
                        },
                        {path   => [qw[deep deeper reallydeep.ext]],
                         length => 2448
                        }
                       ]
            )
        {   is_deeply new_ok($s->class, [{files => $files}])->files, $files,
                sprintf 'list of files match. (%d total)', scalar @{$files};
        }
        throws_ok sub { $s->class->new(files => 'test') },
            qr[Validation failed],
            'test is not a valid file';
        throws_ok sub { $s->class->new(files => '') }, qr[Validation failed],
            'empty string is not a valid file';
        throws_ok
            sub { $s->class->new(files => {path => 'test', length => 100}) },
            qr[Validation failed],
            '{ path => \'test\', length => 100 } is invalid';
        throws_ok
            sub { $s->class->new(files => [{path => 'test', length => 100}]) }
            , qr[Validation failed],
            '[{ path => \'test\', length => 100 }] is invalid';
        throws_ok sub { $s->class->new(files => [{path => ['test']}]) },
            qr[Validation failed], '[{ path => [\'test\'] }] is invalid';
        throws_ok sub { $s->class->new(files => [{length => 100}]) },
            qr[Validation failed], '[{ length => 100 }] is invalid';
        throws_ok sub {
            $s->class->new(files => {path => ['test'], length => -100});
            },
            qr[Validation failed],
            '{ path => [\'test\'], length => -100 } is invalid';
        throws_ok
            sub { $s->class->new(files => {path => ['test'], length => 0}) },
            qr[Validation failed],
            '{ path => [\'test\'], length => 0 } is invalid';
        throws_ok sub {
            $s->class->new(files => {path => ['test'], length => 'long'});
            },
            qr[Validation failed],
            '{ path => [\'test\'], length => \'long\' } is invalid';
        throws_ok
            sub { $s->class->new(files => {path => ['test'], length => ''}) },
            qr[Validation failed],
            '{ path => [\'test\'], length => \'\' } is invalid';
    }

    sub new_pieces : Test( 2 ) {
        my $s = shift;
        is new_ok($s->class, [pieces => pack 'H80', 'A' x 40])->pieces, '',
            'Cannot set pieces attribute with new( ... )';
    }

    sub new_piece_length : Test( 8 ) {
        my $s = shift;
        is new_ok($s->class, [])->piece_length, 262144,
            'using default piece length';
        is new_ok($s->class, [piece_length => 1024])->piece_length, 1024,
            '{ piece_length => 1024 } is valid';
        throws_ok
            sub { $s->class->new(piece_length => '') },
            qr[Value is not a positive integer],
            '{ piece_length => \'\' }} is invalid';
        throws_ok
            sub { $s->class->new(piece_length => 0) },
            qr[Value is not a positive integer],
            '{ piece_length => 0 }} is invalid';
        throws_ok
            sub { $s->class->new(piece_length => 'ABCD') },
            qr[Value is not a positive integer],
            '{ piece_length => \'ABCD\' }} is invalid';
        throws_ok
            sub { $s->class->new(piece_length => -1024) },
            qr[Value is not a positive integer],
            '{ piece_length => -1024 }} is invalid';
    }

    sub infohash : Test( 7 ) {
        my $s = shift;
        is $s->{'m'}->info_hash->to_Hex, $s->info_hash;
        is new_ok($s->class,
                  [{files => [{path   => [qw[deep deeper deepest.ext]],
                               length => 1024
                              }
                    ]
                   }
                  ]
            )->info_hash->to_Hex,
            '369A5DDB2348887573871DB41F038917F17C2318',
            'deep single file torrent infohash is okay';
        is new_ok($s->class,
                  [{files => [{path => [qw[simple.ext]], length => 1024}]}])
            ->info_hash->to_Hex,
            'C1E32528BD921212A633CD7C11321EA6D6CAE359',
            'simple single file torrent infohash is okay';
        is new_ok($s->class,
                  [{files => [{path   => [qw[deep deeper deepest.ext]],
                               length => 1024
                              },
                              {path   => [qw[deep deeper reallydeep.ext]],
                               length => 2448
                              }
                    ],
                    length => 1024
                   }
                  ]
            )->info_hash->to_Hex,
            '02E48FAC7C76C8E59E051187BB105616C6A35924',
            'multifile torrent infohash is okay';
    }

    sub as_string : Test( 7 ) {
        my $s = shift;
        is $s->{'m'}->as_string, $s->stringish;
        is new_ok($s->class,
                  [{files => [{path   => [qw[deep deeper deepest.ext]],
                               length => 1024
                              }
                    ]
                   }
                  ]
            )->as_string,
            'd4:infod5:filesld6:lengthi1024e4:pathl4:deep6:deeper11:deepest.exteee12:piece_lengthi262144e6:pieces0:ee',
            'deep single file torrent as_string is okay';
        is new_ok($s->class,
                  [{files => [{path => [qw[simple.ext]], length => 1024}]}])
            ->as_string,
            'd4:infod12:piece_lengthi262144e6:pieces0:e6:lengthi1024e4:name10:simple.exte',
            'simple single file torrent as_string is okay';
        is new_ok($s->class,
                  [{files => [{path   => [qw[deep deeper deepest.ext]],
                               length => 1024
                              },
                              {path   => [qw[deep deeper reallydeep.ext]],
                               length => 2448
                              }
                    ],
                    length => 1024
                   }
                  ]
            )->as_string,
            'd4:infod5:filesld6:lengthi1024e4:pathl4:deep6:deeper11:deepest.exteed6:lengthi2448e4:pathl4:deep6:deeper14:reallydeep.exteee12:piece_lengthi262144e6:pieces0:ee',
            'multifile torrent as_string is okay';
    }

    sub class_can : Test( 0 ) {
        my $s = shift;
    }

    sub moose_does : Test( 0 ) {
        my $s = shift;
    }

    sub moose_attributes : Test( 2 ) {
        my $s = shift;
        has_attribute_ok $s->{'m'}, 'files',    'has files';
        has_attribute_ok $s->{'m'}, 'announce', 'has announce';
    }

    sub moose_meta : Test( 1 ) {
        my $s = shift;
        meta_ok $s->{'m'};
    }

    #
    __PACKAGE__->runtests() if !caller;
}
1;

=pod

=head1 Author

Sanko Robinson <sanko@cpan.org> - http://sankorobinson.com/

CPAN ID: SANKO

=head1 License and Legal

Copyright (C) 2008-2010 by Sanko Robinson <sanko@cpan.org>

This program is free software; you can redistribute it and/or modify it under
the terms of
L<The Artistic License 2.0|http://www.perlfoundation.org/artistic_license_2_0>.
See the F<LICENSE> file included with this distribution or
L<notes on the Artistic License 2.0|http://www.perlfoundation.org/artistic_2_0_notes>
for clarification.

When separated from the distribution, all original POD documentation is
covered by the
L<Creative Commons Attribution-Share Alike 3.0 License|http://creativecommons.org/licenses/by-sa/3.0/us/legalcode>.
See the
L<clarification of the CCA-SA3.0|http://creativecommons.org/licenses/by-sa/3.0/us/>.

Neither this module nor the L<Author|/Author> is affiliated with BitTorrent,
Inc.

=for rcs $Id$

=cut
