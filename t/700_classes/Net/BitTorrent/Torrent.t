#!C:\perl\bin\perl.exe -w
use strict;
use warnings;
use Module::Build;
use Test::More;

#
use lib q[../../../../lib];
$|++;

# let's keep track of where we are...
my $test_builder = Test::More->builder;

#
my $simple_dot_torrent = q[./t/900_data/950_torrents/953_miniswarm.torrent];
my $credits_dot_txt    = q[./t/900_data/950_torrents/credits.txt];

# Make sure the path is correct
chdir q[../../../../] if not -f $simple_dot_torrent;

#
my $build           = Module::Build->current;
my $okay_tcp        = $build->notes(q[okay_tcp]);
my $release_testing = $build->notes(q[release_testing]);
my $verbose         = $build->notes(q[verbose]);
my $threads         = $build->notes(q[threads]);
$SIG{__WARN__} = ($verbose ? sub { diag shift } : sub { });

#
my ($flux_capacitor) = (0, ());

#
BEGIN {
    plan tests => 52;
    use_ok(q[File::Temp],            qw[tempdir]);
    use_ok(q[Scalar::Util],          qw[/weak/]);
    use_ok(q[File::Spec::Functions], qw[rel2abs]);

    # Mine
    use_ok(q[Net::BitTorrent]);
    use_ok(q[Net::BitTorrent::Torrent]);
    use_ok(q[Net::BitTorrent::Util]);
}
SKIP: {
    skip(
        q[Fine grained regression tests skipped; turn on $ENV{RELESE_TESTING} to enable],
        ($test_builder->{q[Expected_Tests]} - $test_builder->{q[Curr_Test]})
    ) if not $release_testing;

    #
    my ($tempdir)
        = tempdir(q[~NBSF_test_XXXXXXXX], CLEANUP => 1, TMPDIR => 1);
    warn(sprintf(q[File::Temp created '%s' for us to play with], $tempdir));
    my $client = Net::BitTorrent->new({LocalHost => q[127.0.0.1]});
    if (!$client) {
        warn(sprintf q[Socket error: [%d] %s], $!, $!);
        skip(q[Failed to create client],
             (      $test_builder->{q[Expected_Tests]}
                  - $test_builder->{q[Curr_Test]}
             )
        );
    }
    my $torrent = $client->add_torrent({Path    => $simple_dot_torrent,
                                        BaseDir => $tempdir
                                       }
    );
    warn sprintf q[%d|%d], 6, $test_builder->{q[Curr_Test]};

    END {
        return if not defined $torrent;
        for my $file (@{$torrent->files}) { $file->_close() }
    }

    #
    warn(q[Very simple .torrent file]);
    is($torrent->path,
        rel2abs($simple_dot_torrent),
        q[Absolute paths returned from path()]);
    is_deeply($torrent->trackers, [], q[Empty list of trackers]);
    is($torrent->_complete,     0,     q[Download is incomplete]);
    is($torrent->_downloaded,   0,     q[Nothing has been downloaded]);
    is($torrent->_uploaded,     0,     q[Nothing has been uploaded]);
    is($torrent->size,          49998, q[Full size is correct]);
    is($torrent->_block_length, 16384, q[Block size is correct]);
    is($torrent->_piece_length, 32768, q[Piece size is correct]);
    is($torrent->_private,      0,     q[.torrent is not private]);
    is($torrent->infohash,
        q[2b3aaf361bd40540bf7e3bfd140b954b90e4dfbc],
        q[Infohash checks out]);
    is($torrent->bitfield,       chr(0),  q[Bitfield is correct]);
    is($torrent->_client,        $client, q[Client is correct]);
    is($torrent->_compact_nodes, q[],     q[Empty list of compact nodes]);
    ok($torrent->status,       q[Status indicates...]);
    ok($torrent->status & 64,  q[ ...torrent is attached to a client.]);
    ok($torrent->status & 128, q[ ...torrent was loaded properly.]);

    # Try bad stuff
    is(Net::BitTorrent::Torrent->new(),
        undef, q[Requires a set of parameters]);
    is(Net::BitTorrent::Torrent->new(q[FAIL!]),
        undef, q[Requires parameters to be passed as a hashref]);
    is(Net::BitTorrent::Torrent->new({}),
        undef, q[Requires a 'Path' parameter]);
    is(Net::BitTorrent::Torrent->new({Path => q[]}),
        undef, q[! -f Path parameter]);
    is( Net::BitTorrent::Torrent->new(
                                     {Path   => $simple_dot_torrent,
                                      Client => bless(\{}, q[Not::BitTorrent])
                                     }
        ),
        undef,
        q[Requires a blessed 'Client' object (2)]
    );
    isa_ok(Net::BitTorrent::Torrent->new(
                              {Path => $simple_dot_torrent, Client => $client}
           ),
           q[Net::BitTorrent::Torrent],
           q[Requires a blessed 'Client' object (3)]
    );
    my $orphan_torrent = Net::BitTorrent::Torrent->new(
                              {Path => $simple_dot_torrent, Client => undef});
    isa_ok($orphan_torrent, q[Net::BitTorrent::Torrent],
           q[Works without a 'Client' parameter too]);
    ok($orphan_torrent->status, q[Status indicates...]);
    ok($orphan_torrent->status | 64,
        q[ ...torrent is not attached to a client.]);
    ok($orphan_torrent->status & 128, q[ ...torrent was loaded properly.]);
    $orphan_torrent = undef;
    is( Net::BitTorrent::Torrent->new(
                              {Path => $simple_dot_torrent, Client => q[Test]}
        ),
        undef,
        q[Requires a blessed 'Client' object if defined]
    );
TODO: {
        todo_skip
            q[Undocumented stuff may fail. ...that's why it's undocumented.],
            2;

        # Undocumented BlockLength parameter tests
        isa_ok(Net::BitTorrent::Torrent->new({Path   => $simple_dot_torrent,
                                              Client => $client,
                                              BlockLength => q[Test]
                                             }
               ),
               q[Net::BitTorrent::Torrent],
               q[Undocumented BlockLength parameter]
        );
        isa_ok(Net::BitTorrent::Torrent->new({Path   => $simple_dot_torrent,
                                              Client => $client,
                                              BlockLength => 4000000
                                             }
               ),
               q[Net::BitTorrent::Torrent],
               q[Undocumented BlockLength parameter]
        );
    }

    # TODO: Test BaseDir param
    warn(q[TODO: Test BaseDir param]);

    #
    is( Net::BitTorrent::Torrent->new(
                                 {Path => $credits_dot_txt, Client => $client}
        ),
        undef,
        q[.torrent's data is malformed]
    );

    # Back to the original...
    is($torrent->_downloaded,             0,    q[    _downloaded == 0]);
    is($torrent->_add_downloaded(1024),   1024, q[_add_downloaded(1024)]);
    is($torrent->_add_downloaded(-1024),  1024, q[_add_downloaded(-1024)]);
    is($torrent->_add_downloaded('dude'), 1024, q[_add_downloaded('dude')]);
    is($torrent->_downloaded,             1024, q[    _downloaded == 1024]);
    is($torrent->_uploaded,               0,    q[    _uploaded == 0]);
    is($torrent->_add_uploaded(1024),     1024, q[_add_uploaded(1024)]);
    is($torrent->_add_uploaded(-1024),    1024, q[_add_uploaded(-1024)]);
    is($torrent->_add_uploaded('dude'),   1024, q[_add_uploaded('dude')]);
    is($torrent->_uploaded,               1024, q[    _uploaded == 1024]);

=for status
        # (201=Started, 961=Force Download, 1000=finished)
        #     1 = Started
        #     2 = Checking
        #     4 = Start after check
        #     8 = Checked
        #    16 = Error
        #    32 = Paused
        #    64 = Queued
        #   128 = Loaded
        #   256 =
        #   512 = Force
        #  1000 = Complete
=cut

    #
    warn q[Test multithreaded stuff...];
SKIP: {
        skip q[Multi-threaded tests have been skipped], 4 if !$threads;
        use_ok(q[threads]);
        use_ok(q[threads::shared]);

        #
        my $_threaded_torrent
            = Net::BitTorrent::Torrent->new({Path => $simple_dot_torrent});
        ok($_threaded_torrent->_set_bitfield(chr(0)),
            q[Parent sets bitfield to "\0"]);
        ok($_threaded_torrent->_set_status(4), q[Parent sets status to 4]);
        threads->create(
            sub {
                $_threaded_torrent->_set_bitfield(chr(1));
                $_threaded_torrent->_set_status(0);
                return 1;
            }
        )->join();
        is($_threaded_torrent->bitfield,
            chr(1), q[Parent reads bitfield as "\1"]);
        is($_threaded_torrent->status, 0, q[Parent reads status as 0]);
    }
}

# $Id$
