#!C:\perl\bin\perl.exe -w
use strict;
use warnings;
use Module::Build;
use Test::More;

# Yours
use File::Temp qw[tempdir];
use Scalar::Util qw[/weak/];
use File::Spec::Functions qw[rel2abs];

#
use lib q[../../../../lib];

# Mine
use Net::BitTorrent::Torrent;
use Net::BitTorrent::Util;
use Net::BitTorrent;

#
$|++;

# let's keep track of where we are...
my $test_builder = Test::More->builder;

#
my %torrents = (
              single    => q[./t/900_data/950_torrents/951_single.torrent],
              multi     => q[./t/900_data/950_torrents/952_multi.torrent],
              miniswarm => q[./t/900_data/950_torrents/953_miniswarm.torrent],
              unicode   => q[./t/900_data/950_torrents/955_unicode.torrent]
);
my $credits_dot_txt = q[./t/900_data/950_torrents/credits.txt];

# Make sure the path is correct
chdir q[../../../../] if not -f $torrents{q[single]};

#
plan tests => int(6 + (82 * scalar keys %torrents));

#
my $build           = Module::Build->current;
my $okay_tcp        = $build->notes(q[okay_tcp]);
my $release_testing = $build->notes(q[release_testing]);
my $verbose         = $build->notes(q[verbose]);
my $threads         = $build->notes(q[threads]);
my $profile         = $build->notes(q[profile]);
$SIG{__WARN__} = ($verbose ? sub { diag shift } : sub { });

#
my ($flux_capacitor) = (0, ());
SKIP: {
    skip(
        q[Fine grained regression tests skipped; turn on $ENV{RELESE_TESTING} to enable],
        ($test_builder->{q[Expected_Tests]} - $test_builder->{q[Curr_Test]})
    ) if not $release_testing;
    is(Net::BitTorrent::Torrent->new(), undef, q[new() returns undef]);
    is(Net::BitTorrent::Torrent->new(q[FAIL!]),
        undef, q[new('FAIL!') returns undef (requires hashref)]);
    is(Net::BitTorrent::Torrent->new({}), undef, q[new({}) returns undef]);
    is(Net::BitTorrent::Torrent->new({Path => q[]}),
        undef, q[new({Path => ''}) returns undef]);
    is(Net::BitTorrent::Torrent->new({Path => $credits_dot_txt}),
        undef, sprintf q[{ Path => '%s' } returns undef],
        $credits_dot_txt);
    is( Net::BitTorrent::Torrent->new({Path => q[./]}), undef,
        sprintf q[{ Path => '%s' } returns undef], q[./]);

    # sort through the different sorts of .torrents
    for my $_key (keys %torrents) {
        my $dot_torrent = $torrents{$_key};
        warn sprintf q[Testing with '%s'], $dot_torrent;

        #
        my ($tempdir)
            = tempdir(q[~NBSF_test_XXXXXXXX], CLEANUP => 1, TMPDIR => 1);
        warn(sprintf(q[File::Temp created '%s' for us to play with], $tempdir)
        );
        my $client = Net::BitTorrent->new({LocalHost => q[127.0.0.1]});
        if (!$client) {
            warn(sprintf q[Socket error: [%d] %s], $!, $!);
            skip(q[Failed to create client],
                 (      $test_builder->{q[Expected_Tests]}
                      - $test_builder->{q[Curr_Test]}
                 )
            );
        }
        my $torrent =
            $client->add_torrent({Path    => $dot_torrent,
                                  BaseDir => $tempdir
                                 }
            );

        END {
            return if not defined $torrent;
            for my $file (@{$torrent->files}) { $file->_close() }
        }

        #
        is($torrent->path, rel2abs($dot_torrent),
            q[Absolute paths returned from path()]);
        is_deeply($torrent->raw_data, _raw_data($_key),
                  sprintf q[Raw data for %s torrent looks good.], $_key);
        is($torrent->infohash, _infohash($_key),
            sprintf q[Infohash checks out (%s)], $_key);
        is_deeply($torrent->trackers, [_trackers($_key)],
                  sprintf q[List of trackers (%s)], $_key);
        is($torrent->is_complete, 0, sprintf q[Download is incomplete (%s)],
            $_key);
        is($torrent->downloaded, 0,
            sprintf q[Nothing has been downloaded (%s)], $_key);
        is($torrent->uploaded, 0, sprintf q[Nothing has been uploaded (%s)],
            $_key);
        is($torrent->size, _size($_key), sprintf q[Full size is correct (%s)],
            $_key);
        is($torrent->_block_length, 16384,
            sprintf q[Block size is correct (%s)], $_key);
        is($torrent->private, 0, sprintf q[.torrent is not private (%s)],
            $_key);
        is( $torrent->bitfield,
            pack(q[b*],
                 qq[\0] x int(
                         length(
                             unpack(
                                 q[H*], _raw_data($_key)->{q[info]}{q[pieces]}
                             )
                             ) / 40
                 )
            ),
            sprintf q[Bitfield is correct (%s)],
            $_key
        );
        is($torrent->_client, $client, sprintf q[Client is correct (%s)],
            $_key);
        is($torrent->_compact_nodes, q[],
            sprintf q[Empty list of compact nodes (%s)], $_key);
        is($torrent->comment,
            _raw_data($_key)->{q[comment]},
            sprintf q[comment is correct (%s)], $_key);
        is($torrent->created_by,
            _raw_data($_key)->{q[created by]},
            sprintf q[created_by is correct (%s)], $_key);
        is($torrent->creation_date,
            _raw_data($_key)->{q[creation date]},
            sprintf q[creation_date is correct (%s)], $_key);
        is($torrent->name,
            _raw_data($_key)->{q[info]}{q[name]},
            sprintf q[name is correct (%s)], $_key);

        # we do more later
        ok($torrent->status, sprintf q[Status indicates (%s )...], $_key);
        ok($torrent->status & 128,
            sprintf q[ ...%s torrent is attached to a client.], $_key);
        ok($torrent->status & 64,
            sprintf q[ ...%s torrent was loaded properly.], $_key);

        # Try bad stuff
        is( Net::BitTorrent::Torrent->new(
                                  {Path   => $dot_torrent,
                                   Client => bless(\{}, q[Not::BitTorrent])
                                  }
            ),
            undef,
            sprintf q[Requires a blessed 'Client' object (2) (%s)],
            $_key
        );
        isa_ok(Net::BitTorrent::Torrent->new(
                                     {Path => $dot_torrent, Client => $client}
               ),
               q[Net::BitTorrent::Torrent],
               sprintf q[Requires a blessed 'Client' object (3) (%s)],
               $_key
        );
        my $orphan_torrent = Net::BitTorrent::Torrent->new(
                                     {Path => $dot_torrent, Client => undef});
        isa_ok($orphan_torrent, q[Net::BitTorrent::Torrent],
               sprintf q[Works with an undef 'Client' parameter (%s)], $_key);
        isa_ok(Net::BitTorrent::Torrent->new({Path => $dot_torrent}),
               q[Net::BitTorrent::Torrent],
               sprintf q[Works without a 'Client' parameter too (%s)],
               $_key
        );
        is( Net::BitTorrent::Torrent->new(
                                     {Path => $dot_torrent, Client => q[Test]}
            ),
            undef,
            sprintf q[Requires a blessed 'Client' object if defined (%s)],
            $_key
        );
        {
            warn sprintf q[Undocumented BlockLength parameter tests], $_key;
            my $_torrent_Test =
                Net::BitTorrent::Torrent->new({Path        => $dot_torrent,
                                               BlockLength => q[Test]
                                              }
                );
            isa_ok($_torrent_Test, q[Net::BitTorrent::Torrent],
                   sprintf q[{ [...],  BlockLength => 'Test'} (%s)], $_key);
            is($_torrent_Test->_block_length,
                16384, sprintf q[ ...BlockLength == 16384 (default) (%s)],
                $_key);
            my $_torrent_4000000 =
                Net::BitTorrent::Torrent->new({Path        => $dot_torrent,
                                               BlockLength => 4000000
                                              }
                );
            isa_ok($_torrent_4000000, q[Net::BitTorrent::Torrent],
                   sprintf q[{ [...],  BlockLength => 4000000} (%s)], $_key);
            is( $_torrent_4000000->_block_length,           4000000,
                sprintf q[ ...BlockLength == 4000000 (%s)], $_key);
        }
        {

=status
           1 = Started  (New peers are accepted, etc.)
           2 = Checking (Currently hashchecking)
           4 = Start after Check*
           8 = Checked
          16 = Error*   (Activity is halted and may require user intervention)
          32 = Paused
          64 = Loaded
         128 = Queued   (Has an associated Net::BitTorrent parent)
 * Currently unused
=cut

            my %IS = qw[
                Started  1 Checking  2 StartAfterCheck  4 Checked   8
                Error   16 Paused   32 Loaded          64 Queued  128
            ];

            #
            ok($orphan_torrent->status, sprintf q[Status indicates... (%s)],
                $_key);
            warn $orphan_torrent->status;
            is($orphan_torrent->status, $IS{q[Loaded]},
                sprintf q[ ...torrent is loaded but has no parent. (%s)],
                $_key);

            #
            my $_torrent_Test =
                Net::BitTorrent::Torrent->new({Path   => $dot_torrent,
                                               Status => q[Test],
                                               Client => $client
                                              }
                );
            isa_ok($_torrent_Test, q[Net::BitTorrent::Torrent],
                   sprintf q[{ [...],  Status => 'Test'} (%s)], $_key);
            is( $_torrent_Test->status,
                ($IS{q[Started]} + $IS{q[Queued]} + $IS{q[Loaded]}),
                sprintf
                    q[ ...torrent is started (pending hashcheck) and loaded. (%s)],
                $_key
            );
            warn $_torrent_Test->status;
            ok($_torrent_Test->stop, sprintf q[Stop an active torrent (%s)],
                $_key);
            warn $_torrent_Test->status;
            ok($_torrent_Test->status ^ $IS{q[Started]},
                sprintf q[ ...Status says we're stopped. (%s)], $_key);
            ok($_torrent_Test->start, sprintf q[Start a stopped torrent (%s)],
                $_key);
            warn $_torrent_Test->status;
            ok($_torrent_Test->status & $IS{q[Started]},
                sprintf q[ ...Status says we're started. (%s)], $_key);
            ok($_torrent_Test->pause, sprintf q[Pause a torrent (%s)], $_key);
            warn $_torrent_Test->status;
            ok($_torrent_Test->pause & $IS{q[Paused]},
                sprintf q[ ...Status says we're paused. (%s)], $_key);
            ok($_torrent_Test->start, sprintf q[Start a paused torrent (%s)],
                $_key);
            warn $_torrent_Test->status;
            ok($_torrent_Test->status ^ $IS{q[Paused]},
                sprintf q[ ...Status says we're started. (%s)], $_key);

            #die;
            warn
                q[TODO: 'Bad' status like this one should fall back to default];
            my $_torrent_unchecked =    # Undocumented Status param
                Net::BitTorrent::Torrent->new(
                                         {Path   => $dot_torrent,
                                          Status => $IS{q[StartAfterCheck]}
                                         }
                );
            isa_ok($_torrent_unchecked,
                   q[Net::BitTorrent::Torrent],
                   sprintf q[{ [...],  Status => %d} (%s)],
                   $IS{q[StartAfterCheck]},
                   $_key
            );
            is( $_torrent_unchecked->status,
                $IS{q[Loaded]} + $IS{q[StartAfterCheck]},
                sprintf
                    q[ ...Status == %d (Loaded, Start after Check, Orphan) (%s)],
                $IS{q[Loaded]} + $IS{q[StartAfterCheck]},
                $_key
            );
            ok($_torrent_unchecked->hashcheck,
                sprintf q[ ...checking (%s)], $_key);
            is( $_torrent_unchecked->status,
                $IS{q[Loaded]} + $IS{q[Checked]},
                sprintf q[ ...Status == %d (Loaded, Checked, Orphan) (%s)],
                $IS{q[Loaded]} + $IS{q[Checked]},
                $_key
            );
        }

        # TODO: Test BaseDir param
        warn(q[TODO: Test BaseDir param]);

        #
        is($torrent->downloaded, 0, sprintf q[    downloaded == 0 (%s)],
            $_key);
        is( $torrent->_add_downloaded(1024),       1024,
            sprintf q[_add_downloaded(1024) (%s)], $_key);
        is( $torrent->_add_downloaded(-1024),       1024,
            sprintf q[_add_downloaded(-1024) (%s)], $_key);
        is( $torrent->_add_downloaded(q[dude]),      1024,
            sprintf q[_add_downloaded('dude') (%s)], $_key);
        is($torrent->downloaded, 1024, sprintf q[    downloaded == 1024 (%s)],
            $_key);
        is($torrent->uploaded, 0, sprintf q[    uploaded == 0 (%s)], $_key);
        is( $torrent->_add_uploaded(1024),       1024,
            sprintf q[_add_uploaded(1024) (%s)], $_key);
        is( $torrent->_add_uploaded(-1024),       1024,
            sprintf q[_add_uploaded(-1024) (%s)], $_key);
        is( $torrent->_add_uploaded(q[dude]),      1024,
            sprintf q[_add_uploaded('dude') (%s)], $_key);
        is($torrent->uploaded, 1024, sprintf q[    uploaded == 1024 (%s)],
            $_key);

       #
       #my $torrent_with_files = new_ok( # Requires Test::More 0.30+
       #              q[Net::BitTorrent::Torrent] => [{Path => $dot_torrent}],
       #              sprintf q[Created new object to play with (%s)], $_key);
        my $torrent_with_files
            = Net::BitTorrent::Torrent->new({Path => $dot_torrent});
        isa_ok($torrent_with_files, q[Net::BitTorrent::Torrent],
               sprintf q[Created new object to play with (%s)], $_key);
        my $_wanted = $torrent_with_files->_wanted;
        like(unpack(q[b*], $torrent_with_files->_wanted()), qr[^1+0*$],
             sprintf q[By default, we want everything (%s)], $_key);
        for my $file (@{$torrent_with_files->files()}) {
            $file->set_priority(0);
        }
        like(unpack(q[b*], $torrent_with_files->_wanted()), qr[^0+$],
             sprintf q[And now we want nothing (%s)], $_key);

        #
        warn q[TODO: _piece_by_index  # returns undef];
        is($torrent->_piece_by_index(),
            undef, sprintf q[_piece_by_index( ) requires an index (%s)],
            $_key);
        is($torrent->_piece_by_index(q[Nine!]),
            undef, sprintf q[_piece_by_index('%s') is just plain wrong (%s)],
            q[Nine!], $_key);
        is($torrent->_piece_by_index(0),
            undef, sprintf q[_piece_by_index(%d) doesn't exist yet (%s)],
            0, $_key);
        is( $torrent->_piece_by_index($torrent->_piece_count),
            undef,
            sprintf q[_piece_by_index(%d) doesn't exist yet (%s)],
            $torrent->_piece_count,
            $_key
        );
        is( $torrent->_piece_by_index($torrent->_piece_count + 1),
            undef,
            sprintf q[_piece_by_index(%d) will never exist (%s)],
            ($torrent->_piece_count + 1),
            $_key
        );
        is($torrent->_piece_by_index(-1),
            undef, sprintf q[_piece_by_index(%d) will never exist (%s)],
            -1, $_key);
        is($orphan_torrent->_piece_by_index(0),
            undef,
            sprintf q[_piece_by_index(%d) orphans never have pieces (%s)],
            (0), $_key);

#warn q[TODO: _pick_piece      # returns piece];
#is($torrent->_pick_piece(), undef, sprintf q[_pick_piece( ) requires an peer (%s)], $_key);
#is($torrent->_pick_piece(bless \%ENV, q[Net::BitTorrent::Peer]), undef, sprintf q[_pick_piece(%d) doesn't exist yet (%s)],0, $_key);
#is($torrent->_pick_piece($torrent->_piece_count), undef, sprintf q[_pick_piece(%d) doesn't exist yet (%s)],$torrent->_piece_count , $_key);
#is($torrent->_pick_piece($torrent->_piece_count + 1), undef, sprintf q[_pick_piece(%d) will never exist (%s)],($torrent->_piece_count + 1), $_key);
#die;
        warn q[TODO: _piece_by_index  # returns piece ...maybe];

#is($torrent->_piece_by_index(), undef, sprintf q[_piece_by_index( ) requires an index (%s)], $_key);
#is($torrent->_piece_by_index(0), undef, sprintf q[_piece_by_index(%d) doesn't exist yet (%s)],0, $_key);
#is($torrent->_piece_by_index($torrent->_piece_count), undef, sprintf q[_piece_by_index(%d) doesn't exist yet (%s)],$torrent->_piece_count , $_key);
#is($torrent->_piece_by_index($torrent->_piece_count + 1), undef, sprintf q[_piece_by_index(%d) will never exist (%s)],($torrent->_piece_count + 1), $_key);
        warn q[TODO: _write_data];
        warn q[TODO: _read_data];
        warn q[TODO: _check_piece_by_index];
        warn q[TODO: hashcheck];
        warn q[TODO: _add_tracker];
        warn q[TODO: _append_compact_nodes];
        warn q[TODO: _new_peer  # skipped if no tcp tests];
        warn q[TODO: _peers     # skipped if no tcp tests requires parent];
        warn q[We try something similar in the multi-threaded tests];
        my $_bitfield     = $torrent->bitfield();
        my $_new_bitfield = $_bitfield;
        vec($_new_bitfield, 0, 8) = 1;
        ok($torrent->_set_bitfield($_new_bitfield),
            sprintf q[Bitfield is being changed... (%s)], $_key);
        isn't($torrent->bitfield, $_bitfield,
              sprintf q[Bitfield is no longer default... (%s)], $_key);
        is($torrent->bitfield, $_new_bitfield,
            sprintf q[Bitfield has been faked. (%s)], $_key);
        is( $torrent->_set_bitfield(qq[\0] x 4),      undef,
            sprintf q[Bitfield is too short... (%s)], $_key);
        is($torrent->bitfield, $_new_bitfield,
            sprintf q[Bitfield has not changed. (%s)], $_key);
        ok( $torrent->_set_status($torrent->status | 2),    # Ruins everything
            sprintf q[Set status | 2 (hashchecking).. (%s)], $_key);
        is( $torrent->_set_bitfield($_bitfield),
            undef,
            sprintf
                q[Bitfield is is proper size but ignored due to status... (%s)],
            $_key
        );
        is($torrent->bitfield, $_new_bitfield,
            sprintf q[Bitfield has not changed. (%s)], $_key);
        is(scalar($torrent->_as_string()),
            1, sprintf q[_as_string( ) | scalar context (%s)], $_key);
        is(scalar($torrent->_as_string(1)),
            1, sprintf q[_as_string(1) | scalar context (%s)], $_key);
        ok($torrent->_as_string(),
            sprintf q[_as_string( ) | list context (%s)], $_key);
        ok($torrent->_as_string(1),
            sprintf q[_as_string(1) | list context (%s)], $_key);

        #
        warn q[Test multithreaded stuff...];
    SKIP: {
            skip q[Multi-threaded tests have been skipped], 5 if !$threads;
            skip
                q[Multi-threaded tests have been skipped. (Devel::Cover is not thread safe)],
                5
                if defined $Devel::Cover::VERSION;
            skip
                q[Multi-threaded tests have been skipped. (Devel::NYTProf is not thread safe)],
                5
                if $profile;
            use_ok(q[threads]);
            use_ok(q[threads::shared]);

            #
            my $_threaded_torrent
                = Net::BitTorrent::Torrent->new({Path => $dot_torrent});
            ok($_threaded_torrent->_set_status(4),
                sprintf q[Parent sets status to 4 (%s)], $_key);
            my $_bitfield = $_threaded_torrent->bitfield();
            threads->create(
                sub {
                    warn q[Change contents of cached bitfield...];
                    vec($_bitfield, 0, 8) = 1;
                    $_threaded_torrent->_set_bitfield($_bitfield);
                    $_threaded_torrent->_set_status(0);
                    return 1;
                }
            )->join();
            isn't($_threaded_torrent->bitfield,
                  $_bitfield,
                  sprintf q[Parent sees bitfield has changed (%s)], $_key);
            is($_threaded_torrent->status, 0,
                sprintf q[Parent reads status is now 0 (%s)], $_key);
        }
    }
}

# stuff that should be updated from time to time really should go away...
sub _infohash {
    my ($key) = @_;
    return {single    => q[98be9ffbf2876fee212607faefd91705fc047581],
            multi     => q[46ece60594afc29a0138a255660fe47521bb2966],
            miniswarm => q[2b3aaf361bd40540bf7e3bfd140b954b90e4dfbc],
            unicode   => q[c74fbd947223503fa16caec93ca24265698d8d5e]
    }->{$key};
}

sub _trackers {    # will not need to be updated
    my ($key) = @_;
    return (map { bless \$_->[0], q[Net::BitTorrent::Torrent::Tracker] }
               _raw_data($key)->{q[announce-list]}->[0]
            || _raw_data($key)->{q[announce]}
            || ());
}

sub _size {        # will not need to be updated
    my ($key) = @_;
    my $return = _raw_data($key)->{q[info]}{q[length]};
    $return
        || grep { $return += $_->{q[length]} }
        @{_raw_data($key)->{q[info]}{q[files]}};
    return $return;
}

sub _raw_data {
    my ($key) = @_;
    warn q[=======> $key == ] . $key;
    return {
        single => do {
            require MIME::Base64;
            {announce => q[http://borft.student.utwente.nl:6969/announce],
             q[announce-list] => [
                    [q[http://borft.student.utwente.nl:6969/announce],
                     q[http://core-tracker.enlist-a-distro.net:9800/announce],
                     q[http://core-tracker.depthstrike.com:9800/announce],
                     q[http://www.ooodev.org:6969/announce],
                    ],
             ],
             azureus_properties => {dht_backup_enable => 1},
             q[creation date]   => 1215078571,
             info               => {
                 q[length] => 122426547,
                 name =>
                     q[OOo_3.0.0beta2rc1_20080701_Win32Intel_install_en-US.exe],
                 q[piece length] => 131072,
                 pieces =>
                     MIME::Base64::decode(
                     q[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]
                     ),
             },
             nodes => [[q[router.bitcomet.com],   554],
                       [q[router.bittorrent.com], 6881],
                       [q[router.utorrent.com],   6881],
             ],
             sources => [
                 q[http://ftp5.gwdg.de/pub/openoffice/contrib/rc/3.0.0beta2rc1/OOo_3.0.0beta2rc1_20080701_Win32Intel_install_en-US.exe],
                 q[http://spout.ussg.indiana.edu/openoffice/contrib/rc/3.0.0beta2rc1/OOo_3.0.0beta2rc1_20080701_Win32Intel_install_en-US.exe],
             ],
            };
        },
        multi => {
             q[created by]    => q[uTorrent/1750],
             q[creation date] => 1198695914,
             encoding         => q[UTF-8],
             info             => {
                 files => [
                          {q[length] => 19, path => [qw[Directory File.txt]]},
                          {q[length] => 8,  path => [q[This is a test.txt]]},
                 ],
                 name            => q[tmp],
                 q[piece length] => 65536,
                 pieces =>
                     pack(q[H*], q[41e066c09e638fc15eb17205c48c911124a93169]),
             },
        },
        miniswarm => {
            comment          => q[See credit.txt for attributions.],
            q[created by]    => q[Net::BitTorrent::GenTorrent 0.1],
            q[creation date] => 1214665975,
            encoding         => q[UTF-8],
            info             => {
                files => [{q[length] => 28229,
                           path      => [q[1291672777_30adc6a421_o.jpg]]
                          },
                          {q[length] => 21769,
                           path      => [q[2183742557_5c9a91727d_m.jpg]]
                          },
                ],
                name            => q[seed],
                q[piece length] => 32768,
                pieces =>
                    pack(q[H*],
                    q[8836370e9e200bf3dee71f630296c456af8fa96bf0374921853dbdaa38a329a8fcde724e488dd413]
                    ),
            },
        },
        unicode => {
             comment       => q[Test of Unicode/utf8.  Especially on Win32.],
             q[created by] => q[uTorrent/180B],
             q[creation date] => 1211379829,
             encoding         => q[UTF-8],
             info             => {
                 q[length]       => 15,
                 name            => qq[\xE4\xB8\x89\xE5\x85\x89.txt],
                 q[piece length] => 65536,
                 pieces =>
                     pack(q[H*], q[e8b6c7cfe995e0953be668e851709f453f21dd90]),
             },
        }
    }->{$key};
}

# $Id$
