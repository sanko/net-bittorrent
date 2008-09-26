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
my $credits_dot_txt    = q[./t/900_data/950_torrents/credits.txt];
# Make sure the path is correct
chdir q[../../../../] if not -f $simple_dot_torrent;
#

my $build = Module::Build->current;
my $can_talk_to_ourself = $build->notes(q[can_talk_to_ourself]);



# Make sure the path is correct
chdir q[../../../../] if not -f $simple_dot_torrent;

#
my ($flux_capacitor, %peers) = (0, ());

#
BEGIN {
    use Test::More;
    plan tests => 41;
$SIG{__WARN__} = sub {}; # Quiet Carp
    # Ours
    use_ok(q[File::Temp],            qw[tempdir]);
    use_ok(q[Scalar::Util],          qw[/weak/]);
    use_ok(q[File::Spec::Functions], qw[rel2abs]);

    # Mine
    use_ok(q[Net::BitTorrent]);
    use_ok(q[Net::BitTorrent::Session]);
    use_ok(q[Net::BitTorrent::Util]);
}
my ($tempdir) = tempdir(q[~NBSF_test_XXXXXXXX], CLEANUP => 1, TMPDIR => 1);
 diag(
           sprintf(q[File::Temp created '%s' for us to play with], $tempdir));
my $client = Net::BitTorrent->new({LocalHost => q[127.0.0.1]});
if (!$client) {
     diag(sprintf q[Socket error: [%d] %s], $!, $!);
    skip(($test_builder->{q[Expected_Tests]} - $test_builder->{q[Curr_Test]}),
         q[Failed to create client]
    );
}
my $session = $client->add_session({Path    => $simple_dot_torrent,
                                    BaseDir => $tempdir
                                   }
);
warn sprintf q[%d|%d], 6, $test_builder->{q[Curr_Test]};

END {
    return if not defined $session;
    for my $file (@{$session->files}) { $file->_close() }
}

#
diag(q[Very simple .torrent file]);
is($session->path,
    rel2abs($simple_dot_torrent),
    q[Absolute paths returned from path()]);
is_deeply($session->trackers, [], q[Empty list of trackers]);
is($session->_complete,      0,     q[Download is incomplete]);
is($session->_downloaded,   0,     q[Nothing has been downloaded]);
is($session->_uploaded,     0,     q[Nothing has been uploaded]);
is($session->size,          49998, q[Full size is correct]);
is($session->_block_length, 16384, q[Block size is correct]);
is($session->_piece_length, 32768, q[Piece size is correct]);
is($session->_private,      0,     q[.torrent is not private]);
is($session->infohash,
    q[2b3aaf361bd40540bf7e3bfd140b954b90e4dfbc],
    q[Infohash checks out]);
is($session->bitfield,       chr(0),  q[Bitfield is correct]);
is($session->_client,        $client, q[Client is correct]);
is($session->_compact_nodes, q[],     q[Empty list of compact nodes]);

# Try bad stuff
is(Net::BitTorrent::Session->new(), undef, q[Requires a set of parameters]);
is(Net::BitTorrent::Session->new(q[FAIL!]),
    undef, q[Requires parameters to be passed as a hashref]);
is(Net::BitTorrent::Session->new({}), undef, q[Requires a 'Path' parameter]);
is(Net::BitTorrent::Session->new({Path => q[]}),
    undef, q[! -f Path parameter]);
is(Net::BitTorrent::Session->new({Path => $simple_dot_torrent}),
    undef, q[Requires a 'Client' parameter]);
is( Net::BitTorrent::Session->new(
                                {Path => $simple_dot_torrent, Client => undef}
    ),
    undef,
    q[Requires a 'Client' parameter (2)]
);
is( Net::BitTorrent::Session->new(
                              {Path => $simple_dot_torrent, Client => q[Test]}
    ),
    undef,
    q[Requires a blessed 'Client' object]
);
is( Net::BitTorrent::Session->new({Path   => $simple_dot_torrent,
                                   Client => bless(\{}, q[Not::BitTorrent])
                                  }
    ),
    undef,
    q[Requires a blessed 'Client' object (2)]
);
isa_ok(Net::BitTorrent::Session->new(
                              {Path => $simple_dot_torrent, Client => $client}
       ),
       q[Net::BitTorrent::Session],
       q[Requires a blessed 'Client' object (3)]
);

# Undocumented BlockLength parameter tests
isa_ok(Net::BitTorrent::Session->new({Path        => $simple_dot_torrent,
                                      Client      => $client,
                                      BlockLength => q[Test]
                                     }
       ),
       q[Net::BitTorrent::Session],
       q[Undocumented BlockLength parameter]
);
isa_ok(Net::BitTorrent::Session->new({Path        => $simple_dot_torrent,
                                      Client      => $client,
                                      BlockLength => 4000000
                                     }
       ),
       q[Net::BitTorrent::Session],
       q[Undocumented BlockLength parameter]
);

# TODO: Test BaseDir param
diag(q[TODO: Test BaseDir param]);

#
is( Net::BitTorrent::Session->new(
                                 {Path => $credits_dot_txt, Client => $client}
    ),
    undef,
    q[.torrent's data is malformed]
);

# Back to the original...
is($session->_downloaded, 0,           q[    _downloaded == 0]);
is($session->_add_downloaded(1024), 1024,  q[_add_downloaded(1024)]);
is($session->_add_downloaded(-1024), 1024, q[_add_downloaded(-1024)]);
is($session->_add_downloaded('dude'), 1024, q[_add_downloaded('dude')]);
is($session->_downloaded, 1024,           q[    _downloaded == 1024]);

is($session->_uploaded, 0,           q[    _uploaded == 0]);
is($session->_add_uploaded(1024), 1024,  q[_add_uploaded(1024)]);
is($session->_add_uploaded(-1024), 1024, q[_add_uploaded(-1024)]);
is($session->_add_uploaded('dude'), 1024, q[_add_uploaded('dude')]);
is($session->_uploaded, 1024,           q[    _uploaded == 1024]);



