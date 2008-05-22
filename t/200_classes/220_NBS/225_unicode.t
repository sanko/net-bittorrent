# -*- perl -*-
# t/200_classes/220_NBS/225_unicode.t
# $Id$
use strict;
use warnings;
use Test::More tests => 9;
use File::Temp qw[tempdir];
use Net::BitTorrent::Session;
{

    package Fake::BitTorrent;    # Real N::B will try to open a port
    sub new { return bless [q[Fake Net::BitTorrent object!]]; }
    sub _connections { }
    sub _do_callback { }
    sub _set_pulse   { }
    sub _del_pulse   { }
}
my $client = new Fake::BitTorrent;
SKIP: {    #skip q[Internal changes not yet reflected in tests], 9;
SKIP: {
        skip q[Failed to create Fake::BitTorrent object], 9
            unless defined $client
                and ref $client eq q[Fake::BitTorrent];
    SKIP: {    # cwd is messed up while testing locally
            skip q[Cannot find unicode.torrent], 9
                unless -f q[./t/data/torrents/unicode.torrent];
            my $session = new Net::BitTorrent::Session(
                {   path   => q[./t/data/torrents/unicode.torrent],
                    client => $client,

                    #skip_hashcheck => 1,
                    base_dir => tempdir(CLEANUP => 1)
                }
            );
            ok( defined($session)
                    && ref $session eq q[Net::BitTorrent::Session],
                q[Session started]
            );
            is(  # hash of metainfo; if this is good, nothing else should fail
                $$session,
                q[c74fbd947223503fa16caec93ca24265698d8d5e],
                q[bdecode | Infohash]
            );
            my $piece = ($session->pieces())[0]->[0];
            ok($piece->_write(q[Sanko     here.]), q[W | All data (bad)]);
            isn't($piece->verify, 1, q[H | Fail]);
            ok($piece->_write(q[was], 6), q[W | Missing data (good)]);
            is($piece->verify, 1, q[H | Pass]);
            is($piece->_read(0, 5), q[Sanko], q[R | My name]);
            is($piece->_read(80), undef, q[R | Reading too much data failed]);
            ok($session->close_files, q[Close open files]);
        }
    }
}
1;
