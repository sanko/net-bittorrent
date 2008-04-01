# -*- perl -*-

# t/200_classes/220_NBS/221_new.t
# $Id$

use strict;
use warnings;

#use lib q[../../../lib/];chdir q[../../../];

use Test::More tests => 19;
use File::Temp qw[tempdir];

use Net::BitTorrent::Session;

{

    package Fake::BitTorrent;    # Real N::B will try to open a port
    sub new { return bless [q[Fake Net::BitTorrent object!]]; }
    sub connections  { }
    sub _do_callback { }
    sub use_unicode  { }
}

my $client = new Fake::BitTorrent;

SKIP: {

    #skip q[Internal changes not yet reflected in tests], 19;
SKIP: {
        skip q[Failed to create Fake::BitTorrent object], 19
            unless defined $client
                and ref $client eq q[Fake::BitTorrent];
    SKIP: {    # cwd is messed up while testing locally
            skip q[Cannot find example-A.torrent], 19
                unless -f q[./t/data/torrents/example-A.torrent];
            my $session =
                new Net::BitTorrent::Session(
                              { path => q[./t/data/torrents/example-A.torrent],
                                client         => $client,
                                skip_hashcheck => 1,
                                base_dir => tempdir( CLEANUP => 1 )
                              }
                );
            ok( defined($session)
                    && ref $session eq q[Net::BitTorrent::Session],
                q[Session started]
            );
            is( # hash of metainfo; if this is good, nothing else should fail
                $$session,
                q[46ece60594afc29a0138a255660fe47521bb2966],
                q[bdecode | Infohash]
            );

            my @pieces = $session->pieces();
            my $piece  = $pieces[0][0];

            ok( $piece->_write( q[hin bad data here], 3 ),
                q[W | Middle of file one (bad data)] );
            ok( $piece->_write(q[Not]), q[W | Start file one] );
            isn't( $piece->_write(
                                q[g of any use perlfdsafdsafdsafda], 6
                   ),
                   1,
                   q[W | Fail to write too much data]
            );
            ok( $piece->_write( q[g of any use.T], 6 ),
                q[W | Span files one and two] );
            ok( $piece->_write( q[e. es    .], 17 ),
                q[W | Span files one and two (bad data)] );
            ok( $piece->_write( q[ting], 22 ),
                q[W | Fill in the blank] );
            isn't( $piece->verify, 1, q[H | Pass] );
            ok( $piece->_write( qq[\0] x 27 ),
                q[W | Fill with nulls] );
            ok( $piece->_write(q[Nothing of any use.Testing.]),
                q[W | Entire block] );
            ok( $piece->verify, q[H | Pass] );
            is( $piece->_read( 0, 27 ),
                q[Nothing of any use.Testing.],
                q[R | Entire block] );
            is( $piece->_read(80), undef,
                q[R | Reading too much data failed] );
            is( $piece->_read( 6, 15 ),
                q[g of any use.Te],
                q[R | Span files one and two] );
            is( $piece->_read( 5, 6 ),
                q[ng of ], q[R | Part of file one] );
            is( $piece->_read( 18, 9 ),
                q[.Testing.],
                q[R | All of file two]
            );
            ok( $piece->verify,        q[H | Pass] );
            ok( $session->close_files, q[Close open files] );
        }
    }
}
1;
