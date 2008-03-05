# -*- perl -*-

# t/004_NBS_file_IO.t - If these fail, _nothing_ will work.
# $Id$

use strict;
use warnings;

use Test::More tests => 21;
use File::Temp qw[tempdir];

BEGIN { use_ok(q[Net::BitTorrent::Session]) }

{

    package Fake::BitTorrent;    # Real N::B will try to open a port
    sub new { return bless [q[Fake Net::BitTorrent object!]]; }
    sub callback_log { diag($_[1]->{q[message]}); }
    sub _do_callback { }
}

my $client = new Fake::BitTorrent;

SKIP: {
    skip q[Internal changes not yet reflected in tests], 20;
  SKIP: {
        skip q[Failed to create Fake::BitTorrent object], 20
          unless defined $client and ref $client eq q[Fake::BitTorrent];
      SKIP: {    # cwd is messed up while testing locally
            skip q[Cannot find example-A.torrent], 20
              unless -f q[./t/etc/example-A.torrent];
            my $session = new Net::BitTorrent::Session(
                {   path           => q[./t/etc/example-A.torrent],
                    client         => $client,
                    skip_hashcheck => 1,
                    base_dir       => tempdir(CLEANUP => 1)
                }
            );
            ok(
                defined($session)
                  && ref $session eq q[Net::BitTorrent::Session],
                q[Session started]
            );
            is(    # hash of metainfo; if this is good, nothing else should fail
                $$session,
                q[46ece60594afc29a0138a255660fe47521bb2966],
                q[bdecode | Infohash]
            );

            # delete the files if they exist
            unlink map { $_->path } @{$session->files};
            ok(
                $session->write(0, q[hin bad data here], 3),
                q[W | Middle of file one (bad data)]
            );
            ok($session->write(0, q[Not]), q[W | Start file one]);
            isn't($session->write(0, q[g of any use perlfdsafdsafdsafda], 6),
                1, q[W | Fail to write too much data]);
            ok($session->write(0, q[g of any use.T], 6),
                q[W | Span files one and two]);
            ok(
                $session->write(0, q[e. es    .], 17),
                q[W | Span files one and two (bad data)]
            );
            ok($session->write(0, q[T],    19), q[W | Begin file two]);
            ok($session->write(0, q[ting], 22), q[W | Fill in the blank]);
            ok($session->hashcheck, q[H | Pass]);
            ok($session->write(0, qq[\0] x 27), q[W | Fill with nulls]);
            is($session->hashcheck, 0, q[H | Fail]);
            ok($session->write(0, q[Nothing of any use.Testing.]),
                q[W | Entire block]);
            ok($session->hashcheck, q[H | Pass]);
            is(
                $session->_read(0, 27),
                q[Nothing of any use.Testing.],
                q[R | Entire block]
            );
            is($session->_read(0, 80),
                undef, q[R | Reading too much data failed]);
            is($session->_read(0, 6, 15),
                q[use.Te], q[R | Span files one and two]);
            is($session->_read(0, 5, 6),  q[g of ],    q[R | Part of file one]);
            is($session->_read(0, 8, 19), q[Testing.], q[R | All of file two]);
            is($session->hashcheck, 1, q[H | Pass]);
        }
    }
}
1
