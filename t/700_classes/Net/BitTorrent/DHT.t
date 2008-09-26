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

# Make sure the path is correct
chdir q[../../../../] if not -f $simple_dot_torrent;
#

my $build = Module::Build->current;
my $can_talk_to_ourself = $build->notes(q[can_talk_to_ourself]);

#
my ($flux_capacitor, %peers) = (0, ());

#
BEGIN {
    use Test::More;
    plan tests => 4;

    # Ours
    use_ok(q[File::Temp],   qw[tempdir]);
    use_ok(q[Scalar::Util], qw[/weak/]);

    # Mine
    use_ok(q[Net::BitTorrent]);
    use_ok(q[Net::BitTorrent::Session]);
}
SKIP: {
    my ($tempdir)
        = tempdir(q[~NBSF_test_XXXXXXXX], CLEANUP => 1, TMPDIR => 1);
    diag(
           sprintf(q[File::Temp created '%s' for us to play with], $tempdir));
    my $client = Net::BitTorrent->new({LocalHost => q[127.0.0.1]});
    if (!$client) {
        diag(sprintf q[Socket error: [%d] %s], $!, $!);
        skip((      $test_builder->{q[Expected_Tests]}
                  - $test_builder->{q[Curr_Test]}
             ),
             q[Failed to create client]
        );
    }
    my $session = $client->add_session({Path    => $simple_dot_torrent,
                                        BaseDir => $tempdir
                                       }
    );
    warn sprintf q[%d|%d], 4, $test_builder->{q[Curr_Test]};

    END {
        return if not defined $session;
        for my $file (@{$session->files}) { $file->_close() }
    }

    #
    diag(q[TODO: Install event handlers]);

    #
}
