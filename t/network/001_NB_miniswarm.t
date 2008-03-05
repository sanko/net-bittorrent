#!/usr/bin/perl -w

# /t/network/001_NB_miniswarm.t - Miniature swarm of 4 seeds and 10 new peers
# This .t tests EVERYTHING.
# $Id$

use strict;
use warnings;

use lib q[../../lib];
use Test::More tests => 20;
BEGIN { use_ok(q[Net::BitTorrent]); }
BEGIN { use_ok(q[Net::BitTorrent::Util]); }
use File::Temp qw[];

$|++;

#$Net::BitTorrent::DEBUG=1;

my %client;
my $complete = 0;

for my $chr ( q[A] .. q[D] ) {
    $client{ q[seed_] . $chr } =
      new Net::BitTorrent( { LocalAddr => q[127.0.0.1], Timeout => 0.1 } );

    my $session = $client{ q[seed_] . $chr }->add_session(
        {
            path     => q[./t/etc/miniswarm.torrent],
            base_dir => q[./t/etc/miniswarm/]
        }
      )
      or fail( sprintf( q[Failed to load session for seed_%s], $chr ) )
      and next;
    ok( scalar( $session->complete ), sprintf( q[seed_%s ok], $chr ) );
}

for my $chr ( q[a] .. q[j] ) {
    $client{$chr} =
      new Net::BitTorrent( { LocalAddr => q[127.0.0.1], Timeout => 0.1 } );
    $client{$chr}->set_callback_on_piece_hash_pass(
        sub {
            my ( $self, $piece ) = @_;
            my $session    = $piece->session;
            my $completion = (
                (
                    (
                        (
                            scalar grep { $_->priority and $_->check }
                              @{ $session->pieces }
                        ) / ( scalar @{ $session->pieces } )
                    )
                ) * 100
            );
            my $line = sprintf(
                q[(%s|%02d) [%s] %.2f%%],
                $chr,
                $piece->index,
                join(
                    q[],
                    map ( (
                              $_->check
                            ? $piece->index == $_->index
                                  ? q[*]
                                  : q[|]
                            : scalar $_->working ? q[.]
                            : q[ ]
                        ),
                        @{ $session->pieces } )
                ),
                $completion
            );
            diag($line);
            $complete++ if $session->complete;
            return;
        }
    );
    $client{$chr}->add_session(
        {
            path     => q[./t/etc/miniswarm.torrent],
            base_dir => File::Temp::tempdir(
                sprintf( q[miniswarm_%s_XXXX], $chr ),
                CLEANUP => 1,
                TMPDIR  => 1
            ),
            skip_hashcheck => 1
        }
    ) or fail( sprintf( q[Failed to load session for client_%s], $chr ) );
}
my $nodes = Net::BitTorrent::Util::compact( [ map { $$_ } values %client ] );
grep { $_->sessions->[0]->append_nodes($nodes) if scalar @{ $_->sessions } }
  values %client;

diag(q[Setting up miniswarm. Please wait...]);

#my $tb = Test::More->builder;
#while ($tb->{q[Curr_Test]} < 16) {
while ( $complete < 10 ) {
    grep { $_->do_one_loop } values %client;
}
grep {
    ok( $_->sessions->[0]->complete );
    $_->remove_session( $_->sessions->[0] ) if scalar @{ $_->sessions }
} values %client;
