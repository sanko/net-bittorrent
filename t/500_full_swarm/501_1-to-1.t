# -*- perl -*-
# /t/500_full_swarm/501_1-to-1.t - Miniature swarm of 1 seed and 1 new peer
# $Id$
system qq["$^X" t/data/etc/http_miniswarm.pl 1 1 30];
use Test::More;
if ($? == -1) {
    diag sprintf q[failed to execute: %s], $!;
}
elsif ($? & 127) {
    diag sprintf q[child died with signal %d, %s coredump],
        ($? & 127), ($? & 128) ? q[with] : q[without];
}
