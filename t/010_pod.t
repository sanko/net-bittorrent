# -*- perl -*-

# t/001_pod.t - check for POD errors
# $Id$

use Test::More;

eval q[use Test::Pod 1.00];

plan skip_all => q[Test::Pod 1.00 required for testing POD] if $@;
all_pod_files_ok();
