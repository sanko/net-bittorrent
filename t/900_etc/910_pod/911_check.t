# -*- perl -*-

# t/900_etc/910_pod/911_check.t -
# $Id$

use strict;
use warnings;

use Test::More;

if ( not $ENV{RELEASE_TESTING} ) {
    plan( skip_all =>
        q[Release test.  Set $ENV{RELEASE_TESTING} to a true value to run.]
    );
}

eval q[use Test::Pod 1.00];

plan skip_all => q[Test::Pod 1.00 required for testing POD] if $@;
all_pod_files_ok();
