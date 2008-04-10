#!/usr/bin/perl -w

# /t/000_basic/010_load_prerequisites.t
#
# $Id$

use strict;
use warnings;

use Test::More qw[no_plan];

BEGIN {
    use_ok(q[Carp]);
    use_ok(q[Cwd]);
    use_ok(q[Digest::SHA]);
    use_ok(q[Exporter]);
    use_ok(q[Fcntl]);
    use_ok(q[File::Spec]);
    use_ok(q[overload]);
    use_ok(q[version]);
    if ( $^O eq q[MSWin32] ) {
        use_ok(q[Encode]);
        use_ok(q[utf8]);
        use_ok(q[Win32API::File]);
    }
}

1;
