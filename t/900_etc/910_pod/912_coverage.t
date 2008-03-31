# -*- perl -*-

# t/900_etc/910_pod/912_coverge.t -
# $Id$

use strict;
use warnings;

use Test::More;

if ( not $ENV{TEST_AUTHOR} ) {
    plan( skip_all =>
        q[Author test.  Set $ENV{TEST_AUTHOR} to a true value to run.]
    );
}

plan( skip_all => q[POD coverage is on my TODO list] );

eval {qq[use Test::Pod::Coverage;]} and 1;

if (    $@
     or ( not defined $Test::Pod::Coverage::VERSION )
     or ( $Test::Pod::Coverage::VERSION < 1.08 ) )
{
    plan( skip_all =>
         q[Test::Pod::Coverage 1.06 required for testing POD coverage]
    );
}
elsif (    ( not defined $Pod::Coverage::VERSION )
        or ( $Pod::Coverage::VERSION < 0.19 ) )
{
    plan( skip_all =>
          q[Pod::Coverage 0.19 required for testing POD coverage] );
}

all_pod_coverage_ok();
