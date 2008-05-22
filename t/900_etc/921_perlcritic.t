# -*- perl -*-

# t/900_etc/921_perlcritic.t -
# $Id$

use strict;
use warnings;
use File::Spec;
use Test::More;

if ( not $ENV{RELEASE_TESTING} ) {
    my $msg
        = 'Release test.  Set $ENV{RELEASE_TESTING} to a true value to run.';
    plan( skip_all => $msg );
}

eval { require Test::Perl::Critic; };

if ($@) {
    my $msg = 'Test::Perl::Critic required to criticise code';
    plan( skip_all => $msg );
}

my $rcfile
    = File::Spec->catfile( q[t], q[data], q[etc], q[perlcritic.rc] );
Test::Perl::Critic->import( -profile => $rcfile );
all_critic_ok();
