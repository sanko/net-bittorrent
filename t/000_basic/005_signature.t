#!/usr/bin/perl -w

# /t/000_basic/005_signature.t
#
# $Id$

use strict;
use warnings;

use Test::More tests => 1;

eval { require Test::Signature; };

if ($@) {
    my $msg = 'Test::Signature required to verify distribution';
    plan( skip_all => $msg );
}

Test::Signature::signature_ok();
