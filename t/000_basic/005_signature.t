# -*- perl -*-
# /t/000_basic/005_signature.t - verify signed distribution
# $Id$
use strict;
use warnings;
use Test::More;

plan skip_all => q[TODO];

#if ( not $ENV{TEST_AUTHOR} ) {
#    plan( skip_all =>
#        q[Author test.  Set $ENV{TEST_AUTHOR} to a true value to run.]
#    );
#}
if (!eval { require Module::Signature; 1 }) {
    plan skip_all =>
        q[Module::Signature required to verify distribution];
}
elsif (!-e q[SIGNATURE]) {
    plan skip_all => q[SIGNATURE not found];
}
elsif (-s q[SIGNATURE] == 0) {
    plan skip_all => q[SIGNATURE file empty];
}
elsif (!eval { require Socket; Socket::inet_aton(q[pgp.mit.edu]) }) {
    plan skip_all =>
        q[Cannot connect to the keyserver to check module signature];
}
else {
    plan tests => 1;
}
unlink(qw[pod2htmd.tmp pod2htmi.tmp]);    # Thanks, M::B! >.>


my $ret = Module::Signature::verify({skip=>q[Build.bat]});
SKIP: {
    skip q[Module::Signature cannot verify], 1
        if $ret eq Module::Signature::CANNOT_VERIFY();
    cmp_ok $ret, q[==], Module::Signature::SIGNATURE_OK(),
        q[Valid signature];
}
