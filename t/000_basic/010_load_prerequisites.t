#!/usr/bin/perl -w
# /t/000_basic/010_load_prerequisites.t
#
# $Id$
use strict;
use warnings;
use Test::More qw[no_plan];

BEGIN {
    use_ok(q[Exporter]);
    use_ok(q[File::Spec]);
    use_ok(q[File::Path]);
    use_ok(q[Cwd]);
    use_ok(q[Time::HiRes]);
    use_ok(q[Fcntl]);
    use_ok(q[Digest::SHA]);
    use_ok(q[version]);
    use_ok(q[Scalar::Util]);
    use_ok(q[Socket]);
    #use_ok(q[Module::Build]);  #...it's already loaded
    #use_ok(q[Test::More]);     #...so is this
    #{                          # these modules have been moved to 'recommends'
        #if ($^O eq q[MSWin32]) {
        #    use_ok(q[Encode]);
        #    use_ok(q[utf8]);
        #    use_ok(q[Win32API::File]);
        #    use_ok(q[Win32]);
        #}
        #if ($ENV{RELEASE_TESTING}) {
        #    use_ok(q[Test::Perl::Critic]);
        #    use_ok(q[Test::Pod]);
        #    use_ok(q[Test::Pod::Coverage]);
        #    use_ok(q[Pod::Coverage]);
        #    use_ok(q[Test::Signature]);
        #    use_ok(q[Module::Signature]);
        #}
    #}
}
1;
