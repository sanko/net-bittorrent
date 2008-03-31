#!/usr/bin/perl -w

# /t/000_basic/002_load.t
#
# $Id$

use strict;
use warnings;

use File::Spec;
use File::Find;
use Test::More;
use constant DISTRIBUTION => q[Net::BitTorrent];

# Set this to true if all packages have the same version number
use constant CHECK_VERSION => 0;

sub file_to_pm {
    my ( $dir, $file ) = @_;
    $file =~ s/\.pm$// || return;    # we only care about .pm files
    $file =~ s{\\}{/}g;              # to make win32 happy
    $dir  =~ s{\\}{/}g;              # to make win32 happy
    $file =~ s/^$dir//;
    my $_package
        = join '::' => grep $_ => File::Spec->splitdir($file);

    # untaint that puppy!
    my ($package) = $_package =~ /^([\w]+(?:::[\w]+)*)$/;
    return DISTRIBUTION eq $package ? () : $package;
}

BEGIN {
    my $dir = 'lib';

    my @classes;
    find(
        {  no_chdir => 1,      # keeps it taint safe
           wanted   => sub {
               -f
                   && /\.pm$/
                   && push @classes =>
                   file_to_pm( $dir, $File::Find::name );
               }
        },
        $dir,
    );
    my $tests_per_class = CHECK_VERSION ? 2 : 1;
    plan tests => $tests_per_class + $tests_per_class * @classes;

    foreach my $class ( DISTRIBUTION, sort @classes ) {
        use_ok $class or BAIL_OUT("Could not load $class");
        if (CHECK_VERSION) {
            is $class->VERSION, DISTRIBUTION->VERSION,
                "... and $class should have the correct version";
        }
    }

 #diag("Testing Test::Harness $Test::Harness::VERSION, Perl $], $^X");
}
