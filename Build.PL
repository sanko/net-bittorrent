#!/usr/bin/perl -w
use strict;
use warnings;
use File::Find;
use Module::Build 0.30;
use 5.008_001;
use Config;    # Figure out if this perl supports threads

# ...
$|++;

# http://cpantest.grango.org/wiki/CPANAuthorNotes
my $automated_testing = $ENV{q[AUTOMATED_TESTING]}
    || $ENV{q[PERL_MM_USE_DEFAULT]};
my $is_developer = ((-d q[.svn]) ? 1 : 0) or ($ENV{RELEASE_TESTING} ? 1 : 0);
my $okay_tcp     = okay_tcp();
my $okay_udp     = okay_udp();
my @tests;
find \&find_cb, sort qw[t/000_miniswarm/ t/700_classes/];
@tests = reverse sort @tests;    # Test suite runs in reverse

#
print $okay_tcp || $okay_udp ? <<'FTW': <<'FAIL';
 ****************************************************************************
   During the test phase, we will be opening ports, contacting a tiny local
   tracker, and trading data to simulate actual swarms.  By design, the
   tests transfer only to the local system.

   NOTE: These tests may fail due to restrictive firewalling, solar flare
   activity, or other connectivity problems.
 ****************************************************************************
FTW
 ****************************************************************************
   Hrm... Your system seems to be misconfigured; an attempt to create a
   loopback has failed.  We'll work around this by skipping most of the
   socket-related tests.

   NOTE: Skipping these tests greatly reduces the usefullness of the
   Net::BitTorrent test suite and makes life (in general) difficult.
 ****************************************************************************
FAIL

#
my $mb = Module::Build->new(
    module_name => q[Net::BitTorrent],
    license     => q[artistic_2],        # requires MB 0.2808_01 or better
    dist_author       => q[Sanko Robinson <sanko@cpan.org>],
    dist_abstract     => q[BitTorrent peer-to-peer protocol],
    dist_version_from => q[lib/Net/BitTorrent/Version.pm],
    requires          => {
                 q[Cwd]           => 0,           # dev on 3.2701
                 q[Digest::SHA]   => 5.45,        # dev on 5.47|CORE in 5.10
                 q[Errno]         => 0,           # dev on 1.10
                 q[Exporter]      => 0,           # dev on 5.63
                 q[Fcntl]         => 0,           # dev on 1.06
                 q[File::Path]    => 0,           # dev on 2.04
                 q[File::Spec]    => 0,           # dev on 3.2701
                 q[Module::Build] => 0.30,        # dev on 0.30
                 q[perl]          => q[5.8.1],    # dev on 5.11.0
                 q[Scalar::Util]  => 1.19,        # dev on 1.19
                 q[Socket]        => 1.77,        # dev on 1.81
                 q[Test::More]    => 0.80,        # dev on 0.80
                 q[Time::HiRes]   => 0,           # dev on 1.9715
                 q[version]       => 0.74         # dev on 0.76|CORE in 5.10
    },
    build_requires => {q[Module::Build] => 0.30,        # dev on 0.30
                       q[Test::More]    => 0.80,        # dev on 0.80
    },
    recommends    => {q[Data::Dump] => 0},
    auto_features => {
        win32_utf8_support => {
            description => q[Support for Wide-char filenames on Windows],
            requires    => {
                         q[Encode]         => 0,           # dev on 2.26
                         q[perl]           => q[5.8.7],    # dev on 5.11.0
                         q[utf8]           => 0,           # dev on 1.07
                         q[Win32]          => 0,           # dev on 0.38
                         q[Win32API::File] => 0.10,        # dev on 0.1001_01
            },
        }
    },
    script_files => qw[./scripts/bittorrent.pl],
    test_files   => \@tests,
    meta_merge   => {
         resources => {
             bugtracker =>
                 q[http://code.google.com/p/net-bittorrent/issues/list],
             homepage => q[http://sankorobinson.com/net-bittorrent/],
             license => q[http://www.perlfoundation.org/artistic_license_2_0],
             MailingList =>
                 q[http://groups.google.com/group/net-bittorrent-discuss],
             repository =>
                 q[http://code.google.com/p/net-bittorrent/source/browse/]
         },
         keywords     => [qw[BitTorrent client peer p2p torrent socket dht]],
         generated_by => q[Sanko Robinson <sanko@cpan.org>]
    },
);

# Set some random state data for the .t files to get later
$mb->notes(okay_tcp          => $okay_tcp);
$mb->notes(okay_udp          => $okay_udp);
$mb->notes(automated_testing => $automated_testing ? 1 : 0);
$mb->notes(release_testing   => $is_developer);
$mb->notes(test_suite        => \@tests);
$mb->notes(gmtime            => gmtime);
$mb->notes(verbose => scalar grep {m[^v$]} keys %{$mb->args()});
$mb->notes(threads => $Config::Config{q[useithreads]} ? 1 : 0);

# M::B is lazy and leaves this file behind on Win32 but it needs that to wrap
# things up...
# $mb->add_to_cleanup(qw[./Build.bat]);
# I'm lazy and... well... yeah...
#$mb->fix_shebang_line(@tests);
#$mb->fix_shebang_line(@{($mb->rscan_dir(q[./lib], qr[\.pm$]))[0]});
# Break it off, let's go.
$mb->create_build_script;
exit 0;

# Taken from LWP
sub okay_tcp {
    return 0 if not -f q[t/900_data/910_scripts/TCP-talk-to-ourself.pl];
    system(qq["$^X" t/900_data/910_scripts/TCP-talk-to-ourself.pl]);
    return $? ? 0 : 1;
}

sub okay_udp {
    return 0 if not -f q[t/900_data/910_scripts/UDP-talk-to-ourself.pl];
    system(qq["$^X" t/900_data/910_scripts/UDP-talk-to-ourself.pl]);
    return $? ? 0 : 1;
}

# Find only .t files
sub find_cb {
    return if -d $_ or -l $_;
    return unless -T $_;
    return unless $_ =~ m[.+\.t$];
    return push @tests, $File::Find::name;
}

=pod

=head1 Author

Sanko Robinson <sanko@cpan.org> - http://sankorobinson.com/

CPAN ID: SANKO

=head1 License and Legal

Copyright (C) 2008 by Sanko Robinson E<lt>sanko@cpan.orgE<gt>

This program is free software; you can redistribute it and/or modify
it under the terms of The Artistic License 2.0.  See the F<LICENSE>
file included with this distribution or
http://www.perlfoundation.org/artistic_license_2_0.  For
clarification, see http://www.perlfoundation.org/artistic_2_0_notes.

When separated from the distribution, all POD documentation is covered
by the Creative Commons Attribution-Share Alike 3.0 License.  See
http://creativecommons.org/licenses/by-sa/3.0/us/legalcode.  For
clarification, see http://creativecommons.org/licenses/by-sa/3.0/us/.

Neither this module nor the L<Author|/Author> is affiliated with
BitTorrent, Inc.

=for svn $Id$

=cut
