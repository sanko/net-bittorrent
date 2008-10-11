#!/usr/bin/perl -w
use strict;
use warnings;
use File::Find;
use Module::Build;
use 5.008001;

# ...
$|++;

# http://cpantest.grango.org/wiki/CPANAuthorNotes
my $automated_testing = $ENV{q[AUTOMATED_TESTING]}
    || $ENV{q[PERL_MM_USE_DEFAULT]};
my $is_developer = ((-d q[.svn]) ? 1 : 0) or ($ENV{RELEASE_TESTING} ? 1 : 0);
my $okay_tcp     = okay_tcp();
my $okay_udp     = okay_udp();
my $verbose = grep {m[^-v$]i} @ARGV;    # see M:B::API's args()
my @tests;
find \&find_cb, sort qw[t/000_miniswarm/ t/700_classes/];

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
   loopback has failed.  We'll work around the by skipping most of the
   socket-related tests.

   NOTE: Skipping these tests greatly reduces the usefullness of the
   Net::BitTorrent test suite and makes life difficult.
 ****************************************************************************
FAIL

#
my $mb = Module::Build->new(
    module_name => q[Net::BitTorrent],
    license     => q[artistic_2],        # requires MB 0.2808_01 or better
    dist_author       => q[Sanko Robinson <sanko@cpan.org>],
    dist_abstract     => q[BitTorrent peer-to-peer protocol],
    dist_version_from => q[lib/Net/BitTorrent/Version.pm],
    build_requires    => {
               q[Module::Build] => 0.30,        # dev on 0.30
                                                # min 0.2808_01 for artistic_2
               q[Test::More]    => 0.8,         # dev on 0.8
    },
    requires => {q[Cwd]           => 0,           # dev on 3.2701
                 q[Digest::SHA]   => 5.45,        # dev on 5.47|CORE in 5.10
                 q[Errno]         => 0,           # dev on 1.1
                 q[Exporter]      => 0,           # dev on 5.62
                 q[Fcntl]         => 0,           # dev on 1.06
                 q[File::Path]    => 0,           # dev on 2.04
                 q[File::Spec]    => 0,           # dev on 3.2701
                 q[Module::Build] => 0.30,        # dev on 0.30
                 q[perl]          => q[5.8.1],    # dev on 5.11
                 q[Scalar::Util]  => 1.19,        # dev on 1.19
                 q[Socket]        => 1.77,        # dev on 1.80
                 q[Time::HiRes]   => 0,           # dev on 1.9712
                 q[Test::More]    => 0.8,         # dev on 0.8
                 q[version]       => 0.74         # dev on 0.76|CORE in 5.10
    },
    recommends => {
        q[Data::Dumper] => 0,            # core...
             #q[Data::Dump] => 0,              # _should_ be core...
        ($^O eq q[MSWin32]
         ? (q[Encode] => 0,
            q[perl]   => q[5.8.7],
            q[utf8]   => 0,          # utf8::is_utf8 internal as of Perl 5.8.1
            q[Win32]  => 0,
            q[Win32API::File] => 0.10,
             )
         : ()
        )
    },
    test_files => \@tests,
    meta_merge => {
          resources => {
              bugtracker =>
                  q[http://code.google.com/p/net-bittorrent/issues/list],
              homepage => q[http://sankorobinson.com/net-bittorrent/],
              MailingList =>
                  q[http://groups.google.com/group/net-bittorrent-discuss],
              repository => q[http://net-bittorrent.googlecode.com/svn/trunk/]
          },
          keywords     => [qw[BitTorrent client peer p2p torrent socket dht]],
          generated_by => q[Sanko Robinson <sanko@cpan.org>]
    },
);

# Set some random state data for the .t files to get
$mb->notes(okay_tcp          => $okay_tcp);
$mb->notes(okay_udp          => $okay_udp);
$mb->notes(automated_testing => $automated_testing);
$mb->notes(release_testing   => $is_developer);
$mb->notes(verbose           => $verbose);
$mb->notes(test_suite        => \@tests);
$mb->notes(gmtime            => gmtime);

# M::B is lazy and leaves this file behind on Win32 but it needs that to wrap
# things up...
# $mb->add_to_cleanup(qw[./Build.bat]);
# I'm lazy and... well... yeah...
$mb->fix_shebang_line(@tests);
$mb->fix_shebang_line(@{($mb->rscan_dir(q[./lib], qr/\.pm$/))[0]});

# Break it off, let's go.
$mb->create_build_script;

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
    return unshift @tests, $File::Find::name;    # suite runs in 'reverse'
}

# $Id$
