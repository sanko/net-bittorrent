#!/usr/bin/perl -w
use strict;
use warnings;
use File::Find;
use Module::Build;
my @search = qw[t/000_basic/ t/100_data_parsing/
    t/200_classes/ t/400_broken_data/ t/900_etc/];
if (    # http://cpantest.grango.org/wiki/CPANAuthorNotes
    not $ENV{q[AUTOMATED_TESTING]}
    and can_talk_to_ourself()
    and local $| = 1
    and print(
        q[
  *****************************************************************
   During the test phase, we may run tests that open ports and
   simulate typical swarms.  By design, no data will be
   transferred to remote systems during these tests.

   NOTE: These tests can be painfully slow, redundant, and may
   fail due to restrictive firewalling, solar flare activity or
   other connectivity problems.
  *****************************************************************

Do you want to perform these tests in addition to core tests? [no] ]
    )
    and readline(STDIN) =~ m[^\s*(y)]i
    )
{   push @search, q[t/500_full_swarm/];
}
my @tests;
find \&find_cb, sort @search;
my $mb = Module::Build->new(
    module_name       => q[Net::BitTorrent],
    license           => q[perl],
    dist_author       => q[Sanko Robinson <sanko@cpan.org>],
    dist_abstract     => q[BitTorrent peer-to-peer protocol],
    dist_version_from => q[lib/Net/BitTorrent.pm],

    #create_makefile_pl => q[passthrough],
    sign => 0,    # chokes on build.bat on win32
    build_requires => {
        q[Module::Build] => 0.2805,    # non-CORE
        q[Test::More]    => 0.72       # dev on 0.74
                                       #q[Test::Signature]   => 1.04,
                                       #q[Module::Signature] => 0.22,
    },
    requires => {
             q[Exporter]     => 0,          # dev on 5.62
             q[File::Spec]   => 0,          # dev on 3.2701
             q[Carp]         => 0,          # dev on
             q[Cwd]          => 0,          # dev on
             q[Time::HiRes]  => 0,          # dev on 1.9712
             q[Fcntl]        => 0,          # dev on 1.06
             q[Digest::SHA]  => 5.45,       # dev on 5.45|CORE in 5.10
             q[version]      => 0.74,       # dev on 0.74|CORE in 5.10
             q[Carp]         => 1.08,       # dev on 1.08
             q[Scalar::Util] => 1.19,       # dev on 1.19
             q[Socket]       => 1.80,       # dev on 1.80
    },
    recommends => {
        q[Data::Dumper] => 0,
        ($^O eq q[MSWin32]
         ? (q[Encode] => 0,
            q[utf8]   => 0,  # utf8::is_utf8 internal as of Perl 5.8.1
            q[Win32API::File] => 0.10
             )
         : ()
        )
    },
    test_files => \@tests,
    meta_merge => {
        resources => {
            homepage => q[http://net-bittorrent.googlecode.com/],
            bugtracker =>
                q[http://code.google.com/p/net-bittorrent/issues/list],
            repository =>
                q[http://net-bittorrent.googlecode.com/svn/trunk/],
            MailingList =>
                q[http://groups.google.com/group/net-bittorrent-discuss]
        },
        keywords => [qw(BitTorrent client peer p2p torrent)],
        generated_by => q[Sanko Robinson <sanko@cpan.org>]    # lies
    },
);
$mb->create_build_script;

sub can_talk_to_ourself {

    # Check if we should try to run tests that talk to ourself
    system(qq("$^X" t/data/etc/talk-to-ourself.pl));
    return $? ? 0 : 1;
}

sub find_cb {
    return if -d $_ or -l $_;
    return unless -T $_;
    return unless $_ =~ m[.+\.t$];
    return push @tests, $File::Find::name;
}

# $Id$
