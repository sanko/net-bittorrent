#!/usr/bin/perl -w
use strict;
use warnings;
use File::Find;
use Module::Build;
my $GIVE_THEM_A_CHOICE = 0;
my @search
    = qw[t/000_basic/ t/100_data_parsing/ t/200_classes/ t/400_broken_data/ t/900_etc/];
if (    # http://cpantest.grango.org/wiki/CPANAuthorNotes
    ($GIVE_THEM_A_CHOICE
     ? (not $ENV{q[AUTOMATED_TESTING]} and not $ENV{q[PERL_MM_USE_DEFAULT]})
     : 1
    )
    and can_talk_to_ourself()
    and local $| = 1
    and print(
        <<'END'
 ****************************************************************************
   During the test phase, we will be opening ports, contacting a tiny local
   tracker, and trading data to simulate an actual swarm.  By design, the
   tests transfer only to the local system.  To view transfer diagnostics
   during these tests, set $ENV{RELEASE_TESTING} to a true value.

   NOTE: These tests can be painfully slow, redundant, and may fail due to
   restrictive firewalling, solar flare activity, or other connectivity
   problems.
 ****************************************************************************
END
    )
    and (
        $GIVE_THEM_A_CHOICE
        ? (print(
               q[
Do you want to perform these tests in addition to core tests? [no] ]
               )
               and readline(*STDIN) =~ m[^y]i
        )
        : 1
    )
    )
{   push @search, q[t/500_full_swarm/];
}
my @tests;
find \&find_cb, sort @search;
my $mb = Module::Build->new(
    module_name        => q[Net::BitTorrent],
    license            => q[perl],
    dist_author        => q[Sanko Robinson <sanko@cpan.org>],
    dist_abstract      => q[BitTorrent peer-to-peer protocol],
    dist_version_from  => q[lib/Net/BitTorrent.pm],
    #create_makefile_pl => q[traditional],                        # Old skool.
    sign => 0,    # chokes on build.bat on win32
    build_requires => {q[Module::Build] => 0.2805,    # core in 5.009004
                       q[Test::More]    => 0.72,      # dev on 0.74
    },
    requires => {
               q[Cwd]           => 0,           # dev on
               q[Digest::SHA]   => 5.45,        # dev on 5.45|CORE in 5.10
               q[Exporter]      => 0,           # dev on 5.62
               q[Fcntl]         => 0,           # dev on 1.06
               q[File::Path]    => 0,           # dev on 2.04
               q[File::Spec]    => 0,           # dev on 3.2701
               q[Module::Build] => 0.2805,      # dev on 0.280802|CORE in 5.10
               q[perl]          => 5.008001,    # dev on 5.11
               q[Scalar::Util]  => 1.19,        # dev on 1.19
               q[Socket]        => 1.77,        # dev on 1.80
               q[Time::HiRes]   => 0,           # dev on 1.9712
               q[Test::More]    => 0.72,        # dev on 0.74
               q[version]       => 0.74         # dev on 0.74|CORE in 5.10
    },
    recommends => {
        q[Data::Dumper] => 0,            # core
        ($^O eq q[MSWin32]
         ? (q[Encode] => 0,
            q[perl]   => 5.008007,
            q[utf8]   => 0,          # utf8::is_utf8 internal as of Perl 5.8.1
            q[Win32]  => 0,
            q[Win32API::File] => 0.10,
             )
         : ()
        ),
        ($ENV{RELEASE_TESTING}
         ? (                         #q[Module::Signature] => 0.22,
             q[Pod::Coverage]       => 0.19,
             q[Test::Perl::Critic]  => 0,
             q[Test::Pod]           => 1.00,
             q[Test::Pod::Coverage] => 1.08,

             #q[Test::Signature]   => 1.04,
             )
         : ()
        )
    },
    test_files => \@tests,
    meta_merge => {
        resources => {
             homepage => q[http://sankorobinson.com/net-bittorrent/],
             bugtracker =>
                 q[http://code.google.com/p/net-bittorrent/issues/list],
             repository => q[http://net-bittorrent.googlecode.com/svn/trunk/],
             MailingList =>
                 q[http://groups.google.com/group/net-bittorrent-discuss]
        },
        keywords => [qw[BitTorrent client peer p2p torrent socket dht]],
        generated_by => q[Sanko Robinson <sanko@cpan.org>]    # lies
    },
);
$mb->create_build_script;

sub can_talk_to_ourself {
    system(qq["$^X" t/data/etc/talk-to-ourself.pl]);          # taken from LWP
    return $? ? 0 : 1;
}

sub find_cb {
    return if -d $_ or -l $_;
    return unless -T $_;
    return unless $_ =~ m[.+\.t$];
    return push @tests, $File::Find::name;
}

# $Id$
