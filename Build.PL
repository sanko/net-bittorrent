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

   NOTE: These tests can be painfully slow and may fail due to
   restrictive firewalling, local weather conditions, or other
   connectivity problems.
  *****************************************************************

Do you want to perform these tests in addition to core tests? [no] ]
    )
    and readline(STDIN) =~ m[^\s*(y)]i
    )
{
    push @search, q[t/500_full_swarm/];
}
my @tests;

find \&find_cb, sort @search;
my $mb = Module::Build->new(
    module_name        => q[Net::BitTorrent],
    license            => q[perl],
    dist_author        => q[Sanko Robinson <sanko@cpan.org>],
    dist_abstract      => q[BitTorrent peer-to-peer protocol],
    dist_version_from  => q[lib/Net/BitTorrent.pm],
    #create_makefile_pl => q[passthrough],
    sign               => 1,
    requires           => {
        q[Module::Build]         => 0.2805,    # non-CORE
        q[Digest::SHA]           => 5.45,      # CORE in 5.10
        q[version]               => 0.50,      # CORE in 5.10
        q[Test::More]            => 0.74,
        q[Time::HiRes]           => 0,
        q[Data::Dumper]          => 0,
        q[Exporter]              => 0,
        q[Config]                => 0,
        q[File::Basename]        => 0,
        q[File::Path]            => 0,
        q[File::Spec::Functions] => 0,
        q[IO::Select]            => 0,
        q[IO::Socket::INET]      => 0,
        q[Carp]                  => 0,
        q[Cwd]                   => 0,
        q[Fcntl]                 => 0,
        q[overload]              => 0,
        (  $^O eq q[MSWin32]
           ? ( q[Encode] => 0,
               q[utf8] => 0, # utf8::is_utf8 internal as of Perl 5.8.1
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

sub can_talk_to_ourself
{    # Check if we should try to run tests that talk to ourself
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
