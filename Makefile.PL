#!/usr/bin/perl -w
use strict;
use warnings;
use ExtUtils::MakeMaker qw[WriteMakefile prompt];
my @tests = glob(
    q[t/0*.t] . (
        ( # AUTOMATED_TESTING | http://cpantest.grango.org/wiki/CPANAuthorNotes
           (  not $ENV{q[AUTOMATED_TESTING]} and prompt(
                  q[
  *****************************************************************
   During the "make test" phase, we may run tests that open ports.
   By design, no data will be transferred to remote systems during
   these tests.

   NOTE: These tests can fail or timeout due to restrictive
   firewalling, or other connectivity problems.
  *****************************************************************

Do you want to perform these tests in addition to core tests?], q[no]
                  ) =~ m[^\s*(y)]i
           )
        ) ? q[ t/network/00*.t] : q[]
    )
);
WriteMakefile(
    NAME         => q[Net::BitTorrent],
    AUTHOR       => q[Sanko Robinson <sanko@cpan.org>],
    VERSION_FROM => q[lib/Net/BitTorrent.pm],
    ABSTRACT     => q[BitTorrent peer-to-peer protocol],
    LICENSE      => q[perl],
    PREREQ_PM    => {
        q[Digest::SHA]           => 5.45,    # CORE in 5.10
        q[version]               => 0.50,    # CORE in 5.10
        q[Test::More]            => 0.74,
        q[Data::Dumper]          => 0,
        q[English]               => 0,
        q[Class::ISA]            => 0,
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
        q[Encode]                => 0,
        (  $^O eq q[MSWin32]
           ? ( q[Win32API::File] => 0.10,
               q[utf8] => 0  # utf8::is_utf8 internal as of Perl 5.8.1
               )
           : ()
        )
    },
    dist => {
        COMPRESS => q[gzip -9f],
        SUFFIX   => q[.gz],

#PREOP => q[svn commit -m "make dist" && pod2text .\lib\Net\BitTorrent.pm > README]
#PREOP => q[svn commit -m "make dist"]
    },
    test => { TESTS => join q[ ], @tests },

    #SIGN=> 1 # doesn't play nice with my .svn dirs
);

# $Id$
