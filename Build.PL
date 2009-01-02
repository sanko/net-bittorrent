#!/usr/bin/perl -w
use strict;
use warnings;
use File::Find;
use Module::Build;
use Config;
$|++;
my $automated_testing = $ENV{q[AUTOMATED_TESTING]}
    || $ENV{q[PERL_MM_USE_DEFAULT]};
my $is_developer = ((-d q[.svn]) ? 1 : 0) or ($ENV{RELEASE_TESTING} ? 1 : 0);
my $okay_tcp     = okay_tcp();
my $okay_udp     = okay_udp();
my @tests;
find \&find_cb, qw[t/000_miniswarm/ t/700_classes/];
@tests = reverse sort @tests;
printf $okay_tcp || $okay_udp
    ? <<'FTW': <<'FAIL', map { $_ ? 'En' : 'Dis' } ($okay_tcp, $okay_udp);
 ****************************************************************************
   During the test phase, we will be opening ports, contacting a tiny local
   tracker, and trading data to simulate actual swarms.  By design, the
   tests transfer only to the local system.

     - TCP tests are %sabled.
     - UDP tests are %sabled.

   NOTE: These tests may fail due to restrictive firewalling, solar flare
   activity, or other connectivity problems.
 ****************************************************************************
FTW
 ****************************************************************************
   Hrm... Your system seems to be misconfigured; an attempt to create a
   loopback has failed.  We'll work around this by skipping most of the
   socket-related tests.

     - TCP tests are %sabled.
     - UDP tests are %sabled.

   NOTE: Skipping these tests greatly reduces the usefullness of the
   Net::BitTorrent test suite and makes life (in general) difficult.
 ****************************************************************************
FAIL
my $class = $is_developer
    ? Module::Build->subclass(class => q[Net::BitTorrent::Build],
                              code  => <<'SUBCLASS' ) : q[Module::Build];
# TODO: add pod
sub ACTION_profile {
    my ($self) = @_;
    unless (Module::Build::ModuleInfo->find_module_by_name('Devel::NYTProf'))
    {   warn(
            qq[Cannot run testcover action unless Devel::NYTProf is installed.\n]
        );
        return;
    }
    $self->add_to_cleanup('nytprof.out', 'nytprof');
    $self->depends_on('code');

    # See whether any of the *.pm files have changed since last time
    # profile was run.  If so, start over.
    if (-e 'nytprof.out') {
        my $pm_files =
            $self->rscan_dir(File::Spec->catdir($self->blib, 'lib'),
                             qr[\.pm$]);
        my $cover_files
            = $self->rscan_dir('cover_db', sub { -f $_ and not /\.html$/ });
        $self->do_system(qw(cover -delete))
            unless $self->up_to_date($pm_files, $cover_files)
                && $self->up_to_date($self->test_files, $cover_files);
    }
    local $Test::Harness::switches = local $Test::Harness::Switches
        = local $ENV{HARNESS_PERL_SWITCHES} = q[-d:NYTProf];
    $self->notes(profile => 1);
    $self->depends_on(q[test]);
    $self->do_system(q[nytprofhtml --open]);
    $self->notes(profile => 0);    # clean up
}

sub ACTION_tidy {
    my ($self) = @_;
    unless (Module::Build::ModuleInfo->find_module_by_name('Perl::Tidy')) {
        warn(qq[Cannot run tidy action unless Perl::Tidy is installed.\n]);
        return;
    }
    require Perl::Tidy;
    my $demo_files =
        $self->rscan_dir(File::Spec->catdir('tatoeba'), qr[\.pl$]);
    for my $files ([keys(%{$self->script_files})],       # scripts first
                   [values(%{$self->find_pm_files})],    # modules
                   [@{$self->find_test_files}],          # test suite next
                   [@{$demo_files}]                      # demos last
        )
    {   $files = [sort map { File::Spec->rel2abs(q[./] . $_) } @{$files}];

        # One at a time...
        for my $file (@$files) {
            printf qq[Running perltidy on '%s' ...\n],
                File::Spec->abs2rel($file);
            $self->add_to_cleanup($file . q[.tidy]);
            Perl::Tidy::perltidy(argv => <<'END' . $file); } }
--brace-tightness=2
--block-brace-tightness=1
--block-brace-vertical-tightness=2
--paren-tightness=2
--paren-vertical-tightness=2
--square-bracket-tightness=2
--square-bracket-vertical-tightness=2
--brace-tightness=2
--brace-vertical-tightness=2

--delete-old-whitespace
--no-indent-closing-brace
--line-up-parentheses
--no-outdent-keywords
--no-outdent-long-quotes
--no-space-for-semicolon
--swallow-optional-blank-lines

--continuation-indentation=4
--maximum-line-length=78

--want-break-before='% + - * / x != == >= <= =~ !~ < > | & >= < = **= += *= &= <<= &&= -= /= |= \ >>= ||= .= %= ^= x= ? :'

--standard-error-output
--warning-output

--backup-and-modify-in-place
--backup-file-extension=tidy

END
    $self->depends_on(q[code]);
    return 1;
}

sub ACTION_wastetime {
    my ($self) = @_;
    unless (Module::Build::ModuleInfo->find_module_by_name('File::Copy')) {
        warn(qq[Cannot run mindist action unless File::Copy is installed.\n]);
        return;
    }
    require File::Copy;
    mkdir q[./archive] if !-d q[./archive];
    my $dist_dir = q[];
    for my $i (1 .. 999) {
        $self->SUPER::ACTION_distdir();
        $dist_dir = $self->dist_dir;
        $self->do_system(qq[tar --mode=0755 -cf $dist_dir.tar $dist_dir]);
        $self->do_system(qq[gzip -9 -f -n $dist_dir.tar]);
       #File::Copy::copy($dist_dir . q[.tar.gz],
       #     q[X:/archive/] . $dist_dir . q[.tar.gz] . sprintf(q[.%03d], $i));
        rename $dist_dir . q[.tar.gz], q[./archive/] . $dist_dir . q[.tar.gz]
            if !-f q[./archive/] . $dist_dir . q[.tar.gz]
                or -s $dist_dir . q[.tar.gz]
                < -s q[./archive/] . $dist_dir . q[.tar.gz];
        printf qq[dist #%03d ... %d bytes\n], $i, -s $dist_dir . q[.tar.gz];
        unlink $dist_dir . q[.tar.gz];
        $self->delete_filetree($dist_dir);
    }
    return
        File::Copy::copy(q[./archive/] . $dist_dir . q[.tar.gz],
                         $dist_dir . q[.tar.gz]);
}

sub ACTION_spellcheck {
    my ($self) = @_;
    my $demo_files =
        $self->rscan_dir(File::Spec->catdir('tatoeba'), qr[\.pl$]);
    for my $files (
        [keys(%{$self->script_files})],       # scripts first
        [values(%{$self->find_pm_files})],    # modules
        [@{$self->find_test_files}],          # test suite
        [values(%{shift->_find_file_by_type(q[pod], q[.])})], # docs
        [@{$demo_files}]                      # demos
        )
    {   $files = [sort map { File::Spec->rel2abs(q[./] . $_) } @{$files}];
        for my $file (@$files) {
            $file = File::Spec->abs2rel($file);
            system(
                 sprintf(q[title aspell - '%s'], File::Spec->abs2rel($file)));
            $self->do_system(sprintf q[perldoc %s > %s.spell], $file, $file);
            $self->add_to_cleanup($file . q[.spell]);
            system(q[aspell check ] . $file . q[.spell]);
            $self->add_to_cleanup($file . q[.bak]);
        }
    }
    $self->depends_on(q[code]);
}
SUBCLASS
my $mb = $class->new(
    module_name       => q[Net::BitTorrent],
    license           => q[artistic_2],
    dist_author       => q[Sanko Robinson <sanko@cpan.org>],
    dist_abstract     => q[BitTorrent peer-to-peer protocol],
    dist_version_from => q[lib/Net/BitTorrent/Version.pm],
    requires          => {
        q[Cwd]           => 0,
        q[Digest::SHA]   => 5.45,
        q[Errno]         => 0,
        q[Exporter]      => 0,
        q[Fcntl]         => 0,
        q[File::Path]    => 0,
        q[File::Spec]    => 0,
        q[Module::Build] => 0.30,
        q[perl]          => q[5.8.1],
        q[Scalar::Util]  => 1.19,
        q[Socket]        => 1.77,
        q[Test::More]    => 0.80,
        q[Time::HiRes]   => 0,
        q[version]       => 0.74,
        ((!eval {
              my $eval = pack(q[Q], time);
              1;
          }
         )
         ? (q[Math::BigInt] => 0)
         : ()
        )
    },
    build_requires => {q[Module::Build] => 0.30,
                       q[Test::More]    => 0.80
    },
    recommends    => {q[Data::Dump] => 0},
    auto_features => {win32_utf8_support => {
                          description => q[Unicode filename support on Win32],
                          requires    => {
                                       q[Encode]         => 0,
                                       q[perl]           => q[5.8.7],
                                       q[utf8]           => 0,
                                       q[Win32]          => 0,
                                       q[Win32API::File] => 0.10
                          }
                      }
    },
    script_files => qw[scripts/bittorrent.pl],
    test_files   => \@tests,
    meta_merge   => {
         no_index  => {directory => [q[tatoeba]]},
         resources => {
             bugtracker =>
                 q[http://code.google.com/p/net-bittorrent/issues/list],
             homepage => q[http://sankorobinson.com/net-bittorrent/],
             license => q[http://www.perlfoundation.org/artistic_license_2_0],
             MailingList => q[http://groups.google.com/group/net-bittorrent],
             repository =>
                 q[http://code.google.com/p/net-bittorrent/source/browse/]
         },
         keywords     => [qw[BitTorrent client peer p2p torrent socket dht]],
         generated_by => q[Sanko Robinson version 1.0 (stable ...usually)]
    },
);
$mb->notes(okay_tcp          => $okay_tcp);
$mb->notes(okay_udp          => $okay_udp);
$mb->notes(automated_testing => $automated_testing ? 1 : 0);
$mb->notes(release_testing   => $is_developer);
$mb->notes(test_suite        => \@tests);
$mb->notes(gmtime            => gmtime);
$mb->notes(verbose => scalar grep {m[^v$]} keys %{$mb->args()});
$mb->notes(threads => $Config::Config{q[useithreads]} ? 1 : 0);
$mb->create_build_script;
exit 0;

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

sub find_cb {
    return if -d $_ or -l $_;
    return unless -T $_;
    return unless $_ =~ m[.+\.t$];
    return push @tests, $File::Find::name;
}

BEGIN {    # Tired of getting FAIL-mail from outdated build environments
    if ($] < 5.008001) {    # 5+ years old-- anything less is just silly
        warn sprintf
            q[Perl v5.8.1 required--this is only v%vd, stopped],
            $^V;
        exit 0;
    }
    if ($Module::Build::VERSION < 0.3) {
        warn sprintf
            q[Module::Build version 0.3 required--this is only version %s],
            $Module::Build::VERSION;
        exit 0;
    }
}
__END__
Copyright (C) 2008 by Sanko Robinson <sanko@cpan.org>

This program is free software; you can redistribute it and/or modify it
under the terms of The Artistic License 2.0.  See the LICENSE file
included with this distribution or
http://www.perlfoundation.org/artistic_license_2_0.  For
clarification, see http://www.perlfoundation.org/artistic_2_0_notes.

When separated from the distribution, all POD documentation is covered by
the Creative Commons Attribution-Share Alike 3.0 License.  See
http://creativecommons.org/licenses/by-sa/3.0/us/legalcode.  For
clarification, see http://creativecommons.org/licenses/by-sa/3.0/us/.

$Id$
