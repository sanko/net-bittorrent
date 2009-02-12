#!/usr/bin/perl -w
use strict;
use warnings;
use File::Find;
use Module::Build;
use Config;
$|++;
my $automated_testing = $ENV{q[AUTOMATED_TESTING]}
    || $ENV{q[PERL_MM_USE_DEFAULT]};
my $is_developer = ((-d q[.git]) ? 1 : 0) or ($ENV{RELEASE_TESTING} ? 1 : 0);
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
use strict;
use warnings;

# TODO: add pod
sub ACTION_profile {
    my ($self) = @_;
    unless (Module::Build::ModuleInfo->find_module_by_name('Devel::NYTProf'))
    {   warn(
            "Cannot run testcover action unless Devel::NYTProf is installed.\n"
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
        = local $ENV{HARNESS_PERL_SWITCHES} = '-d:NYTProf';
    $self->notes(profile => 1);
    $self->depends_on('test');
    $self->do_system('nytprofhtml --open');
    $self->notes(profile => 0);    # clean up
}

sub ACTION_tidy {
    my ($self) = @_;
    unless (Module::Build::ModuleInfo->find_module_by_name('Perl::Tidy')) {
        warn("Cannot run tidy action unless Perl::Tidy is installed.\n");
        return;
    }
    require Perl::Tidy;
    my $demo_files
        = $self->rscan_dir(File::Spec->catdir('tatoeba'), qr[\.pl$]);
    for my $files ([keys(%{$self->script_files})],       # scripts first
                   [values(%{$self->find_pm_files})],    # modules
                   [@{$self->find_test_files}],          # test suite next
                   [@{$demo_files}]                      # demos last
        )
    {   $files = [sort map { File::Spec->rel2abs('./' . $_) } @{$files}];

        # One at a time...
        for my $file (@$files) {
            printf "Running perltidy on '%s' ...\n",
                File::Spec->abs2rel($file);
            $self->add_to_cleanup($file . '.tidy');
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
    $self->depends_on('code');
    return 1;
}

sub ACTION_wastetime {
    my ($self) = @_;
    unless (Module::Build::ModuleInfo->find_module_by_name('File::Copy')) {
        warn("Cannot run mindist action unless File::Copy is installed.\n");
        return;
    }
    require File::Copy;
    my $_quiet = $self->quiet(1);
    mkdir './archive' if !-d './archive';
    my $dist_dir = q[];
    for my $i (1 .. 999) {
        $self->SUPER::ACTION_distdir();
        $dist_dir = $self->dist_dir;
        $self->make_tarball($dist_dir, $dist_dir, 1);

        #File::Copy::copy($dist_dir . '.tar.gz',
        #     'X:/archive/' . $dist_dir . '.tar.gz' . sprintf('.%03d', $i));
        rename $dist_dir . '.tar.gz', './archive/' . $dist_dir . '.tar.gz'
            if !-f './archive/' . $dist_dir . '.tar.gz'
                or -s $dist_dir . '.tar.gz'
                < -s './archive/' . $dist_dir . '.tar.gz';
        printf "dist #%03d ... %d bytes\n", $i, -s $dist_dir . '.tar.gz';
        unlink $dist_dir . '.tar.gz';
        $self->delete_filetree($dist_dir);
    }
    File::Copy::copy('./archive/' . $dist_dir . '.tar.gz',
                     $dist_dir . '.tar.gz');
    return $self->quiet($_quiet);
}

sub ACTION_spellcheck {
    my ($self) = @_;
    my $demo_files
        = $self->rscan_dir(File::Spec->catdir('tatoeba'), qr[\.pl$]);
    for my $files (
        [keys(%{$self->script_files})],       # scripts first
        [values(%{$self->find_pm_files})],    # modules
        [@{$self->find_test_files}],          # test suite
        [values(%{shift->_find_file_by_type('pod', '.')})],    # docs
        [@{$demo_files}]                                       # demos
        )
    {   $files = [sort map { File::Spec->rel2abs('./' . $_) } @{$files}];
        for my $file (@$files) {
            $file = File::Spec->abs2rel($file);
            system(
                  sprintf('title aspell - "%s"', File::Spec->abs2rel($file)));
            $self->do_system(sprintf 'perldoc %s > %s.spell', $file, $file);
            $self->add_to_cleanup($file . '.spell');
            system('aspell check ' . $file . '.spell');
            $self->add_to_cleanup($file . '.bak');
        }
    }
    $self->depends_on('code');
}

sub ACTION_changes {
    my ($self) = @_;
    require Fcntl;
    require POSIX;
    require File::Spec::Functions;
    {
        print 'Update Changes file... ';

        # open and lock the file
        sysopen(my ($CHANGES_R), 'Changes', Fcntl::O_RDONLY())
            || die 'Failed to open Changes for reading';
        flock $CHANGES_R, Fcntl::LOCK_EX();

        # Read the file's content and scroll back to the top
        sysread($CHANGES_R, my ($CHANGES_D), -s $CHANGES_R)
            || die 'Failed to read Changes';

        # Okay, we're done with this file for now
        flock $CHANGES_R, Fcntl::LOCK_UN();
        close $CHANGES_R;

        # gather various info
        my (@bits) = split q[,], qx[git log --pretty=format:"%at,%H,%h" -n 1];

        #my $Mod = qx[git log -n 1 --pretty=format:"%cr"];
        my $Date = POSIX::strftime('%Y-%m-%d %H:%M:%SZ (%a, %d %b %Y)',
                                   gmtime($bits[0]));
        my $Commit = $bits[1];
        my $dist = sprintf("Version %s | %s | %s",
                           ($self->dist_version()->is_alpha()
                            ? (q[0.0XX], q[Distant future])
                            : ($self->dist_version()->numify, $Date)
                           ),
                           $bits[2]
        );

        # start changing the data around
        $CHANGES_D =~ s[.+(\r?\n)][$dist$1];
        $CHANGES_D
            =~ s[(_ -.-. .... .- -. --. . ... _+).*][$1 . sprintf <<'END',
        $self->{'properties'}{'meta_merge'}{'resources'}{'ChangeLog'},
        $self->dist_version
    ]se;

For more information, see the commit log:
    %s

$Ver: %s $ from git
$Date$
$Rev$
$URL$
END

     # Keep a backup (just in case) and move the file so we can create it next
        rename('Changes', 'Changes.bak')
            || die sprintf 'Failed to rename Changes (%s)', $^E;

        # open and lock the file
        sysopen(my ($CHANGES_W), 'Changes', Fcntl::O_WRONLY() | Fcntl::O_CREAT())
            || die 'Failed to open Changes for reading';
        sysseek($CHANGES_W, 0, Fcntl::SEEK_SET())
            || die 'Failed to seek in Changes';

        # hope all went well and save the new log to disk
        syswrite($CHANGES_W, $CHANGES_D) || die 'Failed to update Changes';

        # unlock the file and close it
        flock $CHANGES_W, Fcntl::LOCK_UN();
        close($CHANGES_W) || die 'Failed to close Changes';
        printf "Done.\n    (%s)\n", $dist;
    }
    {
        print 'Fake SVN...';
        my @manifest_files = sort grep {
                   $_ !~ m[\.torrent]
                && $_ !~ m[\.jpg$]
                && $_ !~ m[\.yml$]i

                #&& $_ !~ m[^Changes$]
                && $_ !~ m[^Build.PL$]
        } keys %{$self->_read_manifest('MANIFEST')};
    FILE: for my $file (@manifest_files) {
            print q[.];

            #warn sprintf q[%s | %s | %s], $date, $commit, $file;
            my $mode = (stat $file)[2];
            chmod($mode | oct(222), $file)
                or die "Can't make $file writable: $!";

            # open and lock the file
            sysopen(my ($CHANGES_R), $file, Fcntl::O_RDONLY())
                || die sprintf 'Failed to open "%s" for reading', $file;
            flock $CHANGES_R, Fcntl::LOCK_EX();

            # Read the file's content and scroll back to the top
            sysread($CHANGES_R, my ($CHANGES_D), -s $CHANGES_R)
                || die "Failed to read $file";

            # Okay, we're done with this file for now
            flock $CHANGES_R, Fcntl::LOCK_UN();
            close $CHANGES_R;

            # gather various info
            # gather various info
            my (@bits) = split q[,],
                qx[git log --pretty=format:"%at,%H,%x25%x73 %h %x25%x2E%x32%x30%x73 %ce" -n 1 $file];

            #my $Mod = qx[git log -n 1 --pretty=format:"%cr"];
            my $Date = POSIX::strftime('%Y-%m-%d %H:%M:%SZ (%a, %d %b %Y)',
                                       gmtime($bits[0]));
            my $Commit = $bits[1];
            #die $bits[2];
            my $Id = sprintf $bits[2], (File::Spec->splitpath($file))[2],
                $Date;

            # start changing the data around
            my $CHANGES_O = $CHANGES_D;
            $CHANGES_D =~ s[\$Date(:[^\$]*)?\$][\$Date: $Date \$]ig;
            $CHANGES_D =~ s[\$Id(:[^\$]*)?\$][\$Id: $Id \$]ig;
            $CHANGES_D
                =~ s[\$Url(:[^\$]*)?\$][\$Url: http://github.com/sanko/net-bittorrent/raw/$Commit/$file \$]ig;
            $CHANGES_D =~ s[\$(Rev(:?ision)?)(:[^\$]*)?\$][\$$1: $Commit \$]ig;

            #$CHANGES_D =~ s[\$Mod:.+\$][\$Mod: $Mod \$]ig;
            # Skip to the next file if this one wasn't updated
            next FILE if $CHANGES_D eq $CHANGES_O;

     #warn qq[Updated $file];
     #die $CHANGES_D;
     # Keep a backup (just in case) and move the file so we can create it next
            rename($file, $file . '.bak')
                || die sprintf 'Failed to rename %s (%s)', $file, $^E;

            # open and lock the file
            sysopen(my ($CHANGES_W), $file, Fcntl::O_WRONLY() | Fcntl::O_CREAT())
                || warn(sprintf q[Failed to open %s for reading: %s], $file,
                        $^E)
                && next FILE;
            sysseek($CHANGES_W, 0, Fcntl::SEEK_SET())
                || warn 'Failed to seek in ' . $file && next FILE;

            # hope all went well and save the new log to disk
            syswrite($CHANGES_W, $CHANGES_D)
                || warn 'Failed to update ' . $file && next FILE;

            # unlock the file and close it
            flock $CHANGES_W, Fcntl::LOCK_UN();
            close($CHANGES_W) || die 'Failed to close Changes';
            chmod($mode, $file);
        }
        print "Done.\n";
    }
    return 1;
}

sub ACTION_distmeta {
    my ($self) = @_;
    $self->do_create_makefile_pl if $self->create_makefile_pl;
    $self->do_create_readme      if $self->create_readme;
    $self->do_create_metafile;
    $self->SUPER::depends_on('changes');
}

sub make_tarball {
    my ($self, $dir, $file, $quiet) = @_;
    $file ||= $dir;
    $self->do_system(
            'tar --mode=0755 -c' . ($quiet ? q[] : 'v') . "f $file.tar $dir");
    $self->do_system("gzip -9 -f -n $file.tar");
    return 1;
}
1;
SUBCLASS
my $mb = $class->new(
    module_name       => q[Net::BitTorrent],
    license           => q[artistic_2],
    dist_author       => q[Sanko Robinson <sanko@cpan.org>],
    dist_abstract     => q[BitTorrent peer-to-peer protocol],
    dist_version_from => q[lib/Net/BitTorrent/Version.pm],
    requires          => {
                 q[Cwd]           => 0,
                 q[Data::Dumper]  => 0,
                 q[Digest::SHA]   => 5.45,
                 q[Errno]         => 0,
                 q[Exporter]      => 0,
                 q[Fcntl]         => 0,
                 q[File::Path]    => 0,
                 q[File::Spec]    => 0,
                 q[Math::BigInt]  => 1.78,
                 q[Module::Build] => 0.30,
                 q[perl]          => q[5.8.1],
                 q[Scalar::Util]  => 1.19,
                 q[Socket]        => 1.77,
                 q[Test::More]    => 0.80,
                 q[Time::HiRes]   => 0,
                 q[version]       => 0.74
    },
    build_requires => {q[Module::Build] => 0.30,
                       q[Test::More]    => 0.80
    },
    recommends => {q[Data::Dump]         => 0,
                   q[perl]               => q[5.10.0],
                   q[Math::Pari]         => 0,
                   q[Math::BigInt::Pari] => 0
    },
    auto_features => {
                 win32_utf8_support => {
                          description => q[Unicode filename support on Win32],
                          requires    => {
                                       q[Encode]         => 0,
                                       q[perl]           => q[5.8.7],
                                       q[utf8]           => 0,
                                       q[Win32]          => 0,
                                       q[Win32API::File] => 0.10
                          }
                 },
                 improved_message_stream_encryption_speed => {
                     description =>
                         q[MSE-related calculations are significantly faster],
                     requires => {q[Math::Pari]         => 0,
                                  q[Math::BigInt::Pari] => 0
                     }
                 }
    },

#script_files => qw[scripts/bittorrent.pl], # relocated to tatoeba/005-console.pl
    test_files => \@tests,
    meta_merge => {
        generated_by => q[Conversion, software version 7.0],
        keywords     => [qw[BitTorrent client peer p2p torrent socket DHT]],

        #no_index  => {directory => [q[tatoeba]]},
        resources => {
             bugtracker =>
                 q[http://code.google.com/p/net-bittorrent/issues/list],
             ChangeLog =>
                 q[http://github.com/sanko/net-bittorrent/commits/master],
             homepage => q[http://sankorobinson.com/net-bittorrent/],
             license => q[http://www.perlfoundation.org/artistic_license_2_0],
             MailingList => q[http://groups.google.com/group/net-bittorrent],
             repository  => q[http://github.com/sanko/net-bittorrent/]
        }
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
    if ($] < 5.008001) {  # already 5+ years old-- anything less is just silly
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
Copyright (C) 2008-2009 by Sanko Robinson <sanko@cpan.org>

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
