package MBX::Net::BitTorrent::Developer;
{
    use strict;
    use warnings;
    use base 'Module::Build';

    # TODO: add pod
    sub ACTION_profile {
        my ($self) = @_;
        unless (
             Module::Build::ModuleInfo->find_module_by_name('Devel::NYTProf'))
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
            my $cover_files = $self->rscan_dir('cover_db',
                                             sub { -f $_ and not /\.html$/ });
            $self->do_system(qw(cover -delete))
                unless $self->up_to_date($pm_files, $cover_files)
                    && $self->up_to_date($self->test_files, $cover_files);
        }
        local $Test::Harness::Switches = local $Test::Harness::Switches
            = local $ENV{HARNESS_PERL_SWITCHES} = '-d:NYTProf';
        $self->notes(profile => 1);
        $self->depends_on('test');
        $self->do_system('nytprofhtml --open');
        $self->notes(profile => 0);    # clean up
    }

    sub ACTION_tidy {
        my ($self) = @_;
        unless (Module::Build::ModuleInfo->find_module_by_name('Perl::Tidy'))
        {   warn("Cannot run tidy action unless Perl::Tidy is installed.\n");
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
        unless (Module::Build::ModuleInfo->find_module_by_name('File::Copy'))
        {   warn("Cannot run mindist action unless File::Copy is installed.\n"
            );
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
                     sprintf('title aspell - "%s"', File::Spec->abs2rel($file)
                     )
                );
                $self->do_system(sprintf 'perldoc %s > %s.spell',
                                 $file, $file);
                $self->add_to_cleanup($file . '.spell');
                system('aspell check ' . $file . '.spell');
                $self->add_to_cleanup($file . '.bak');
            }
        }
        $self->depends_on('code');
    }

    sub ACTION_changelog {
        my ($self) = @_;
        require POSIX;
        print 'Updating Changes file... ';

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
        my @bits = split ',', qx[git log --pretty=format:"%at,%H,%h" -n 1];
        my $Date = POSIX::strftime('%Y-%m-%d %H:%M:%SZ (%a, %d %b %Y)',
                                   gmtime($bits[0]));
        my $Commit = $bits[1];
        my $dist = sprintf(
            'Version %s | %s | %s',
            ($self->dist_version() =~ m[_]   # $self->dist_version()->is_alpha
             ? ('0.0XXX', 'In the not too distant future')
             : ($self->dist_version(), $Date$self->dist_version()->numify
            ),
            $bits[2]
        );

        # start changing the data around
        $CHANGES_D =~ s[.+(\r?\n)][$dist$1];
        $CHANGES_D
            =~ s[(_ -.-. .... .- -. --. . ... _+).*][$1 . sprintf <<'END',
        $self->{'properties'}{'meta_merge'}{'resources'}{'ChangeLog'}||'',
        $self->dist_version , qw[$ $ $]
    ]se;

For more information, see the commit log:
    %s

$Ver$ from git $Rev%s
$Date%s
$Url%s
END

     # Keep a backup (just in case) and move the file so we can create it next
        rename('Changes', 'Changes.bak')
            || die sprintf 'Failed to rename Changes (%s)', $^E;

        # open and lock the file
        sysopen(my ($CHANGES_W),
                'Changes', Fcntl::O_WRONLY() | Fcntl::O_CREAT())
            || die 'Failed to open Changes for reading';
        sysseek($CHANGES_W, 0, Fcntl::SEEK_SET())
            || die 'Failed to seek in Changes';

        # hope all went well and save the new log to disk
        syswrite($CHANGES_W, $CHANGES_D)
            || die 'Failed to update Changes';

        # unlock the file and close it
        flock $CHANGES_W, Fcntl::LOCK_UN();
        close($CHANGES_W) || die 'Failed to close Changes';
        printf "okay (%s)\n", $dist;
    }

    sub ACTION_RCS {
        my ($self) = @_;
        require POSIX;
        print 'Running fake RCS...';
        my @manifest_files = sort keys %{$self->_read_manifest('MANIFEST')};
    FILE: for my $file (@manifest_files, __FILE__) {
            print '.';

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
            my (@bits) = split q[,],
                qx[git log --pretty=format:"%at,%H,%x25%x73 %h %x25%x2E%x32%x30%x73 %ce" -n 1 $file];
            next FILE if !@bits;
            my $Mod  = qx[git log --pretty=format:"%cr" -n 1 $file];
            my $Date = POSIX::strftime('%Y-%m-%d %H:%M:%SZ (%a, %d %b %Y)',
                                       gmtime($bits[0]));
            my $Commit = $bits[1];
            my $Commit_short = substr($bits[1], 0, 7);
            my $Id$bits[2], (File::Spec->splitpath($file))[2],
                $Date;
            my $Repo
                = $self->{'properties'}{'meta_merge'}{'resources'}
                {'repository'}
                || '';

            # start changing the data around
            my $CHANGES_O = $CHANGES_D;
            $CHANGES_D =~ s[\$(Id)(:[^\$]*)?\$][\$$1: $Id$]ig;
            $CHANGES_D =~ s[\$(Date)(:[^\$]*)?\$][\$$1: $Date$]ig;
            $CHANGES_D =~ s[\$(Mod(ified)?)(:[^\$]*)?\$][\$$1: $Mod \$]ig;
            $CHANGES_D
                =~ s[\$(Url)(:[^\$]*)?\$][\$$1: $Repo/raw/$Commit/$file \$]ig
                if $Repo;
            $CHANGES_D =~ s|//raw|/raw|g;    # cleanup
            $CHANGES_D
                =~ s[\$(Rev(ision)?)(?::[^\$]*)?\$]["\$$1: ". ($2?$Commit:$Commit_short)." \$"]ige;

            # Skip to the next file if this one wasn't updated
            next FILE if $CHANGES_D eq $CHANGES_O;

     #warn qq[Updated $file];
     #die $CHANGES_D;
     # Keep a backup (just in case) and move the file so we can create it next
            rename($file, $file . '.bak')
                || die sprintf 'Failed to rename %s (%s)', $file, $^E;

            # open and lock the file
            sysopen(my ($CHANGES_W),
                    $file, Fcntl::O_WRONLY() | Fcntl::O_CREAT())
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
        print "okay\n";
        return 1;
    }

    sub ACTION_testkwalitee {
        my ($self) = @_;
        use Test::More;
        eval { require Test::Kwalitee; Test::Kwalitee->import() };
        plan(skip_all => 'Test::Kwalitee not installed; skipping') if $@;
    }

    sub ACTION_testpod {
        my ($self) = @_;
        use Test::More;
        eval "use Test::Pod 1.00";
        plan skip_all => "Test::Pod 1.00 required for testing POD" if $@;
        all_pod_files_ok(all_pod_files(qw[lib scripts tatoeba t]));
    }

    sub ACTION_distdir {
        my ($self, $args) = @_;
        if ($self->notes('do_rcs')) {
            $self->SUPER::depends_on('changelog');
            $self->SUPER::depends_on('RCS');
        }
        $self->notes('do_rcs' => 1);
        $self->SUPER::ACTION_distdir(@_);
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
