#!/usr/bin/perl -w
use strict;
use warnings;
BEGIN { exit 0 if $] < 5.008001 }
require 5.8.1;
$|++;
unless (eval q[use Module::Build::Compat 0.02; 1;]) {
    print qq[This module requires Module::Build to install itself.\n];
    require ExtUtils::MakeMaker;
    my $yn =
        ExtUtils::MakeMaker::prompt(q[  Install Module::Build now from CPAN?],
                                    q[y]);
    unless ($yn =~ m[^y]i) {
        die qq[ *** Cannot install without Module::Build.  Exiting ...\n];
    }
    require Cwd;
    require File::Spec;
    require CPAN;

    # Save this 'cause CPAN will chdir all over the place.
    my $cwd = Cwd::cwd();
    CPAN::Shell->install(q[Module::Build::Compat]);
    CPAN::Shell->expand(q[Module], q[Module::Build::Compat])->uptodate
        or die qq[Couldn't install Module::Build, giving up.\n];
    chdir $cwd or die qq[Cannot chdir() back to $cwd: $!];
}
eval q[use Module::Build::Compat 0.02; 1] or die $@;
Module::Build::Compat->run_build_pl(args => \@ARGV);
require Module::Build;
Module::Build::Compat->write_makefile(build_class => q[Module::Build]);
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
