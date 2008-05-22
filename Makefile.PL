use strict;
use warnings;
unless (eval q[use Module::Build::Compat 0.02; 1]) {
    print qq[This module requires Module::Build to install itself.\n];
    require ExtUtils::MakeMaker;
    my $yn =
        ExtUtils::MakeMaker::prompt(q[  Install Module::Build now from CPAN?],
                                    q[y]);
    unless ($yn =~ /^y/i) {
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

# $Id$
