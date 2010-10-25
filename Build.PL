#!/usr/bin/perl -w
use strict;
use warnings;
use File::Find;
use Module::Build;
our $MAJOR = 0; our $MINOR = 74; our $DEV = 13; our $VERSION = sprintf('%0d.%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
$|++;
my $automated_testing = $ENV{'AUTOMATED_TESTING'}
    || $ENV{'PERL_MM_USE_DEFAULT'};
my $is_developer = ((-d '.git') ? 1 : 0) or ($ENV{'RELEASE_TESTING'} ? 1 : 0);
my @tests;
find \&find_cb, qw[t/1000_basic/];
my $class = $is_developer ? 'inc::MBX::Developer' : 'Module::Build';

if (!eval "require $class") {
    printf 'Failed to load %s: %s This ain\'t good, so... bye!', $class, $@;
    exit 0;
}
my $mb = $class->new(
    module_name   => 'Net::BitTorrent',
    license       => 'artistic_2',
    dist_author   => 'Sanko Robinson <sanko@cpan.org>',
    dist_abstract => 'BitTorrent peer-to-peer protocol',
    dist_version  => $VERSION,
    requires      => {

        #AnyEvent                     => 0,
        #'AnyEvent::Handle::Throttle' => '0.000002004',
        'Bit::Vector' => 7.1,
        'Digest::SHA' => 5.45,
        Exporter      => 0,

        #'File::Path'                 => 2.07,
        #'File::Spec'                 => 0,
        Moose => 1.17,
        perl  => '5.10.0',

        #'Scalar::Util'               => 1.19
    },
    build_requires => {
        AnyEvent => 0,

        #'AnyEvent::Handle::Throttle' => '0.000002004',
        'Bit::Vector' => 7.1,
        'Digest::SHA' => 5.45,
        Exporter      => 0,

        #'File::Path'                 => 2.07,
        #'File::Spec'                 => 0,
        #'File::Temp'                 => 0,
        #'Module::Build'              => 0.30,
        Moose             => 1.17,
        parent            => 0,
        perl              => '5.10.0',
        'TAP::Harness'    => 3.18,
        'Test::Class'     => 0.35,
        'Test::Exception' => 0,

        #'Test::Most'                 => 0,
        #($is_developer
        # ? ('CPAN::Meta::Converter' => 0,
        #    'CPAN::Meta::Validator' => 0,
        #    JSON                    => 0,
        #    'Perl::Tidy'            => 0
        #     )
        # : ()
        #)
    },
    test_files       => \@tests,
    test_file_exts   => ['.t'],
    use_tap_harness  => 1,
    tap_harness_args => {
                    jobs  => 9,
                    rules => {par => [{seq => '/t/*'}, {seq => '/xt/*'}, '*']}
    },
    meta_merge => {
        keywords => [
            qw[BitTorrent torrent protocol p2p swarm],    # basic
            qw[BEP03 bencode bdecode],    # NB::Protocol::BEP03::Bencode
            qw[]
        ],
        resources => {
             bugtracker => {
                       web => 'http://github.com/sanko/net-bittorrent/issues',
                       mailto => 'sanko@cpan.org'
             },
             repository => {
                        url => 'git://github.com/sanko/net-bittorrent.git',
                        web => 'http://github.com/sanko/net-bittorrent/tree/',
                        type => 'git'
             },
             ChangeLog => 'http://github.com/sanko/net-bittorrent/commits/',
             homepage  => 'http://sankorobinson.com/net-bittorrent/',
             license => ['http://www.perlfoundation.org/artistic_license_2_0']
        }
    }
);
$mb->notes(automated_testing => $automated_testing ? 1 : 0);
$mb->notes(release_testing   => $is_developer);
$mb->notes(test_suite        => \@tests);
$mb->notes(gmtime            => gmtime);
$mb->create_build_script;
exit 0;

sub find_cb {
    return if -d $_ or -l $_;
    return unless -T $_;
    return unless $_ =~ m[.+\.t$];
    return push @tests, $File::Find::name;
}

=pod

=head1 Author

Sanko Robinson <sanko@cpan.org> - http://sankorobinson.com/

CPAN ID: SANKO

=head1 License and Legal

Copyright (C) 2008-2010 by Sanko Robinson <sanko@cpan.org>

This program is free software; you can redistribute it and/or modify it under
the terms of
L<The Artistic License 2.0|http://www.perlfoundation.org/artistic_license_2_0>.
See the F<LICENSE> file included with this distribution or
L<notes on the Artistic License 2.0|http://www.perlfoundation.org/artistic_2_0_notes>
for clarification.

When separated from the distribution, all original POD documentation is
covered by the
L<Creative Commons Attribution-Share Alike 3.0 License|http://creativecommons.org/licenses/by-sa/3.0/us/legalcode>.
See the
L<clarification of the CCA-SA3.0|http://creativecommons.org/licenses/by-sa/3.0/us/>.

Neither this module nor the L<Author|/Author> is affiliated with BitTorrent,
Inc.

=for rcs $Id$

=cut
