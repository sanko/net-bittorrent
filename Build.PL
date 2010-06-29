#!/usr/bin/perl -w
use strict;
use warnings;
use File::Find;
use Module::Build;
$|++;
my $automated_testing = $ENV{'AUTOMATED_TESTING'}
    || $ENV{'PERL_MM_USE_DEFAULT'};
my $is_developer = ((-d '.git') ? 1 : 0) or ($ENV{'RELEASE_TESTING'} ? 1 : 0);
my @tests;
find \&find_cb, qw[t/10000_by_class/];
@tests = reverse sort @tests;
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
    requires      => {
                 'AnyEvent'                   => 0,
                 'AnyEvent::Handle::Throttle' => '0.000002002',
                 'Bit::Vector'                => 0,
                 'Moose'                      => 0,
                 'Digest::SHA'                => 5.45,
                 'Exporter'                   => 0,
                 'File::Path'                 => 0,
                 'File::Spec'                 => 0,
                 'Module::Build'              => 0.30,
                 'perl'                       => '5.10.0',
                 'Scalar::Util'               => 1.19,
                 'Test::More'                 => 0.80
    },
    build_requires => {'Module::Build' => 0.30,
                       'Test::More'    => 0.80,
                       'perl'          => '5.10.0',
                       'Module::Build' => 0.30,
                       'TAP::Harness'  => 3.18
    },
    recommends       => {'perl' => '5.12.0'},
    test_files       => \@tests,
    test_file_exts   => [qw(.t .tap .txt)],
    use_tap_harness  => 1,
    tap_harness_args => {
        jobs  => 9,
        rules => {
            par => [
                {seq => '/t/10000_by_class/Net/BitTorrent/DHT/*'},

                #{ seq => '../ext/IO_Compress_Zlib/t/*' },
                #{ seq => '../lib/CPANPLUS/*' },
                #{ seq => '../lib/ExtUtils/t/*' },
                '*'
            ]
        }
    },
    meta_merge => {
        keywords  => [qw[BitTorrent torrent client DHT UDP]],
        resources => {
            bugtracker => {
                       web => 'http://github.com/sanko/net-bittorrent/issues',
                       mailto => 'sanko@cpan.org'
            },
            repository => {
                url => 'git://github.com/sanko/net-bittorrent.git',
                web =>
                    'http://github.com/sanko/net-bittorrent/tree/summer-2010-moose-anyevent',
                type => 'git'
            },
            ChangeLog =>
                'http://github.com/sanko/net-bittorrent/commits/summer-2010-moose-anyevent/',
            homepage => 'http://sankorobinson.com/net-bittorrent/',
            license  => ['http://www.perlfoundation.org/artistic_license_2_0']
        }
    }
);
$mb->notes(automated_testing => $automated_testing ? 1 : 0);
$mb->notes(release_testing   => $is_developer);
$mb->notes(test_suite        => \@tests);
$mb->notes(gmtime            => gmtime);
$mb->notes(verbose => scalar grep {m[^v$]} keys %{$mb->args()});
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
