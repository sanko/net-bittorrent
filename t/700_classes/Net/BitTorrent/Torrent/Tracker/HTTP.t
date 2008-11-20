#!C:\perl\bin\perl.exe -w
use strict;
use warnings;
use Module::Build;
use Test::More;
use File::Temp qw[tempdir];
use Scalar::Util qw[/weak/];
use lib q[../../../../../../lib];
use Net::BitTorrent::Torrent::Tracker::HTTP;
use Net::BitTorrent::Torrent::Tracker;
use Net::BitTorrent;
$|++;
my $test_builder       = Test::More->builder;
my $simple_dot_torrent = q[./t/900_data/950_torrents/953_miniswarm.torrent];
chdir q[../../../../../../] if not -f $simple_dot_torrent;
my $build           = Module::Build->current;
my $okay_tcp        = $build->notes(q[okay_tcp]);
my $release_testing = $build->notes(q[release_testing]);
my $verbose         = $build->notes(q[verbose]);
$SIG{__WARN__} = ($verbose ? sub { diag shift } : sub { });
my ($flux_capacitor, %peers) = (0, ());
plan tests => 15;
SKIP: {
    my ($tempdir)
        = tempdir(q[~NBSF_test_XXXXXXXX], CLEANUP => 1, TMPDIR => 1);
    warn(sprintf(q[File::Temp created '%s' for us to play with], $tempdir));
    my $client = Net::BitTorrent->new({LocalHost => q[127.0.0.1]});
    if (!$client) {
        diag(sprintf q[Socket error: [%d] %s], $!, $!);
        skip(q[Failed to create client],
             (      $test_builder->{q[Expected_Tests]}
                  - $test_builder->{q[Curr_Test]}
             )
        );
    }
    my $torrent = $client->add_torrent({Path    => $simple_dot_torrent,
                                        BaseDir => $tempdir
                                       }
    );

    END {
        return if not defined $torrent;
        for my $file (@{$torrent->files}) { $file->_close() }
    }
    my $_tier = Net::BitTorrent::Torrent::Tracker->new(
               {URLs => [qw[udp://blah.net/announce/]], Torrent => $torrent});
    is(Net::BitTorrent::Torrent::Tracker::HTTP->new(),
        undef, q[new() == undef]);
    is(Net::BitTorrent::Torrent::Tracker::HTTP->new({}),
        undef, q[new({ }) == undef]);
    is(Net::BitTorrent::Torrent::Tracker::HTTP->new({URL => []}),
        undef, q[new({URL=>[]}) == undef]);
    is( Net::BitTorrent::Torrent::Tracker::HTTP->new(
                                                   {URL => q[udp://blah.net/]}
        ),
        undef,
        q[new({URL=>q[udp://blah.net/]}) == undef]
    );
    is( Net::BitTorrent::Torrent::Tracker::HTTP->new(
                                         {URL => q[http://blah.net/announce/]}
        ),
        undef,
        q[new({URL=>q[http://blah.net/announce/]}) == undef]
    );
    is( Net::BitTorrent::Torrent::Tracker::HTTP->new(
                       {URL => q[http://blah.net/announce/], Tier => q[blah!]}
        ),
        undef,
        q[new({URL=>q[http://blah.net/announce/], Tier => q[blah!]}) == undef]
    );
    is( Net::BitTorrent::Torrent::Tracker::HTTP->new(
                                      {URL  => q[http://blah.net/announce/],
                                       Tier => bless \$verbose,
                                       $verbose
                                      }
        ),
        undef,
        q[new({URL=>q[http://blah.net/announce/], Tier => [a random blessed object]}) == undef]
    );
    my $_ip_address = Net::BitTorrent::Torrent::Tracker::HTTP->new(
                      {URL => q[http://127.0.0.1/announce/], Tier => $_tier});
    my $_host_address = Net::BitTorrent::Torrent::Tracker::HTTP->new(
                      {URL => q[http://localhost/announce/], Tier => $_tier});
    isa_ok($_ip_address,
           q[Net::BitTorrent::Torrent::Tracker::HTTP],
           q[{URL=>q[http://127.0.0.1/announce/], Tier => [...]}]);
    isa_ok($_host_address,
           q[Net::BitTorrent::Torrent::Tracker::HTTP],
           q[{URL=>q[http://localhost/announce/], Tier => [...]}]);
    skip(
        q[HTTP-based tests have been disabled due to system misconfiguration.],
        ($test_builder->{q[Expected_Tests]} - $test_builder->{q[Curr_Test]})
    ) unless $okay_tcp;
    is($_ip_address->_announce(q[SomeUnexpectedEvent]),
        undef, q[_announce('SomeUnexpectedEvent') == undef]);
    ok($_ip_address->_announce(),           q[_announce() == okay]);
    ok($_ip_address->_announce(q[started]), q[_announce('started') == okay]);
    ok($_ip_address->_announce(q[stopped]), q[_announce('stopped') == okay]);
    ok($_ip_address->_announce(q[completed]),
        q[_announce('completed') == okay]);
    ok($_ip_address->_as_string, q[_as_string]);
    warn q[TODO: Install event handlers];
}

=head1 Author

Sanko Robinson <sanko@cpan.org> - http://sankorobinson.com/

CPAN ID: SANKO

=head1 License and Legal

Copyright (C) 2008 by Sanko Robinson E<lt>sanko@cpan.orgE<gt>

This program is free software; you can redistribute it and/or modify
it under the terms of The Artistic License 2.0.  See the F<LICENSE>
file included with this distribution or
http://www.perlfoundation.org/artistic_license_2_0.  For
clarification, see http://www.perlfoundation.org/artistic_2_0_notes.

When separated from the distribution, all POD documentation is covered
by the Creative Commons Attribution-Share Alike 3.0 License.  See
http://creativecommons.org/licenses/by-sa/3.0/us/legalcode.  For
clarification, see http://creativecommons.org/licenses/by-sa/3.0/us/.

Neither this module nor the L<Author|/Author> is affiliated with
BitTorrent, Inc.

=for svn $Id$

=cut
