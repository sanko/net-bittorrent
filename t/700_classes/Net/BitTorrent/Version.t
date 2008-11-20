#!C:\perl\bin\perl.exe -w
use strict;
use warnings;
use Test::More;
use Module::Build;
use lib q[../../../../lib];
use Net::BitTorrent::Version;
$|++;
my $test_builder       = Test::More->builder;
my $simple_dot_torrent = q[./t/900_data/950_torrents/953_miniswarm.torrent];
chdir q[../../../../] if not -f $simple_dot_torrent;
my $build           = Module::Build->current;
my $okay_tcp        = $build->notes(q[okay_tcp]);
my $release_testing = $build->notes(q[release_testing]);
my $verbose         = $build->notes(q[verbose]);
$SIG{__WARN__} = ($verbose ? sub { diag shift } : sub { });
$|++;
plan tests => 5;
SKIP: {
    like(Net::BitTorrent::Version->gen_peerid(),
         qr[^NB\d{3}[SU]-.{13}$],
         q[Peer ID conforms to spec. (initial test)]
    );
    if ($Net::BitTorrent::Version::VERSION =~ m[_]) {
        like(Net::BitTorrent::Version->gen_peerid(),
             qr[^NB\d{3}U-.{13}$],
             q[Peer ID conforms to spec. (unstable/SVN build)]
        );
        my $old_value = $Net::BitTorrent::Version::UNSTABLE_RELEASE;
        $Net::BitTorrent::Version::UNSTABLE_RELEASE = 0;
        like(Net::BitTorrent::Version->gen_peerid(),
             qr[^NB\d{3}S-.{13}$],
             q[Peer ID conforms to spec. (fake stable/CPAN build)]
        );
        $Net::BitTorrent::Version::UNSTABLE_RELEASE = $old_value;
    }
    else {
        like(Net::BitTorrent::Version->gen_peerid(),
             qr[^NB\d{3}S-.{13}$],
             q[Peer ID conforms to spec. (stable/CPAN build)]
        );
        my $old_value = $Net::BitTorrent::Version::UNSTABLE_RELEASE;
        $Net::BitTorrent::Version::UNSTABLE_RELEASE = 1;
        like(Net::BitTorrent::Version->gen_peerid(),
             qr[^NB\d{3}U-.{13}$],
             q[Peer ID conforms to spec. (fake unstable/SVN build)]
        );
        $Net::BitTorrent::Version::UNSTABLE_RELEASE = $old_value;
    }
    like(Net::BitTorrent::Version->gen_peerid(),
         qr[^NB\d{3}[SU]-.{13}$], q[Peer ID conforms to spec.]);
    ok(length(Net::BitTorrent::Version->gen_node_id()) == 20,
        q[DHT node id conforms to spec.]);
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
