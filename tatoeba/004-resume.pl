#!perl -w -I../lib
use strict;
use warnings;
use Time::HiRes qw[sleep];
use Net::BitTorrent;
use Data::Dump qw[pp];
my $client = Net::BitTorrent->new();
my $torrent = $client->add_torrent({Path => 'a.legal.torrent'}) or exit;
$torrent->on_event(q[piece_hash_pass], sub { save(shift) });
$torrent->hashcheck;
sleep(0.25) and $client->do_one_loop(0.25) while !$torrent->is_complete;

END {
    for my $t (values %{$client->torrents || {}}) { save($t); }
}

sub save {
    my ($torrent) = @_;
    rename $torrent->path, $torrent->path . q[.bak]
        if !-f $torrent->path . q[.bak];
    open my $TORRENT, q[>], $torrent->path or next;
    syswrite($TORRENT, $torrent->resume_data) or next;
    close $TORRENT;
}

=pod

=head1 NAME

/tatoeba/004-resume.pl - Demonstration of Net::BitTorrent::Torrent's Resume System

=head1 Description

This is a basic example of how the Resume System built into
L<Net::BitTorrent::Torrent|Net::BitTorrent::Torrent> is to be used.

=head1 Synopsis

 004-resume.pl

=head1 Lowdown

=over

=item Line 8

Loads our .torrent file.  If
L<Net::BitTorrent::Torrent|Net::BitTorrent::Torrent/"new ( { [ARGS] } )">
finds any resume data, it's automatically restored.

=item Line 9

Sets a per-torrent callback which, when triggered, saves our resume data.

=item Line 13-15

This is probably the most important place to save resume data because on
restore, the last modified times of each file is compared with the times
stored in the resume data.  If any of them fail to match, all of the
resume data is considered invalid.

Here, at the end of the process, we store resume data for every torrent
in the client.  Yes, yes, I know... this little script only loads a
single torrent.  Consider it a bonus for folks writing your own your own
clients.  A small and obvious bonus, sure, but a bonus all the same.

=item Line 19

Let's keep a backup of the original metadata just in case
L<Net::BitTorrent::Torrent|Net::BitTorrent::Torrent> (slim chance) or you
(rather likely) makes a mistake and ruins everything.

=item Line 21

Opens the .torrent file in write mode.  Once the file is parsed in
L<C<new( )>|Net::BitTorrent::Torrent/"new ( { [ARGS] } )">,
L<Net::BitTorrent::Torrent|Net::BitTorrent::Torrent> is finished with it,
so if the file is locked and fails to open here, some other process may
be to blame.

=item Line 22

Writes the new
L<'resume data'|Net::BitTorrent::Torrent/"resume_data ( [ RAW ] )"> to
the file.  Notice I'm using C<syswrite> here because the resume data will
certainly contain binary data.  For more, see the warnings listed in
L<Net::BitTorrent::Notes|Net::BitTorrent::Notes/"badresumedata">.

Now, the next time L<Net::BitTorrent::Torrent|Net::BitTorrent::Torrent">
loads this file, it will see the resume data and if it looks okay, your
progress will be restored.

=back

=head1 Notes

In this script, we store resume data after each piece validates and at
the end of the script.  In practice, you may want to store this on a more
regular basis or on a schedule (every half hour).  I B<do not> suggest
saving resume data every time a block is written to disk; true, this
would keep resume data as up to date as possible, but there are certain
internal steps taken while resume data is gathered that would, in the
long run, slow everything down to a crawl.  For more, see
L<C<resume_data ( )>|Net::BitTorrent::Torrent/"resume_data ( [ RAW ] )"> in
L<Net::BitTorrent::Torrent|Net::BitTorrent::Torrent> and the sections
L<'Resume API'|Net::BitTorrent::Notes/"Resume API"> and
L<'How do I quick Resume a .torrent Session Between Client Sessions?'|Net::BitTorrent::Notes/"Quick Resume a .torrent Session Between Client Sessions">
in L<Net::BitTorrent::Notes|Net::BitTorrent::Notes>.

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
