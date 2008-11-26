#!/usr/bin/perl
use strict;
use warnings;
use Getopt::Long;
use Pod::Usage;
use Carp qw[croak carp];
use Time::HiRes qw[sleep];
use lib q[../lib];
use Net::BitTorrent::Protocol qw[:types];
use Net::BitTorrent;
$|++;
my ($VERSION, $dir, $check, $int, $man, $help, $port, $ver, @torrents)
    = (sprintf(q[%.3f], (qw$Rev$)[1] / 1000), q[./], 1, 1, 0);
GetOptions(q[help|?]       => \$help,
           q[man]          => \$man,
           q[torrent|t=s@] => \@torrents,
           q[port:i]       => \$port,
           q[directory:s]  => \$dir,
           q[check!]       => \$check,
           q[version]      => \$ver
) or pod2usage(2);
@torrents = grep {-f} @torrents, @ARGV;
$ver && exit printf <<VER, $0, $VERSION, $Net::BitTorrent::VERSION, $^V, $^O;
%s version %.3f
Net::BitTorrent version %s
Perl version %vd on %s
VER
pod2usage({-verbose    => 99,
           -sections   => q[NAME|Options|Author|License and Legal],
           -exitstatus => 0
          }
) if $man;
pod2usage({-verbose    => 99,
           -sections   => q[NAME|Synopsis|Author],
           -exitstatus => 1
          }
    )
    if $help
        or !@torrents;
my $bt = new Net::BitTorrent({LocalPort => $port})
    or croak sprintf q[Failed to create Net::BitTorrent object (%s)], $^E;
$bt->on_event(
    q[incoming_packet],
    sub {
        trans_status(q[RECEIVED], $_[1]) if $_[1]->{q[Type]} eq PIECE;
    }
);
$bt->on_event(
    q[outgoing_packet],
    sub {
        trans_status(q[REQUESTING], $_[1]) if $_[1]->{q[Type]} eq REQUEST;
    }
);
$bt->on_event(q[piece_hash_pass], sub { piece_status(q[pass], $_[1]); });
$bt->on_event(q[piece_hash_fail], sub { piece_status(q[fail], $_[1]); });
for my $path (@torrents) {
    stat print qq[Loading '$path'...];
    my $obj = $bt->add_torrent({Path => $path, BaseDir => $dir})
        || carp(qq[Cannot load '$path': $^E]) && next;
    print qq[Hash checking...\n] and $obj->hashcheck if $check;
    printf qq[Loaded '$path' [%s...%s%s]\n],
        ($obj->infohash =~ (m[^(.{4}).+(.{4})$])),
        ($obj->private ? q[|No DHT] : q[]);
}
$SIG{q[INT]} = sub {
    $int = ($int + 10 > time) ? exit : time;
    print qq[\n--> Press Ctrl-C again within 10 seconds to exit <--\n]
        . $bt->_as_string(1)
        . (join q[\n], map { $_->_as_string(1) } values %{$bt->torrents});
};
$bt->do_one_loop(0.25) && sleep(0.50) while 1;

sub piece_status {
    my ($msg, $args) = @_;
    my $torrent = $args->{q[Torrent]};
    return printf qq[hash%s: %04d|%s|%4d/%4d|% 3.2f%%\r],
        $msg, $args->{q[Index]}, $torrent->_as_string(),
        (scalar grep {$_} split q[], unpack(q[b*], $torrent->bitfield)),
        $torrent->piece_count,
        (((  (scalar grep {$_} split q[], unpack q[b*], $torrent->bitfield)
           / ($torrent->piece_count)
          )
         ) * 100
        );
}

sub trans_status {
    printf(qq[%-10s p:%15s:%-5d i:%4d o:%7d l:%5d\r],
           shift,
           $_[0]->{q[Peer]}->_host,
           $_[0]->{q[Peer]}->_port,
           $_[0]->{q[Payload]}->{q[Index]},
           $_[0]->{q[Payload]}->{q[Offset]},
           $_[0]->{q[Payload]}->{q[Length]}
    );
}

=pod

=head1 NAME

bittorrent - Very basic example BitTorrent client

=head1 Synopsis

 bittorrent file.torrent
   or
 bittorrent [options] [file ...]

 Options:
   -t     --torrent     .torrent file to load
   -p     --port        TCP/UDP port opened for incoming connections
   -d     --directory   Base directory to store downloaded files
   --no-check           Skip integrity check at start
   -?     --help        Display brief help message
   --man                Display full documentation
   --version            Display version information

=head1 Options

=over 8

=item B<--torrent>

Open this .torrent file.

You may pass several -torrent parameters and load more than one .torrent
torrent.

=item B<--port>

Port number opened to the world for incoming connections.  This defaults
to C<0> and lets L<Net::BitTorrent|Net::BitTorrent> bind to a random,
unused port.

=item B<--directory>

Relative or absolute directory used as a base directory for storage.  By
default, this is the current working directory.

Please see L<Net::BitTorrent::Torrent|Net::BitTorrent::Torrent> for
related information.

=item B<--no-check>

If found, the files will not be checked for integrity and we assume that
we have none of the data of this torrent.

=item B<--help>

Print a brief help message and exit.

=item B<--man>

Print the manual page and exit.

=item B<--version>

Guess.

=back

=head1 Description

This is a B<very> basic demonstration of a full C<Net::BitTorrent>-based
client.

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
