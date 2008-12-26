#!/usr/bin/perl
use strict;
use warnings;
use Getopt::Long;
use Pod::Usage;
use Carp qw[croak carp];
use Time::HiRes qw[sleep];
use lib q[../lib];
use Net::BitTorrent::Protocol qw[:types];
use Net::BitTorrent::Util qw[:bencode];
use Net::BitTorrent::Torrent qw[:status];
use Net::BitTorrent;
$|++;
my ($dir, $chk, $int, $VERSION, $port, $ver, @tor, %opts)
    = (q[./], 1, 1, sprintf q[%.3f], (qw$Rev$)[1] / 1000);
GetOptions(
    q[check!]      => \$chk,
    q[directory:s] => \$dir,
    q[help|?]      => sub {
        pod2usage({-verbose  => 99,
                   -sections => q[Synopsis|Options|Author]
                  }
        );
    },
    q[options=s%] => sub { $opts{$_[1]} = $_[2] if $_[1] =~ m[^_set] },
    q[port:i]     => \$port,
    q[torrent=s@] => \@tor,
    q[version] => sub { exit printf qq[$0 v$VERSION] },
) || pod2usage(2);
@tor = grep {-f} @tor, @ARGV;
pod2usage({-verbose  => 99,
           -sections => q[Synopsis|Author],
          }
) if !@tor;
my $bt = new Net::BitTorrent({LocalPort => $port})
    or croak q[Failed to create Net::BitTorrent object];

sub piece_status {
    my $args   = pop;
    my $t      = $args->{q[Torrent]};
    my $have   = grep {$_} split //, unpack q[b*], $t->bitfield;
    my $wanted = grep {$_} split //, unpack q[b*], $t->_wanted;
    return printf qq[%s: %04d|%s|%4d/%4d|% 3.2f%%\r],
        pop, $args->{q[Index]}, $t->as_string(), $have, $t->piece_count,
        100 - ($wanted / $t->piece_count * 100);
}

sub trans_status {
    printf qq[%-10s p:%15s:%-5d i:%4d o:%7d l:%5d  \r],
        shift, $_[0]->{q[Peer]}->_host, $_[0]->{q[Peer]}->_port,
        $_[0]->{q[Payload]}{q[Index]},
        $_[0]->{q[Payload]}{q[Offset]},
        $_[0]->{q[Payload]}{q[Length]};
}

sub save {
    for my $torrent (values %{$bt->torrents || {}}) {
        open my $TORRENT, q[>], $torrent->path or next;
        syswrite($TORRENT, $torrent->resume_data) or next;
    }
}
$SIG{q[INT]} = sub {
    save();
    $int = $int + 3 > time ? exit : time;
    print join qq[\n], map { $_->as_string(1) } $bt, values %{$bt->torrents};
};
END { save if $bt }
$bt->on_event(
    q[incoming_packet],
    sub {
        trans_status(q[REJECTED], $_[1]) if $_[1]{q[Type]} eq REJECT;
        trans_status(q[RECEIVED], $_[1]) if $_[1]{q[Type]} eq PIECE;
    }
);
$bt->on_event(
    q[outgoing_packet],
    sub {
        trans_status(q[REQUESTING], $_[1]) if $_[1]{q[Type]} eq REQUEST;
        trans_status(q[CANCELING],  $_[1]) if $_[1]{q[Type]} eq CANCEL;
        trans_status(q[SENDING],    $_[1]) if $_[1]{q[Type]} eq PIECE;
    }
);
$bt->on_event(q[piece_hash_pass], sub { piece_status(q[pass], $_[1]); });
$bt->on_event(q[piece_hash_fail], sub { piece_status(q[fail], $_[1]); });
for my $path (@tor) {
    my $obj = $bt->add_torrent({Path    => $path,
                                BaseDir => $dir,
                                Status  => ($chk ? START_AFTER_CHECK : ())
                               }
    ) || next;
    $obj->status & CHECKED || $obj->hashcheck;
    printf qq[Loaded '$path' [%s...%s%s]\n],
        $obj->infohash =~ m[^(.{4}).+(.{4})$], $obj->private ? q[] : q[|DHT];
}
$bt->on_event(q[file_error], sub { carp $_[1]->{q[Message]} });
for my $obj ($bt, values %{$bt->torrents}) {
    for my $opt (keys %opts) { $obj->$opt($opts{$opt}) if $obj->can($opt); }
}
$bt->do_one_loop(0.25) && sleep(0.50) while 1;

=pod

=head1 NAME

bittorrent - Very basic example BitTorrent client

=head1 Description

This is a B<very> basic demonstration of a full C<Net::BitTorrent>-based
client.

=head1 Synopsis

 bittorrent file.torrent
   or
 bittorrent [options] [file ...]

 Options:
   -t     --torrent     .torrent file to load
   -p     --port        TCP/UDP port opened for incoming connections
   -d     --directory   Base directory to store downloaded files
   --no-check           Skip integrity check at start
   --options            Advanced settings
   -?     --help        Display full documentation
   --version            Display version information

To get client-wide progress updates, press C<Ctrl+C>.  For more, see
perldoc.

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

=item B<--options>

Allows otherwise private settings to be changed.  For example, to set the
upload bandwidth limit...

  bittorrent --options _set_max_ul_rate=8192 [...]

You may pass several C<--options> parameters.

=item B<--version>

Guess.

=back

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
