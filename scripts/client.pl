#!/usr/bin/perl
use strict;
use warnings;
use Getopt::Long;
use Pod::Usage;
use Carp qw[cluck croak carp];
use lib q[../lib];
use Net::BitTorrent;
$|++;
my $man            = 0;
my $help           = 0;
my $localport      = 0;
my @dot_torrents   = ();
my $basedir        = q[./];
my $skip_hashcheck = 0;
my $sig_int        = 0;
GetOptions(q[help|?]             => \$help,
           q[man]                => \$man,
           q[torrent|t=s@]       => \@dot_torrents,
           q[port|p:i]           => \$localport,
           q[store|d|base_dir:s] => \$basedir,
           q[skip_hashcheck]     => \$skip_hashcheck
) or pod2usage(2);

if (not scalar @dot_torrents and scalar @ARGV) {
    push @dot_torrents, shift @ARGV while (defined $ARGV[0] and -f $ARGV[0]);
}
pod2usage(1) if $help or not scalar @dot_torrents;
pod2usage(-verbose => 2) if $man;
my $client = new Net::BitTorrent(
    {LocalPort => $localport,

     #LocalPort => [80, 6881 .. 6889],
     #LocalPort => 80,
     #LocalAddr   => q[127.0.0.1],
    }
    )
    or croak sprintf q[Failed to create Net::BitTorrent object (%s)],
    $^E;
$SIG{q[INT]} = sub {    # One Ctrl-C combo shows status.  Two exits.
    if ($sig_int + 10 > time) { exit; }
    $sig_int = time;
    print q[=] x 10
        . q[> Press Ctrl-C again within 10 seconds to exit <]
        . (q[=] x 10), qq[\n];
    return print $client->as_string(1);
};

sub hashpass {
    my ($self, $piece) = @_;
    my $session = $piece->session;
    return printf qq[hashpass: %04d|%s|%4d/%4d|%3.2f%%\n],
        $piece->index,
        $$session,
        (scalar grep { $_->check } @{$session->pieces}),
        (scalar @{$session->pieces}),
        ((  (scalar grep { $_->check } @{$session->pieces})
          / (scalar @{$session->pieces})
         )
        ) * 100;
}

sub request_out {
    my ($self, $peer, $request) = @_;
    return
        printf(qq[REQUESTING p:%15s:%-5d i:%4d o:%7d l:%5d\n],
               $peer->peerhost,  $peer->peerport, $request->index,
               $request->offset, $request->length
        );
}

sub block_in {
    my ($self, $peer, $block) = @_;
    return
        printf(qq[RECIEVED   p:%15s:%-5d i:%4d o:%7d l:%5d\n],
               $peer->peerhost, $peer->peerport, $block->index,
               $block->offset,  $block->length,
        );
}
$client->set_callback(q[peer_incoming_block],   \&block_in);
$client->set_callback(q[peer_outgoing_request], \&request_out);
$client->set_callback(q[piece_hash_pass],       \&hashpass);

#$client->set_callback(q[log], sub { shift; shift; warn shift; } );
#$client->debug_level(1000);

for my $dot_torrent (sort @dot_torrents) {
    next if not -e $dot_torrent;
    printf q[Loading '%s'...], $dot_torrent;
    my $session =
        $client->add_session({path           => $dot_torrent,
                              base_dir       => $basedir,
                              skip_hashcheck => $skip_hashcheck
                             }
        )
        or carp sprintf q[Cannot load .torrent (%s): %s],
        $dot_torrent, $^E;
    printf qq[ OK. Infohash = %s\n], $$session;
}
while (scalar $client->sessions > 0) { $client->do_one_loop }
__END__

=pod

=head1 Name

basic.pl - Very basic BitTorrent client

=head1 SYNOPSIS

basic [options] [file ...]

 Options:
   -torrent         .torrent file to load
   -port            port number opened to incoming connections
   -store           base directory to store downloaded files
   -skip_hashcheck  skip integrity check at start
   -help            brief help message
   -man             full documentation

=head1 Options

=over 8

=item B<-torrent>

Open this .torrent file.

You may pass several -torrent parameters and load more than one
.torrent session.

=item B<-port>

Port number opened to the world for incoming connections.  This defaults
to C<0> and lets IO::Socket bind to a random, unused port.

=item B<-store>

Relative or absolute directory to store downloaded files.  All files will
be downloaded using this as the base directory.  Single file torrents
will go directly into this directory, multifile torrents will create a
directory within this and download there.  By default, this is the
current working directory.

=item B<-skip_hashcheck>

If found, the files will not be checked for integrity and we assume that
we have none of the data of this torrent.

=item B<-help>

Print a brief help message and exit.

=item B<-man>

Print the manual page and exit.

=back

=head1 Description

This is a B<very> basic demonstration of a full
C<Net::BitTorrent>-based client.

=for svn $Id$

=cut
