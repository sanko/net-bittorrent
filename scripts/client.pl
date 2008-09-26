#!/usr/bin/perl
use strict;
use warnings;
use Getopt::Long;
use Pod::Usage;
use List::Util qw[sum];
use Carp qw[cluck croak carp];
use lib q[../lib];
use Net::BitTorrent;
$|++;
my $man          = 0;
my $help         = 0;
my $verbose      = 0;
my $localport    = 0;
my @dot_torrents = ();
my $basedir      = q[./];
my $hashcheck    = 1;
my $dht          = 1;
my $sig_int      = 0;
my $loaded_okay  = 0;
GetOptions(q[help|?]             => \$help,
           q[man]                => \$man,
           q[torrent|t=s@]       => \@dot_torrents,
           q[port|p:i]           => \$localport,
           q[store|d|base_dir:s] => \$basedir,
           q[hashcheck!]         => \$hashcheck,
           q[dht!]               => \$dht,
           q[verbose|v]          => \$verbose
) or pod2usage(2);

if (not scalar @dot_torrents and scalar @ARGV) {
    push @dot_torrents, shift @ARGV while (defined $ARGV[0] and -f $ARGV[0]);
}
pod2usage(1) if $help or not scalar @dot_torrents;
pod2usage(-verbose => 2) if $man;
my $bt = new Net::BitTorrent({LocalPort => $localport})
    or croak sprintf q[Failed to create Net::BitTorrent object (%s)], $^E;

#
$SIG{q[INT]} = sub {    # One Ctrl-C combo shows status.  Two exits.
    if ($sig_int + 10 > time) { exit; }
    $sig_int = time;
    print qq[\n]
        . (q[=] x 10)
        . q[> Press Ctrl-C again within 10 seconds to exit <]
        . (q[=] x 10);

    #return print $client->as_string(1); # TODO
};

#
sub piece_status {
    my ($msg, $args) = @_;

    #
    my $session  = $args->{q[Session]};
    my $bitfield = $session->bitfield;
    printf q[%shash%s: %04d|%s|%4d/%4d|%3.2f%%%s],
        qq[\r], $msg, $args->{q[Index]},
        $$session,
        sum(split q[], unpack(q[b*], $session->bitfield)),
        $session->_piece_count,
        ((((sum split q[], unpack q[b*], $bitfield) / ($session->_piece_count)
          )
         ) * 100
        ),
        ($loaded_okay ? qq[\n] : q[]);

    # ...we should really seed a while first...
    if ($session->_complete) { $bt->remove_session($session); }

    #
    return 1;
}
$bt->on_event(
    q[packet_incoming_block],
    sub {
        my ($self, $args) = @_;
        return
            printf(qq[\rRECIEVED   p:%15s:%-5d i:%4d o:%7d l:%5d],
                   $args->{q[Peer]}->_host, $args->{q[Peer]}->_port,
                   $args->{q[Index]},       $args->{q[Offset]},
                   $args->{q[Length]}
            );
    }
);
$bt->on_event(
    q[packet_outgoing_request],
    sub {
        my ($self, $args) = @_;
        return
            printf(qq[\rREQUESTING p:%15s:%-5d i:%4d o:%7d l:%5d],
                   $args->{q[Peer]}->_host, $args->{q[Peer]}->_port,
                   $args->{q[Index]},       $args->{q[Offset]},
                   $args->{q[Length]}
            );
    }
);
$bt->on_event(
    q[piece_hash_pass],
    sub {
        my ($self, $args) = @_;
        return piece_status(q[pass], $args);
    }
);
$bt->on_event(
    q[piece_hash_fail],
    sub {
        my ($self, $args) = @_;
        return piece_status(q[fail], $args);
    }
);

#
if ($verbose) {

    # Advanced
    $bt->on_event(
        q[file_error],
        sub {
            my ($self, $args) = @_;

            #
            printf qq[\rFile error ('%s'): %s], $args->{q[File]}->path,
                $args->{q[Message]};
        }
    );
    $bt->on_event(
        q[file_close],
        sub {
            my ($self, $args) = @_;

            #
            printf qq[\rClosed '%s'], $args->{q[File]}->path;
        }
    );
    $bt->on_event(
        q[file_open],
        sub {
            my ($self, $args) = @_;

            #
            printf qq[\rOpened '%s' for %s], $args->{q[File]}->path,
                (($args->{q[Mode]} eq q[r]) ? q[read] : q[write]);
        }
    );
    $bt->on_event(
        q[file_read],
        sub {
            my ($self, $args) = @_;

            #
            printf qq[\rRead %d bytes from '%s'], $args->{q[Length]},
                $args->{q[File]}->path;
        }
    );
    $bt->on_event(
        q[file_write],
        sub {
            my ($self, $args) = @_;

            #
            printf qq[\rWrote %d bytes to '%s'], $args->{q[Length]},
                $args->{q[File]}->path;
        }
    );
    $bt->on_event(
        q[peer_read],
        sub {
            my ($self, $args) = @_;

            #
            printf qq[\rRead %d bytes from '%s'], $args->{q[Length]},
                $args->{q[Peer]}->_as_string;
        }
    );
    $bt->on_event(
        q[peer_write],
        sub {
            my ($self, $args) = @_;

            #
            printf qq[\rWrote %d bytes to '%s'], $args->{q[Length]},
                $args->{q[Peer]}->_as_string;
        }
    );
    $bt->on_event(
        q[peer_disconnect],
        sub {
            my ($self, $args) = @_;

            #
            printf qq[\rDisconnected from '%s'%s],
                $args->{q[Peer]}->_as_string,
                ($args->{q[Reason]}
                 ? (q[ (] . $args->{q[Reason]} . q[)])
                 : q[]
                );
        }
    );
    $bt->on_event(
        q[peer_connect],
        sub {
            my ($self, $args) = @_;

            #
            printf qq[\r%s connection %s '%s'],
                ($args->{q[Peer]}->_incoming ? q[Incoming] : q[Outgoing]),
                ($args->{q[Peer]}->_incoming ? q[from]     : q[to]),
                $args->{q[Peer]}->_as_string;
        }
    );
    $bt->on_event(
        q[packet_incoming_request],
        sub {
            my ($self, $args) = @_;

            #
            printf qq[\r%s is requesting [I:%4d O:%6d L:%6d]],
                $args->{q[Peer]}->_as_string,
                $args->{q[Index]},
                $args->{q[Offset]},
                $args->{q[Length]};
        }
    );
    $bt->on_event(
        q[packet_outgoing_request],
        sub {
            my ($self, $args) = @_;

            #
            printf qq[\rRequesting [I:%4d O:%6d L:%6d] from %s],
                $args->{q[Index]},
                $args->{q[Offset]},
                $args->{q[Length]},
                $args->{q[Peer]}->_as_string;
        }
    );
    $bt->on_event(
        q[packet_incoming_cancel],
        sub {
            my ($self, $args) = @_;

            #
            printf qq[\r%s is canceling [I:%4d O:%6d L:%6d]],
                $args->{q[Peer]}->_as_string,
                $args->{q[Index]},
                $args->{q[Offset]},
                $args->{q[Length]};
        }
    );
    $bt->on_event(
        q[packet_outgoing_cancel],
        sub {
            my ($self, $args) = @_;

            #
            printf qq[\rCanceling [I:%4d O:%6d L:%6d] from %s],
                $args->{q[Index]},
                $args->{q[Offset]},
                $args->{q[Length]},
                $args->{q[Peer]}->_as_string;
        }
    );
    $bt->on_event(
        q[packet_incoming_block],
        sub {
            my ($self, $args) = @_;

            #
            printf
                qq[\r%s sent us [I:%4d O:%6d L:%6d] I have now downloaded %d bytes],
                $args->{q[Peer]}->_as_string,
                $args->{q[Index]},
                $args->{q[Offset]},
                $args->{q[Length]},
                $args->{q[Peer]}->_session->_downloaded;
        }
    );
    $bt->on_event(
        q[packet_outgoing_block],
        sub {
            my ($self, $args) = @_;

            #
            printf
                qq[\rSending [I:%4d O:%6d L:%6d] to %s. I have now uploaded %d bytes],
                $args->{q[Index]},
                $args->{q[Offset]},
                $args->{q[Length]},
                $args->{q[Peer]}->_as_string,
                $args->{q[Peer]}->_session->_uploaded;
        }
    );
    $bt->on_event(
        q[packet_incoming_have],
        sub {
            my ($self, $args) = @_;

            #
            printf qq[\r%s has %4d],
                $args->{q[Peer]}->_as_string,
                $args->{q[Index]};
        }
    );
    $bt->on_event(
        q[packet_outgoing_interested],
        sub {
            my ($self, $args) = @_;

            #
            printf qq[\rI am interested in %s], $args->{q[Peer]}->_as_string;
        }
    );
    $bt->on_event(
        q[packet_incoming_interested],
        sub {
            my ($self, $args) = @_;

            #
            printf qq[\r%s is interested in me], $args->{q[Peer]}->_as_string;
        }
    );
    $bt->on_event(
        q[packet_outgoing_unchoke],
        sub {
            my ($self, $args) = @_;

            #
            printf qq[\rUnchoking %s], $args->{q[Peer]}->_as_string;
        }
    );
    $bt->on_event(
        q[packet_incoming_unchoke],
        sub {
            my ($self, $args) = @_;

            #
            printf qq[\rUnchoked by %s], $args->{q[Peer]}->_as_string;
        }
    );
    $bt->on_event(
        q[packet_outgoing_choke],
        sub {
            my ($self, $args) = @_;

            #
            printf qq[\rChoking %s], $args->{q[Peer]}->_as_string;
        }
    );
    $bt->on_event(
        q[packet_incoming_choke],
        sub {
            my ($self, $args) = @_;

            #
            printf qq[\rChoked by %s], $args->{q[Peer]}->_as_string;
        }
    );
    $bt->on_event(
        q[packet_outgoing_bitfield],
        sub {
            my ($self, $args) = @_;

            #
            printf qq[\rBitfield to %s], $args->{q[Peer]}->_as_string;
        }
    );
    $bt->on_event(
        q[packet_incoming_bitfield],
        sub {
            my ($self, $args) = @_;

            #
            return printf qq[\rBitfield from %s],
                $args->{q[Peer]}->_as_string;
        }
    );
    $bt->on_event(
        q[packet_incoming_handshake],
        sub {
            my ($self, $args) = @_;

            #
            return printf qq[\rHandshake from %s],
                $args->{q[Peer]}->_as_string;
        }
    );
    $bt->on_event(
        q[ip_filter],
        sub {
            my ($self, $args) = @_;

            #
            printf qq[\rCheck IP filter for %s], $args->{q[Address]};

            # Return true (or undef) if the node is okay,
            # Return an explicitly false value (0) if this address is banned
            return;
        }
    );
}

#
TORRENT: for my $dot_torrent (sort @dot_torrents) {
    next if not -e $dot_torrent;
    printf q[Loading '%s'...], $dot_torrent;
    my $session =
        $bt->add_session({Path    => $dot_torrent,
                          BaseDir => $basedir
                         }
        );
    if (not defined $session) {
        carp sprintf q[Cannot load .torrent (%s): %s], $dot_torrent, $^E;
        next TORRENT;
    }

    # TODO: command line param for file priorities
    #for my $file (@{$session->files}) {
    #    $file->set_priority($file->path =~ m[(?:[avi|(?:jm)pe?g])$] ? 1 : 0);
    #    $file->set_priority($file->path =~ m[jpe?g$] ? 50 : $file->priority);
    #    printf qq[[%d] prio: %d - %s\n], $file->index, $file->priority,
    #        $file->path;
    #}
    #
    if ($hashcheck) {
        print qq[Hash checking...\n];
        $session->hashcheck;
    }
    else {
        print qq[Hash checking... Skipped\n];
    }

    #
    printf qq[\rLoaded '%s' [%s...%s%s]\n], $dot_torrent,
        ($$session =~ (m[^(.{4}).+(.{4})$])),
        ($session->_private ? q[|No DHT] : q[]);
}

#
$loaded_okay = 1;

#
if ($dht) { $bt->_use_dht(1); }

#
$bt->do_one_loop(1) while keys %{$bt->sessions};

#
exit;
__END__

=pod

=head1 NAME

client.pl - Very basic BitTorrent client

=head1 Synopsis

client.pl [options] [file ...]

 Options:
   -torrent           .torrent file to load
   -port              port number opened to incoming connections
   -store             base directory to store downloaded files
   --no-hashcheck     skip integrity check at start
   --no-dht           disable DHT
   -verbose           display all sorts of useless info
   -help              brief help message
   -man               full documentation

=head1 Options

=over 8

=item B<-torrent>

Open this .torrent file.

You may pass several -torrent parameters and load more than one .torrent
session.

=item B<-port>

Port number opened to the world for incoming connections.  This defaults
to C<0> and lets L<Net::BitTorrent|Net::BitTorrent> bind to a random,
unused port.

=item B<-store>

Relative or absolute directory to store downloaded files.  All files will
be downloaded using this as the base directory.  Single file torrents
will go directly into this directory, multifile torrents will create a
directory within this and download there.  By default, this is the
current working directory.

=item B<--no-hashcheck>

If found, the files will not be checked for integrity and we assume that
we have none of the data of this torrent.

=item B<--no-dht>

Disables DHT.

=item B<-verbose>

Print loads of extra information as we download; Stuff like file open
messages, packet transmission status, etc.  Useful for bug reporting.

=item B<-help>

Print a brief help message and exit.

=item B<-man>

Print the manual page and exit.

=back

=head1 Description

This is a B<very> basic demonstration of a full C<Net::BitTorrent>-based
client.

=for svn $Id$

=cut
