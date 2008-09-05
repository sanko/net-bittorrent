# ideal brief client
use strict;
use warnings;

#
use List::Util qw[sum];

#
$|++;

#
use lib q[../lib];
use Net::BitTorrent;

#
my $bt = Net::BitTorrent->new({LocalPort => 1338}) or die $^E;

#warn pp $bt;
#print q[.];
#sleep 10;
#
my $x = $bt->add_session({Path    => q[test.torrent],
                          BaseDir => q[q:\net-bittorrent]
                         }
);

#
for my $file (@{$x->files}) {
    $file->set_priority($file->path =~ m[(?:[avi|(?:jm)pe?g])$] ? 1 : 0);
    warn sprintf q[[%d] prio: %d - %s ], $file->index, $file->priority,
        $file->path;
}

#die;
#sleep 10;
#print q[.];
#
$bt->on_event(
    q[piece_hash_pass],
    sub {
        my ($self, $args) = @_;

        #
        my $session = $args->{q[Session]};
        warn sprintf q[pass %6d | %6d of %6d | %40s], $args->{q[Index]},
            sum(split q[], unpack(q[b*], $session->bitfield)),
            $session->_piece_count, $$session;
        if (index(unpack(q[b*], $session->_wanted), q[1], 0) == -1) {
            $bt->remove_session($session);

            #exit;
        }
    }
);
$bt->on_event(
    q[piece_hash_fail],
    sub {
        my ($self, $args) = @_;

        #
        my $session = $args->{q[Session]};
        warn sprintf q[fail %6d | %6d of %6d | %40s], $args->{q[Index]},
            sum(split q[], unpack(q[b*], $session->bitfield)),
            $session->_piece_count, $$session;
    }
);

# Advanced
$bt->on_event(
    q[file_error],
    sub {
        my ($self, $args) = @_;

        #
        warn sprintf q[File error ('%s'): %s], $args->{q[File]}->path,
            $args->{q[Message]};
    }
);
$bt->on_event(
    q[file_close],
    sub {
        my ($self, $args) = @_;

        #
        warn sprintf q[Closed '%s'], $args->{q[File]}->path;
    }
);
$bt->on_event(
    q[file_open],
    sub {
        my ($self, $args) = @_;

        #
        warn sprintf q[Opened '%s' for %s], $args->{q[File]}->path,
            (($args->{q[Mode]} eq q[r]) ? q[read] : q[write]);
    }
);
$bt->on_event(
    q[file_read],
    sub {
        my ($self, $args) = @_;

        #
        warn sprintf q[Read %d bytes from '%s'], $args->{q[Length]},
            $args->{q[File]}->path;
    }
);
$bt->on_event(
    q[file_write],
    sub {
        my ($self, $args) = @_;

        #
        warn sprintf q[Wrote %d bytes to '%s'], $args->{q[Length]},
            $args->{q[File]}->path;
    }
);
$bt->on_event(
    q[peer_read],
    sub {
        my ($self, $args) = @_;

        #
        warn sprintf q[Read %d bytes from '%s'], $args->{q[Length]},
            $args->{q[Peer]}->as_string;
    }
);
$bt->on_event(
    q[peer_write],
    sub {
        my ($self, $args) = @_;

        #
        warn sprintf q[Wrote %d bytes to '%s'], $args->{q[Length]},
            $args->{q[Peer]}->as_string;
    }
);
$bt->on_event(
    q[peer_disconnect],
    sub {
        my ($self, $args) = @_;

        #
        warn sprintf q[Disconnected from '%s'%s], $args->{q[Peer]}->as_string,
            ($args->{q[Reason]} ? (q[ (] . $args->{q[Reason]} . q[)]) : q[]);
    }
);
$bt->on_event(
    q[peer_connect],
    sub {
        my ($self, $args) = @_;

        #
        warn sprintf q[%s connection %s '%s'],
            ($args->{q[Peer]}->_incoming ? q[Incoming] : q[Outgoing]),
            ($args->{q[Peer]}->_incoming ? q[from]     : q[to]),
            $args->{q[Peer]}->as_string;
    }
);
$bt->on_event(
    q[packet_incoming_request],
    sub {
        my ($self, $args) = @_;

        #
        warn sprintf q[%s is requesting [I:%4d O:%6d L:%6d]],
            $args->{q[Peer]}->as_string,
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
        warn sprintf q[Requesting [I:%4d O:%6d L:%6d] from %s],
            $args->{q[Index]},
            $args->{q[Offset]},
            $args->{q[Length]},
            $args->{q[Peer]}->as_string;
    }
);
$bt->on_event(
    q[packet_incoming_cancel],
    sub {
        my ($self, $args) = @_;

        #
        warn sprintf q[%s is canceling [I:%4d O:%6d L:%6d]],
            $args->{q[Peer]}->as_string,
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
        warn sprintf q[Canceling [I:%4d O:%6d L:%6d] from %s],
            $args->{q[Index]},
            $args->{q[Offset]},
            $args->{q[Length]},
            $args->{q[Peer]}->as_string;
    }
);
$bt->on_event(
    q[packet_incoming_block],
    sub {
        my ($self, $args) = @_;

        #
        warn sprintf
            q[%s sent us [I:%4d O:%6d L:%6d] I have now downloaded %d bytes],
            $args->{q[Peer]}->as_string,
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
        warn sprintf
            q[Sending [I:%4d O:%6d L:%6d] to %s. I have now uploaded %d bytes],
            $args->{q[Index]},
            $args->{q[Offset]},
            $args->{q[Length]},
            $args->{q[Peer]}->as_string,
            $args->{q[Peer]}->_session->_uploaded;
    }
);
$bt->on_event(
    q[packet_incoming_have],
    sub {
        my ($self, $args) = @_;

        #
        warn sprintf q[%s has %4d],
            $args->{q[Peer]}->as_string,
            $args->{q[Index]};
    }
);
$bt->on_event(
    q[packet_outgoing_interested],
    sub {
        my ($self, $args) = @_;

        #
        warn sprintf q[I am interested in %s], $args->{q[Peer]}->as_string;
    }
);
$bt->on_event(
    q[packet_incoming_interested],
    sub {
        my ($self, $args) = @_;

        #
        warn sprintf q[%s is interested in me], $args->{q[Peer]}->as_string;
    }
);
$bt->on_event(
    q[packet_outgoing_unchoke],
    sub {
        my ($self, $args) = @_;

        #
        warn sprintf q[Unchoking %s], $args->{q[Peer]}->as_string;
    }
);
$bt->on_event(
    q[packet_incoming_unchoke],
    sub {
        my ($self, $args) = @_;

        #
        warn sprintf q[Unchoked by %s], $args->{q[Peer]}->as_string;
    }
);
$bt->on_event(
    q[packet_outgoing_choke],
    sub {
        my ($self, $args) = @_;

        #
        warn sprintf q[Choking %s], $args->{q[Peer]}->as_string;
    }
);
$bt->on_event(
    q[packet_incoming_choke],
    sub {
        my ($self, $args) = @_;

        #
        warn sprintf q[Choked by %s], $args->{q[Peer]}->as_string;
    }
);
$bt->on_event(
    q[packet_outgoing_bitfield],
    sub {
        my ($self, $args) = @_;

        #
        warn sprintf q[Bitfield to %s], $args->{q[Peer]}->as_string;
    }
);
$bt->on_event(
    q[packet_incoming_bitfield],
    sub {
        my ($self, $args) = @_;

        #
        return warn sprintf q[Bitfield from %s], $args->{q[Peer]}->as_string;
    }
);
$bt->on_event(
    q[packet_incoming_handshake],
    sub {
        my ($self, $args) = @_;

        #
        return warn sprintf q[Handshake from %s], $args->{q[Peer]}->as_string;
    }
);
$bt->on_event(
    q[ip_filter],
    sub {
        my ($self, $args) = @_;

        #
        warn sprintf q[Check IP filter for %s], $args->{q[Address]};

        # Return true (or undef) if the node is okay,
        # Return an explicitly false value (0) if this address is banned
        return;
    }
);

#
for (values %{$bt->sessions}) {
    warn sprintf q[Hash Checking '%s'...], $_->path;
    $_->hashcheck;
}

#$bt->on_event(q[file_open], undef);    # removes event callback
#$bt->do_one_loop(5) until $t->complete;    # This could use a better name...
$bt->do_one_loop(5) while keys %{$bt->sessions};

# 6248|3436
#
exit;
##########################################
# ideal long
#
use Net::BitTorrent;

#
my $bt2 = Net::BitTorrent->new({LocalPort => 555,
                                LocalHost => q[127.0.0.1],
                               }
);

# Basic events
$bt2->on_event(q[peer_check_filter], sub { my ($client, $peer) = @_; })
    ;    # XXX - better name
$bt2->on_event(q[?session_complete], sub { my ($client, $session) = @_; });
$bt2->on_event(q[tracker_announce],  sub { my ($client, $tracker) = @_; });
$bt2->on_event(q[?log],              sub { my ($client, $file)    = @_; });

# Advanced events
$bt2->on_event(q[packet_incoming_handshake],
               sub { my ($client, $peer) = @_; });
$bt2->on_event(q[packet_incoming_bitfield],
               sub { my ($client, $peer) = @_; });
$bt2->on_event(q[packet_incoming_block],
               sub { my ($client, $peer, $block) = @_; });
$bt2->on_event(q[packet_incoming_cancel],
               sub { my ($client, $peer, $request) = @_; });
$bt2->on_event(q[packet_incoming_choke], sub { my ($client, $peer) = @_; });
$bt2->on_event(q[packet_incoming_data],
               sub { my ($client, $peer, $length_int) = @_; });
$bt2->on_event(q[packet_incoming_disinterested],
               sub { my ($client, $peer) = @_; });
$bt2->on_event(q[packet_incoming_handshake],
               sub { my ($client, $peer) = @_; });
$bt2->on_event(q[packet_incoming_have],
               sub { my ($client, $peer, $piece) = @_; });
$bt2->on_event(q[packet_incoming_interested],
               sub { my ($client, $peer) = @_; });
$bt2->on_event(q[packet_incoming_keepalive],
               sub { my ($client, $peer) = @_; });
$bt2->on_event(q[packet_incoming_packet],
               sub { my ($client, $peer, $packet_hash) = @_; });
$bt2->on_event(q[packet_incoming_request],
               sub { my ($client, $peer, $request) = @_; });
$bt2->on_event(q[packet_incoming_unchoke], sub { my ($client, $peer) = @_; });
$bt2->on_event(q[packet_outgoing_bitfield],
               sub { my ($client, $peer) = @_; });
$bt2->on_event(q[packet_outgoing_block],
               sub { my ($client, $peer, $request) = @_; });
$bt2->on_event(q[packet_outgoing_cancel],
               sub { my ($client, $peer, $block) = @_; });
$bt2->on_event(q[packet_outgoing_choke], sub { my ($client, $peer) = @_; });
$bt2->on_event(q[packet_outgoing_data],
               sub { my ($client, $peer, $length_int) = @_; });
$bt2->on_event(q[packet_outgoing_not_interested],
               sub { my ($client, $peer) = @_; });
$bt2->on_event(q[packet_outgoing_handshake],
               sub { my ($client, $peer) = @_; });
$bt2->on_event(q[packet_outgoing_have],
               sub { my ($client, $peer, $piece) = @_; });
$bt2->on_event(q[packet_outgoing_interested],
               sub { my ($client, $peer) = @_; });
$bt2->on_event(q[packet_outgoing_keepalive],
               sub { my ($client, $peer) = @_; });
$bt2->on_event(q[packet_outgoing_request],
               sub { my ($client, $peer, $block) = @_; });
$bt2->on_event(q[packet_outgoing_unchoke], sub { my ($client, $peer) = @_; });
$bt2->on_event(q[packet_outgoing_port],    sub { my ($client, $peer) = @_; });
$bt2->on_event(q[tracker_connect],    sub { my ($client, $tracker) = @_; });
$bt2->on_event(q[tracker_disconnect], sub { my ($client, $tracker) = @_; });
$bt2->on_event(q[tracker_failure],
               sub { my ($client, $tracker, $message) = @_; });
$bt2->on_event(q[tracker_incoming_data],
               sub { my ($client, $tracker, $length) = @_; });
$bt2->on_event(q[tracker_outgoing_data],
               sub { my ($client, $tracker, $length) = @_; });

#
$bt2->do_one_loop() while $bt2->torrents;
__END__

