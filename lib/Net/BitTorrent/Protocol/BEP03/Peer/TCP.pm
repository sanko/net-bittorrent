package Net::BitTorrent::Protocol::BEP03::Peer::TCP;
{
    use Moose;
    use lib '../../../../../lib';

    #extends 'Net::BitTorrent::Peer';
    use Net::BitTorrent::Protocol::BEP03::Packets qw[:all];
    our $MAJOR = 0; our $MINOR = 74; our $DEV = 14; our $VERSION = sprintf('%0d.%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    sub _build_reserved {
        my ($self) = @_;
        my @reserved = qw[0 0 0 0 0 0 0 0];
        $reserved[5] |= 0x10;    # Ext Protocol
        $reserved[7] |= 0x04;    # Fast Ext
        return join '', map {chr} @reserved;
    }

    sub _send_handshake {
        my $s = shift;
        confess 'torrent is undefined' if !$s->has_torrent;
        my $packet = build_handshake($s->_build_reserved,
                                     $s->torrent->info_hash,
                                     $s->client->peer_id
        );
        $s->push_write($packet);

        #$s->_set_handshake_step(
        #                 $s->local_connection ? 'REG_THREE' : 'REG_OKAY');
        #1;
    }
    my %_packet_dispatch;
    sub _handle_packet_handshake { ...; }
    sub _handle_packet_keepalive {;}        # Noop

    sub _handle_packet_choke {
        my $s = shift;
        $s->_set_remote_choked;
        for my $b (@{$s->requests}) {
            $s->_delete_request($b);
            $b->clear_peer;
        }
    }
    sub _handle_packet_unchoke    { shift->_unset_remote_choked }
    sub _handle_packet_interested { shift->_set_remote_interested }
    after '_set_remote_choked' =>
        sub { shift->_delete_quest('request_block') };
    after '_unset_remote_choked' => sub {
        my $s = shift;
        return if $s->has_quest('request_block');
        require Scalar::Util;
        Scalar::Util::weaken $s;
        $s->_add_quest(
            'request_block',
            AE::timer(
                3, 5,
                sub {
                    $s // return;
                    my $now_requests = scalar $s->_queued_requests;

                    #warn sprintf '...now %d (%d) with %s', $now_requests,
                    #    $s->_count_requests, $s->peer_id;
                    for (
                        $now_requests .. (    # XXX - max_requests attribute
                              $now_requests >= 12 ? 15
                            : $now_requests >= 8  ? 12
                            : $now_requests >= 5  ? 8
                            : $now_requests >= 3  ? 5
                            : 3
                        )
                        )
                    {   $s // last;
                        $s->_wanted_pieces->Norm || last;
                        my $piece = $s->torrent->select_piece($s);
                        next if !$piece;
                        my $b = $piece->_first_unassigned_block();
                        next if !$b;
                        $s->_add_request($b);
                    }
                }
            )
        );
    };

    sub _handle_packet_have {
        my ($s, $i) = @_;
        $s->_set_piece($i);
        my $seed = $s->torrent->piece_count;
        my $have = $s->pieces->Norm;
        my $perc = (($have / $seed) * 100);
        $s->trigger_peer_have(
            {peer     => $s,
             index    => $i,
             severity => 'debug',
             message  => sprintf
                 '%s:%d (%s) has piece #%d and now claims to have %d out of %d pieces (%3.2f%%) of %s',
             $s->host, $s->port,
             $s->has_peer_id ? $s->peer_id : '[Unknown peer]',
             $i,
             $have, $seed, $perc,
             $s->torrent->info_hash->to_Hex
            }
        );
    }

    sub _handle_packet_bitfield {
        my ($s, $b) = @_;
        $s->_set_pieces($b);
        my $seed = $s->torrent->piece_count;
        my $have = $s->pieces->Norm();
        my $perc = (($have / $seed) * 100);
        $s->trigger_peer_bitfield(
            {peer     => $s,
             severity => 'debug',
             message  => sprintf
                 '%s:%d (%s) claims to have %d out of %d pieces (%3.2f%%) of %s',
             $s->host, $s->port,
             $s->has_peer_id ? $s->peer_id : '[Unknown peer]',
             $have, $seed, $perc,
             $s->torrent->info_hash->to_Hex
            }
        );
    }
    after qw[_handle_packet_bitfield _handle_packet_have] => sub {
        shift->_check_interest;
    };
    after '_handle_packet_handshake' => sub {
        my $s = shift;
        $s->_add_quest(
            'send_keepalive',
            AE::timer(
                30,
                5 * 60,
                sub {
                    return if !defined $s;
                    $s->_send_keepalive;
                }
            )
        );
    };

    sub _handle_packet_request {
        my ($s, $r) = @_;
        my ($i, $o, $l) = @$r;
        return
            $s->disconnect(sprintf 'Bad piece index in request: %d > %d',
                           $i, $s->torrent->piece_count)
            if $i > $s->torrent->piece_count;
        my $_l
            = ($i == $s->torrent->piece_count - 1)
            ? $s->torrent->size % $s->torrent->piece_length
            : $s->torrent->piece_length;
        return
            $s->disconnect(sprintf 'Bad piece length for index %d: %d > %d',
                           $i, $l, $_l)
            if $l > $_l;
        return
            $s->disconnect(sprintf 'Bad offset for index %d: %d > %d',
                           $i, $o + $l, $_l)
            if $o + $l > $_l;

        # XXX - Choke peer if they have too many requests in queue
        $s->_add_remote_request($r);
        require Scalar::Util;
        Scalar::Util::weaken($s);
        $s->_add_quest(
            'fill_remote_requests',
            AE::timer(
                5, 15,
                sub {

                    #warn 'HERE!!!!!!!!!!!!';
                    return if !defined $s;

               #warn 'Here...';
               # XXX - return if outgoing data queue is larger than block x 8?
                    my $request = $s->_shift_remote_requests;
                    $s->_delete_quest('fill_remote_requests')
                        if !$s->_count_remote_requests;

                    # XXX - make sure we have this piece
                    return $s->_send_piece(@$request);
                }
            )
        ) if !$s->has_quest('fill_remote_requests');
    }

    sub _handle_packet_piece {
        my ($s, $p) = @_;
        $p // return;
        my ($i, $o, $d) = @$p;
        warn sprintf 'peer sent i:%d o:%d l:%d', $i, $o, length $d;
        my $req = $s->_find_request($i, $o, length $d);
        $req // return $s->disconnect(
                               'Peer sent us a block we were not asking for');
        $req->_write($d);
        $s->_delete_request($req);
        return if $req->piece->_first_incompete_block;
        my $piece
            = $s->torrent->piece_selector->_get_working_piece($req->index);
        $_->has_peer && $_->peer->_delete_request($_)
            for $piece->_complete_blocks;

        if ($s->torrent->hash_check($req->index)) {

            # XXX - High five everyone involved
            $s->torrent->piece_selector->_del_working_piece($p);
        }
        else {

            # XXX - Kick everyone involved
            $piece->clear_blocks;    # Try again
        }
    }

    sub _handle_packet_ext_protocol {
        my ($s, $pid, $p) = @_;
        if ($pid == 0) {             # Setup/handshake
            if (defined $p->{'p'} && $s->client->has_dht) {
                $s->client->dht->ipv4_add_node(
                            [join('.', unpack 'C*', $p->{'ipv4'}), $p->{'p'}])
                    if defined $p->{'ipv4'};
                $s->client->dht->ipv6_add_node(
                    [   [join ':', (unpack 'H*', $p->{'ipv6'}) =~ m[(....)]g],
                        $p->{'p'}
                    ]
                ) if defined $p->{'ipv6'};
            }
        }
        return 1;
    }

    sub _handle_packet_cancel {
        my ($s, $r) = @_;
        my ($i, $o, $l) = @$r;
        $s->_delete_remote_request($r);

        #return
        #    $s->disconnect(sprintf 'Bad piece index in request: %d > %d',
        #                   $i, $s->torrent->piece_count)
        #    if $i > $s->torrent->piece_count;
    }

    sub _handle_packet_reject {
        my ($s, $r) = @_;
        my ($i, $o, $l) = @$r;
        warn sprintf 'peer rejected i:%d o:%d l:%d', $i, $o, $l;
        my $p = $s->torrent->piece_selector->_get_working_piece($i);
        $p // return $s->disconnect(
                             'Peer rejected a block we were not asking for.');
        my $b = $p->_get_block($o, $l);
        $b // return $s->disconnect(
                               'Peer rejected a block which does not exist.');
        $b->has_peer
            || return $s->disconnect(
                  'Peer sent us a block we have yet to request from anyone.');
        $b->peer->peer_id eq $s->peer_id
            || return $s->disconnect(
                   'Peer rejected a block we asked someone else to give us.');
        $s->_delete_request($b);
        $b->clear_peer;

        #return
        #    $s->disconnect(sprintf 'Bad piece index in request: %d > %d',
        #                   $i, $s->torrent->piece_count)
        #    if $i > $s->torrent->piece_count;
    }
    sub _handle_packet_have_all  { shift->_set_seed }
    sub _handle_packet_have_none { shift->_unset_seed }

    #after qr[^_handle_packet_[^(keepalive|reject|choke)]+$]
    after [qw[_handle_packet_piece _handle_packet_request]] => sub {
        my $s = shift;
        $s->clear_timeout;
        $s->timeout;
    };
    after 'BUILDALL' => sub { shift->clear_timeout };

    # XXX - Old version
    # use AnyEvent;
    # use Scalar::Util;
    #has 'timeout' => (isa      => 'Ref',
    #                  is       => 'ro',
    #                  init_arg => undef,
    #                  clearer  => 'clear_timeout',
    #                  builder  => '_build_timeout',
    #                  required => 1
    #);
    #sub _build_timeout {
    #    my $s = shift;
    #    Scalar::Util::weaken $s;
    #    AE::timer(
    #        10 * 60,
    #        0,
    #        sub {
    #            $s // return;
    #            $s->disconnect('Peer disconnected due to inactivity');
    #        }
    #    );
    #}
    sub _handle_packet {
        my ($s, $p) = @_;
        return if $s->local_connection && !$s->has_torrent;
        return if !$s->has_handle;
        %_packet_dispatch = ($KEEPALIVE   => 'keepalive',
                             $HANDSHAKE   => 'handshake',
                             $CHOKE       => 'choke',
                             $UNCHOKE     => 'unchoke',
                             $INTERESTED  => 'interested',
                             $HAVE        => 'have',
                             $BITFIELD    => 'bitfield',
                             $REQUEST     => 'request',
                             $PIECE       => 'piece',
                             $EXTPROTOCOL => 'ext_protocol',
                             $REJECT      => 'reject',
                             $HAVE_ALL    => 'have_all',
                             $HAVE_NONE   => 'have_none'
        ) if !keys %_packet_dispatch;
        $s->trigger_peer_packet_in(
                       {peer     => $s,
                        packet   => $p,
                        severity => 'debug',
                        message  => sprintf 'Recieved %s packet from %s',
                        $_packet_dispatch{$p->{'type'}} // 'unknown packet ( '
                            . ($p->{'type'} // $p->{'payload'}) . ' )',
                        $s->has_peer_id ? $s->peer_id : '[Unknown peer]'
                       }
        );
        my $code
            = $s->can('_handle_packet_' . $_packet_dispatch{$p->{'type'}});
        return $code->($s, $p->{'payload'}) if $code;
        return if !eval 'require Data::Dump;';
        Data::Dump::ddx($p);
    }
    override 'disconnect' => sub {
        my ($s) = @_;
        if (!$s->handle->destroyed) {
            $s->handle->push_shutdown if defined $s->handle->{'fh'};
            $s->handle->destroy;
        }
        super;
    };

    # Outgoing packets
    sub _send_keepalive      { shift->push_write(build_keepalive()) }
    sub _send_interested     { shift->push_write(build_interested()) }
    sub _send_not_interested { shift->push_write(build_not_interested()) }
    sub _send_have           { shift->push_write(build_have(shift)) }
    sub _send_choke          { shift->push_write(build_choke()) }
    sub _send_unchoke        { shift->push_write(build_unchoke()) }

    sub _send_bitfield {
        my $s = shift;
        $s->push_write(build_bitfield($s->torrent->have));
    }

    sub _send_request {
        my ($s, $b) = @_;
        return if $s->remote_choked;
        warn sprintf 'Sending request for i:%d o:%d l:%d to %s', $b->index,
            $b->offset, $b->length, $s->peer_id;
        return $s->push_write(
                            build_request($b->index, $b->offset, $b->length));
    }

    sub _send_cancel {
        my ($s, $b) = @_;
        return if $s->remote_choked;
        warn sprintf 'Sending cancel for i:%d o:%d l:%d to %s', $b->index,
            $b->offset, $b->length, $s->peer_id;
        return $s->push_write(
                             build_cancel($b->index, $b->offset, $b->length));
    }

    sub _send_piece {
        my ($s, $i, $o, $l) = @_;
        return if $s->choked;
        warn sprintf 'Sending block i:%d o:%d l:%d to %s', $i, $o, $l,
            $s->peer_id;
        return $s->push_write(
                      build_piece($i, $o, $l, $s->torrent->read($i, $o, $l)));
    }
    sub DEMOLISH { $_->clear_peer for @{shift->requests} }

    #
    no Moose;
    __PACKAGE__->meta->make_immutable
}
1;

=pod

=head1 NAME

Net::BitTorrent::Protocol::BEP03::Peer::TCP - Old skool, TCP-based peer

=head1 Description

Go away.

=head1 Author

Sanko Robinson <sanko@cpan.org> - http://sankorobinson.com/

CPAN ID: SANKO

=head1 License and Legal

Copyright (C) 2008-2011 by Sanko Robinson <sanko@cpan.org>

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
