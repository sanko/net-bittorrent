{

    package Net::BitTorrent::Protocol::BEP03::Peer;
    use Moose;
    use lib '../../../../../lib';
    extends 'Net::BitTorrent::Peer';
    use Net::BitTorrent::Protocol::BEP03::Packets qw[:all];

    sub _build_reserved {
        my ($self) = @_;
        my @reserved = qw[0 0 0 0 0 0 0 0];
        $reserved[5] |= 0x10;    # Ext Protocol
        $reserved[7] |= 0x04;    # Fast Ext
        return join '', map {chr} @reserved;
    }

    sub _send_handshake {
        my $s = shift;
        confess 'torrent is undefined' if !$s->_has_torrent;
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
    sub _handle_packet_handshake  {...}
    sub _handle_packet_choke      { shift->_set_remote_choked }
    sub _handle_packet_unchoke    { shift->_unset_remote_choked }
    sub _handle_packet_interested { shift->_set_remote_interested }
    after '_unset_remote_choked' => sub {
        my $s = shift;
        return if $s->_has_quest('request_block');
        require Scalar::Util;
        Scalar::Util::weaken $s;
        my $max_requests = 4;    # XXX - max_requests attribute
        $s->_add_quest(
            'request_block',
            AE::timer(
                0, 3,
                sub {
                    return if !defined $s;
                    for (0 .. $max_requests) {
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
        warn $i;
        warn $s->pieces->bit_test($i) ? 'already have! :(' : 'yay';
        my $x = $s->pieces->to_Bin;
        $s->_set_piece($i);
        warn $s->pieces->bit_test($i)
            ? 'now have! :D'
            : 'aw, man... :( Broken!';
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
             $s->_has_peer_id ? $s->peer_id : '[Unknown peer]',
             $i,
             $have, $seed, $perc,
             $s->torrent->info_hash->to_Hex
            }
        );
        die if $x eq $s->pieces->to_Bin;
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
             $s->_has_peer_id ? $s->peer_id : '[Unknown peer]',
             $have, $seed, $perc,
             $s->torrent->info_hash->to_Hex
            }
        );
    }
    after qw[_handle_packet_bitfield _handle_packet_have] => sub {
        shift->_check_interest;
    };

    sub _handle_packet_request {
        my ($s, $r) = @_;
        my ($i, $o, $l) = @$r;

        # XXX - Choke peer if they have too many requests in queue
        $s->_add_remote_request($r);
        require Scalar::Util;
        Scalar::Util::weaken($s);
        $s->_add_quest(
            'fill_remote_requests',
            AE::timer(
                0, 15,
                sub {
                    return if !defined $s;

               # XXX - return if outgoing data queue is larger than block x 8?
                    my $request = $s->_shift_remote_requests;
                    $s->_delete_quest('fill_remote_requests')
                        if !$s->_count_remote_requests;

                    # XXX - make sure we have this piece
                    return $s->_send_piece(@$request);
                }
            )
        ) if !$s->_has_quest('fill_remote_requests');
    }
    sub _handle_packet_piece {...}

    sub _handle_packet_ext_protocol {
        my ($s, $pid, $p) = @_;
        if ($pid == 0) {    # Setup/handshake
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

    sub _handle_packet {
        my ($s, $p) = @_;
        return if $s->local_connection && !$s->_has_torrent;
        return if !$s->_has_handle;
        %_packet_dispatch = ($HANDSHAKE   => 'handshake',
                             $CHOKE       => 'choke',
                             $UNCHOKE     => 'unchoke',
                             $INTERESTED  => 'interested',
                             $HAVE        => 'have',
                             $BITFIELD    => 'bitfield',
                             $REQUEST     => 'request',
                             $PIECE       => 'piece',
                             $EXTPROTOCOL => 'ext_protocol'
        ) if !keys %_packet_dispatch;
        $s->trigger_peer_packet_in(
                         {peer     => $s,
                          packet   => $p,
                          severity => 'debug',
                          message  => sprintf 'Recieved %s packet from %s',
                          $_packet_dispatch{$p->{'type'}} // 'unknown packet',
                          $s->_has_peer_id ? $s->peer_id : '[Unknown peer]'
                         }
        );
        my $code
            = $s->can('_handle_packet_' . $_packet_dispatch{$p->{'type'}});
        return $code->($s, $p->{'payload'}) if $code;
        return if !eval 'require Data::Dump;';
        ddx $p;
    }
    override 'disconnect' => sub {
        my ($s) = @_;
        if (!$s->_handle->destroyed) {
            $s->_handle->push_shutdown;
            $s->_handle->destroy;
        }
        super;
    };

    # Outgoing packets
    sub _send_interested     { shift->push_write(build_interested()) }
    sub _send_not_interested { shift->push_write(build_not_interested()) }
    sub _send_choke          { shift->push_write(build_choke()) }
    sub _send_unchoke        { shift->push_write(build_unchoke()) }

    sub _send_bitfield {
        my $s = shift;
        $s->push_write(build_bitfield($s->torrent->have));
    }

    sub _send_request {
        my ($s, $b) = @_;
        return if $s->choked;
        warn sprintf 'Sending request for %d:%d:%d to %s', $b->index,
            $b->offset, $b->length, $s->peer_id;
        return $s->push_write(
                            build_request($b->index, $b->offset, $b->length));
    }

    sub _send_piece {
        my ($s, $i, $o, $l) = @_;
        return if $s->choked;
        warn sprintf 'Sending block %d:%d:%d to %s', $i, $o, $l, $s->peer_id;
        return $s->push_write(
                      build_piece($i, $o, $l, $s->torrent->read($i, $o, $l)));
    }

    #
    no Moose;
    no Moose::Util::TypeConstraints;
    __PACKAGE__->meta->make_immutable
}
1;

=pod

=head1 NAME

Net::BitTorrent::Protocol::BEP03::Peer - Old skool, TCP-based peer

=head1 Description

Go away.

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
