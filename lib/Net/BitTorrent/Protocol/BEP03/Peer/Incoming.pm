{

    package Net::BitTorrent::Protocol::BEP03::Peer::Incoming;
    use Moose;
    use Moose::Util::TypeConstraints;
    use lib '../../../../../../lib';
    use Net::BitTorrent::Types qw[:addr];
    extends 'Net::BitTorrent::Protocol::BEP03::Peer';
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 10; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    has '_handle' => (
        is          => 'ro',
        isa         => 'AnyEvent::Handle::Throttle',
        predicate   => '_has_handle',
        initializer => '_initializer_handle',
        init_arg    => 'handle',
        required    => 1,
        handles     => {
            rbuf           => 'rbuf',
            push_read      => 'push_read',
            push_write     => 'push_write',
            total_download => 'download_total',
            fh             => sub { shift->_handle->{'fh'} },
            host           => sub {
                my $s = shift;
                return $_[0] = undef  #$s->disconnect('Failed to open socket')
                    if !defined $s->fh;    # XXX - error creating socket?
                require Socket;
                my (undef, $addr) = Socket::sockaddr_in(getpeername($s->fh));
                require Net::BitTorrent::Network::Utility;
                Net::BitTorrent::Network::Utility::paddr2ip($addr);
            },
            port => sub {
                my $s = shift;
                return $_[0] = undef  #$s->disconnect('Failed to open socket')
                    if !defined $s->fh;    # XXX - error creating socket?
                require Socket;
                my ($port, undef) = Socket::sockaddr_in(getpeername($s->fh));
                $port;
                }
        }
    );

    sub _initializer_handle {
        my ($s, $c, $set, $attr) = @_;
        $set->($c);
        require Scalar::Util;
        Scalar::Util::weaken $s;
        $c->on_read(
            sub {
                return if !defined $s;
                require Net::BitTorrent::Protocol::BEP03::Packets;
            PACKET:
                while (
                      my $p =
                      Net::BitTorrent::Protocol::BEP03::Packets::parse_packet(
                                                            \$s->_handle->rbuf
                      )
                    )
                {   $s->_handle_packet($p);
                    last PACKET if !defined $s || !$s->rbuf || !$p;
                }
            }
        );

      #on_connect => sub {
      #           return if !defined $s;
      #           $s->_send_handshake;
      #       },
      #       on_read => sub {
      #           return if !defined $s;
      #           require Net::BitTorrent::Protocol::BEP03::Packets;
      #       PACKET:
      #           while (
      #               my $p =
      #               Net::BitTorrent::Protocol::BEP03::Packets::parse_packet(
      #                                                   \$s->_handle->rbuf
      #               )
      #               )
      #           {   $s->_handle_packet($p);
      #               last PACKET if !defined $s || !$s->rbuf || !$p;
      #           }
      #       }
    }
    my $infohash_constraint;

    sub _handle_packet_handshake {
        my ($s, $p) = @_;
        my ($reserved, $info_hash, $peer_id) = @$p;
        $infohash_constraint //=
            Moose::Util::TypeConstraints::find_type_constraint(
                                                'NBTypes::Torrent::Infohash');
        $info_hash = $infohash_constraint->coerce($info_hash);
        $s->_set_support_extensions(ord(substr($reserved, 5, 1)) & 0x10);
        $s->_set_peer_id($peer_id);
        my $t = $s->client->torrent($info_hash);
        return
            $s->disconnect(sprintf 'Bad info_hash (We are not serving %s)',
                           $info_hash->to_Hex)
            if !$t;
        $s->_set_torrent($t);
        $s->_check_unique_connection;
        return if !defined $s;

        # send handshake
        return if !defined $t;
        $s->_send_handshake;
        $s->_send_bitfield;
    }

    #
    no Moose;
    no Moose::Util::TypeConstraints;
    __PACKAGE__->meta->make_immutable
}
1;

=pod

=head1 NAME

Net::BitTorrent::Protocol::BEP03::Peer::Incoming - Incoming TCP-based connection

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
