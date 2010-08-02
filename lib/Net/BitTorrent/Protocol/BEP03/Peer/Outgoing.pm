{

    package Net::BitTorrent::Protocol::BEP03::Peer::Outgoing;
    use Moose;
    use Moose::Util::TypeConstraints;
    use lib '../../../../../../lib';
    use Net::BitTorrent::Types qw[:addr];
    extends 'Net::BitTorrent::Protocol::BEP03::Peer';
    has '_handle' => (
        is        => 'ro',
        isa       => 'AnyEvent::Handle::Throttle',
        predicate => '_has_handle',
        init_arg  => undef,
        writer    => '_set_handle',
        handles   => {
            rbuf       => 'rbuf',
            push_read  => 'push_read',
            push_write => 'push_write',
            fh         => sub { shift->_handle->{'fh'} },
            host       => sub {
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
    has '+torrent' => (required => 1);

    sub _initializer_torrent {
        my ($s, $t, $set, $attr) = @_;
        $set->($t);
        $s->_set_pieces($t->have->Shadow);
    }
    has '_connect' => (is          => 'ro',
                       isa         => 'NBTypes::Network::Addr',
                       required    => 1,
                       init_arg    => 'connect',
                       initializer => '_initializer_connect'
    );

    sub _initializer_connect {
        my ($s, $c, $set, $attr) = @_;
        $set->($c);
        require Scalar::Util;
        Scalar::Util::weaken $s;
        require AnyEvent::Handle::Throttle;
        $s->_set_handle(
            AnyEvent::Handle::Throttle->new(
                connect    => $c,
                on_connect => sub {
                    return if !defined $s;
                    $s->_send_handshake;
                },
                on_read => sub {
                    return if !defined $s;
                    require Net::BitTorrent::Protocol::BEP03::Packets;
                PACKET:
                    while (
                        my $p =
                        Net::BitTorrent::Protocol::BEP03::Packets::parse_packet(
                                                                     \$s->rbuf
                        )
                        )
                    {   $s->_handle_packet($p);
                        last PACKET if !defined $s || !$s->rbuf || !$p;
                    }
                }
            )
        );
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
        return $s->disconnect(
                 'Bad info_hash (Does not match the torrent we were seeking)')
            if $info_hash->Compare($s->torrent->info_hash) != 0;
        $s->_check_unique_connection;
        return if !defined $s;
        die;
    }

    #

    no Moose;
    no Moose::Util::TypeConstraints;
    __PACKAGE__->meta->make_immutable
}
1;

=pod

=head1 NAME

Net::BitTorrent::Protocol::BEP03::Peer::Outgoing - Outgoing TCP-based connection

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
