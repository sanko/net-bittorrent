#!C:\perl\bin\perl.exe
package Net::BitTorrent::Protocol;
{
    use strict;      # core as of perl 5
    use warnings;    # core as of perl 5.006

    #
    use Carp qw[carp];    # core as of perl 5

    #
    use lib q[../../../lib];
    use Net::BitTorrent::Util qw[:bencode];

    #
    use version qw[qv];    # core as of 5.009
    our $SVN = q[$Id$];
    our $UNSTABLE_RELEASE = 0; our $VERSION = sprintf(($UNSTABLE_RELEASE ? q[%.3f_%03d] : q[%.3f]), (version->new((qw$Rev$)[1])->numify / 1000), $UNSTABLE_RELEASE);

    #
    use vars               # core as of perl 5.002
        qw[@EXPORT_OK %EXPORT_TAGS];
    use Exporter           # core as of perl 5
        qw[];
    *import = *import = *Exporter::import;
    @EXPORT_OK = qw[
        build_handshake    build_keepalive
        build_choke        build_unchoke
        build_interested   build_not_interested
        build_have         build_bitfield
        build_request      build_piece
        build_cancel       build_port
        build_allowed_fast build_suggest  build_reject
        build_have_all     build_have_none
        build_extended
        parse_packet
        _parse_handshake    _parse_keepalive
        _parse_choke        _parse_unchoke
        _parse_interested   _parse_not_interested
        _parse_have         _parse_bitfield
        _parse_request      _parse_piece
        _parse_cancel       _parse_port
        _parse_suggest
        _parse_have_all     _parse_have_none
        _parse_reject       _parse_allowed_fast
        _parse_extended
        HANDSHAKE          KEEPALIVE
        CHOKE              UNCHOKE
        INTERESTED         NOT_INTERESTED
        HAVE               BITFIELD
        REQUEST            PIECE
        CANCEL             PORT
        SUGGEST
        HAVE_ALL           HAVE_NONE
        REJECT             ALLOWED_FAST
        EXTPROTOCOL
    ];
    %EXPORT_TAGS = (
        all   => [@EXPORT_OK],
        build => [
            qw[
                build_handshake   build_keepalive
                build_choke       build_unchoke
                build_interested  build_not_interested
                build_have        build_bitfield
                build_request     build_piece
                build_cancel      build_port
                build_suggest
                build_allowed_fast
                build_reject
                build_have_all    build_have_none
                build_extended
                ]
        ],
        parse => [
            qw[
                parse_packet
                _parse_handshake     _parse_keepalive
                _parse_choke         _parse_unchoke
                _parse_interested    _parse_not_interested
                _parse_have          _parse_bitfield
                _parse_request       _parse_piece
                _parse_cancel        _parse_port
                _parse_suggest
                _parse_have_all      _parse_have_none
                _parse_reject        _parse_allowed_fast
                _parse_extended
                ]
        ],
        types => [
            qw[
                HANDSHAKE                 KEEPALIVE
                CHOKE                 UNCHOKE
                INTERESTED                 NOT_INTERESTED
                HAVE                 BITFIELD
                REQUEST                PIECE
                CANCEL                PORT
                SUGGEST
                HAVE_ALL                HAVE_NONE
                REJECT                ALLOWED_FAST
                EXTPROTOCOL                ]
        ]
    );

    # Packet types
    sub HANDSHAKE      {-1}
    sub KEEPALIVE      {q[]}
    sub CHOKE          {0}
    sub UNCHOKE        {1}
    sub INTERESTED     {2}
    sub NOT_INTERESTED {3}
    sub HAVE           {4}
    sub BITFIELD       {5}
    sub REQUEST        {6}
    sub PIECE          {7}
    sub CANCEL         {8}
    sub PORT           {9}
    sub SUGGEST        {13}
    sub HAVE_ALL       {14}
    sub HAVE_NONE      {15}
    sub REJECT         {16}
    sub ALLOWED_FAST   {17}
    sub EXTPROTOCOL    {20}

    #
    sub build_handshake {    # -1 (fake) | reserved, infohash, peerid
        my ($reserved, $infohash, $peerid) = @_;
        if (grep { not defined } @_[0 .. 2]) {
            carp
                q[Missing params for Net::BitTorrent::Protocol::build_handshake()];
            return;
        }
        if (length($reserved) != 8) {
            carp
                q[Malformed reserved bytes for Net::BitTorrent::Protocol::build_handshake()];
            return;
        }
        if (length($infohash) != 20) {
            carp
                q[Malformed infohash for Net::BitTorrent::Protocol::build_handshake()];
            return;
        }
        if (length($peerid) != 20) {
            carp
                q[Malformed peerid for Net::BitTorrent::Protocol::build_handshake()];
            return;
        }
        return
            pack(q[c/a* a8 a20 a20],
                 q[BitTorrent protocol],
                 $reserved, $infohash, $peerid);
    }

    sub build_keepalive {    # 0 | No payload, No length
        return pack(q[N], 0);
    }

    sub build_choke {        # 0 | No payload
        return pack(q[Nc], 1, 0);
    }

    sub build_unchoke {      # 1 | No payload
        return pack(q[Nc], 1, 1);
    }

    sub build_interested {    # 2 | No payload
        return pack(q[Nc], 1, 2);
    }

    sub build_not_interested {    # 3 | No payload
        return pack(q[Nc], 1, 3);
    }

    sub build_have {              # 4 | index
        my ($index) = @_;
        if (not defined $index) {
            carp
                q[Net::BitTorrent::Protocol::build_have() requires an index parameter];
            return;
        }
        if ($index !~ m[^\d+$]) {
            carp
                q[Net::BitTorrent::Protocol::build_have() requires an integer index parameter];
            return;
        }
        return pack(q[NcN], 5, 4, $index);
    }

    sub build_bitfield {    # 5 | bitfield
        my ($bitfield) = @_;
        if (not defined $bitfield) {
            carp
                q[Net::BitTorrent::Protocol::build_bitfield() requires an bitfield parameter];
            return;
        }
        if (unpack(q[b*], $bitfield) !~ m[^[01]+$]) {
            carp
                q[Malformed bitfield passed to Net::BitTorrent::Protocol::build_bitfield()];
            return;
        }
        return pack(q[Nca*], (length($bitfield) + 1), 5, $bitfield);
    }

    sub build_request {    # 6 | index, offset, length
        my ($index, $offset, $length) = @_;
        if (not defined $index) {
            carp
                q[Net::BitTorrent::Protocol::build_request() requires an index parameter];
            return;
        }
        if ($index !~ m[^\d+$]) {
            carp
                q[Net::BitTorrent::Protocol::build_request() requires an integer index parameter];
            return;
        }
        if (not defined $offset) {
            carp
                q[Net::BitTorrent::Protocol::build_request() requires an offset parameter];
            return;
        }
        if ($offset !~ m[^\d+$]) {
            carp
                q[Net::BitTorrent::Protocol::build_request() requires an offset parameter];
            return;
        }
        if (not defined $length) {
            carp
                q[Net::BitTorrent::Protocol::build_request() requires an length parameter];
            return;
        }
        if ($length !~ m[^\d+$]) {
            carp
                q[Net::BitTorrent::Protocol::build_request() requires an length index parameter];
            return;
        }

        #
        my $packed = pack(q[NNN], $index, $offset, $length);
        return pack(q[Nca*], length($packed) + 1, 6, $packed);
    }

    sub build_piece {    # 7 | index, offset, data
        my ($index, $offset, $data) = @_;
        if (not defined $index) {
            carp
                q[Net::BitTorrent::Protocol::build_piece() requires an index parameter];
            return;
        }
        if ($index !~ m[^\d+$]) {
            carp
                q[Net::BitTorrent::Protocol::build_piece() requires an integer index parameter];
            return;
        }
        if (not defined $offset) {
            carp
                q[Net::BitTorrent::Protocol::build_piece() requires an offset parameter];
            return;
        }
        if ($offset !~ m[^\d+$]) {
            carp
                q[Net::BitTorrent::Protocol::build_piece() requires an offset parameter];
            return;
        }
        if (not defined $$data) {
            carp
                q[Net::BitTorrent::Protocol::build_piece() requires data to work with];
            return;
        }

        #
        my $packed = pack(q[N2a*], $index, $offset, $$data);
        return pack(q[Nca*], length($packed) + 1, 7, $packed);
    }

    sub build_cancel {    # 8 | index, offset, length
        my ($index, $offset, $length) = @_;
        if (not defined $index) {
            carp
                q[Net::BitTorrent::Protocol::build_cancel() requires an index parameter];
            return;
        }
        if ($index !~ m[^\d+$]) {
            carp
                q[Net::BitTorrent::Protocol::build_cancel() requires an integer index parameter];
            return;
        }
        if (not defined $offset) {
            carp
                q[Net::BitTorrent::Protocol::build_cancel() requires an offset parameter];
            return;
        }
        if ($offset !~ m[^\d+$]) {
            carp
                q[Net::BitTorrent::Protocol::build_cancel() requires an offset parameter];
            return;
        }
        if (not defined $length) {
            carp
                q[Net::BitTorrent::Protocol::build_cancel() requires an length parameter];
            return;
        }
        if ($length !~ m[^\d+$]) {
            carp
                q[Net::BitTorrent::Protocol::build_cancel() requires an length index parameter];
            return;
        }

        #
        my $packed = pack(q[N3], $index, $offset, $length);
        return pack(q[Nca*], length($packed) + 1, 8, $packed);
    }
    {    # Legacy support

        sub build_port {    # 9 | port number
            my ($port) = @_;
            if (not defined $port) {
                carp
                    q[Net::BitTorrent::Protocol::build_port() requires an index parameter];
                return;
            }
            if ($port !~ m[^\d+$]) {
                carp
                    q[Net::BitTorrent::Protocol::build_port() requires an integer index parameter];
                return;
            }
            return pack(q[NcN], length($port) + 1, 9, $port);
        }

        sub build_suggest {    # 13 | index
            my ($index) = @_;
            if (not defined $index) {
                carp
                    q[Net::BitTorrent::Protocol::build_suggest() requires an index parameter];
                return;
            }
            if ($index !~ m[^\d+$]) {
                carp
                    q[Net::BitTorrent::Protocol::build_suggest() requires an integer index parameter];
                return;
            }
            return pack(q[NcN], 5, 13, $index);
        }

        sub build_have_all {    # 14 | No payload
            return pack(q[Nc], 1, 14);
        }

        sub build_have_none {    # 15 | No payload
            return pack(q[Nc], 1, 15);
        }

        sub build_reject {       # 16 | index, offset, length
            my ($index, $offset, $length) = @_;
            if (not defined $index) {
                carp
                    q[Net::BitTorrent::Protocol::build_reject() requires an index parameter];
                return;
            }
            if ($index !~ m[^\d+$]) {
                carp
                    q[Net::BitTorrent::Protocol::build_reject() requires an integer index parameter];
                return;
            }
            if (not defined $offset) {
                carp
                    q[Net::BitTorrent::Protocol::build_reject() requires an offset parameter];
                return;
            }
            if ($offset !~ m[^\d+$]) {
                carp
                    q[Net::BitTorrent::Protocol::build_reject() requires an offset parameter];
                return;
            }
            if (not defined $length) {
                carp
                    q[Net::BitTorrent::Protocol::build_reject() requires an length parameter];
                return;
            }
            if ($length !~ m[^\d+$]) {
                carp
                    q[Net::BitTorrent::Protocol::build_reject() requires an length index parameter];
                return;
            }

            #
            my $packed = pack(q[N3], $index, $offset, $length);
            return pack(q[Nca*], length($packed) + 1, 16, $packed);
        }

        sub build_allowed_fast {    # 17 | index
            my ($index) = @_;
            if (not defined $index) {
                carp
                    q[Net::BitTorrent::Protocol::build_allowed_fast() requires an index parameter];
                return;
            }
            if ($index !~ m[^\d+$]) {
                carp
                    q[Net::BitTorrent::Protocol::build_allowed_fast() requires an integer index parameter];
                return;
            }
            return pack(q[NcN], 5, 17, $index);
        }
    }

    sub build_extended {    # 20 | msgID, hashref payload (to be bencoded)
        my ($msgID, $data) = @_;
        if (not defined $msgID) {
            carp
                q[Net::BitTorrent::Protocol::build_extended() requires a message id parameter];
            return;
        }
        if ($msgID !~ m[^\d+$]) {
            carp
                q[Net::BitTorrent::Protocol::build_extended() requires an integer message id parameter];
            return;
        }
        if (not defined $data) {
            carp
                q[Net::BitTorrent::Protocol::build_extended() requires a payload];
            return;
        }
        if (ref($data) ne q[HASH]) {
            carp
                q[Net::BitTorrent::Protocol::build_extended() requires a payload (hashref)];
            return;
        }
        my $packet = pack(q[ca*], $msgID, bencode($data));
        return pack(q[Nca*], length($packet) + 1, 20, $packet);
    }

    #
    sub parse_packet {
        my ($data) = @_;
        if (ref($data) ne q[SCALAR]) {
            carp q[Net::BitTorrent::Protocol::parse_packet() needs a ref];
            return;
        }
        elsif (not $$data) {
            carp
                q[Net::BitTorrent::Protocol::parse_packet() needs data to parse];
            return;
        }
        my ($packet);
        if (unpack(q[c], $$data) == 0x13) {
            my @payload = _parse_handshake(substr($$data, 0, 68, q[]));
            $packet = {Type    => HANDSHAKE,
                       Payload => @payload
            } if @payload;
        }
        elsif ((defined unpack(q[N], $$data)) and (unpack(q[N], $$data) =~ m[\d])) {
            if ((unpack(q[N], $$data) <= length($$data))) {
                (my ($packet_data), $$data) = unpack(q[N/aa*], $$data);
                (my ($type), $packet_data) = unpack(q[ca*], $packet_data);

                #warn $type;
                #use Data::Dump qw[pp];
                #warn pp $packet_data;
                my %dispatch = (&KEEPALIVE      => \&_parse_keepalive,
                                &CHOKE          => \&_parse_choke,
                                &UNCHOKE        => \&_parse_unchoke,
                                &INTERESTED     => \&_parse_interested,
                                &NOT_INTERESTED => \&_parse_not_interested,
                                &HAVE           => \&_parse_have,
                                &BITFIELD       => \&_parse_bitfield,
                                &REQUEST        => \&_parse_request,
                                &PIECE          => \&_parse_piece,
                                &CANCEL         => \&_parse_cancel,
                                &PORT           => \&_parse_port,
                                &SUGGEST        => \&_parse_suggest,
                                &HAVE_ALL       => \&_parse_have_all,
                                &HAVE_NONE      => \&_parse_have_none,
                                &REJECT         => \&_parse_reject,
                                &ALLOWED_FAST   => \&_parse_allowed_fast,
                                &EXTPROTOCOL    => \&_parse_extended
                );
                if (defined $dispatch{$type}) {
                    my $payload = $dispatch{$type}($packet_data);
                    $packet = {Type => $type,
                               (defined $payload
                                ? (Payload => $payload)
                                : ()
                               )
                    };
                }
                elsif (require Data::Dump) {
                    carp sprintf <<END, Data::Dump->pp($type),Data::Dump->pp($packet);
Unhandled/Unknown packet where:
Type   = %s
Packet = %s
END
                }
            }
        }
        else {

            #warn q[Ooops!!! ] . $$data;
        }

        #
        return $packet;
    }

    sub _parse_handshake {    # -1 (fake) | reserved, infohash, peerid
        my ($packet) = @_;
        if (not defined $packet) {
            return;
        }

        #
        elsif (length($packet) < 68) {
            carp q[Not enough data for handshake packet];
            return;
        }

        #
        my ($protocol_name, $reserved, $infohash, $peerid)
            = unpack(q[c/a a8 a20 a20], $packet);

        #
        if ($protocol_name ne q[BitTorrent protocol]) {
            carp sprintf(q[Improper handshake; Bad protocol name (%s)],
                         $protocol_name);
            return;
        }

        # Do stuff here.
        return [$reserved, $infohash, $peerid];
    }

    sub _parse_keepalive {    # 0 | No payload, No length
        return;
    }

    sub _parse_choke {        # 0 | No payload
        return;
    }

    sub _parse_unchoke {      # 1 | No payload
        return;
    }

    sub _parse_interested {    # 2 | No payload
        return;
    }

    sub _parse_not_interested {    # 3 | No payload
        return;
    }

    sub _parse_have {              # 4 | index
        my ($packet) = @_;

        #
        if (not defined $packet) {
            return;
        }
        elsif (length($packet) < 1) {
            carp q[Incorrect packet length for HAVE];
            return;
        }

        #
        return unpack(q[N], $packet);
    }

    sub _parse_bitfield {    # 5 | bitfield
        my ($packet) = @_;

        #
        if (not defined $packet) {
            return;
        }
        elsif (length($packet) < 1) {
            carp q[Incorrect packet length for BITFIELD];
            return;
        }
        return (pack q[b*], unpack q[B*], $packet);    # vec friendly
    }

    sub _parse_request {    # 6 | index, offset, length
        my ($packet) = @_;

        #
        if (not defined $packet) {
            return;
        }
        elsif (length($packet) < 9) {
            carp
                sprintf(
                     q[Incorrect packet length for REQUEST (%d requires >=9)],
                     length($packet));
            return;
        }

        #
        return ([unpack(q[N3], $packet)]);    # index, offest, length
    }

    sub _parse_piece {                        # 7 | index, offset, data
        my ($packet) = @_;

        #
        if (not defined $packet) {return}
        elsif (length($packet) < 9) {
            carp
                sprintf(
                       q[Incorrect packet length for PIECE (%d requires >=9)],
                       length($packet));
            return;
        }

        #
        return ([unpack(q[N2a*], $packet)]);
    }

    sub _parse_cancel {    # 8 | index, offset, length
        my ($packet) = @_;

        #
        if (not defined $packet) {
            return;
        }
        elsif (length($packet) < 9) {
            carp
                sprintf(
                      q[Incorrect packet length for CANCEL (%d requires >=9)],
                      length($packet));
            return;
        }

        #
        return ([unpack(q[N3], $packet)]);    # index, offest, length
    }

    sub _parse_port {    # 9 | port number | No longer used
        my ($packet) = @_;

        #
        if (not defined $packet) {
            return;
        }
        elsif (length($packet) < 1) {
            carp q[Incorrect packet length for PORT];
            return;
        }

        #
        return (unpack q[N], $packet);
    }
    {    # Ext support

        sub _parse_suggest {    # 13 | index
            my ($packet) = @_;

            #
            if (not defined $packet) {return}
            if (length($packet) < 1) {
                carp q[Incorrect packet length for SUGGEST];
                return;
            }

            #
            return unpack(q[N], $packet);
        }

        sub _parse_have_all {    # 14 | No payload
            return;
        }

        sub _parse_have_none {    # 15 | No payload
            return;
        }

        sub _parse_reject {       # 16 | index, offset, length
            my ($packet) = @_;

            #
            if (not defined $packet) {return}
            elsif (length($packet) < 9) {
                carp
                    sprintf(
                      q[Incorrect packet length for REJECT (%d requires >=9)],
                      length($packet));
                return;
            }

            #
            return ([unpack(q[N3], $packet)]);    # index, offest, length
        }

        sub _parse_allowed_fast {                 # 17 | index
            my ($packet) = @_;

            #
            if (not defined $packet) {return}
            elsif (length($packet) < 1) {
                carp q[Incorrect packet length for FASTSET];
                return;
            }

            #
            return unpack(q[N], $packet);
        }
    }

    sub _parse_extended {    # 20 | msgID, hashref payload (to be bdecoded)
        my ($packet) = @_;
        if (not defined $packet) {return}
        elsif (length $packet < 1) {
            return;
        }

        #
        my ($id, $payload) = unpack(q[ca*], $packet);

        #
        return ([$id, scalar bdecode($payload)]);
    }

#
#     sub _parse_reserved {      # ...sub-packet, actually.
#~         my $self = shift;
#~         my ($reserved) = [map {ord} split(q[], $reserved{$self})];
#~         $_supports_DHT{$self}         = ($reserved->[7] &= 0x01 ? 1 : 0);
#~         $_supports_FastPeers{$self}   = ($reserved->[7] &= 0x04 ? 1 : 0);
#~         $_supports_ExtProtocol{$self} = ($reserved->[5] &= 0x10 ? 1 : 0);
#~         $_supports_Encryption{$self} = 0;    # ...todo
#~         $_supports_BitComet{$self}
#~             = (($reserved->[1] . $reserved->[1] eq q[ex]) ? 1 : 0);
#~         $_fastset_out{$self} = [] if $_supports_FastPeers{$self};
#~         $_fastset_in{$self}  = [] if $_supports_FastPeers{$self};
#~         $client{$self}->get_dht->add_node($$self) if $_supports_DHT{$self};
#~         return 1;
#~     }
#
#
    1;
}

=pod

=head1 NAME

Net::BitTorrent::Protocol - Packet utilities for the BitTorrent protocol

=head1 Synopsis

    use Net::BitTorrent::Protocol qw[:build parse_packet];

    # Tell them what we want...
    my $handshake = build_handshake(
        pack('C*', split('', '00000000')),
        pack('H*', 'ddaa46b1ddbfd3564fca526d1b68420b6cd54201'),
        'your-peer-id-in-here'
    );

    # And the inverse...
    my ($reserved, $infohash, $peerid) = parse_packet( $handshake );

=head1 Description

What would BitTorrent be without packets?   TCP noise, mostly.

For similar work and links to the specifications behind these packets,
move on down to the L<See Also|/"See Also"> section.

=head1 Functions

There are three types of functions exported by this module:

=over

=item Packet Types

These functions return the BitTorrent

=over

=item HANDSHAKE

=item KEEPALIVE

=item CHOKE

=item UNCHOKE

=item INTERESTED

=item NOT_INTERESTED

=item HAVE

=item BITFIELD

=item REQUEST

=item PIECE

=item CANCEL

=item PORT

=item SUGGEST

=item HAVE_ALL

=item HAVE_NONE

=item REJECT

=item ALLOWED_FAST

=item EXTPROTOCOL

=back

=item Building Functions

These create packets ready-to-send to remote peers.  To import these
in one go, use the C<:build> tag.

=item Parsing Functions

These are used to parse unknown data into sensable packets.  To import
these in one go, use the C<:parse> tag.

=back

=head2 Building Functions

=over

=item C<build_handshake ( RESERVED, INFOHASH, PEERID )>

Creates an initial handshake packet.  All parameters must conform to
the BitTorrent spec:

=over

=item C<RESEREVED>

...is the 8 byte string used to represent a client's capabilities for
extentions to the protocol.

=item C<INFOHASH>

...is the 20 byte SHA1 hash of the bencoded info from the metainfo
file.

=item C<PEERID>

...is 20 bytes.

=back

=item C<build_keepalive ( )>

Creates a keep-alive message.  The keep-alive message is a message
with zero bytes, specified with the length prefix set to zero.  There
is no message ID and no payload.  Peers may close a connection if they
receive no messages (keep-alive or any other message) for a certain
period of time, so a keep-alive message must be sent to maintain the
connection alive if no command have been sent for a given amount of
time.  This amount of time is generally two minutes.

=item C<build_choke ( )>

Creates a choke message.  The choke message is fixed-length and has no
payload.

See Also: http://tinyurl.com/NB-docs-choking - Choking and Optimistic
Unchoking

=item C<build_unchoke ( )>

Creates an unchoke message.  The unchoke message is fixed-length and
has no payload.

See Also: http://tinyurl.com/NB-docs-choking - Choking and Optimistic
Unchoking

=item C<build_interested ( )>

Creates an interested message.  The interested message is fixed-length
and has no payload.

=item C<build_not_interested ( )>

Creates a not interested message.  The not interested message is
fixed-length and has no payload.

=item C<build_have ( INDEX )>

Creates a have message.  The have message is fixed length.  The
payload is the zero-based INDEX of a piece that has just been
successfully downloaded and verified via the hash.

I<That is the strict definition, in reality some games may be played.
In particular because peers are extremely unlikely to download pieces
that they already have, a peer may choose not to advertise having a
piece to a peer that already has that piece.  At a minimum "HAVE
supression" will result in a 50% reduction in the number of HAVE
messages, this translates to around a 25-35% reduction in protocol
overhead. At the same time, it may be worthwhile to send a HAVE
message to a peer that has that piece already since it will be useful
in determining which piece is rare.>

I<A malicious peer might also choose to advertise having pieces that
it knows the peer will never download. Due to this attempting to model
peers using this information is a bad idea.>

=item C<build_bitfield ( BITFIELD )>

Creates a bitfield message.  The bitfield message is variable length,
where C<X> is the length of the C<BITFIELD>.  The payload is a
C<BITFIELD> representing the pieces that have been successfully
downloaded.  The high bit in the first byte corresponds to piece index
0.  Bits that are cleared indicated a missing piece, and set bits
indicate a valid and available piece. Spare bits at the end are set to
zero.

A bitfield message may only be sent immediately after the
L<handshaking|/"build_handshake ( RESERVED, INFOHASH, PEERID )">
sequence is completed, and before any other messages are sent.  It is
optional, and need not be sent if a client has no pieces or uses one
of the Fast Extention packets: L<have all|/"build_have_all ( )"> or
L<have none|/"build_have_none ( )">.

=begin :parser

I<A bitfield of the wrong length is considered an error.  Clients
should drop the connection if they receive bitfields that are not of
the correct size, or if the bitfield has any of the spare bits set.>

=end :parser

=item C<build_request ( INDEX, OFFSET, LENGTH )>

Creates a request message.  The request message is fixed length, and
is used to request a block.  The payload contains the following
information:

=over

=item C<INDEX>

...is an integer specifying the zero-based piece index.

=item C<OFFSET>

...is an integer specifying the zero-based byte offset within the
piece.

=item C<LENGTH>

...is an integer specifying the requested length.

=back

See Also: L<build_cancel|/"build_cancel ( INDEX, OFFSET, LENGTH )">

=item C<build_piece ( INDEX, OFFSET, DATA )>

Creates a piece message.  The piece message is variable length, where
C<X> is the length of the L<DATA>.  The payload contains the following
information:

=over

=item C<INDEX>

...is an integer specifying the zero-based piece index.

=item C<OFFSET>

...is an integer specifying the zero-based byte offset within the
piece.

=item C<DATA>

...is the block of data, which is a subset of the piece specified by
C<INDEX>.

=back

Before sending pieces to remote peers, the client should verify that
the piece matches the SHA1 hash related to it in the .torrent
metainfo.

=item C<build_cancel ( INDEX, OFFSET, LENGTH )>

Creates a cancel message.  The cancel message is fixed length, and is
used to cancel
L<block requests|/"build_request ( INDEX, OFFSET, LENGTH )">.  The
payload is identical to that of the
L<request|/"build_request ( INDEX, OFFSET, LENGTH )"> message.  It is
typically used during 'End Game.'

See Also: http://tinyurl.com/NB-docs-EndGame - End Game

=item C<build_extended ( DATA )>

Creates an extended protocol message.

=back

=head3 Legacy Messages

The following messages are either part of the base protocol or one of
the common extentions but have either been superceeded or simply
removed from the majority of clients.  I have provided them here only
for legacy support; they will not be removed in the future.

=over

=item C<build_port ( PORT )>

Creates a port message.

See also: http://bittorrent.org/beps/bep_0003.html - The BitTorrent
Protocol Specification

=item C<build_allowed_fast ( INDEX )>

Creates an Allowed Fast message.

uTorrent never advertises a fast set... why should we?

See also: http://bittorrent.org/beps/bep_0006.html - Fast Extension

=item C<build_suggest ( INDEX )>

Creates a Suggest Piece message.

Super seeding is not supported by Net::BitTorrent.  Yet.

See also: http://bittorrent.org/beps/bep_0006.html - Fast Extension

=item C<build_reject ( INDEX, OFFSET, LENGTH )>

Creates a Reject Request message.

See also: http://bittorrent.org/beps/bep_0006.html - Fast Extension

=item C<build_have_all ( )>

Creates a Have All message.

See also: http://bittorrent.org/beps/bep_0006.html - Fast Extension

=item C<build_have_none ( )>

Creates a Have None message.

See also: http://bittorrent.org/beps/bep_0006.html - Fast Extension

=back

=head2 Parsing Function(s)

=over

=item parse_packet( DATA )

Attempts to parse any known packet from the data (a scalar ref) passed to it.
On success, the payload and type are returned and the packet is removed from
the incoming data ref.  C<undef> is returned on failure.

=back

=head1 Notes

=head2 Support and Availability

Visit the following for support and information related to
L<Net::BitTorrent|Net::BitTorrent>:

=over 4

=item The project's website

For links to a mailing list, svn information, and more, visit the
project's home: http://sankorobinson.com/net-bittorrent/.

=item Bugs and the Issue Tracker

Use http://code.google.com/p/net-bittorrent/issues/list for bug
tracking. For more, see the
L<Issue Tracker|Net::BitTorrent::Notes/"Issue Tracker">,
L<Bug Reporting|Net::BitTorrent::Notes/"Bug Reporting">, and
L<Co-Development and Patch Submission|Net::BitTorrent::Notes/"Co-Development and Patch Submission">
sections of L<Net::BitTorrent::Notes|Net::BitTorrent::Notes>.

=back

=head2 ToDo

=over

=item parsing functions

=back


=head1 See Also

http://bittorrent.org/beps/bep_0003.html - The BitTorrent Protocol
Specification

http://bittorrent.org/beps/bep_0006.html - Fast Extension

http://bittorrent.org/beps/bep_0010.html - Extension Protocol

http://wiki.theory.org/BitTorrentSpecification - An annotated guide to
the BitTorrent protocol

L<Net::BitTorrent::PeerPacket|Net::BitTorrent::PeerPacket> - by Joshua
McAdams

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
