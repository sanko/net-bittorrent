package Net::BitTorrent::Protocol;
{
    use strict;      # core as of perl 5
    use warnings;    # core as of perl 5.006

    #
    use Carp qw[carp];    # core as of perl 5

    #
    #use lib q[../../../lib];
    #use Net::BitTorrent::Util;
    #
    use version qw[qv];    # core as of 5.009
    our $SVN = q[$Id$];
    our $VERSION = sprintf q[%.3f], version->new(qw$Rev: 24 $)->numify / 1000;

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
        build_allowed_fast build_suggest_piece  build_reject_request
        build_have_all     build_have_none
        build_ExtProtocol
        parse_packet
        parse_handshake    parse_keepalive
        parse_choke        parse_unchoke
        parse_interested   parse_not_interested
        parse_have         parse_bitfield
        parse_request      parse_piece
        parse_cancel       parse_port
        parse_have_all     parse_have_none
        parse_reject       parse_allowed_fast
        parse_ExtProtocol
        HANDSHAKE          KEEPALIVE
        CHOKE              UNCHOKE
        INTERESTED         NOT_INTERESTED
        HAVE               BITFIELD
        REQUEST            PIECE
        CANCEL             PORT
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
                build_suggest_piece
                build_allowed_fast
                build_reject_request
                build_have_all    build_have_none
                build_ExtProtocol
                ]
        ],
        parse => [
            qw[
                parse_packet
                parse_handshake     parse_keepalive
                parse_choke         parse_unchoke
                parse_interested    parse_not_interested
                parse_have          parse_bitfield
                parse_request       parse_piece
                parse_cancel        parse_port
                parse_have_all      parse_have_none
                parse_reject        parse_allowed_fast
                parse_ExtProtocol
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

        sub build_suggest_piece {    # 13 | index
            my ($index) = @_;
            if (not defined $index) {
                carp
                    q[Net::BitTorrent::Protocol::build_suggest_piece() requires an index parameter];
                return;
            }
            if ($index !~ m[^\d+$]) {
                carp
                    q[Net::BitTorrent::Protocol::build_suggest_piece() requires an integer index parameter];
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

        sub build_reject_request {    # 16 | index, offset, length
            my ($index, $offset, $length) = @_;
            if (not defined $index) {
                carp
                    q[Net::BitTorrent::Protocol::build_reject_request() requires an index parameter];
                return;
            }
            if ($index !~ m[^\d+$]) {
                carp
                    q[Net::BitTorrent::Protocol::build_reject_request() requires an integer index parameter];
                return;
            }
            if (not defined $offset) {
                carp
                    q[Net::BitTorrent::Protocol::build_reject_request() requires an offset parameter];
                return;
            }
            if ($offset !~ m[^\d+$]) {
                carp
                    q[Net::BitTorrent::Protocol::build_reject_request() requires an offset parameter];
                return;
            }
            if (not defined $length) {
                carp
                    q[Net::BitTorrent::Protocol::build_reject_request() requires an length parameter];
                return;
            }
            if ($length !~ m[^\d+$]) {
                carp
                    q[Net::BitTorrent::Protocol::build_reject_request() requires an length index parameter];
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

    sub build_ExtProtocol {    # 20 | msgID, hashref payload (to be bencoded)
        my ($msgID, $data) = @_;
        if (not defined $msgID) {
            carp
                q[Net::BitTorrent::Protocol::build_ExtProtocol() requires a message id parameter];
            return;
        }
        if ($msgID !~ m[^\d+$]) {
            carp
                q[Net::BitTorrent::Protocol::build_ExtProtocol() requires an integer message id parameter];
            return;
        }
        if (not defined $data) {
            carp
                q[Net::BitTorrent::Protocol::build_ExtProtocol() requires a payload];
            return;
        }
        if (ref($data) ne q[HASH]) {
            carp
                q[Net::BitTorrent::Protocol::build_ExtProtocol() requires a payload (hashref)];
            return;
        }
        my $packet
            = pack(q[ca*], $msgID, Net::BitTorrent::Util::bencode($data));
        return pack(q[Nca*], length($packet) + 1, 20, $packet);
    }

    #
    sub parse_packet {
        my ($data) = @_;
        if (not $$data) {
            carp
                q[Net::BitTorrent::Protocol::parse_packet() needs data to parse];
            return;
        }
        my ($packet);
        if (unpack(q[c], $$data) == 0x13) {
            my @payload = parse_handshake(substr($$data, 0, 68, q[]));
            $packet = {Type    => HANDSHAKE,
                       Payload => @payload
            } if @payload;
        }
        elsif (    (unpack(q[N], $$data) =~ m[\d])
               and (unpack(q[N], $$data) <= length($$data)))
        {   (my ($packet_data), $$data) = unpack(q[N/aa*], $$data);
            (my ($type), $packet_data) = unpack(q[ca*], $packet_data);

            #warn $type;
            #use Data::Dump qw[pp];
            #warn pp $packet_data;
            my %dispatch = (q[] => \&parse_keepalive,
                            0   => \&parse_choke,
                            1   => \&parse_unchoke,
                            2   => \&parse_interested,
                            3   => \&parse_not_interested,
                            4   => \&parse_have,
                            5   => \&parse_bitfield,
                            6   => \&parse_request,
                            7   => \&parse_piece,
                            8   => \&parse_cancel,
                            9   => \&parse_port,
                            14  => \&parse_have_all,
                            15  => \&parse_have_none,
                            16  => \&parse_reject,
                            17  => \&parse_allowed_fast,
                            20  => \&parse_ExtProtocol
            );
            if (defined $dispatch{$type}) {
                my $payload = $dispatch{$type}($packet_data);

                #use Data::Dump qw[pp];
                #warn pp $payload;
                if ($type =~ m[\d] && not defined $payload) {
                    return
                        if 4 <= $type
                            and $type <= 9 || 16 <= $type
                            and $type <= 20;
                }
                $packet = {Type => $type,
                           ($payload
                            ? (Payload => $payload)
                            : ()
                           )
                };
            }
            else {
                if (require Data::Dumper) {
                    warn $type;
                    warn q[Unhandled BitTorrent packet: ]
                        . Data::Dumper->Dump([$type, $packet],
                                             [qw[type infohash]]);
                }
                die;
            }
        }
        else {

            #warn q[Ooops!!! ] . $$data;
        }

        #
        return $packet;
    }

    sub parse_handshake {    # -1 (fake) | reserved, infohash, peerid
        my ($packet) = @_;

        #
        if (length $packet < 68) {
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

    sub parse_keepalive {    # 0 | No payload, No length
        return;
    }

    sub parse_choke {        # 0 | No payload
        return;
    }

    sub parse_unchoke {      # 1 | No payload
        return;
    }

    sub parse_interested {    # 2 | No payload
        return;
    }

    sub parse_not_interested {    # 3 | No payload
        return;
    }

    sub parse_have {              # 4 | index
        my ($packet) = @_;
        #
if (length($packet) < 1) {
            carp q[Incorrect packet length for HAVE];
            return;
        }
        #
        return unpack(q[N], $packet);
    }

    sub parse_bitfield {          # 5 | bitfield
        my ($packet) = @_;

        #
        if (length($packet) < 1) {
            carp q[Incorrect packet length for BITFIELD];
            return;
        }
        return (pack q[b*], unpack q[B*], $packet);    # vec friendly
    }

    sub parse_request {    # 6 | index, offset, length
        my ($packet) = @_;

        #
        if (length($packet) < 9) {
            carp
                sprintf(
                     q[Incorrect packet length for REQUEST (%d requires >=9)],
                     length($packet));
            return;
        }

        #
        return ([unpack(q[N3], $packet)]);    # index, offest, length
    }

    sub parse_piece {                         # 7 | index, offset, data
        my ($packet) = @_;

        #
        if (length($packet) < 9) {
            carp
                sprintf(
                       q[Incorrect packet length for PIECE (%d requires >=9)],
                       length($packet));
            return;
        }

        #
        return ([unpack(q[N2a*], $packet)]);    # index, offest, data
    }

    sub parse_cancel {                          # 8 | index, offset, length
         my ($packet)=@_;

#
        return ([unpack(q[N3], $packet)]);

    }
    {                                           # Legacy support

        sub parse_port {                        # 9 | port number
            warn q[parse_port];
        }

        sub parse_suggest_piece {               # 13 | index
            warn q[parse_suggest_piece];
        }

        sub parse_have_all {                    # 14 | No payload
            warn q[parse_have_all];
        }

        sub parse_have_none {                   # 15 | No payload
            warn q[parse_have_none];
        }

        sub parse_reject_request {              # 16 | index, offset, length
            warn q[parse_reject_request];
        }

        sub parse_allowed_fast {                # 17 | index
            warn q[parse_allowed_fast];
        }
    }

    sub parse_ExtProtocol {    # 20 | msgID, hashref payload (to be bdecoded)
        my ($packet) = @_;
        return [unpack(q[ca*], $packet)];
    }

=pod
 sub _parse_packet {    # TODO: refactor
        my ($self) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my $packet_len = unpack q[N], $queue_incoming{$self};
        return
            if not defined $packet_len
                or length $queue_incoming{$self} == 0;
        my (%ref, $type);
        if (unpack(q[c], $queue_incoming{$self}) == 0x13) {
            %ref = $self->_parse_packet_handshake($queue_incoming{$self});

            #$self->_build_packet_port; # No one ever sends these either...
        }
        else {
            return
                if $packet_len > length $queue_incoming{$self};
            (undef, my $packet_data, $queue_incoming{$self})
                = unpack q[Na] . ($packet_len) . q[ a*],
                $queue_incoming{$self};
            ($type, my $packet) = unpack(q[ca*], $packet_data);
            my %dispatch = (q[] => \&_parse_packet_keepalive,
                            0   => \&_parse_packet_choke,
                            1   => \&_parse_packet_unchoke,
                            2   => \&_parse_packet_interested,
                            3   => \&_parse_packet_disinterested,
                            4   => \&_parse_packet_have,
                            5   => \&_parse_packet_bitfield,
                            6   => \&_parse_packet_request,
                            7   => \&_parse_packet_piece,
                            8   => \&_parse_packet_cancel,
                            9   => \&_parse_packet_port,
                            14  => \&_parse_packet_have_all,
                            15  => \&_parse_packet_have_none,
                            16  => \&_parse_packet_reject,
                            17  => \&_parse_packet_allowed_fast,
                            20  => \&_parse_packet_extended
            );
            if (defined $dispatch{$type}) {
                %ref = $dispatch{$type}($self, $packet_len, $packet);
            }
            else {
                if (require Data::Dumper) {
                    warn $type;
                    warn q[Unhandled BitTorrent packet: ]
                        . Data::Dumper->Dump([$type, $packet],
                                             [qw[type infohash]]);
                }

                # XXX - think about banning this guy
                $self->_disconnect(q[Unknown or malformed packet]);
            }
        }
        if (%ref) {
            $client{$self}->_do_callback(q[peer_incoming_packet],
                                         $self,
                                         {length => $packet_len,
                                          type   => $type,
                                          data   => \%ref
                                         }
            );
            return 1;
        }
        return;
    }


    sub _parse_packet_bitfield {    # ID: 5
        my ($self, $packet_len, $packet) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my %ref = (load => q[]);
        if (length($packet) < 1) {
            $self->_disconnect(q[Incorrect packet length for BITFIELD]);
            return;
        }
        $bitfield{$self} = pack q[b*], unpack q[B*], $packet;   # vec friendly
        $self->_action_check_interesting;
        %ref = (bitfield => $packet);
        $client{$self}->_do_callback(q[peer_incoming_bitfield], $self);
        return %ref;
    }

    sub _parse_packet_keepalive {
        my ($self, $packet_len, $packet) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my %ref = (load => q[]);
        if (defined $packet) {
            $self->_disconnect(
                        sprintf(q[Incorrect packet length for keepalive (%d)],
                                length($packet))
            );
            return;
        }
        $client{$self}->_do_callback(q[peer_incoming_keepalive], $self);
        return %ref;
    }

    sub _parse_packet_unchoke {
        my ($self, $packet_len, $packet) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my %ref = (load => q[]);
        if ($packet_len != 1) {
            $self->_disconnect(q[Incorrect packet length for UNCHOKE]);
            return;
        }
        $is_choking{$self} = 0;
        $client{$self}->_set_pulse($self,
                                   min(time + 2,
                                       $client{$self}->get_pulse($self)
                                   )
        );
        $client{$self}->_do_callback(q[peer_incoming_unchoke], $self);

        # Do stuff here.
        $self->_action_request_block();
        return %ref;
    }

    sub _parse_packet_choke {
        my ($self, $packet_len, $packet) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my %ref = (load => q[]);
        if ($packet_len != 1) {
            $self->_disconnect(q[Incorrect packet length for _choke]);
            return;
        }
        $is_choking{$self}     = 1;
        $is_interesting{$self} = 1;
        grep { $_->_remove_peer($self) } @{$outgoing_requests{$self}};
        $outgoing_requests{$self} = [];
        $client{$self}->_do_callback(q[peer_incoming_choke], $self);

        # Do stuff here.
        return %ref;
    }

    sub _parse_packet_have {    # ID: 4
        my ($self, $packet_len, $packet) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my %ref = (load => q[]);
        if ($packet_len != 5) {
            $self->_disconnect(q[Incorrect packet length for HAVE]);
            return;
        }
        my ($index) = unpack(q[N], $packet);
        if (not defined $index) {
            $self->_disconnect(q[Malformed HAVE packet]);
            return;
        }
        vec($bitfield{$self}, $index, 1) = 1;
        $client{$self}->_do_callback(q[peer_incoming_have], $self, $index);
        %ref = (index => $index);

        # Do stuff here.
        $self->_action_check_interesting;
        return %ref;
    }

    sub _parse_packet_disinterested {
        my ($self, $packet_len, $packet) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my %ref = (load => q[]);
        if ($packet_len != 1) {
            $self->_disconnect(q[Incorrect packet length for DISINTERESTED]);
            return;
        }
        $is_interested{$self} = 0;
        $client{$self}->_do_callback(q[peer_incoming_disinterested], $self);

        # Do stuff here.
        return %ref;
    }

    sub _parse_packet_interested {
        my ($self, $packet_len, $packet) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my %ref = (load => q[]);
        if ($packet_len != 1) {
            $self->_disconnect(q[Incorrect packet length for INTERESTED]);
            return;
        }
        $is_interested{$self} = 1;
        $client{$self}->_do_callback(q[peer_incoming_interested], $self);

        # Do stuff here.
        return %ref;
    }

    sub _parse_packet_cancel {
        my ($self, $packet_len, $packet) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my %ref = (load => q[]);
        if ($packet_len != 13) {
            $self->_disconnect(q[Incorrect packet length for cancel]);
            return;
        }
        my ($index, $offset, $length) = unpack(q[N3], $packet);
        %ref = (index  => $index,
                offset => $offset,
                length => $length
        );
        my ($request) = grep {
                    $_->get_index == $index
                and $_->get_offset == $offset
                and $_->get_length == $length
        } @{$incoming_requests{$self}};
        if (defined $request) {
            $client{$self}
                ->_do_callback(q[peer_incoming_cancel], $self, $request);
        }
        else {
            $self->_disconnect(
                q[Peer has canceled a request they never made or has already been filled.]
            );
        }

        # Do stuff here.
        return %ref;
    }

    sub _parse_packet_have_all {
        my ($self, $packet_len, $packet) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my %ref = (load => q[]);
        if (!$_supports_FastPeers{$self}) {
            $self->_disconnect(
                q[Invalid packet: Peer does not claim to support Fast Extension but has sent us a HAVE ALL message]
            );
            return;
        }
        if ($packet_len != 1) {
            $self->_disconnect(q[Incorrect packet length for HAVE ALL]);
            return;
        }
        $bitfield{$self} = pack(q[b*],
                                (q[1] x ($session{$self}->get_piece_count + 1)
                                )
        );
        $client{$self}->_do_callback(q[peer_incoming_have_all], $self);
        return %ref;
    }

    sub _parse_packet_have_none {
        my ($self, $packet_len, $packet) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my %ref = (load => q[]);
        if (!$_supports_FastPeers{$self}) {
            $self->_disconnect(
                q[Invalid packet: Peer does not claim to support Fast Extension but has sent us a HAVE NONE message]
            );
            return;
        }
        if ($packet_len != 1) {
            $self->_disconnect(q[Incorrect packet length for HAVE NONE]);
            return;
        }
        $bitfield{$self}
            = pack(q[b*], q[0] x ($session{$self}->get_piece_count + 1));
        $client{$self}->_do_callback(q[peer_incoming_have_none], $self);

        # Do stuff here.
        return %ref;
    }

    sub _parse_packet_reject {
        my ($self, $packet_len, $packet) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my %ref = (load => q[]);
        if (!$_supports_FastPeers{$self}) {
            $self->_disconnect(
                q[Invalid packet: Peer does not claim to support Fast Extension but has sent us a REJECT message]
            );
            return;
        }
        if ($packet_len != 13) {
            $self->_disconnect(q[Incorrect packet length for REJECT]);
            return;
        }
        my ($index, $offset, $length) = unpack(q[N3], $packet);
        %ref = (index  => $index,
                offset => $offset,
                length => $length
        );

        # Do stuff here.
        return %ref;
    }

    sub _parse_packet_allowed_fast {
        my ($self, $packet_len, $packet) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my %ref = (load => q[]);
        if (!$_supports_FastPeers{$self}) {
            $self->_disconnect(
                q[Invalid packet: Peer does not claim to support Fast Extension but has sent us an ALLOWED FAST message]
            );
            return;
        }
        if ($packet_len != 5) {
            $self->_disconnect(q[Incorrect packet length for ALLOWED FAST]);
            return;
        }
        my ($index) = unpack(q[N], $packet);
        %ref = (index => $index);

        # Do stuff here.
        return %ref;
    }

    sub _parse_packet_extended {
        my ($self, $packet_len, $packet) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my %ref = (load => q[]);
        if ($_supports_ExtProtocol{$self}) {
            $self->_disconnect(
                q[User does not support the Ext. Protocol but has sent an Ext. Protocol packet]
            );
            return;
        }
        if (length($packet) < 3) {
            $self->_disconnect(q[Incorrect packet length for Ext. Protocol]);
            return;
        }
        my ($messageid, $data) = unpack(q[ca*], $packet);
        my $_content = bdecode($data);
        %ref = (messageid => $messageid,
                packet    => $_content);

        #use Data::Dump qw[pp];
        #warn pp \%ref;
        #warn pp $_content;
        #warn pp $data;
        # messageid:
        #  0 = handshake
        # >0 = extended message as specified by the handshake
        # Do stuff here.
        return %ref;
    }

    sub _parse_packet_piece {
        my ($self, $packet_len, $packet) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my %ref = (load => q[]);
        if (length($packet) < 9) {
            $self->_disconnect(
                   sprintf(
                       q[Incorrect packet length for PIECE (%d requires >=9)],
                       length($packet))
            );
            return;
        }
        my ($index, $offset, $data) = unpack(q[N2a*], $packet);
        %ref = (block  => $data,
                index  => $index,
                offset => $offset,
                length => length($data)
        );
        if (not $session{$self}->get_pieces->[$index]->get_working) {
            $self->_disconnect(q[Malformed PIECE packet. (1)]);
        }
        else {
            my $block = $session{$self}->get_pieces->[$index]
                ->get_blocks->{$offset};
            if (   (not defined $block)
                or (length($data) != $block->get_length))
            {   $self->_disconnect(q[Malformed PIECE packet. (2)]);

                #
                #}
                #elsif (not scalar $block->get_peers
                #    or not $block->get_request_timestamp($self))
                #{
                # TODO: ...should we accept the block anyway if we need it?
                $self->_disconnect(
                    q[Peer sent us a piece we've already canceled or we never asked for.]
                );
            }
            else {
                $client{$self}
                    ->_do_callback(q[peer_incoming_block], $self, $block);
                if ($block->_write($data)) {
                    delete $session{$self}->get_pieces->[$index]
                        ->get_blocks->{$offset};
                    @{$outgoing_requests{$self}}
                        = grep { $_ ne $block } @{$outgoing_requests{$self}};
                    $client{$self}->_set_pulse($self,
                            min(time + 5, $client{$self}->get_pulse($self)));
                    $downloaded{$self} += length $data;
                    $session{$self}->_inc_downloaded(length $data);
                    $previous_incoming_block{$self} = time;
                    $block->get_piece->_set_previous_incoming_block(time);

                  # TODO: if endgame, cancel all other requests for this block
                    if (scalar($block->get_peers) > 1) {
                        for my $peer ($block->get_peers) {
                            $peer->_action_cancel_block($block)
                                unless $peer == $self;
                        }
                    }
                    if (not scalar values %{$block->get_piece->get_blocks}) {
                        if ($block->get_piece->get_verified_integrity) {
                            grep {
                                $_->_build_packet_have(
                                                 $block->get_piece->get_index)
                            } $session{$self}->get_peers;
                        }
                        else {
                            die
                                q[BAD PIECE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!];

                            # TODO: penalize all peers related to piece
                            # See N::B::P::verify()
                        }
                    }
                    else {

                        #die q[Gah];
                        # .:shrugs:.
                    }
                }
            }
        }
        return %ref;
    }

    sub _parse_packet_request {
        my ($self, $packet_len, $packet) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my %ref = (load => q[]);
        if ($packet_len != 13) {
            $self->_disconnect(q[Incorrect packet length for REQUEST]);
            return;
        }
        my ($index, $offset, $length) = unpack(q[N3], $packet);
        %ref = (index  => $index,
                offset => $offset,
                length => $length
        );
        my $request =
            Net::BitTorrent::Session::Peer::Request->new(
                                                        {index  => $index,
                                                         offset => $offset,
                                                         length => $length,
                                                         peer   => $self
                                                        }
            );
        push @{$incoming_requests{$self}}, $request;
        $client{$self}
            ->_do_callback(q[peer_incoming_request], $self, $request);
        return %ref;
    }

    sub _parse_packet_port {    # DHT
        my ($self, $packet_len, $packet) = @_;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        if ($packet_len != 3) {
            $self->_disconnect(q[Incorrect packet length for PORT message]);
            return;
        }
        $listening_port{$self} = unpack(q[n], $packet);
        $self->get_session->append_nodes(
                                compact(sprintf q[%s:%d], $self->get_peerhost,
                                        $listening_port{$self}
                                )
        );
        return;
    }

    sub _parse_reserved {    # ...sub-packet, actually.
        my $self = shift;
        $client{$self}->_do_callback(q[log], TRACE,
                                     sprintf(q[Entering %s for %s],
                                             [caller 0]->[3], $$self
                                     )
        );
        my ($reserved) = [map {ord} split(q[], $reserved{$self})];
        $_supports_DHT{$self}         = ($reserved->[7] &= 0x01 ? 1 : 0);
        $_supports_FastPeers{$self}   = ($reserved->[7] &= 0x04 ? 1 : 0);
        $_supports_ExtProtocol{$self} = ($reserved->[5] &= 0x10 ? 1 : 0);
        $_supports_Encryption{$self} = 0;    # ...todo
        $_supports_BitComet{$self}
            = (($reserved->[1] . $reserved->[1] eq q[ex]) ? 1 : 0);
        $_fastset_out{$self} = [] if $_supports_FastPeers{$self};
        $_fastset_in{$self}  = [] if $_supports_FastPeers{$self};
        $client{$self}->get_dht->add_node($$self) if $_supports_DHT{$self};
        return 1;
    }
=cut

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

There are two types of functions exported by this module:

=over

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

=item C<build_ExtProtocol ( DATA )>

Creates an extended protocol message.

=back

=head3 Legacy Messages

The following messages are either part of the base protocol or one of
the common extentions but have either been superceeded or simply
removed from the majority of clients.  I have provided them here only
for legacy support; they may be removed in the future.

=over

=item C<build_port ( PORT )>

Creates a port message.

See also: http://bittorrent.org/beps/bep_0003.html - The BitTorrent
Protocol Specification

=item C<build_allowed_fast ( INDEX )>

Creates an Allowed Fast message.

See also: http://bittorrent.org/beps/bep_0006.html - Fast Extension

=item C<build_suggest_piece ( INDEX )>

Creates a Suggest Piece message.

See also: http://bittorrent.org/beps/bep_0006.html - Fast Extension

=item C<build_reject_request ( INDEX, OFFSET, LENGTH )>

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

=item ...

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
