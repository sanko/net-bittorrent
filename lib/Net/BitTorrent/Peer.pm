package Net::BitTorrent::Peer;
{
    use Moose;
    use Moose::Util::TypeConstraints;
    use 5.010;
    use lib '../../../lib';
    use Net::BitTorrent::Types qw[:torrent];
    use Net::BitTorrent::Protocol::BEP03::Packets qw[parse_packet :types];
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 2; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    #
    sub BUILD {1}

    #
    has 'torrent' => (is        => 'ro',
                      isa       => 'Net::BitTorrent::Torrent',
                      predicate => 'has_torrent',
                      writer    => '_set_torrent',
                      weak_ref  => 1,
    );
    has 'connect' => (is => 'ro', isa => 'ArrayRef');
    has 'client' => (is       => 'ro',
                     isa      => 'Net::BitTorrent',
                     required => 1,
                     weak_ref => 1,
                     handles  => qr[^trigger_.+$]
    );
    has 'fh' => (is         => 'ro',
                 isa        => 'GlobRef',
                 predicate  => 'has_fh',
                 lazy_build => 1
    );
    sub _build_fh { shift->handle->{'fh'}; }
    has 'handle' => (is         => 'ro',
                     isa        => 'AnyEvent::Handle::Throttle',
                     predicate  => 'has_handle',
                     writer     => '_set_handle',
                     lazy_build => 1,
                     handles    => {
                                 rbuf       => 'rbuf',
                                 push_read  => 'push_read',
                                 push_write => 'push_write'
                     }
    );

    sub _build_handle {
        my ($s) = @_;
        require AnyEvent::Handle::Throttle;
        AnyEvent::Handle::Throttle->new(
                      $s->has_fh ? (fh => $s->fh) : (connect => $s->connect));
    }
    has 'source' => (
             is  => 'ro',
             isa => enum([qw[tracker dht pex lsd resume_data incoming user]]),
             default => 'user'
    );
    has '_id' => (isa      => 'Str',                            # creation id
                  is       => 'ro',
                  init_arg => undef,
                  default  => sub { state $id = 'aa'; $id++ }
    );
    has '_handshake_step' => (
        isa =>
            enum(
            [qw[MSE_ONE MSE_TWO MSE_THREE MSE_FOUR MSE_FIVE REG_ONE REG_TWO REG_THREE REG_OKAY]
            ]
            ),
        is       => 'rw',
        default  => 'MSE_ONE',
        init_arg => undef
    );
    {    # Handshake utils

        sub _build_reserved {
            my ($self) = @_;
            my @reserved = qw[0 0 0 0 0 0 0 0];
            $reserved[5] |= 0x10;    # Ext Protocol
            $reserved[7] |= 0x04;    # Fast Ext
            return join '', map {chr} @reserved;
        }
        sub CRYPTO_PLAIN {0x01}
        sub CRYPTO_RC4   {0x02}
        sub CRYPTO_XOR   {0x04}      # unimplemented
        sub CRYPTO_AES   {0x08}      # unimplemented
        has '_crypto' => (
            isa => enum([CRYPTO_PLAIN, CRYPTO_RC4, CRYPTO_XOR, CRYPTO_AES]),
            is  => 'rw',
            default  => CRYPTO_PLAIN,
            init_arg => undef
        );

        #
        sub DH_P {
            require Bit::Vector;
            state $DH_P
                = Bit::Vector->new_Hex('FFFFFFFFFFFFFFFFC90FDAA22168C234C4C66'
                . '28B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404D'
                . 'DEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B5766'
                . '25E7EC6F44C42E9A63A36210000000000090563');
            $DH_P;
        }
        sub DH_G {2}
        sub VC   { "\0" x 8 }

        sub crypto_provide {
            return pack q[N],
                CRYPTO_PLAIN    # | CRYPTO_RC4    #| CRYPTO_XOR | CRYPTO_AES;
        }
        after 'BUILD' => sub {
            my ($self, $args) = @_;
            if (defined $args->{'connect'}) {    # outgoing
                $self->_set_local_connection;
            }
            else {

                # incoming
            }
        };
    }
    for my $flag (qw[
                  interesting remote_interested
                  choked      remote_choked
                  support_extensions              local_connection
                  handshake   connecting          queued
                  on_parole   seed                optimistic_unchoke
                  snubbed     upload_only]
        )
    {   has $flag => (isa       => 'Bool',
                      traits    => ['Bool'],
                      is        => 'ro',
                      default   => 0,
                      predicate => 'is_' . $flag,
                      handles   => {
                                  '_set_' . $flag    => 'set',
                                  '_unset_' . $flag  => 'unset',
                                  '_toggle_' . $flag => 'toggle',
                                  'is_not_' . $flag  => 'not'
                      }
        );
    }

    #
    my $infohash_constraint;
    after 'BUILD' => sub {
        my ($s, $a) = @_;
        my $rule = $s->client->ip_filter->is_banned($s->handle->{'peername'});
        if (defined $rule) {
            $s->trigger_ip_filter(
                               {protocol => ($s->ipv6 ? 'udp6' : 'udp4'),
                                severity => 'debug',
                                event    => 'ip_filter',
                                ip       => $s->host,
                                rule     => $rule,
                                message => 'Connection terminated by ipfilter'
                               }
            );
            return $s->disconnect('Connection terminated by ipfilter');
        }
        require Scalar::Util;
        Scalar::Util::weaken $s;
        my $hand_shake_writer = sub {
            my $packet =
                Net::BitTorrent::Protocol::BEP03::Packets::build_handshake(
                                  $s->_build_reserved, $s->torrent->info_hash,
                                  $s->client->peer_id);
            $s->push_write($packet);
        };
        my $hand_shake_reader = sub {
            my (undef, $data) = @_;
            if (my ($reserved, $info_hash, $peer_id)
                = $data =~ m[^\23BitTorrent protocol(.{8})(.{20})(.{20})$])
            {   $infohash_constraint //=
                    Moose::Util::TypeConstraints::find_type_constraint(
                                                'NBTypes::Torrent::Infohash');
                $info_hash = $infohash_constraint->coerce($info_hash);
                $s->_set_support_extensions(
                                         ord(substr($reserved, 5, 1)) & 0x10);
                $s->_set_peer_id($peer_id);
                if ($s->local_connection) {
                    return $s->disconnect(
                        'Bad info_hash (Does not match the torrent we were seeking)'
                    ) if $info_hash->Compare($s->torrent->info_hash) != 0;
                }
                else {
                    my $torrent = $a->{'client'}->torrent($info_hash);
                    return $s->disconnect(
                            'Bad info_hash (We are not serving this torrent)')
                        if !$torrent;
                    $s->_set_torrent($torrent);
                    $hand_shake_writer->();
                }
            }
            else {

                # XXX - apply encrypted peer role
            }
            1;
        };
        $s->handle->on_read(
            sub {
            PACKET: while (my $packet = parse_packet(\$s->handle->rbuf)) {
                    $s->_handle_packet($packet);
                    last PACKET if !$s->rbuf;
                }
                warn $s->rbuf if $s->rbuf;
            }
        );
        $s->handle->on_eof(
            sub {
                $s->disconnect('Connection closed by remote peer');
            }
        );
        $s->handle->on_error(
            sub {
                my ($h, $fatal, $msg) = @_;
                warn $msg;
                return if !$fatal;
                $s->disconnect('Error: ' . $msg);
            }
        );

        #$s->handle->on_prepare(sub { $s->set_connecting; 30 });
        #$s->handle->on_connect(
        #    sub {    # outgoing. Send handshake
        #        my ($handle, $host, $port, $retry) = @_;
        #    }
        #);
        $s->handle->on_error(
            sub {
                my ($h, $fatal, $msg) = @_;
                return if !$fatal;
                $s->disconnect($msg);
            }
        );
        $s->handle->on_eof(
            sub {
                $s->disconnect('Connection closed by remote peer');
            }
        );
        $s->handle->on_drain(sub {1});
        $s->handle->on_timeout(sub { my ($handle) = @_; ... });
        $s->handle->rtimeout(60 * 5);
        $s->handle->wtimeout(60 * 10);

        #$s->handle->read_size(1024 * 16);
        #$s->handle->upload_rate(200);
        #$s->handle->download_rate(500);
        #
        $hand_shake_writer->($s->handle) if $s->local_connection;
        $s->push_read(chunk => 68, $hand_shake_reader);

        #$hand_shake_reader->($s->handle, substr($s->handle->rbuf, 0, 68) );
    };

    sub disconnect {
        my ($s, $reason) = @_;
        warn sprintf '%20s | %s', $s->peer_id || '[unknown peer]', $reason;
        if (!$s->handle->destroyed) {
            if (defined $s->handle->{'fh'}) {
                shutdown($s->handle->{'fh'}, 2);
                close($s->handle->{'fh'});
            }
            $s->handle->destroy;
        }
        $s->client->del_peer($s);
        $_[0] = undef;
    }

    sub _build_sockaddr {
        require Net::BitTorrent::Network::Utility;
        Net::BitTorrent::Network::Utility::sockaddr($_[0]->host, $_[0]->port);
    }
    for my $flag (qw[up_speed down_speed payload_up_speed payload_downspeed])
    {   has $flag => (isa     => 'Int',
                      is      => 'rw',
                      default => 0
        );
    }
    for my $dir (qw[up down]) {
        for my $content ('', 'payload_') {   # XXX - Update these every second
            my $attr = sprintf '%s%s_speed', $content, $dir;
            has $attr => (isa      => 'Int',
                          is       => 'ro',
                          init_arg => undef,
                          default  => 0,
                          writer   => '_' . $attr
            );
        }
        my $attr = sprintf 'total_%sload', $dir;
        has $attr => (isa      => 'Int',
                      is       => 'ro',
                      init_arg => undef,
                      traits   => ['Counter'],
                      handles  => {'_inc_' . $attr => 'inc'},
                      default  => 0
        );
        $attr = sprintf '%sload_limit', $dir;
        has $attr => (isa      => subtype(as 'Int' => where   { $_ >= -1 }),
                      is       => 'ro',
                      init_arg => undef,
                      writer   => '_set_' . $attr,
                      default  => -1               # Unlimited
        );
    }
    has 'peer_id' => (isa       => 'NBTypes::Client::PeerID',
                      is        => 'ro',
                      writer    => '_set_peer_id',
                      predicate => 'has_peer_id'
    );
    has 'pieces' => (is         => 'ro',
                     isa        => 'NBTypes::Torrent::Bitfield',
                     lazy_build => 1,
                     coerce     => 1,
                     init_arg   => undef,
                     writer     => '_set_pieces',
                     clearer    => '_clear_pieces'
    );
    sub _build_pieces { '0' x $_[0]->torrent->piece_count }

    sub _XXX_set_seed {
        $_[0]->set_seed($_[0]->pieces->to_Bin =~ m[0] ? 0 : 1);
    }
    for my $action (qw[request active]) {
        has 'last_'
            . $action => (
                     is      => 'ro',
                     isa     => 'Int',
                     traits  => ['Number'],
                     handles => {'set_last_' . $action => ['set', sub {time}]}
            );
    }

=begin comment     after 'BUILD' => sub {
        use Data::Dump;
        ddx \@_;
        ...;
    };
=cut
    my %_packet_dispatch;

    sub _handle_packet {
        my ($self, $packet) = @_;
        return if !$self->has_torrent;
        return if !$self->has_handle;

        #use Data::Dump;
        #ddx $packet;
        %_packet_dispatch = (
            BITFIELD => sub {    # 5
                my ($s, $bitfield) = @_;
                return $s->_set_pieces($bitfield);
            },
            EXTPROTOCOL => sub {    # 20
                my ($s, $pid, $p) = @_;
                if ($pid == 0) {    # Setup/handshake
                    if (defined $packet->{'p'} && $self->client->has_dht) {
                        $self->client->dht->ipv4_add_node(
                                [join('.', unpack 'C*', $packet->{'ipv4'}),
                                 $packet->{'p'}
                                ]
                        ) if defined $packet->{'ipv4'};
                        $self->client->dht->ipv6_add_node(
                                      [[join ':',
                                        (unpack 'H*', $packet->{'ipv6'})
                                            =~ m[(....)]g
                                       ],
                                       $packet->{'p'}
                                      ]
                        ) if defined $packet->{'ipv6'};
                    }
                }
                return 1;
            }
        ) if !keys %_packet_dispatch;
        $_packet_dispatch{$packet->{'type'}}->($self, $packet->{'payload'})
            if defined $_packet_dispatch{$packet->{'type'}};
    }
    {    # Callback system
        after 'BUILD' => sub {
            my $s = shift;

            #$s->trigger_peer_construction($s);
        };
        after 'DEMOLISH' => sub {
            my $s = shift;

            #$s->trigger_peer_destruction($s);
        };
    }
    {    ### Simple plugin system
        my @_plugins;

        sub _register_plugin {
            my $s = shift;
            return $s->meta->apply(@_) if blessed $s;
            my %seen = ();
            return @_plugins = grep { !$seen{$_}++ } @_plugins, @_;
        }
        after 'BUILD' => sub {
            return if !@_plugins;
            my ($s, $a) = @_;
            require Moose::Util;
            Moose::Util::apply_all_roles($s, @_plugins,
                                         {rebless_params => $a});
        };
    }
###
    sub DEMOLISH {
        my $s = shift;
        return if $s->handle->destroyed;
        if (defined $s->handle->{'fh'}) {
            shutdown($s->handle->{'fh'}, 2);
            close($s->handle->{'fh'});
        }
        $s->handle->destroy;
        1;
    }
}
1;

=pod



=head1 Activity Methods


=head1 Status Methods

These methods (or accessors) do not initiate a particular action but return
current state of the peer.

=head2 Net::BitTorrent::Peer->interesting( )

We are interested in pieces from this peer.

=head2 Net::BitTorrent::Peer->choked( )

We have choked this peer.

=head2 Net::BitTorrent::Peer->remote_interested( )

The peer is interested in us.

=head2 Net::BitTorrent::Peer->remote_choked( )

The peer has choked us.

=head2 Net::BitTorrent::Peer->support_extensions( )

Means that this peer supports the extension protocol.

=head2 Net::BitTorrent::Peer->local_connection( )

The connection was initiated by us, the peer has a listen port open, and that
port is the same as in the address of this peer. If this flag is not set, this
peer connection was opened by this peer connecting to us.

=head2 Net::BitTorrent::Peer->handshake( )

The connection is opened, and waiting for the handshake. Until the handshake
is done, the peer cannot be identified.

=head2 Net::BitTorrent::Peer->connecting( )

The connection is in a half-open state (i.e. it is being connected).

=head2 Net::BitTorrent::Peer->queued( )

The connection is currently queued for a connection attempt. This may happen
if there is a limit set on the number of half-open TCP connections.

=head2 Net::BitTorrent::Peer->on_parole( )

The peer has participated in a piece that failed the hash check, and is now
"on parole", which means we're only requesting whole pieces from this peer
until it either fails that piece or proves that it doesn't send bad data.

=head2 Net::BitTorrent::Peer->seed( )

This peer is a seed (it has all the pieces).

=head2 Net::BitTorrent::Peer->optimistic_unchoke( )

This peer is subject to an optimistic unchoke. It has been unchoked for a
while to see if it might unchoke us in return an earn an upload/unchoke slot.
If it doesn't within some period of time, it will be choked and another peer
will be optimistically unchoked.

=head2 Net::BitTorrent::Peer->snubbed( )

This peer has recently failed to send a block within the request timeout from
when the request was sent. We're currently picking one block at a time from
this peer.

=head2 Net::BitTorrent::Peer->upload_only( )

This peer has either explicitly (with an extension) or implicitly (by becoming
a seed) told us that it will not downloading anything more, regardless of
which pieces we have.

=head2 Net::BitTorrent::Peer->host( )

The IP-address to this peer.

=head2 Net::BitTorrent::Peer->up_speed( )

Contains the current upload speed we have to this peer (including any protocol
messages). This figure is updated approximately once every second.

=head2 Net::BitTorrent::Peer->down_speed( )

Contains the current download speed we have from this peer (including any
protocol messages). This figure is updated approximately once every second.

=head2 Net::BitTorrent::Peer->payload_up_speed( )

Contains the current upload speed we have to this peer (includes B<only>
payload data). This figure is updated approximately once every second.

=head2 Net::BitTorrent::Peer->payload_down_speed( )

Contains the current upload speed we have to this peer (includes B<only>
payload data). This figure is updated approximately once every second.

=head2 Net::BitTorrent::Peer->total_download( )

The total number of bytes downloaded from this peer. This number does not
include the protocol chatter, but only the payload data.

=head2 Net::BitTorrent::Peer->total_upload( )

The total number of bytes uploaded to this peer. This number does not include
the protocol chatter, but only the payload data.

=head2 Net::BitTorrent::Peer->peer_id( )

The peer's id as used in the BitTorrent protocol. This id can be used to
extract 'fingerprints' from the peer. Sometimes it can tell you which client
the peer is using.

=head2 Net::BitTorrent::Peer->pieces( )

This is a bitfield with one bit per piece in the torrent. Each bit tells you
if the peer has that piece (if it's set to 1) or if the peer is missing that
piece (set to 0). Like all bitfields, this returns a
L<Bit::Vector|Bit::Vector> object.

=head2 Net::BitTorrent::Peer->upload_limit( )

The number of bytes we are allowed to send to this peer every second. It may
be C<-1> if there's no local limit on the peer. The global limit and the
torrent limit is always enforced anyway.

=head2 Net::BitTorrent::Peer->download_limit( )

The number of bytes per second this peer is allowed to receive. C<-1> means
it's unlimited.

=head2 Net::BitTorrent::Peer->last_request( )

The time since we last sent a request to this peer.

=head2 Net::BitTorrent::Peer->last_active( )

The time since any transfer occurred with this peer.
































=begin TODO

request_timeout is the number of seconds until the current front piece request will time out. This timeout can be adjusted through session_settings::request_timeout. -1 means that there is not outstanding request.

send_buffer_size and used_send_buffer is the number of bytes allocated and used for the peer's send buffer, respectively.

receive_buffer_size and used_receive_buffer are the number of bytes allocated and used as receive buffer, respectively.

num_hashfails is the number of pieces this peer has participated in sending us that turned out to fail the hash check.

country is the two letter ISO 3166 country code for the country the peer is connected from. If the country hasn't been resolved yet, both chars are set to 0. If the resolution failed for some reason, the field is set to "--". If the resolution service returns an invalid country code, it is set to "!!". The countries.nerd.dk service is used to look up countries. This field will remain set to 0 unless the torrent is set to resolve countries, see resolve_countries().

inet_as_name is the name of the AS this peer is located in. This might be an empty string if there is no name in the geo ip database.

inet_as is the AS number the peer is located in.

load_balancing is a measurement of the balancing of free download (that we get) and free upload that we give. Every peer gets a certain amount of free upload, but this member says how much extra free upload this peer has got. If it is a negative number it means that this was a peer from which we have got this amount of free download.

requests_in_buffer is the number of requests messages that are currently in the send buffer waiting to be sent.

download_queue_length is the number of piece-requests we have sent to this peer that hasn't been answered with a piece yet.

upload_queue_length is the number of piece-requests we have received from this peer that we haven't answered with a piece yet.

failcount is the number of times this peer has "failed". i.e. failed to connect or disconnected us. The failcount is decremented when we see this peer in a tracker response or peer exchange message.

You can know which piece, and which part of that piece, that is currently being downloaded from a specific peer by looking at the next four members. downloading_piece_index is the index of the piece that is currently being downloaded. This may be set to -1 if there's currently no piece downloading from this peer. If it is >= 0, the other three members are valid. downloading_block_index is the index of the block (or sub-piece) that is being downloaded. downloading_progress is the number of bytes of this block we have received from the peer, and downloading_total is the total number of bytes in this block.

client is a string describing the software at the other end of the connection. In some cases this information is not available, then it will contain a string that may give away something about which software is running in the other end. In the case of a web seed, the server type and version will be a part of this string.

connection_type can currently be one of standard_bittorrent or web_seed. These are currently the only implemented protocols.

remote_dl_rate is an estimate of the rate this peer is downloading at, in bytes per second.

pending_disk_bytes is the number of bytes this peer has pending in the disk-io thread. Downloaded and waiting to be written to disk. This is what is capped by session_settings::max_queued_disk_bytes.

send_quota and receive_quota are the number of bytes this peer has been assigned to be allowed to send and receive until it has to request more quota from the bandwidth manager.

rtt is an estimated round trip time to this peer, in milliseconds. It is estimated by timing the the tcp connect(). It may be 0 for incoming connections.

num_pieces is the number of pieces this peer has.

download_rate_peak and upload_rate_peak are the highest download and upload rates seen on this connection. They are given in bytes per second. This number is reset to 0 on reconnect.

progress is the progress of the peer in the range [0, 1]. This is always 0 when floating point operations are diabled, instead use progress_ppm.

progress_ppm indicates the download progress of the peer in the range [0, 1000000] (parts per million).

=end TODO


























=cut
