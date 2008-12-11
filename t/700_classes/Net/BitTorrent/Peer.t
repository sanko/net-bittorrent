#!/usr/bin/perl -w
use strict;
use warnings;
use Test::More qw[no_plan];
use Module::Build;
use Socket qw[AF_INET SOCK_STREAM INADDR_LOOPBACK SOL_SOCKET
    sockaddr_in unpack_sockaddr_in inet_ntoa];
use File::Temp qw[tempdir];
use Scalar::Util qw[/weak/];
use lib q[../../../../lib];
use Net::BitTorrent;
use Net::BitTorrent::Torrent;
use Net::BitTorrent::Peer;
use Net::BitTorrent::Protocol qw[:all];
$|++;
my $test_builder       = Test::More->builder;
my $simple_dot_torrent = q[./t/900_data/950_torrents/953_miniswarm.torrent];
chdir q[../../../../] if not -f $simple_dot_torrent;
my $build           = Module::Build->current;
my $okay_tcp        = $build->notes(q[okay_tcp]);
my $release_testing = $build->notes(q[release_testing]);
my $verbose         = $build->notes(q[verbose]);
my $threads         = $build->notes(q[threads]);
$SIG{__WARN__} = ($verbose ? sub { diag shift } : sub { });
my ($flux_capacitor, %peers) = (0, ());

BEGIN {
    *CORE::GLOBAL::time
        = sub () { return CORE::time + ($flux_capacitor * 60); };
}
SKIP: {
    skip(
        q[Due to system configuration, socket-based tests have been disabled.  ...which makes N::B pretty useless.]
    ) if !$okay_tcp;
    skip(
        q[Fine grained regression tests skipped; turn on $ENV{RELESE_TESTING} to enable]
    ) if !$release_testing;
    my ($tempdir)
        = tempdir(q[~NBSF_test_XXXXXXXX], CLEANUP => 1, TMPDIR => 1);
    warn(sprintf(q[File::Temp created '%s' for us to play with], $tempdir));
    my $client = Net::BitTorrent->new({LocalHost => q[127.0.0.1]});
    skip(q[Skip Failed to create client.]) if !$client;
    my $torrent = $client->add_torrent({Path    => $simple_dot_torrent,
                                        BaseDir => $tempdir
                                       }
    );

    END {
        return if not defined $torrent;
        for my $file (@{$torrent->files}) { $file->_close() }
    }
    ok( $client->on_event(
            q[peer_read],
            sub {
                my ($self, $args) = @_;
                is($self, $client,
                    q[Correct args passed to 'peer_read' [$_[0]]]);
                isa_ok($args->{q[Peer]}, q[Net::BitTorrent::Peer],
                       q[  ... [$_[1]->{'Peer'}]]);
                like($args->{q[Length]}, qr[^\d+$],
                     q[  ... [$_[1]->{'Length'}]]);
                my $peer = $args->{q[Peer]};
                delete $args->{q[Peer]};
                my $_len = $args->{q[Length]};
                delete $args->{q[Length]};
                is_deeply($args, {}, q[  ... No other keys in $_[1]]);
                warn(sprintf(q[Read %d bytes from '%s'],
                             $_len, $peer->as_string
                     )
                );
            }
        ),
        q[Installed 'peer_read' event handler]
    );
    ok( $client->on_event(
            q[peer_write],
            sub {
                my ($self, $args) = @_;
                is($self, $client,
                    q[Correct args passed to 'peer_read' [$_[0]]]);
                isa_ok($args->{q[Peer]}, q[Net::BitTorrent::Peer],
                       q[  ... [$_[1]->{'Peer'}]]);
                like($args->{q[Length]}, qr[^\d+$],
                     q[  ... [$_[1]->{'Length'}]]);
                my $peer = $args->{q[Peer]};
                delete $args->{q[Peer]};
                my $_len = $args->{q[Length]};
                delete $args->{q[Length]};
                is_deeply($args, {}, q[  ... No other keys in $_[1]]);
                warn(sprintf(q[Wrote %d bytes from '%s'],
                             $_len, $peer->as_string
                     )
                );
            }
        ),
        q[Installed 'peer_write' event handler]
    );
    ok( $client->on_event(
            q[peer_disconnect],
            sub {
                my ($self, $args) = @_;
                is($self, $client,
                    q[Correct args passed to 'peer_disconnect' [$_[0]]]);
                isa_ok($args->{q[Peer]}, q[Net::BitTorrent::Peer],
                       q[  ... [$_[1]->{'Peer'}]]);
                ok(defined($args->{q[Reason]}), q[  ... [$_[1]->{'Reason'}]]);
                my $peer = $args->{q[Peer]};
                delete $args->{q[Peer]};
                my $_why = $args->{q[Reason]};
                delete $args->{q[Reason]};
                is_deeply($args, {}, q[  ... No other keys in $_[1]]);
                warn(sprintf(q[Disconnected from '%s'%s],
                             $peer->as_string,
                             ($_why
                              ? (q[ (] . $_why . q[)])
                              : q[]
                             )
                     )
                );
            }
        ),
        q[Installed 'peer_disconnect' event handler]
    );
    ok( $client->on_event(
            q[peer_connect],
            sub {
                my ($self, $args) = @_;
                is($self, $client,
                    q[Correct args passed to 'peer_connect' [$_[0]]]);
                isa_ok($args->{q[Peer]}, q[Net::BitTorrent::Peer],
                       q[  ... [$_[1]->{'Peer'}]]);
                my $peer = $args->{q[Peer]};
                delete $args->{q[Peer]};
                is_deeply($args, {}, q[  ... No other keys in $_[1]]);
                like($peer->_host . q[:] . $peer->_port,
                     qr[127.0.0.1:\d+],
                     sprintf q[%s connection %s '%s'],
                     ($peer->_incoming ? q[Incoming] : q[Outgoing]),
                     ($peer->_incoming ? q[from]     : q[to]),
                     $peer->as_string
                );
                return 1;
            }
        ),
        q[Installed 'peer_connect' event handler]
    );
    my @request_offsets = qw[0     16384 0     16384 16344 16354];
    my @request_lengths = qw[16384 16384 16384 16384 16384 46384];
    my @cancel_offsets  = reverse @request_offsets;
    my @indexes         = (0 .. 10);                                 # have
    ok( $client->on_event(
            q[incoming_packet],
            sub {
                my ($self, $args) = @_;
                my $type    = $args->{q[Type]};
                my $peer    = $args->{q[Peer]};
                my $payload = $args->{q[Payload]};
                if ($type eq KEEPALIVE) {
                    warn q[TODO: keepalive];
                }
                elsif ($type == HANDSHAKE) {
                    is($self, $client,
                        q[Correct args passed to 'packet_incoming_handshake' [$_[0]]]
                    );
                    isa_ok($args->{q[Peer]},
                           q[Net::BitTorrent::Peer],
                           q[  ... [$_[1]->{'Peer'}]]
                    );
                    is(scalar(keys %{$args->{'Payload'}}),
                        3, q[  ... scalar(keys %{$payload})]);
                    is(length($args->{'Payload'}{q[Reserved]}),
                        8, q[  ... reserved conforms to spec]);
                    is(length($args->{'Payload'}{q[Infohash]}),
                        20, q[  ... infohash conforms to spec]);
                    is(length($args->{'Payload'}{q[PeerID]}),
                        20, q[  ... peerid conforms to spec]);
                    delete $args->{q[Peer]};
                    my $_len = $args->{q[Payload]};
                    delete $args->{q[Payload]};
                    is_deeply($args,
                              {Type => HANDSHAKE},
                              q[  ... No other keys in $_[1]]);

                    if (   ($peer->peerid eq q[B] x 20)
                        or ($peer->peerid eq q[C] x 20)
                        or ($peer->peerid eq q[UNKNOWN-------------]))
                    {   pass(sprintf q[PeerID is okay (%s)], $peer->peerid);
                    }
                    elsif ($peer->peerid eq $self->peerid) {
                        pass(sprintf q[Peerid match: %s eq %s],
                             $self->peerid, $peer->peerid);
                    }
                    else {
                        fail(sprintf q[Unknown peerid: %s], $peer->peerid);
                    }
                }
                elsif ($type == CHOKE) {
                    my ($self, $args) = @_;
                    is($self, $client,
                        q[Correct args passed to 'packet_incoming_choke' [$_[0]]]
                    );
                    isa_ok($args->{q[Peer]},
                           q[Net::BitTorrent::Peer],
                           q[  ... [$_[1]->{'Peer'}]]
                    );
                    delete $args->{q[Peer]};
                    is_deeply($args,
                              {Payload => {}, Type => CHOKE},
                              q[  ... No other keys in $_[1]]);
                    is($peer->peerid, q[C] x 20, q[Choked by 'CC..CC']);
                }
                elsif ($type == UNCHOKE) {
                    my ($self, $args) = @_;
                    is($self, $client,
                        q[Correct args passed to 'packet_incoming_unchoke' [$_[0]]]
                    );
                    isa_ok($args->{q[Peer]},
                           q[Net::BitTorrent::Peer],
                           q[  ... [$_[1]->{'Peer'}]]
                    );
                    delete $args->{q[Peer]};
                    is_deeply($args,
                              {Payload => {}, Type => UNCHOKE},
                              q[  ... No other keys in $_[1]]);
                    is($peer->peerid, q[C] x 20, q[Unchoked by 'CC..CC']);
                }
                elsif ($type == INTERESTED) {
                    is($self, $client,
                        q[Correct args passed to 'packet_incoming_unchoke' [$_[0]]]
                    );
                    isa_ok($args->{q[Peer]},
                           q[Net::BitTorrent::Peer],
                           q[  ... [$_[1]->{'Peer'}]]
                    );
                    delete $args->{q[Peer]};
                    is_deeply($args,
                              {Payload => {}, Type => INTERESTED},
                              q[  ... No other keys in $_[1]]);
                    warn(
                        sprintf(q[%s is interested in me], $peer->as_string));
                }
                elsif ($type == NOT_INTERESTED) {
                    is($self, $client,
                        q[Correct args passed to 'packet_incoming_unchoke' [$_[0]]]
                    );
                    isa_ok($args->{q[Peer]},
                           q[Net::BitTorrent::Peer],
                           q[  ... [$_[1]->{'Peer'}]]
                    );
                    delete $args->{q[Peer]};
                    is_deeply($args,
                              {Payload => {}, Type => NOT_INTERESTED},
                              q[  ... No other keys in $_[1]]);
                    warn(
                        sprintf(q[%s is interested in me], $peer->as_string));
                }
                elsif ($type == HAVE) {
                    delete $_[1]->{q[Peer]};
                    is_deeply(\@_,
                              [$client,
                               {Payload => {Index => shift(@indexes)},
                                Type    => HAVE
                               }
                              ],
                              q[Correct arguments passed to 'packet_incoming_have' event handler]
                    );
                    if ($peer->peerid eq q[C] x 20) {
                        if ($payload->{q[Index]} == 0) {
                            pass(q[Good peer has i:0]);
                        }
                        elsif ($payload->{q[Index]} == 1) {
                            pass(q[Good peer has i:1]);
                        }
                        else {
                            fail(sprintf q[Peer claims to have %d],
                                 $payload->{q[Index]});
                        }
                    }
                    else {
                        fail(sprintf q[Unknown peer '%s' has %d],
                             $peer->peerid, $args->{q[Index]});
                    }
                }
                elsif ($type == BITFIELD) {
                    is($self, $client,
                        q[Correct args passed to 'packet_incoming_bitfield' [$_[0]]]
                    );
                    isa_ok($args->{q[Peer]},
                           q[Net::BitTorrent::Peer],
                           q[  ... [$_[1]->{'Peer'}]]
                    );
                    delete $args->{q[Peer]};
                    is_deeply($args,
                              {Type    => BITFIELD,
                               Payload => {}
                              },
                              q[  ... No other keys in $_[1]]
                    );
                    if (   ($peer->peerid eq q[B] x 20)
                        or ($peer->peerid eq q[C] x 20))
                    {   pass(sprintf q[PeerID is okay (%s)], $peer->peerid);
                    }
                    elsif ($peer->peerid eq $self->peerid) {
                        pass(sprintf q[Peerid match: %s eq %s],
                             $self->peerid, $peer->peerid);
                    }
                    else {
                        fail(sprintf q[Unknown peerid: %s], $peer->peerid);
                    }
                    warn(sprintf(q[Bitfield from %s], $peer->as_string));
                }
                elsif ($type == REQUEST) {
                    warn sprintf q[%s is requesting [I:%4d O:%6d L:%6d]],
                        $peer->as_string,
                        $payload->{q[Index]},
                        $payload->{q[Offset]},
                        $payload->{q[Length]};
                }
                elsif ($type == PIECE) {
                    delete $_[1]->{q[Peer]};
                    is_deeply(\@_,
                              [$client,
                               {Payload => {Index  => 0,
                                            Length => 16384,
                                            Offset => 0
                                },
                                Type => PIECE
                               }
                              ],
                              q[Correct args passed to 'packet_incoming_block' event handler]
                    );
                    is($peer->_torrent->downloaded,
                        16384, q[Torrent downloaded amount updated]);
                    warn(
                        sprintf
                            q[%s sent us [I:%4d O:%6d L:%6d] I have now downloaded %d bytes],
                        $peer->as_string,      $payload->{q[Index]},
                        $payload->{q[Offset]}, $payload->{q[Length]},
                        $peer->_torrent->downloaded
                    );
                }
                elsif ($type == CANCEL) {
                    ok( 1,
                        sprintf q[%s has canceled [I:%4d O:%6d L:%6d]],
                        $peer->as_string,
                        $args->{q[Index]},
                        $args->{q[Offset]},
                        $args->{q[Length]}
                    );
                }
                elsif ($type == HAVE_ALL) {
                    ok(1, sprintf q[%s says they have everything],
                        $peer->as_string);
                }
                elsif ($type == HAVE_NONE) {
                    ok(1, sprintf q[%s says they have nothing],
                        $peer->as_string);
                }
                else { die q[Unhandled packet: ] . $type }
            }
        ),
        q[Installed 'incoming_packet' event handler (TODO)]
    );
    ok( $client->on_event(
            q[outgoing_packet],
            sub {
                my ($self, $args) = @_;
                my $type    = $args->{q[Type]};
                my $peer    = $args->{q[Peer]};
                my $payload = $args->{q[Payload]};
                if ($type == HANDSHAKE) {
                    is($self, $client,
                        q[Correct args passed for outgoing handshake [$_[0]]]
                    );
                    isa_ok($peer, q[Net::BitTorrent::Peer],
                           q[  ... [$_[1]->{'Peer'}]]);
                    is(scalar(keys %{$payload}),
                        3, q[  ... scalar(@{$payload})]);
                    is(length($payload->{q[Reserved]}), 8,
                        q[  ... [length($payload->{q[Reserved]}) ==  8] (reserved)]
                    );
                    is(length($payload->{q[Infohash]}), 20,
                        q[  ... [length($payload->{q[Infohash]}) == 20] (infohash)]
                    );
                    is(length($payload->{q[PeerID]}), 20,
                        q[  ... [length($payload->{q[PeerID]}) == 20] (peerid)]
                    );
                    delete $args->{q[Peer]};
                    my $_len = $args->{q[Payload]};
                    delete $args->{q[Payload]};
                    is_deeply($args,
                              {Type => HANDSHAKE},
                              q[  ... No other keys in $_[1]]);

                    if ($peer->_incoming) {
                        if (   ($peer->peerid eq q[B] x 20)
                            or ($peer->peerid eq q[C] x 20)
                            or ($peer->peerid eq q[UNKNOWN-------------]))
                        {   pass(sprintf q[PeerID is okay (%s)],
                                 $peer->peerid);
                        }
                        elsif ($peer->peerid eq $self->peerid) {
                            pass(sprintf q[Peerid match: %s eq %s],
                                 $self->peerid, $peer->peerid);
                        }
                        else {
                            fail(sprintf q[Unknown peerid: %s],
                                 $peer->peerid);
                        }
                    }
                }
                elsif ($type == UNCHOKE) {
                    my ($self, $args) = @_;
                    is($self, $client,
                        q[Correct args passed to 'outgoing unchoke' [$_[0]]]);
                    isa_ok($args->{q[Peer]},
                           q[Net::BitTorrent::Peer],
                           q[  ... [$_[1]->{'Peer'}]]
                    );
                    delete $args->{q[Peer]};
                    is_deeply($args,
                              {Payload => {}, Type => UNCHOKE},
                              q[  ... No other keys in $_[1]]);
                    warn(sprintf(q[Unchoking %s], $peer->as_string));
                }
                elsif ($type == REQUEST) {
                    warn(sprintf q[Requesting [I:%4d O:%6d L:%6d] from %s],
                         $payload->{q[Index]},  $payload->{q[Offset]},
                         $payload->{q[Length]}, $peer->as_string
                    );
                    is($self, $client,
                        q[Correct args passed to 'outgoing request' [$_[0]]]);
                    isa_ok($args->{q[Peer]},
                           q[Net::BitTorrent::Peer],
                           q[  ... [$_[1]->{'Peer'}]]
                    );
                    delete $args->{q[Peer]};
                    like($args->{q[Payload]}{q[Index]},
                         qr[^\d+$], q[  ... [$_[1]->{'Payload'}{'Index'}]]);
                    delete $args->{q[Payload]}{q[Index]};
                    like($args->{q[Payload]}{q[Offset]},
                         qr[^\d+$], q[  ... [$_[1]->{'Payload'}{'Offset'}]]);
                    delete $args->{q[Payload]}{q[Offset]};
                    like($args->{q[Payload]}{q[Length]},
                         qr[^\d+$], q[  ... [$_[1]->{'Payload'}{'Length'}]]);
                    delete $args->{q[Payload]}{q[Length]};
                    is_deeply($args,
                              {Payload => {}, Type => REQUEST},
                              q[Correct args passed to 'outgoing request' event handler]
                    );
                }
                elsif ($type == CANCEL) {
                    is($self, $client,
                        q[Correct args passed to 'outgoing cancel' [$_[0]]]);
                    isa_ok($args->{q[Peer]},
                           q[Net::BitTorrent::Peer],
                           q[  ... [$_[1]->{'Peer'}]]
                    );
                    delete $args->{q[Peer]};
                    like($args->{q[Payload]}{q[Index]},
                         qr[^\d+$], q[  ... [$_[1]->{'Payload'}{'Index'}]]);
                    delete $args->{q[Payload]}{q[Index]};
                    like($args->{q[Payload]}{q[Offset]},
                         qr[^\d+$], q[  ... [$_[1]->{'Payload'}{'Offset'}]]);
                    delete $args->{q[Payload]}{q[Offset]};
                    like($args->{q[Payload]}{q[Length]},
                         qr[^\d+$], q[  ... [$_[1]->{'Payload'}{'Length'}]]);
                    delete $args->{q[Payload]}{q[Length]};
                    is_deeply($args,
                              {Payload => {}, Type => CANCEL},
                              q[Correct args passed to 'outgoing cancel' event handler]
                    );
                    warn(sprintf q[Canceling [I:%4d O:%6d L:%6d] from %s],
                         $payload->{q[Index]},  $payload->{q[Offset]},
                         $payload->{q[Length]}, $peer->as_string
                    );
                }
                elsif ($type == PIECE) {
                    warn sprintf
                        q[Sending [I:%4d O:%6d L:%6d] to %s. I have now uploaded %d bytes],
                        $payload->{q[Index]},
                        $payload->{q[Offset]},
                        $payload->{q[Length]},
                        $peer->as_string,
                        $peer->_torrent->uploaded;
                }
                elsif ($type == INTERESTED) {
                    my ($self, $args) = @_;
                    is($self, $client,
                        q[Correct args passed to 'outgoing interested' [$_[0]]]
                    );
                    isa_ok($args->{q[Peer]},
                           q[Net::BitTorrent::Peer],
                           q[  ... [$_[1]->{'Peer'}]]
                    );
                    delete $args->{q[Peer]};
                    is_deeply($args,
                              {Type => INTERESTED, Payload => {}},
                              q[  ... No other keys in $_[1]]);
                    warn(sprintf(q[I am interested in %s], $peer->as_string));
                }
                elsif ($type == CHOKE) {
                    warn sprintf q[ ===> Choking %s], $peer->as_string;
                }
                elsif ($type == BITFIELD) {
                    is($self, $client,
                        q[Correct args passed to 'outgoing bitfield' [$_[0]]]
                    );
                    isa_ok($args->{q[Peer]},
                           q[Net::BitTorrent::Peer],
                           q[  ... [$_[1]->{'Peer'}]]
                    );
                    delete $args->{q[Peer]};
                    is_deeply($args,
                              {Type => BITFIELD},
                              q[  ... No other keys in $_[1]]);
                    warn(sprintf(q[Sent bitfield to %s], $peer->as_string));
                }
                elsif ($type == HAVE_NONE) {
                    is($self, $client,
                        q[Correct args passed to 'outgoing have none' [$_[0]]]
                    );
                    isa_ok($args->{q[Peer]},
                           q[Net::BitTorrent::Peer],
                           q[  ... [$_[1]->{'Peer'}]]
                    );
                    delete $args->{q[Peer]};
                    is_deeply($args,
                              {Type => HAVE_NONE, Payload => {}},
                              q[  ... No other keys in $_[1]]);
                }
                elsif ($type == EXTPROTOCOL) {
                    is($self, $client,
                        q[Correct args passed to 'outgoing extended protocol' [$_[0]]]
                    );
                    isa_ok($args->{q[Peer]},
                           q[Net::BitTorrent::Peer],
                           q[  ... [$_[1]->{'Peer'}]]
                    );
                    delete $args->{q[Peer]};
                    delete $args->{q[Payload]};
                    delete $args->{q[ID]};
                    is_deeply($args,
                              {Type => EXTPROTOCOL},
                              q[  ... No other keys in $_[1]]);
                }
                else { warn q[****************** Unhandled packet: ] . $type }
            }
        ),
        q[Installed 'outgoing_packet' event handler]
    );
    ok( $client->on_event(
            q[ip_filter],
            sub {
                my ($self, $args) = @_;
                is($self, $client,
                    q[Correct params passed to 'ip_filter' ($_[0])]);
            TODO: {
                    local $TODO = q[Temporary DHT boot node breaks this test];
                    like(
                        $args->{q[Address]},   # XXX - removed for DHT testing
                        qr[^\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}:\d+$],
                        q[  ... ($_[1]->{'Address'})]
                    );
                }
                my $address = $args->{'Address'};
                delete $args->{'Address'};
                is_deeply(
                    $args,
                    {},
                    q[Correct params passed to 'ip_filter' (  ... No other keys in $_[1])]
                );
                warn(sprintf(q[Check IP filter for %s], $address));
                return 1;
            }
        ),
        q[Installed 'ip_filter' event handler]
    );
    warn sprintf q[%d|%d], 7, $test_builder->{q[Curr_Test]};
    warn(q[Net::BitTorrent::Peer->new() requires params...]);
    is(Net::BitTorrent::Peer->new(),   undef, q[No params]);
    is(Net::BitTorrent::Peer->new({}), undef, q[Empty hashref]);
    is(Net::BitTorrent::Peer->new({Socket => undef}),
        undef, q[Socket => undef]);
    is(Net::BitTorrent::Peer->new({Socket => 0}), undef, q[Socket => 0]);
    is(Net::BitTorrent::Peer->new({Socket => bless {}, q[GLOB]}),
        undef, q[Missing Client]);
    is( Net::BitTorrent::Peer->new({Socket => bless({}, q[GLOB]), Client => 0}
        ),
        undef,
        q[Client => 0]
    );
    is( Net::BitTorrent::Peer->new({Socket => bless(\{}, q[GLOB]),
                                    Client => bless(\{}, q[junk])
                                   }
        ),
        undef,
        q[Client => bless \{}, 'junk']
    );
    warn sprintf q[%d|%d], 14, $test_builder->{q[Curr_Test]};
    warn(q[For this next bit, we're testing outgoing peers...]);
    is( Net::BitTorrent::Peer->new({Client  => $client,
                                    Torrent => $torrent,
                                    Address => q[junk]
                                   }
        ),
        undef,
        q[Address => 'junk']
    );
    is( Net::BitTorrent::Peer->new({Client  => $client,
                                    Torrent => $torrent,
                                    Address => undef
                                   }
        ),
        undef,
        q[Address => undef]
    );
    is( Net::BitTorrent::Peer->new({Client  => $client,
                                    Address => q[127.0.0.1:0]
                                   }
        ),
        undef,
        q[Missing Torrent]
    );
    is( Net::BitTorrent::Peer->new({Client  => $client,
                                    Torrent => undef,
                                    Address => q[127.0.0.1:0]
                                   }
        ),
        undef,
        q[Torrent => undef]
    );
    is( Net::BitTorrent::Peer->new({Client  => $client,
                                    Torrent => 0,
                                    Address => q[127.0.0.1:0]
                                   }
        ),
        undef,
        q[Torrent => 0]
    );
    is( Net::BitTorrent::Peer->new({Client  => $client,
                                    Torrent => 'junk',
                                    Address => q[127.0.0.1:0]
                                   }
        ),
        undef,
        q[Torrent => 'junk']
    );
    is( Net::BitTorrent::Peer->new({Client  => $client,
                                    Torrent => bless(\{}, 'junk'),
                                    Address => q[127.0.0.1:0]
                                   }
        ),
        undef,
        q[Torrent => bless(\{}, 'junk')]
    );
    warn sprintf q[%d|%d], 21, $test_builder->{q[Curr_Test]};
    warn(q[Test incoming peers]);
    {
        $peers{q[A]} =
            Net::BitTorrent::Peer->new({Client  => $client,
                                        Torrent => $torrent,
                                        Address => q[127.0.0.1:0]
                                       }
            );
        isa_ok($peers{q[A]}, q[Net::BitTorrent::Peer], q[new()]);
        weaken $peers{q[A]};
        ok(isweak($peers{q[A]}),    q[  ...make $peers{q[A]} a weak ref]);
        ok($peers{q[A]}->as_string, q[as_string]);
        isa_ok($peers{q[A]}->_socket, q[GLOB], q[_socket]);
        isa_ok($peers{q[A]}->_torrent,
               q[Net::BitTorrent::Torrent], q[_torrent]);
        is($peers{q[A]}->_bitfield,     "\0", q[_bitfield]);
        is($peers{q[A]}->_peer_choking, 1,    q[Default peer_choking status]);
        is($peers{q[A]}->_am_choking,   1,    q[Default am_choking status]);
        is($peers{q[A]}->_peer_interested,
            0, q[Default peer_interested status]);
        is($peers{q[A]}->_am_interested, 0, q[Default am_interested status]);
        is($peers{q[A]}->_incoming,      0, q[Direction status is correct.]);
        warn sprintf q[%d|%d], 39, $test_builder->{q[Curr_Test]};
    }
    {
        my $newsock_A = newsock($client);
        ok($client->do_one_loop(1), q[    do_one_loop(1)]);
        warn sprintf q[%d|%d], 44, $test_builder->{q[Curr_Test]};
        is( syswrite($newsock_A,
                     build_handshake(chr(0) x 8, q[A] x 20, q[B] x 20)
            ),
            68,
            q[Sent handshake to client]
        );
        my $with_peer = scalar keys %{$client->_connections};
        is(syswrite($newsock_A, build_bitfield(chr(1))),
            6, q[Sent bitfield to client]);
        is(syswrite($newsock_A, build_bitfield(chr(0))),
            6, q[Sent bitfield to client]);
        is(syswrite($newsock_A, build_bitfield(chr(0))),
            6, q[Sent bitfield to client]);
        is(syswrite($newsock_A, build_bitfield(chr(0))),
            6, q[Sent bitfield to client]);
        ok($client->do_one_loop(1), q[    do_one_loop(1)]);
        ok(($with_peer > scalar keys %{$client->_connections}),
            q[Peer removed from list of connections]);
        warn sprintf q[%d|%d], 71, $test_builder->{q[Curr_Test]};
    }
    {
        my $newsock_B = newsock($client);
        ok($client->do_one_loop(1), q[    do_one_loop(1)]);
        is( syswrite($newsock_B,
                     build_handshake(chr(0) x 8,
                                     pack(q[H40], $torrent->infohash),
                                     $client->peerid
                     )
            ),
            68,
            q[Sent handshake to client]
        );
        my $with_peer = scalar keys %{$client->_connections};
        ok($client->do_one_loop(1), q[    do_one_loop(1)]);
        ok(($with_peer > scalar keys %{$client->_connections}),
            q[Peer removed from list of connections]);
        warn sprintf q[%d|%d], 99, $test_builder->{q[Curr_Test]};
    }
    {
        my $newsock_C = newsock($client);
        ok($client->do_one_loop(1), q[    do_one_loop(1)]);
        is( syswrite($newsock_C,
                     build_handshake(chr(0) x 8,
                                     pack(q[H40], $torrent->infohash),
                                     q[C] x 20
                     )
            ),
            68,
            q[Sent handshake to client]
        );
        ok($client->do_one_loop(1), q[    do_one_loop(1)]);
        ($peers{q[C]}) = map {
            (    $_->{q[Object]}->isa(q[Net::BitTorrent::Peer])
             and defined $_->{q[Object]}->peerid
             and ($_->{q[Object]}->peerid eq q[C] x 20))
                ? $_->{q[Object]}
                : ()
        } values %{$client->_connections};
        weaken $peers{q[C]};
        ok(isweak($peers{q[C]}), q[  ...make $peers{q[C]} a weak ref]);
        warn sprintf q[%d|%d], 131, $test_builder->{q[Curr_Test]};
        like(${$peers{q[C]}}, qr[127.0.0.1:\d+],
             q[Address properly resolved]);
        is($peers{q[C]}->_host, q[127.0.0.1], q[_host]);
        like($peers{q[C]}->_port, qr[^\d+$], q[_port]);
        is($peers{q[C]}->peerid, q[C] x 20, q[PeerID check]);
        isa_ok($peers{q[C]}->_socket, q[GLOB], q[Socket stored properly]);
        warn sprintf q[%d|%d], 136, $test_builder->{q[Curr_Test]};
        is($peers{q[C]}->_am_choking,      1, q[Initial outgoing choke]);
        is($peers{q[C]}->_peer_choking,    1, q[Initial incoming choke]);
        is($peers{q[C]}->_am_interested,   0, q[Initial outgoing interest]);
        is($peers{q[C]}->_peer_interested, 0, q[Initial incoming interest]);
        ok(syswrite($newsock_C, build_interested), q[Incoming interested]);
        ok($client->do_one_loop(1), q[    do_one_loop(1)]);
        is($peers{q[C]}->_peer_interested, 1, q[Peer is interested]);
        ok(syswrite($newsock_C, build_unchoke), q[Incoming unchoke]);
        ok($client->do_one_loop(1), q[    do_one_loop(1)]);
        is($peers{q[C]}->_peer_choking, 0, q[Peer has unchoked us]);
        warn sprintf q[%d|%d], 172, $test_builder->{q[Curr_Test]};
        ok(syswrite($newsock_C, build_choke), q[Incoming choke]);
        ok($client->do_one_loop(1), q[    do_one_loop(1)]);
        is($peers{q[C]}->_peer_choking,
            1, q[Post-choke incoming choke status]);
        is($peers{q[C]}->_am_interested,
            0, q[Post-choke outgoing interest status]);
        warn sprintf q[%d|%d], 184, $test_builder->{q[Curr_Test]};
        ok($client->do_one_loop(1), q[    do_one_loop(1)]);
        is($peers{q[C]}->_am_choking,
            0, q[Post-interested outgoing choke status]);
        warn sprintf q[%d|%d], 186, $test_builder->{q[Curr_Test]};
        is($peers{q[C]}->_peer_choking,
            1, q[Post-interested incoming choke status]);
        is($peers{q[C]}->_am_interested,
            0, q[Post-interested outgoing interest status]);
        $client->do_one_loop(0);
        is($peers{q[C]}->_peer_interested,
            1, q[Post-interested incoming interest status]);
        warn sprintf q[%d|%d], 189, $test_builder->{q[Curr_Test]};
        ok(shutdown($newsock_C, 2),
            q[Peer closes socket. Leaving us hanging.]);
        my $with_peer = scalar keys %{$client->_connections};
        ok($client->do_one_loop(1), q[    do_one_loop(1)]);
        ok(($with_peer > scalar keys %{$client->_connections}),
            q[Peer removed from list of connections]);
        warn sprintf q[%d|%d], 197, $test_builder->{q[Curr_Test]};
    }
    {
        my $newsock_D = newsock($client);
        ok($client->do_one_loop(1), q[    do_one_loop(1)]);
        is( syswrite($newsock_D,
                     build_handshake(qq[\0\0\0\0\0\20\0\4],
                                     pack(q[H40], $torrent->infohash),
                                     q[C] x 20
                     )
            ),
            68,
            q[Sent handshake to client]
        );
        ok($client->do_one_loop(1), q[    do_one_loop(1)]);
        my $data = q[];

        #ok(sysread($newsock_D, $data, 68), q[Read handshake reply]);
        warn sprintf q[%d|%d], 234, $test_builder->{q[Curr_Test]};
        is(syswrite($newsock_D, build_keepalive()),
            4, q[Sent keepalive to client]);
        ok($client->do_one_loop(1), q[    do_one_loop(1)]);
        is(syswrite($newsock_D, build_unchoke()),
            5, q[Sent unchoke to client]);
        ok($client->do_one_loop(1), q[    do_one_loop(1)]);
        warn sprintf q[%d|%d], 254, $test_builder->{q[Curr_Test]};
        is(syswrite($newsock_D, build_choke()), 5, q[Sent choke to client]);
        is(syswrite($newsock_D, build_not_interested()),
            5, q[Sent not interested to client]);
        ok($client->do_one_loop(1), q[    do_one_loop(1)]);
        is(syswrite($newsock_D, build_have(0)), 9, q[Sent have to client]);
        ok($client->do_one_loop(1), q[    do_one_loop(1)]);
        ok(sysread($newsock_D, $data, 1024, length $data), q[Read]);
        warn sprintf q[%d|%d], 280, $test_builder->{q[Curr_Test]};
        is(syswrite($newsock_D, build_unchoke()),
            5, q[Sent unchoke to client]);
        ok($client->do_one_loop(1), q[    do_one_loop(1)]);
        ok(sysread($newsock_D, $data, 1024, length $data), q[Read]);
        my $fake_piece = q[A] x 16384;
        is(syswrite($newsock_D, build_piece(0, 0, \$fake_piece)),
            16397, q[Sent piece i:0 o:0 l:16384 to client]);
        warn sprintf q[%d|%d], 308, $test_builder->{q[Curr_Test]};
        ok($client->do_one_loop(1), q[    do_one_loop(1)]);
        is(syswrite($newsock_D, build_choke()), 5, q[Sent choke to client]);
        ok($client->do_one_loop(1), q[    do_one_loop(1)]);
        is(syswrite($newsock_D, build_unchoke()),
            5, q[Sent choke to client to read second unchoke]);
        ok($client->do_one_loop(1), q[    do_one_loop(1)]);
        warn sprintf q[%d|%d], 345, $test_builder->{q[Curr_Test]};
        $flux_capacitor = 0.5;
        ok($client->do_one_loop(1), q[    do_one_loop(1)]);
        warn sprintf q[%d|%d], 350, $test_builder->{q[Curr_Test]};
        $flux_capacitor = 1;
        ok($client->do_one_loop(3), q[    do_one_loop(3)]);
        ok(sysread($newsock_D, $data, 1024, length $data), q[Read]);
        warn sprintf q[%d|%d], 352, $test_builder->{q[Curr_Test]};
        ok(syswrite($newsock_D, build_keepalive()), q[Write keepalive]);
        ok($client->do_one_loop(1), q[    do_one_loop(1)]);
        is(syswrite($newsock_D, build_interested()),
            5, q[Sent interested to client]);
        $flux_capacitor = 2.5;
        ok($client->do_one_loop(1), q[    do_one_loop(1)]);

        #is(sysread($newsock_D, my ($in), 1024),
        #    undef, q[Fail to read data because socket was closed.]);
        #ok($client->do_one_loop(1), q[    do_one_loop(1)]);
        warn q[TODO: Test multithreaded stuff...];
    }
    {
        my $newsock_E = newsock($client);
        ok($client->do_one_loop(1), q[    do_one_loop(1)]);
        is( syswrite($newsock_E,
                     build_handshake(qq[\0\0\0\0\0\0\0\4],
                                     pack(q[H40], $torrent->infohash),
                                     q[UNKNOWN-------------]
                     )
            ),
            68,
            q[Sent handshake to client]
        );
        ok($client->do_one_loop(1), q[    do_one_loop(1)]);
        is(syswrite($newsock_E, build_have_all()),
            5, q[Sent HAVEALL to client]);
        ok($client->do_one_loop(1), q[    do_one_loop(1)]);
        warn sprintf q[%d|%d], 426, $test_builder->{q[Curr_Test]};
    }
    {
        my $newsock_F = newsock($client);
        ok($client->do_one_loop(1), q[    do_one_loop(1)]);
        is( syswrite($newsock_F,
                     build_handshake(qq[\0\0\0\0\0\0\0\4],
                                     pack(q[H40], $torrent->infohash),
                                     q[UNKNOWN-------------]
                     )
            ),
            68,
            q[Sent handshake to client]
        );
        ok($client->do_one_loop(1), q[    do_one_loop(1)]);
        is(syswrite($newsock_F, build_have_none()),
            5, q[Sent HAVEALL to client]);
        ok($client->do_one_loop(1), q[    do_one_loop(1)]);
        warn sprintf q[%d|%d], 475, $test_builder->{q[Curr_Test]};
    }
}

sub newsock {
    my ($server) = @_;
    my ($port, $packed_ip) = unpack_sockaddr_in(getsockname($server->_tcp));
    warn(sprintf q[Creating new sockpair to connect to %s:%d],
         inet_ntoa($packed_ip), $port);
    my $outgoing;
    socket($outgoing, AF_INET, SOCK_STREAM, getprotobyname(q[tcp]))
        ? do {
        pass(q[Created new socket]);
        connect($outgoing, getsockname($server->_tcp));
        }
        : fail(q[Failed to create new socket]);
    return $outgoing;
}
__END__
Copyright (C) 2008 by Sanko Robinson <sanko@cpan.org>

This program is free software; you can redistribute it and/or modify it
under the terms of The Artistic License 2.0.  See the LICENSE file
included with this distribution or
http://www.perlfoundation.org/artistic_license_2_0.  For
clarification, see http://www.perlfoundation.org/artistic_2_0_notes.

When separated from the distribution, all POD documentation is covered by
the Creative Commons Attribution-Share Alike 3.0 License.  See
http://creativecommons.org/licenses/by-sa/3.0/us/legalcode.  For
clarification, see http://creativecommons.org/licenses/by-sa/3.0/us/.

$Id$
