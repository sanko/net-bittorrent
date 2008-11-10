#!C:\perl\bin\perl.exe -w
use strict;
use warnings;
use Test::More;
use Module::Build;

#
use lib q[../../../../lib];
$|++;

# let's keep track of where we are...
my $test_builder = Test::More->builder;

#
my $simple_dot_torrent = q[./t/900_data/950_torrents/953_miniswarm.torrent];

# Make sure the path is correct
chdir q[../../../../] if not -f $simple_dot_torrent;

#
my $build           = Module::Build->current;
my $okay_tcp        = $build->notes(q[okay_tcp]);
my $release_testing = $build->notes(q[release_testing]);
my $verbose         = $build->notes(q[verbose]);
my $threads         = $build->notes(q[threads]);
$SIG{__WARN__} = ($verbose ? sub { diag shift } : sub { });

#
my ($flux_capacitor, %peers) = (0, ());

#
BEGIN {
    plan tests => 315;
    *CORE::GLOBAL::time    # Let's do the time warp again!
        = sub () { return CORE::time + ($flux_capacitor * 60); };

    # Ours
    use_ok(
        q[Socket], qw[AF_INET SOCK_STREAM INADDR_LOOPBACK SOL_SOCKET
            sockaddr_in unpack_sockaddr_in inet_ntoa]);
    use_ok(q[File::Temp],   qw[tempdir]);
    use_ok(q[Scalar::Util], qw[/weak/]);

    # Mine
    use_ok(q[Net::BitTorrent]);
    use_ok(q[Net::BitTorrent::Torrent]);
    use_ok(q[Net::BitTorrent::Peer]);
    use_ok(q[Net::BitTorrent::Protocol], qw[:all]);
}

#
SKIP: {

#skip(
#    q[Fine grained regression tests skipped; turn on $ENV{RELESE_TESTING} to enable],
#    ($test_builder->{q[Expected_Tests]} - $test_builder->{q[Curr_Test]})
#) if not $release_testing;
    skip(q[Socket-based tests have been disabled due to bad system config.],
         ($test_builder->{q[Expected_Tests]} - $test_builder->{q[Curr_Test]})
    ) if not $okay_tcp;

    #
    my ($tempdir)
        = tempdir(q[~NBSF_test_XXXXXXXX], CLEANUP => 1, TMPDIR => 1);
    warn(sprintf(q[File::Temp created '%s' for us to play with], $tempdir));
    my $client = Net::BitTorrent->new({LocalHost => q[127.0.0.1]});
    if (!$client) {
        warn(sprintf q[Socket error: [%d] %s], $!, $!);
        skip((      $test_builder->{q[Expected_Tests]}
                  - $test_builder->{q[Curr_Test]}
             ),
             q[Failed to create client]
        );
    }
    my $torrent = $client->add_torrent({Path    => $simple_dot_torrent,
                                        BaseDir => $tempdir
                                       }
    );
    warn sprintf q[%d|%d], 7, $test_builder->{q[Curr_Test]};

    END {
        return if not defined $torrent;
        for my $file (@{$torrent->files}) { $file->_close() }
    }

    #
    $client->do_one_loop();

    #
    ok( $client->on_event(
            q[peer_read],
            sub {
                my ($self, $args) = @_;

                # Param validation
                is($self, $client,
                    q[Correct args passed to 'peer_read' [$_[0]]]);
                isa_ok($args->{q[Peer]}, q[Net::BitTorrent::Peer],
                       q[  ... [$_[1]->{'Peer'}]]);
                like($args->{q[Length]}, qr[^\d+$],
                     q[  ... [$_[1]->{'Length'}]]);
                my $_peer = $args->{q[Peer]};    # we delete this and do the
                delete $args->{q[Peer]};         # next test because we don't
                my $_len = $args->{q[Length]};   # really know what's in
                delete $args->{q[Length]};       # $args->{q[Peer]}; It's
                                                 # basically just a socket at
                                                 # this point.
                is_deeply($args, {}, q[  ... No other keys in $_[1]]);

                #
                warn(sprintf(q[Read %d bytes from '%s'],
                             $_len, $_peer->_as_string
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

                # Param validation
                is($self, $client,
                    q[Correct args passed to 'peer_read' [$_[0]]]);
                isa_ok($args->{q[Peer]}, q[Net::BitTorrent::Peer],
                       q[  ... [$_[1]->{'Peer'}]]);
                like($args->{q[Length]}, qr[^\d+$],
                     q[  ... [$_[1]->{'Length'}]]);
                my $_peer = $args->{q[Peer]};    # we delete this and do the
                delete $args->{q[Peer]};         # next test because we don't
                my $_len = $args->{q[Length]};   # really know what's in
                delete $args->{q[Length]};       # $args->{q[Peer]}; It's
                                                 # basically just a socket at
                                                 # this point.
                is_deeply($args, {}, q[  ... No other keys in $_[1]]);

                #
                warn(sprintf(q[Wrote %d bytes from '%s'],
                             $_len, $_peer->_as_string
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

                # Param validation
                is($self, $client,
                    q[Correct args passed to 'peer_disconnect' [$_[0]]]);
                isa_ok($args->{q[Peer]}, q[Net::BitTorrent::Peer],
                       q[  ... [$_[1]->{'Peer'}]]);
                ok(defined($args->{q[Reason]}), q[  ... [$_[1]->{'Reason'}]]);
                my $_peer = $args->{q[Peer]};    # we delete this and do the
                delete $args->{q[Peer]};         # next test because we don't
                my $_why = $args->{q[Reason]};   # really know what's in
                delete $args->{q[Reason]};       # $args->{q[Peer]}; It's
                                                 # basically just a socket at
                                                 # this point.
                is_deeply($args, {}, q[  ... No other keys in $_[1]]);

                #
                warn(sprintf(q[Disconnected from '%s'%s],
                             $_peer->_as_string,
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
                my $_peer = $args->{q[Peer]}; # we delete this and do the next
                delete $args->{q[Peer]};      # test because we don't really
                    # know what's in $args->{q[Peer]}; It's basically just a
                    # socket at this point.
                is_deeply($args, {}, q[  ... No other keys in $_[1]]);
                like($$_peer,
                     qr[127.0.0.1:\d+],
                     sprintf q[%s connection %s '%s'],
                     ($_peer->_incoming ? q[Incoming] : q[Outgoing]),
                     ($_peer->_incoming ? q[from]     : q[to]),
                     $_peer->_as_string
                );
                return 1;
            }
        ),
        q[Installed 'peer_connect' event handler]
    );
    ok( $client->on_event(
            q[packet_incoming_request],
            sub {
                my ($self, $args) = @_;

                #
                warn sprintf q[%s is requesting [I:%4d O:%6d L:%6d]],
                    $args->{q[Peer]}->_as_string,
                    $args->{q[Index]},
                    $args->{q[Offset]},
                    $args->{q[Length]};
            }
        )
    );
    my @request_offsets = qw[0 16384 16384 16384];
    ok( $client->on_event(
            q[packet_outgoing_request],
            sub {
                my ($self, $args) = @_;
                is_deeply(\@_,
                          [$client,
                           {Index  => 0,
                            Length => 16384,
                            Offset => shift(@request_offsets),
                            Peer   => $peers{q[C]}
                           }
                          ],
                          q[Correct args passed to 'packet_outgoing_request' event handler]
                );
                warn(sprintf q[Requesting [I:%4d O:%6d L:%6d] from %s],
                     $args->{q[Index]},
                     $args->{q[Offset]},
                     $args->{q[Length]},
                     $args->{q[Peer]}->_as_string
                );
                return 1;
            }
        ),
        q[Installed 'packet_outgoing_request' event handler]
    );
    ok( $client->on_event(
            q[packet_incoming_cancel],
            sub {
                my ($self, $args) = @_;

                #
                warn sprintf q[%s has canceled [I:%4d O:%6d L:%6d]],
                    $args->{q[Peer]}->_as_string,
                    $args->{q[Index]},
                    $args->{q[Offset]},
                    $args->{q[Length]};
            }
        )
    );
    my @cancel_offsets = reverse @request_offsets;
    ok( $client->on_event(
            q[packet_outgoing_cancel],
            sub {
                my ($self, $args) = @_;
                is_deeply(\@_,
                          [$client,
                           {Index  => 0,
                            Length => 16384,
                            Offset => shift(@cancel_offsets),
                            Peer   => $peers{q[C]}
                           }
                          ],
                          q[Correct args passed to 'packet_outgoing_cancel' event handler]
                );

                #
                warn(sprintf(q[I have canceled [I:%4d O:%6d L:%6d] from %s],
                             $args->{q[Index]},  $args->{q[Offset]},
                             $args->{q[Length]}, $args->{q[Peer]}->_as_string
                     )
                );
            }
        ),
        q[Installed 'packet_outgoing_cancel' event handler]
    );
    ok( $client->on_event(
            q[packet_incoming_block],
            sub {
                my ($self, $args) = @_;

                # Param validation
                is_deeply(\@_,
                          [$client,
                           {Index  => 0,
                            Length => 16384,
                            Offset => 0,
                            Peer   => $peers{q[C]}
                           }
                          ],
                          q[Correct args passed to 'packet_incoming_block' event handler]
                );
                is($args->{q[Peer]}->_torrent->downloaded,
                    16384, q[Torrent downloaded amount updated]);

                #
                warn(
                    sprintf
                        q[%s sent us [I:%4d O:%6d L:%6d] I have now downloaded %d bytes],
                    $args->{q[Peer]}->_as_string,
                    $args->{q[Index]},
                    $args->{q[Offset]},
                    $args->{q[Length]},
                    $args->{q[Peer]}->_torrent->downloaded
                );
            }
        ),
        q[Installed 'packet_incoming_block' event handler]
    );
    ok( $client->on_event(
            q[packet_outgoing_block],
            sub {
                my ($self, $args) = @_;

                #
                warn sprintf
                    q[Sending [I:%4d O:%6d L:%6d] to %s. I have now uploaded %d bytes],
                    $args->{q[Index]},
                    $args->{q[Offset]},
                    $args->{q[Length]},
                    $args->{q[Peer]}->_as_string,
                    $args->{q[Peer]}->_torrent->uploaded;
            }
        )
    );
    my @indexes = (0 .. 10);
    ok( $client->on_event(
            q[packet_incoming_have],
            sub {
                my ($self, $args) = @_;

                # Param validation
                is_deeply(\@_,
                          [$client,
                           {Index => shift(@indexes),
                            Peer  => $peers{q[C]}
                           }
                          ],
                          q[Correct arguments passed to 'packet_incoming_have' event handler]
                );
                if ($args->{q[Peer]}->peerid eq q[C] x 20) {
                    if ($args->{q[Index]} == 0) { pass(q[Good peer has i:0]) }
                    elsif ($args->{q[Index]} == 1) {
                        pass(q[Good peer has i:1]);
                    }
                    else {
                        fail(sprintf q[Peer claims to have %d],
                             $args->{q[Index]});
                    }
                }
                else {
                    fail(sprintf q[Unknown peer '%s' has %d],
                         $args->{q[Peer]}->peerid,
                         $args->{q[Index]});
                }
                return 1;
            }
        ),
        q[Installed 'packet_incoming_have' event handler]
    );
    ok( $client->on_event(
            q[packet_outgoing_interested],
            sub {
                my ($self, $args) = @_;

                # Param validation
                is($self, $client,
                    q[Correct args passed to 'packet_outgoing_interested' [$_[0]]]
                );
                isa_ok($args->{q[Peer]}, q[Net::BitTorrent::Peer],
                       q[  ... [$_[1]->{'Peer'}]]);
                my $_peer = $args->{q[Peer]};    # we delete this and do the
                delete $args->{q[Peer]};         # next test because we don't
                                                 # really know what's in
                                                 # $args->{q[Peer]}; It's
                                                 # basically just a socket at
                                                 # this point.
                is_deeply($args, {}, q[  ... No other keys in $_[1]]);

                #
                warn(sprintf(q[I am interested in %s], $_peer->_as_string));
            }
        ),
        q[Installed 'packet_outgoing_interested' event handler]
    );
    ok( $client->on_event(
            q[packet_incoming_interested],
            sub {
                my ($self, $args) = @_;

                # Param validation
                is($self, $client,
                    q[Correct args passed to 'packet_incoming_unchoke' [$_[0]]]
                );
                isa_ok($args->{q[Peer]}, q[Net::BitTorrent::Peer],
                       q[  ... [$_[1]->{'Peer'}]]);
                my $_peer = $args->{q[Peer]};    # we delete this and do the
                delete $args->{q[Peer]};         # next test because we don't
                                                 # really know what's in
                                                 # $args->{q[Peer]}; It's
                                                 # basically just a socket at
                                                 # this point.
                is_deeply($args, {}, q[  ... No other keys in $_[1]]);

                #
                warn(sprintf(q[%s is interested in me], $_peer->_as_string));
            }
        ),
        q[Installed 'packet_incoming_interested' event handler]
    );
    ok( $client->on_event(
            q[packet_outgoing_unchoke],
            sub {
                my ($self, $args) = @_;

                # Param validation
                is($self, $client,
                    q[Correct args passed to 'packet_outgoing_unchoke' [$_[0]]]
                );
                isa_ok($args->{q[Peer]}, q[Net::BitTorrent::Peer],
                       q[  ... [$_[1]->{'Peer'}]]);
                my $_peer = $args->{q[Peer]};    # we delete this and do the
                delete $args->{q[Peer]};         # next test because we don't
                                                 # really know what's in
                                                 # $args->{q[Peer]}; It's
                                                 # basically just a socket at
                                                 # this point.
                is_deeply($args, {}, q[  ... No other keys in $_[1]]);

                #
                warn(sprintf(q[Unchoking %s], $_peer->_as_string));
            }
        ),
        q[Installed 'packet_outgoing_unchoke' event handler]
    );
    ok( $client->on_event(
            q[packet_incoming_unchoke],
            sub {
                my ($self, $args) = @_;

                # Param validation
                is($self, $client,
                    q[Correct args passed to 'packet_incoming_unchoke' [$_[0]]]
                );
                isa_ok($args->{q[Peer]}, q[Net::BitTorrent::Peer],
                       q[  ... [$_[1]->{'Peer'}]]);
                my $_peer = $args->{q[Peer]};    # we delete this and do the
                delete $args->{q[Peer]};         # next test because we don't
                                                 # really know what's in
                                                 # $args->{q[Peer]}; It's
                                                 # basically just a socket at
                                                 # this point.
                is_deeply($args, {}, q[  ... No other keys in $_[1]]);

                #
                is($_peer->peerid, q[C] x 20, q[Unchoked by 'CC..CC']);
                return 1;
            }
        ),
        q[Installed 'packet_incoming_unchoke' event handler]
    );
    ok( $client->on_event(
            q[packet_outgoing_choke],
            sub {
                my ($self, $args) = @_;

                #
                warn sprintf q[Choking %s], $args->{q[Peer]}->_as_string;
            }
        )
    );
    ok( $client->on_event(
            q[packet_incoming_choke],
            sub {
                my ($self, $args) = @_;

                # Param validation
                is($self, $client,
                    q[Correct args passed to 'packet_incoming_choke' [$_[0]]]
                );
                isa_ok($args->{q[Peer]}, q[Net::BitTorrent::Peer],
                       q[  ... [$_[1]->{'Peer'}]]);
                my $_peer = $args->{q[Peer]};    # we delete this and do the
                delete $args->{q[Peer]};         # next test because we don't
                                                 # really know what's in
                                                 # $args->{q[Peer]}; It's
                                                 # basically just a socket at
                                                 # this point.
                is_deeply($args, {}, q[  ... No other keys in $_[1]]);

                #
                is($_peer->peerid, q[C] x 20, q[Choked by 'CC..CC']);
                return 1;
            }
        ),
        q[Installed 'packet_incoming_choke' event handler]
    );
    ok( $client->on_event(
            q[packet_outgoing_bitfield],
            sub {
                my ($self, $args) = @_;

                # Param validation
                is($self, $client,
                    q[Correct args passed to 'packet_outgoing_bitfield' [$_[0]]]
                );
                isa_ok($args->{q[Peer]}, q[Net::BitTorrent::Peer],
                       q[  ... [$_[1]->{'Peer'}]]);
                my $_peer = $args->{q[Peer]};    # we delete this and do the
                delete $args->{q[Peer]};         # next test because we don't
                                                 # really know what's in
                                                 # $args->{q[Peer]}; It's
                                                 # basically just a socket at
                                                 # this point.
                is_deeply($args, {}, q[  ... No other keys in $_[1]]);

                #
                warn(sprintf(q[Sent bitfield to %s], $_peer->_as_string));
            }
        ),
        q[Installed 'packet_outgoing_bitfield' event handler]
    );
    ok( $client->on_event(
            q[packet_incoming_bitfield],
            sub {
                my ($self, $args) = @_;

                # Param validation
                is($self, $client,
                    q[Correct args passed to 'packet_incoming_bitfield' [$_[0]]]
                );
                isa_ok($args->{q[Peer]}, q[Net::BitTorrent::Peer],
                       q[  ... [$_[1]->{'Peer'}]]);
                my $_peer = $args->{q[Peer]};    # we delete this and do the
                delete $args->{q[Peer]};         # next test because we don't
                                                 # really know what's in
                                                 # $args->{q[Peer]}; It's
                                                 # basically just a socket at
                                                 # this point.
                is_deeply($args, {}, q[  ... No other keys in $_[1]]);

                #
                if (   ($_peer->peerid eq q[B] x 20)
                    or ($_peer->peerid eq q[C] x 20))
                {   pass(sprintf q[PeerID is okay (%s)], $_peer->peerid);
                }
                elsif ($_peer->peerid eq $self->peerid) {
                    pass(sprintf q[Peerid match: %s eq %s],
                         $self->peerid, $_peer->peerid);
                }
                else {
                    fail(sprintf q[Unknown peerid: %s], $_peer->peerid);
                }

                #
                warn(sprintf(q[Bitfield from %s], $_peer->_as_string));
                return 1;
            }
        ),
        q[Installed 'packet_incoming_bitfield' event handler]
    );
    ok( $client->on_event(
            q[packet_incoming_handshake],
            sub {
                my ($self, $args) = @_;

                # Param validation
                is($self, $client,
                    q[Correct args passed to 'packet_incoming_handshake' [$_[0]]]
                );
                isa_ok($args->{q[Peer]}, q[Net::BitTorrent::Peer],
                       q[  ... [$_[1]->{'Peer'}]]);
                is(scalar(@{$args->{'Payload'}}),
                    3, q[  ... scalar(@{$_[1]->{'Payload'}})]);
                is(length($args->{'Payload'}->[0]), 8,
                    q[  ... [length($_[1]->{'Payload'}->[0]) ==  8] (reserved)]
                );
                is(length($args->{'Payload'}->[1]), 20,
                    q[  ... [length($_[1]->{'Payload'}->[1]) == 20] (infohash)]
                );
                is(length($args->{'Payload'}->[2]), 20,
                    q[  ... [length($_[1]->{'Payload'}->[2]) == 20] (peerid)]
                );
                my $_peer = $args->{q[Peer]};    # we delete this and do the
                delete $args->{q[Peer]};         # next test because we don't
                my $_len = $args->{q[Payload]};  # really know what's in
                delete $args->{q[Payload]};      # $args->{q[Peer]}; It's
                                                 # basically just a socket at
                                                 # this point.
                is_deeply($args, {}, q[  ... No other keys in $_[1]]);

                #
                if (   ($_peer->peerid eq q[B] x 20)
                    or ($_peer->peerid eq q[C] x 20))
                {   pass(sprintf q[PeerID is okay (%s)], $_peer->peerid);
                }
                elsif ($_peer->peerid eq $self->peerid) {
                    pass(sprintf q[Peerid match: %s eq %s],
                         $self->peerid, $_peer->peerid);
                }
                else {
                    fail(sprintf q[Unknown peerid: %s], $_peer->peerid);
                }
                return 1;
            }
        ),
        q[Installed 'packet_incoming_handshake' event handler]
    );
    ok( $client->on_event(
            q[ip_filter],
            sub {
                my ($self, $args) = @_;
                is($self, $client,
                    q[Correct params passed to 'ip_filter' ($_[0])]);
                like($args->{q[Address]},
                     qr[^\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}:\d+$],
                     q[  ... ($_[1]->{'Address'})]);
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
    warn sprintf q[%d|%d], 28, $test_builder->{q[Curr_Test]};

    #
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
    warn sprintf q[%d|%d], 35, $test_builder->{q[Curr_Test]};

    #
    warn(q[For this next bit, we're testing outgoing peers...]);
    {
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
                                        Address => q[127.0.0.1:1995]
                                       }
            ),
            undef,
            q[Missing Torrent]
        );
        is( Net::BitTorrent::Peer->new({Client  => $client,
                                        Torrent => undef,
                                        Address => q[127.0.0.1:1995]
                                       }
            ),
            undef,
            q[Torrent => undef]
        );
        is( Net::BitTorrent::Peer->new({Client  => $client,
                                        Torrent => 0,
                                        Address => q[127.0.0.1:1995]
                                       }
            ),
            undef,
            q[Torrent => 0]
        );
        is( Net::BitTorrent::Peer->new({Client  => $client,
                                        Torrent => 'junk',
                                        Address => q[127.0.0.1:1995]
                                       }
            ),
            undef,
            q[Torrent => 'junk']
        );
        is( Net::BitTorrent::Peer->new({Client  => $client,
                                        Torrent => bless(\{}, 'junk'),
                                        Address => q[127.0.0.1:1995]
                                       }
            ),
            undef,
            q[Torrent => bless(\{}, 'junk')]
        );
        warn sprintf q[%d|%d], 42, $test_builder->{q[Curr_Test]};
        {

            #
            warn(q[Test incoming peers]);
            $peers{q[A]} =
                Net::BitTorrent::Peer->new({Client  => $client,
                                            Torrent => $torrent,
                                            Address => q[127.0.0.1:1995]
                                           }
                );
            isa_ok($peers{q[A]}, q[Net::BitTorrent::Peer], q[new()]);
            weaken $peers{q[A]};
            ok(isweak($peers{q[A]}), q[  ...make $peers{q[A]} a weak ref]);
            ok($peers{q[A]}->_as_string, q[_as_string]);
            isa_ok($peers{q[A]}->_socket, q[GLOB], q[_socket]);
            isa_ok($peers{q[A]}->_torrent,
                   q[Net::BitTorrent::Torrent], q[_torrent]);
            is($peers{q[A]}->_bitfield, qq[\0], q[_bitfield]);
            is($peers{q[A]}->_peer_choking, 1,
                q[Default peer_choking status]);
            is($peers{q[A]}->_am_choking, 1, q[Default am_choking status]);
            is($peers{q[A]}->_peer_interested,
                0, q[Default peer_interested status]);
            is($peers{q[A]}->_am_interested,
                0, q[Default am_interested status]);
            is($peers{q[A]}->_incoming, 0, q[Direction status is correct.]);
            warn sprintf q[%d|%d], 56, $test_builder->{q[Curr_Test]};
        }

        # Add some incoming peers to the client
        my $newsock_A = newsock($client);
        $client->do_one_loop();
        my $newsock_B = newsock($client);
        $client->do_one_loop();
        my $newsock_C = newsock($client);
        $client->do_one_loop();
        my $newsock_D = newsock($client);
        $client->do_one_loop();

        #
        {    # Peer_A provides an infohash we aren't serving.
            warn sprintf q[%d|%d], 72, $test_builder->{q[Curr_Test]};
            is( syswrite($newsock_A,
                         build_handshake(chr(0) x 8, q[A] x 20, q[B] x 20)
                ),
                68,
                q[Sent handshake to client]
            );
            is(syswrite($newsock_A, build_bitfield(chr(1))),
                6, q[Sent bitfield to client]);
            my $with_peer = scalar keys %{$client->_connections};
            $client->do_one_loop();
            $client->do_one_loop();
            ok(($with_peer > scalar keys %{$client->_connections}),
                q[Peer removed from list of connections]);
        }

        #
        {    # Peer_B provides our own peer_id. (Simulating a loopback)
            warn sprintf q[%d|%d], 95, $test_builder->{q[Curr_Test]};
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
            $client->do_one_loop();
            ok(($with_peer > scalar keys %{$client->_connections}),
                q[Peer removed from list of connections]);
        }

        #
        {    # Peer_C closes the socket in the middle of conversation
            warn sprintf q[%d|%d], (121 - 4), $test_builder->{q[Curr_Test]};
            is( syswrite($newsock_C,
                         build_handshake(chr(0) x 8,
                                         pack(q[H40], $torrent->infohash),
                                         q[C] x 20
                         )
                ),
                68,
                q[Sent handshake to client]
            );
            $client->do_one_loop();
            ok(close($newsock_C), q[Peer closes socket. Leaving us hanging.]);
            my $with_peer = scalar keys %{$client->_connections};
            $client->do_one_loop();
            ok(($with_peer > scalar keys %{$client->_connections}),
                q[Peer removed from list of connections]);
        }
        {    # Peer_D is well behaved but times out later on (time warp)
            warn sprintf q[%d|%d], (151 - 4), $test_builder->{q[Curr_Test]};
            is( syswrite($newsock_D,
                         build_handshake(chr(0) x 8,
                                         pack(q[H40], $torrent->infohash),
                                         q[C] x 20
                         )
                ),
                68,
                q[Sent handshake to client]
            );
            is(syswrite($newsock_A, build_bitfield(chr(0))),
                6, q[Sent bitfield to client]);
            $client->do_one_loop();
            $client->do_one_loop();
            ok(sysread($newsock_D, my ($data), 1024),
                q[Read handshake reply]);
            ($peers{q[C]}) = map {
                (    $_->{q[Object]}->isa(q[Net::BitTorrent::Peer])
                 and defined $_->{q[Object]}->peerid
                 and ($_->{q[Object]}->peerid eq q[C] x 20))
                    ? $_->{q[Object]}
                    : ()
            } values %{$client->_connections};
            weaken $peers{q[C]};
            ok(isweak($peers{q[C]}), q[  ...make $peers{q[C]} a weak ref]);
            warn sprintf q[%d|%d], (178 - 4), $test_builder->{q[Curr_Test]};

            # Status check
            like(${$peers{q[C]}}, qr[127.0.0.1:\d+],
                 q[Address properly resolved]);
            is($peers{q[C]}->peerid, q[C] x 20, q[PeerID check]);
            isa_ok($peers{q[C]}->_socket, q[GLOB], q[Socket stored properly]);
            warn sprintf q[%d|%d], (181 - 4), $test_builder->{q[Curr_Test]};
            is($peers{q[C]}->_am_choking, 1,
                q[Initial outgoing choke status]);
            is($peers{q[C]}->_peer_choking,
                1, q[Initial incoming choke status]);
            is($peers{q[C]}->_am_interested,
                0, q[Initial outgoing interest status]);
            is($peers{q[C]}->_peer_interested,
                0, q[Initial incoming interest status]);
            warn sprintf q[%d|%d], (185 - 4), $test_builder->{q[Curr_Test]};

            #
            is(syswrite($newsock_D, build_keepalive()),
                4, q[Sent keepalive to client]);
            $client->do_one_loop();
            is(syswrite($newsock_D, build_unchoke()),
                5, q[Sent unchoke to client]);
            $client->do_one_loop();
            warn sprintf q[%d|%d], (199 - 4), $test_builder->{q[Curr_Test]};
            is($peers{q[C]}->_am_choking,
                1, q[Post-unchoke outgoing choke status]);
            is($peers{q[C]}->_peer_choking,
                0, q[Post-unchoke incoming choke status]);
            is($peers{q[C]}->_am_interested,
                0, q[Post-unchoke outgoing interest status]);
            is($peers{q[C]}->_peer_interested,
                0, q[Post-unchoke incoming interest status]);
            warn sprintf q[%d|%d], (203 - 4), $test_builder->{q[Curr_Test]};
            is(syswrite($newsock_D, build_choke()),
                5, q[Sent choke to client]);
            $client->do_one_loop();
            is($peers{q[C]}->_am_choking,
                1, q[Post-choke outgoing choke status]);
            is($peers{q[C]}->_peer_choking,
                1, q[Post-choke incoming choke status]);
            is($peers{q[C]}->_am_interested,
                0, q[Post-choke outgoing interest status]);
            warn sprintf q[%d|%d], (215 - 4), $test_builder->{q[Curr_Test]};
            is($peers{q[C]}->_peer_interested,
                0, q[Post-choke incoming interest status]);
            is(syswrite($newsock_D, build_interested()),
                5, q[Sent interested to client]);
            $client->do_one_loop();
            is($peers{q[C]}->_am_choking,
                0, q[Post-interested outgoing choke status]);
            warn sprintf q[%d|%d], (228 - 4), $test_builder->{q[Curr_Test]};
            is($peers{q[C]}->_peer_choking,
                1, q[Post-interested incoming choke status]);
            is($peers{q[C]}->_am_interested,
                0, q[Post-interested outgoing interest status]);
            is($peers{q[C]}->_peer_interested,
                1, q[Post-interested incoming interest status]);
            warn sprintf q[%d|%d], (231 - 4), $test_builder->{q[Curr_Test]};
            is(syswrite($newsock_D, build_not_interested()),
                5, q[Sent not interested to client]);
            $client->do_one_loop();
            is(syswrite($newsock_D, build_have(0)), 9,
                q[Sent have to client]);
            $client->do_one_loop();
            ok(sysread($newsock_D, $data, 1024, length $data), q[Read]);
            warn sprintf q[%d|%d], (251 - 4), $test_builder->{q[Curr_Test]};

            #is(syswrite($newsock_D, build_have(1)), 9,
            #    q[Sent have to client]);
            #$client->do_one_loop();
            #ok(sysread($newsock_D, $data, 1024, length $data), q[Read]);
            is(syswrite($newsock_D, build_unchoke()),
                5, q[Sent unchoke to client]);
            $client->do_one_loop();
            ok(sysread($newsock_D, $data, 1024, length $data), q[Read]);
            my $fake_piece = q[A] x 16384;
            is(syswrite($newsock_D, build_piece(0, 0, \$fake_piece)),
                16397, q[Sent piece i:0 o:0 l:16384 to client]);
            warn sprintf q[%d|%d], (269 - 4), $test_builder->{q[Curr_Test]};
            $client->do_one_loop();
            is(syswrite($newsock_D, build_choke()),
                5, q[Sent choke to client]);
            $client->do_one_loop();
            is(syswrite($newsock_D, build_unchoke()),
                5, q[Sent choke to client to read second unchoke]);
            $client->do_one_loop();
            $client->do_one_loop();

            #ok(sysread($newsock_D, $data, 1024, length $data), q[Read]);
            warn sprintf q[%d|%d], (301 - 4), $test_builder->{q[Curr_Test]};

            #
            $flux_capacitor = 1.5;
            $client->do_one_loop();

            #ok(sysread($newsock_D, $data, 1024, length $data), q[Read]);
            warn sprintf q[%d|%d], (313 - 4), $test_builder->{q[Curr_Test]};

            #
            $flux_capacitor = 2.5;
            $client->do_one_loop;
            ok(sysread($newsock_D, $data, 1024, length $data), q[Read]);
            warn sprintf q[%d|%d], (319 - 4), $test_builder->{q[Curr_Test]};

#
#use Data::Dump;
#dd $data;
#
#is(sysread($newsock_C, my ($in), 1024), 0, q[Fail to read data because socket was closed.]);
        }

        #
        #$client->do_one_loop for 1 .. 50;
    }
    warn q[TODO: Test multithreaded stuff...];

    #SKIP: {
    #        skip q[Multi-threaded tests have been skipped], 4 if !$threads;
    #        use_ok(q[threads]);
    #        use_ok(q[threads::shared]);
    #
    #if ($threads::shared::threads_shared) {
    #       threads::shared::share($_bitfield{refaddr $self})
    #           if defined $_bitfield{refaddr $self};
    #       threads::shared::share($_am_choking{refaddr $self});
    #       threads::shared::share($_am_interested{refaddr $self});
    #       threads::shared::share($_peer_choking{refaddr $self});
    #       threads::shared::share($_peer_interested{refaddr $self});
    #   }
    #
    #    }
}
{    # Utility functions

    sub newsock {
        my ($server) = @_;
        my ($port, $packed_ip)
            = unpack_sockaddr_in(getsockname($server->_socket));
        warn(sprintf q[Creating new sockpair to connect to %s:%d],
             inet_ntoa($packed_ip), $port);
        my $outgoing;
        socket($outgoing, AF_INET, SOCK_STREAM, getprotobyname(q[tcp]))
            ? do {
            pass(q[Created new socket]);
            connect($outgoing, getsockname($server->_socket));
            }
            : fail(q[Failed to create new socket]);
        return $outgoing;
    }
}

# $Id$
