package t::10000_by_class::Net::BitTorrent::Protocol::BEP03::Peer::Incoming;
{
    use strict;
    use warnings;
    use 5.012;
    use AnyEvent;
    use AnyEvent::Socket qw[tcp_connect];
    use AnyEvent::Handle;
    use Test::Most;
    use lib '../', '../../../../../../../', '../../../../../../../lib', 'lib';
    use Net::BitTorrent;
    use Net::BitTorrent::Torrent;
    use Net::BitTorrent::Protocol::BEP03::Packets qw[:all];
    use parent 'Test::Class';
    $|++;

    # Basic utility functions/methods
    sub class {'Net::BitTorrent::Protocol::BEP03::Peer::Incoming'}

    sub new_args {
        my $s = shift;
        -f $s->torrent ? last : chdir '..' for 0 .. 15;
        $s->{'client'} //= Net::BitTorrent->new(
            on_peer_connect => sub {
                my (undef, $args) = @_;
                subtest 'on_peer_connect', sub {
                    plan tests => 3;
                    $s->{'peer'} = $args->{'peer'};
                    is $args->{'peer'}->host, '127.0.0.1', 'local peer...';
                    isa_ok $s->{'peer'}, 'Net::BitTorrent::Peer';
                    isa_ok $s->{'peer'}, $s->class;
                };
            }
        );
        $s->{'torrent'} //=
            Net::BitTorrent::Torrent->new(path => $s->torrent);
        $s->{'client'}->add_torrent($s->{'torrent'});
        $s->{'client'}->port;
    }

    sub startup : Test( startup => 0 ) {
        my $s = shift;
        $s->{'cv'}->begin;

        #$s->{'handle'} = AnyEvent::Handle->new(
        #        connect    => $s->new_args(),
        #on_connect => sub {use Data::Dump; ddx \@_; die;},
        #on_read => sub {
        #    my $expected = $s->__expect;
        #    defined $expected
        #        ? subtest $expected, sub {
        #        $s->{'cv'}->begin;
        #        $s->__dispatch($expected)->($s);
        #        $s->{'cv'}->end;
        #        }
        #        : explain 'No idea what to do with this packet: ',
        #        $s->{'handle'}->rbuf;
        #},
        #on_write => sub {...}
        #    );
        #ok $s->{'handle'}, 'handle created opened';
    }

    sub __expect {
        my ($s, $k) = @_;
        state $expect;
        $expect //= [qw[handshake interested]];
        return wantarray ? @$expect : shift @$expect;
    }
    sub _send_handshake {return}

    sub _9000_open_socket : Test( startup => 0 ) {
        my $s = shift;
        $s->{'cv'}->begin;
        $s->{'socket'} = tcp_connect '127.0.0.1', $s->new_args(), sub {
            my ($fh, $host, $port) = @_;
            $s->{'handle'}->push_write(
                   build_handshake($s->reserved, $s->info_hash, $s->peer_id));
            $s->{'handle'}->push_read(
                sub {
                    AnyEvent->one_event for 1 .. 5;    # at least 3
                    subtest 'pre handshake', sub {
                        plan tests => 4;
                        ok $s->{'peer'}->_has_torrent,
                            '...->torrent is defined';
                        is $s->{'peer'}->torrent->info_hash->to_Hex,
                            $s->info_hash,
                            '...->torrent->info_hash->to_Hex is correct';
                        is $s->{'peer'}->peer_id, $s->peer_id,
                            '...->peer_id is correct';
                        ok !$s->{'peer'}->_has_pieces,
                            'initial value for ...->pieces in unset until we get a bitfield/fast peer packet/have/etc.';
                    };
                    1;
                }
            );
            }, sub {
            my ($fh) = @_;
            if (!defined $fh) {
                note 'connect failed: ' . $!;
                return $s->{'cv'}->send;
            }
            $s->{'fh'} = $fh;
            $s->{'handle'} = AnyEvent::Handle->new(
                fh       => $s->{'fh'},
                on_drain => sub { note 'drain' },
                on_read  => sub {
                    my $expected = $s->__expect;
                    defined $expected
                        ? subtest $expected, sub {
                        $s->{'cv'}->begin;
                        $s->__dispatch($expected)->($s);
                        $s->{'cv'}->end;
                        }
                        : explain 'No idea what to do with this packet: ',
                        $s->{'handle'}->rbuf;
                }
            );
            }
    }

    # Handshake data
    sub reserved  { "\0" x 8 }
    sub torrent   {'t/90000_data/95000_torrents/95003_miniswarm.torrent'}
    sub info_hash {'2B3AAF361BD40540BF7E3BFD140B954B90E4DFBC'}
    sub peer_id   {'This ain\'t a peer_id'}

    # Callbacks
    sub on_peer_disconnect {
        my ($s, $a) = @_;
        use Data::Dump;
        ddx $a;
        is $a->{'peer'}->handshake, 0, 'disconnect mid-handshake';

        # Regression test
        my $match
            = '127\.0\.0\.1:\d+ \('
            . substr(peer_id(), 0, 20)
            . '\) disconnect: Bad info_hash \(We are not serving '
            . sprintf(info_hash, 0, 40) . '\)';
        like $a->{'message'}, qr[^$match],
            'peer disconnected (unknown torrent)';
    }

    # Basic utility functions/methods
    # AnyEvent
    sub _00000_init : Test( startup ) {
        my $s = shift;
        note 'Adding condvar for later use...';
        $s->{'cv'} = AE::cv();
        $s->{'cv'}->begin(sub { $s->{'cv'}->send });
        note '...which will timeout in 30s';
        $s->{'to'} = AE::timer(
            30, 0,
            sub {
                note 'Timeout while expecting ', join ', ', $s->__expect;
                $s->{'cv'}->send;
            }
        );
    }

    sub wait : Test( shutdown => no_plan ) {
        my $s = shift;
        $s->{'cv'}->end;
        $s->{'cv'}->recv;
    }

    # Setup/teardown
    sub setup : Test( setup ) {
        my $s = shift;
    }

    sub shutdown : Test( shutdown ) {
    }

    sub __dispatch {
        my ($s, $k) = @_;
        state $dispatch;
        $dispatch //= {
            handshake => sub {
                plan tests => 10;
                my $s = shift;
                ok $s->{'handle'}->rbuf, 'read handshake packet';
                ok length $s->{'handle'}->rbuf >= 68,
                    'handshake was >= 68 bytes';
                my $p = parse_packet(\$s->{'handle'}->rbuf);
                is ref $p, 'HASH', 'packet parses to hashref';
                is $p->{'type'},           -1, 'fake handshake type';
                is $p->{'packet_length'},  68, 'parsed packet was 68 bytes';
                is $p->{'payload_length'}, 48, 'parsed payload was 48 bytes';
                is scalar @{$p->{'payload'}}, 3,
                    'parsed payload has 3 elements';
                is length $p->{'payload'}[0], 8, 'reserved is eight bytes';
                like $p->{'payload'}[1], qr[^[A-F\d]{40}$]i, 'info_hash';
                like $p->{'payload'}[2], qr[^NB\d\d\d[SU]-.{13}+$], 'peer_id';

                # Next step
                $s->{'handle'}->push_write(build_bitfield(pack 'B*', '10'));
            },
            bitfield => sub {
                plan tests => 2;
                my $s = shift;
                is length $s->{'handle'}->rbuf, 5, 'read 5 bytes from peer';
                is_deeply parse_packet(\$s->{'handle'}->rbuf),
                    {packet_length  => 6,
                     payload        => "\0",
                     payload_length => 1,
                     type           => 5
                    },
                    'bitfield is correct';

                # Next step
                $s->{'handle'}->push_read(
                    sub {
                        AnyEvent->one_event for 1 .. 10;
                        subtest 'post bitfield', sub {
                            plan tests => 2;
                            is $s->{'peer'}->pieces->to_Enum, '0',
                                'new value for ...->pieces->to_Enum is correct';
                            ok $s->{'peer'}->interesting,
                                'peer is now interested in us';
                            $s->{'cv'}->send;
                        };
                        1;
                    }
                );
            },
            interested => sub {
                plan tests => 2;
                my $s = shift;
                is length $s->{'handle'}->rbuf, 5, 'read 5 bytes from peer';
                is_deeply parse_packet(\$s->{'handle'}->rbuf),
                    {packet_length  => 5,
                     payload_length => 0,
                     type           => 2
                    },
                    'interested packet is correct';

                # Next step
                $s->{'handle'}->push_write(build_unchoke());
                $s->{'handle'}->push_read(
                    sub {
                        AnyEvent->one_event for 1 .. 10;
                        subtest 'post unchoke', sub {
                            plan tests => 1;
                            is $s->{'peer'}->remote_choked, 0,
                                'peer is now unchoked by us';
                            $s->{'cv'}->send;
                        };
                        1;
                    }
                );
                }
        };
        $dispatch->{$k} // sub {...}
    }

    #
    #$ENV{'TEST_VERBOSE'}++;
    __PACKAGE__->runtests() if !caller;
}
1
