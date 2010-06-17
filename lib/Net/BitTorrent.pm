package Net::BitTorrent;
{
    use 5.010;
    use Moose;
    use Moose::Util::TypeConstraints;
    our $MAJOR = 0.075; our $MINOR = 0; our $DEV = -1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use AnyEvent;
    use lib '../../lib';
    use Net::BitTorrent::Types qw[:client];
    sub BUILD {1}

    #
    sub timer { shift; AnyEvent->timer(@_) }
    sub run { AnyEvent->condvar->recv }

    #
    has 'peer_id' => (isa        => 'NBTypes::Client::PeerID',
                      is         => 'ro',
                      lazy_build => 1,
                      builder    => '_build_peer_id',
                      required   => 1
    );

    sub _build_peer_id {
        return pack(
            'a20',
            (sprintf(
                 'NB%03d%1s-%8s%5s',
                 $MAJOR * 1000,
                 ($DEV > 0 ? 'U' : 'S'),
                 (join '',
                  map {
                      ['A' .. 'Z', 'a' .. 'z', 0 .. 9, qw[- . _ ~]]
                      ->[rand(66)]
                      } 1 .. 8
                 ),
                 'KaiLi'
             )
            )
        );
    }

    #
    has 'torrents' => (traits  => ['Array'],
                       isa     => 'ArrayRef[Net::BitTorrent::Torrent]',
                       is      => 'ro',
                       reader  => '_torrents',
                       default => sub { [] },
                       coerce  => 1,
                       handles => {
                                add_torrent     => 'push',
                                clear_torrents  => 'clear',
                                count_torrents  => 'count',
                                filter_torrents => 'grep',
                                find_torrent    => 'first',
                                has_torrents    => 'count',
                                info_hashes => ['map', sub { $_->info_hash }],
                                map_torrents     => 'map',
                                no_torrents      => 'is_empty',
                                shuffle_torrents => 'shuffle',
                                sort_torrents    => 'sort',
                                torrent          => 'get',
                       }
    );
    around 'add_torrent' => sub {
        my ($code, $self) = (shift, shift);
        my $torrent;
        if (blessed $_[0]) { $torrent = $_[0]; }
        else {
            require Net::BitTorrent::Torrent;
            $torrent = Net::BitTorrent::Torrent->new(@_);
        }
        return
               blessed $torrent
            && $code->($self, $torrent)
            && $torrent->client($self);
    };
    my $infohash_constraint;
    around 'torrent' => sub {
        my ($code, $self, $index) = @_;
        my $torrent;
        {
            $infohash_constraint //=
                Moose::Util::TypeConstraints::find_type_constraint(
                                                'NBTypes::Torrent::Infohash');
            my $infohash = $infohash_constraint->coerce($index);
            $torrent = $self->find_torrent(
                sub {
                    $_->info_hash->Lexicompare($infohash) == 0;
                }
            );
        }
        $torrent = $code->($self, $index)
            if !defined $torrent && $index =~ m[^\d$];
        return $torrent;
    };

    # DHT (requires udp attribute)
    has 'dht' => (is         => 'ro',
                  isa        => 'Net::BitTorrent::DHT',
                  lazy_build => 1
    );

    sub _build_dht {
        require Net::BitTorrent::DHT;
        Net::BitTorrent::DHT->new(client => shift);
    }

    # Sockets
    has 'port' => (is      => 'ro',
                   lazy    => 1,
                   default => 0,
                   isa     => 'Int',
                   writer  => '_port'
    );
    has 'udp' => (init_arg   => undef,
                  is         => 'ro',
                  isa        => 'Net::BitTorrent::Network::UDP',
                  lazy_build => 1
    );

    sub _build_udp {
        my ($self) = @_;
        require Net::BitTorrent::Network::UDP;
        Net::BitTorrent::Network::UDP->new(
                        port            => $self->port,
                        ipv4_on_data_in => sub { $self->_ipv4_on_udp_in(@_) },
                        ipv6_on_data_in => sub { $self->_ipv6_on_udp_in(@_) }
        );
    }
    after 'BUILD' => sub { shift->udp };

    sub _ipv4_on_udp_in {
        my $self = shift;
        my ($udp, $sock, $paddr, $host, $port, $data, $flags) = @_;
        $self->dht->_ipv4_on_data_in(@_);
    }

    sub _ipv6_on_udp_in {
        my $self = shift;
        my ($udp, $sock, $paddr, $host, $port, $data, $flags) = @_;
        $self->dht->_ipv6_on_data_in(@_);
    }
    has 'tcp' => (init_arg   => undef,
                  is         => 'ro',
                  isa        => 'Net::BitTorrent::Network::TCP',
                  lazy_build => 1
    );

    sub _build_tcp {
        my ($self) = @_;
        require Net::BitTorrent::Network::TCP;
        Net::BitTorrent::Network::TCP->new(
                        port            => $self->port,
                        ipv4_on_data_in => sub { $self->_ipv4_on_tcp_in(@_) },
                        ipv6_on_data_in => sub { $self->_ipv6_on_tcp_in(@_) }
        );
    }
    after 'BUILD' => sub { shift->tcp };
    has 'handles' => (
        is      => 'HashRef[AnyEvent::Handle]',    # by creation id
        is      => 'ro',
        traits  => ['Hash'],
        handles => {handle        => 'get',
                    add_handle    => 'set',
                    del_handle    => 'delete',
                    has_handle    => 'defined',
                    clear_handles => 'clear',
                    count_handles => 'count',
                    no_handles    => 'is_empty'
        },
        default => sub { {} }
    );
    around [qw[handle del_handle has_handle]] => sub {
        my ($code, $self, $handle) = @_;
        blessed $handle
            ? $code->($self, $handle->{'hid'})
            : $code->($self, $handle);
    };
    around 'add_handle' => sub {
        my ($code, $self, $handle) = @_;
        $code->($self, $handle->{'hid'}, $handle);
    };
    sub hid { state $hid = 'a'; $hid++ }    # handleID generator

    sub _ipv4_on_tcp_in {
        my ($self, $tcp, $peer, $paddr, $host, $port) = @_;
        require AnyEvent::Handle::Throttle;
        my $handle = AnyEvent::Handle::Throttle->new(
            fh       => $peer,
            on_error => sub {
                warn "error $_[2]\n";
                ...;
                $_[0]->destroy;
            },
            on_eof => sub {
                $self->handle->destroy;    # destroy handle
                warn "done.\n";
                ...;
            },
            on_prepare => sub { $self->set_connecting; 15 },    # timeout
            on_connect =>
                sub { my ($handle, $host, $port, $retry) = @_; ... },
            on_connect_error =>
                sub { my ($handle, $message) = @_; die $message },
            on_error => sub {
                my ($handle, $fatal, $message) = @_;
                warn sprintf '%s%s', $fatal ? '[fatal] ' : '', $message;
            },
            on_read => sub {
                my ($handle) = @_;
                require Net::BitTorrent::Peer;
                my $new_peer = # NBPeer gets first shot
                    Net::BitTorrent::Peer->new(host   => $host,
                                               port   => $port,
                                               source => 'incoming',
                                               handle => $handle
                    );
                return 1 if $new_peer;
            },
            on_eof => sub { my ($handle) = @_; ... },
            on_drain => sub { my ($handle) = @_; },
            rtimeout => 60 * 5,
            wtimeout => 60 * 10,
            on_timeout => sub { my ($handle) = @_; ... },
            read_size  => 1024 * 16,
            hid        => $self->hid
        );
        return $self->add_handle($handle);
    }

    sub _ipv6_on_tcp_in {
        my ($self, $tcp, $peer, $paddr, $host, $port) = @_;

        #use Data::Dump;
        #ddx \@_;
        #...;
    }
}
1;

=pod

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
