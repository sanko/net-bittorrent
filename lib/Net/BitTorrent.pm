package Net::BitTorrent;
{
    use 5.010;
    use Moose;
    use Moose::Util::TypeConstraints;
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 4; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
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

    #
    has 'ip_filter' => (is       => 'ro',
                        isa      => 'Net::BitTorrent::Network::IPFilter',
                        init_arg => undef
    );

    sub _build_ip_filter {
        require Net::BitTorrent::Network::IPFilter;
        Net::BitTorrent::Network::IPFilter->new();
    }

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
    has 'port' => (
        is      => 'ro',
        isa     => 'Int|ArrayRef[Int]',
        default => 0,
        writer  => '_set_port',

        #trigger => sub {...}
    );
    {
        my %_sock_types = (4 => '0.0.0.0', 6 => '::');
        for my $prot (qw[tcp udp]) {
            for my $ipv (keys %_sock_types) {
                has $prot
                    . $ipv => (is         => 'ro',
                               init_arg   => undef,
                               isa        => 'Maybe[Object]',
                               lazy_build => 1,
                               writer     => '_set_' . $prot . $ipv,
                               predicate  => '_has_' . $prot . $ipv
                    );
                has $prot 
                    . $ipv
                    . '_sock' => (
                                 is         => 'ro',
                                 init_arg   => undef,
                                 isa        => 'GlobRef',
                                 lazy_build => 1,
                                 weak_ref   => 1,
                                 writer => '_set_' . $prot . $ipv . '_sock',
                                 predicate => '_has_' . $prot . $ipv . '_sock'
                    );
                has $prot 
                    . $ipv
                    . '_host' => (
                                 is      => 'ro',
                                 isa     => 'Str',
                                 default => $_sock_types{$ipv},
                                 writer  => '_set_' . $prot . $ipv . '_host',
                                 predicate => '_has_' . $prot . $ipv . '_host'
                    );
            }
        }
    }
    after 'BUILD' => sub { $_[0]->$_() for qw[udp6 tcp6 udp4 tcp4] };

    sub _build_tcp6 {
        my $s = shift;
        my $server;
        for my $port (ref $s->port ? @{$s->port} : $s->port) {
            require Net::BitTorrent::Network::Utility;
            $server = Net::BitTorrent::Network::Utility::server(
                $s->tcp6_host,
                $port,
                sub { $s->_on_tcp6_in(@_); },
                sub {
                    my ($actual_socket, $actual_host, $actual_port) = @_;

                    #if ($self->port != $port) { ...; }
                    $s->_set_tcp6_sock($actual_socket);
                    $s->_set_tcp6_host($actual_host);
                    $s->_set_port($actual_port);
                },
                'tcp'
            );
            last if defined $server;
        }
        return $server if $server;
        $s->trigger_listen_failed(
            {protocol => 'tcp6',
             severity => 'fatal',
             event    => 'listen_failed',
             message => 'Failed to open IPv6 port to the outside world: ' . $!
            }
        );
        return;
    }

    sub _build_tcp4 {
        my $s = shift;
        my $server;
        for my $port (ref $s->port ? @{$s->port} : $s->port) {
            require Net::BitTorrent::Network::Utility;
            $server = Net::BitTorrent::Network::Utility::server(
                $s->tcp4_host,
                $port,
                sub { $s->_on_tcp4_in(@_); },
                sub {
                    my ($actual_socket, $actual_host, $actual_port) = @_;

                    #if ($self->port != $port) { ...; }
                    $s->_set_tcp4_sock($actual_socket);
                    $s->_set_tcp4_host($actual_host);
                    $s->_set_port($actual_port);
                },
                'tcp'
            );
            last if defined $server;
        }
        return $server if $server;
        $s->trigger_listen_failed(
            {protocol => 'tcp4',
             severity => 'fatal',
             event    => 'listen_failed',
             message => 'Failed to open IPv4 port to the outside world: ' . $!
            }
        );
        return;
    }

    sub _build_udp6 {
        my $s = shift;
        my $server;
        for my $port (ref $s->port ? @{$s->port} : $s->port) {
            require Net::BitTorrent::Network::Utility;
            $server = Net::BitTorrent::Network::Utility::server(
                $s->udp6_host,
                $port,
                sub { $s->_on_udp6_in(@_); },
                sub {
                    my ($actual_socket, $actual_host, $actual_port) = @_;

                    #if ($self->port != $port) { ...; }
                    $s->_set_udp6_sock($actual_socket);
                    $s->_set_udp6_host($actual_host);
                    $s->_set_port($actual_port);
                },
                'udp'
            );
            last if defined $server;
        }
        return $server if $server;
        $s->trigger_listen_failed(
            {protocol => 'udp6',
             severity => 'fatal',
             event    => 'listen_failed',
             message => 'Failed to open IPv6 port to the outside world: ' . $!
            }
        );
        return;
    }

    sub _build_udp4 {
        my $s = shift;
        my $server;
        for my $port (ref $s->port ? @{$s->port} : $s->port) {
            require Net::BitTorrent::Network::Utility;
            $server = Net::BitTorrent::Network::Utility::server(
                $s->udp4_host,
                $port,
                sub { $s->_on_udp4_in(@_); },
                sub {
                    my ($actual_socket, $actual_host, $actual_port) = @_;

                    #if ($self->port != $port) { ...; }
                    $s->_set_udp4_sock($actual_socket);
                    $s->_set_udp4_host($actual_host);
                    $s->_set_port($actual_port);
                },
                'udp'
            );
            last if defined $server;
        }
        return $server if $server;
        $s->trigger_listen_failed(
            {protocol => 'udp4',
             severity => 'fatal',
             event    => 'listen_failed',
             message => 'Failed to open IPv4 port to the outside world: ' . $!
            }
        );
        return;
    }

    sub _on_tcp4_in {
        my ($self, $peer, $paddr, $host, $port) = @_;
        require Net::BitTorrent::Peer;
        $self->add_peer(
                    Net::BitTorrent::Peer->new(fh => $peer, client => $self));
    }

    sub _on_tcp6_in {
        my ($self, $peer, $paddr, $host, $port) = @_;

        #use Data::Dump;
        #ddx \@_;
        #...;
    }

    sub _on_udp4_in {
        my $self = shift;
        my ($udp, $sock, $paddr, $host, $port, $data, $flags) = @_;
        $self->dht->_on_udp4_in(@_);
    }

    sub _on_upd6_in {
        my $self = shift;
        my ($udp, $sock, $paddr, $host, $port, $data, $flags) = @_;
        $self->dht->_on_udp6_in(@_);
    }

    #
    has '_peers' => (
        is      => 'HashRef[Net::BitTorrent::Peer]',    # by creation id
        is      => 'ro',
        traits  => ['Hash'],
        handles => {
               peer        => 'get',
               add_peer    => 'set',
               del_peer    => 'delete',
               peer_ids    => 'keys',
               has_peer    => 'defined',
               peers       => 'values',
               clear_peers => 'clear',                     # removes all peers
               count_peers => 'count',
               no_peers    => 'is_empty'
        },
        default => sub { {} }
    );
    around [qw[peer add_peer del_peer has_peer]] => sub {
        my ($code, $self, $arg) = @_;
        blessed $arg ? $code->($self, $arg->_id, $arg) : $code->($self, $arg);
    };
    if (0) {                                            # Callback system

        sub _build_callback_no_op {
            sub {1}
        }
        has "on_$_" => (isa        => 'CodeRef',
                        is         => 'rw',
                        traits     => ['Code'],
                        handles    => {"trigger_$_" => 'execute_method'},
                        lazy_build => 1,
                        builder    => '_build_callback_no_op',
                        weak_ref   => 1
            )
            for qw[
            peer_construction peer_destruction
        ];
    }
    {    # Callback System II
        for my $type (qw[peer_construction peer_destruction]) {
            has "_${type}_callbacks" => (
                                   isa      => 'ArrayRef[Ref]',
                                   is       => 'ro',
                                   init_arg => undef,
                                   traits   => ['Array'],
                                   handles  => {
                                       "add_${type}_callback"   => 'push',
                                       "${type}_callbacks"      => 'elements',
                                       "get_${type}_callback"   => 'get',
                                       "grep_${type}_callbacks" => 'grep',
                                       "map_${type}_callbacks"  => 'map',
                                       "trigger_${type}_callback" =>
                                           ['grep', sub { $_->[1]->() if $_ }]
                                   },
                                   default => sub { [] }
            );
            around "add_${type}_callback" => sub {
                my ($c, $s, $cb) = @_;
                require Scalar::Util;
                Scalar::Util::weaken $s;
                $cb = ['*', $cb, [], ()];
                $c->($s, $cb);
                Scalar::Util::weaken $s->{"_${type}_callbacks"}->[-1];
                return $cb;
            };

            #my $x =  qq[sub trigger_${type} { ... }];
            #warn $x;
            #eval $x;
        }

        #
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
}
1;

=pod

=head1 NAME

Net::BitTorrent - Wheeeeeeee!!!

=head1 Synopsis

  use Net::BitTorrent;
  use AnyEvent;

  my $c = Net::BitTorrent->new();

  # XXX - callback system is incomplete

  my $t = $c->add_torrent( path => 'a/legal.torrent' )
      || die 'Cannot load .torrent';

  $c->run; # Simple wrapper around AnyEvent->condvar->recv

=head1 Description

L<Net::BitTorrent|Net::BitTorrent> is a class based implementation of the
BitTorrent Protocol for distributed data exchange.

=for html <span style="color:#F00;font-weight:700">

=for text
******************************************************************************

Note that this distribution is being rewritten from scratch and will
eventually use L<Moose|Moose> and L<AnyEvent|AnyEvent>. If you see this
notice, I am probably trying out some new thing I'd like to have some CPAN
tester data on.

You are invited to test but please B<DO NOT ATTEMPT TO USE THIS IN PRODUCTION>
as everything from the API to basic internal functionality is subject to
change. For more, see this new
L<branch in the public repository|http://github.com/sanko/net-bittorrent/tree/summer-2010-moose-anyevent>.

All bugs and feature requests should be made on the issue tracker found at
L<github.com/sanko/net-bittorrent/issues|http://github.com/sanko/net-bittorrent/isues>
or via IRC at
L<irc://freenode.net/#net-bittorrent|irc://freenode.net/#net-bittorrent>.

=for text
******************************************************************************

=for html </span>

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
