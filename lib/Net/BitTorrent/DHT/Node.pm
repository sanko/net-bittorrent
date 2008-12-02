#!/usr/bin/perl -w
package Net::BitTorrent::DHT::Node;
{
    use strict;
    use warnings;
    use Socket qw[SOL_SOCKET /F_INET/ SOCK_DGRAM /sockaddr_in/ /inet_/];
    use Carp qw[carp];
    use Scalar::Util qw[blessed weaken refaddr];
    use lib q[../../../../lib/];
    use Net::BitTorrent::Util qw[:bencode :compact];
    use version qw[qv];
    our $SVN = q[$Id$];
    our $UNSTABLE_RELEASE = 0; our $VERSION = sprintf(($UNSTABLE_RELEASE ? q[%.3f_%03d] : q[%.3f]), (version->new((qw$Rev$)[1])->numify / 1000), $UNSTABLE_RELEASE);
    my (@CONTENTS)
        = \my (%dht,        %packed_host,   %node_id,
               %added,      %infohashes,    %_token,
               %_last_seen, %ping_schedule, %query_schedule
        );
    my %REGISTRY;

    sub new {
        my ($class, $args) = @_;
        my $self = undef;
        if (!$args || !ref($args) || !ref($args) eq q[HASH]) {
            carp __PACKAGE__
                . q[->new( { ... } ) requires ]
                . q[parameters a hash ref];
            return;
        }
        if (   !$args->{q[DHT]}
            or !blessed($args->{q[DHT]})
            or !$args->{q[DHT]}->isa(q[Net::BitTorrent::DHT]))
        {   carp __PACKAGE__
                . q[->new( { ... } ) requires a blessed parent DHT];
            return;
        }
        if (!$args->{q[PackedHost]}) {
            carp __PACKAGE__ . q[->new( { ... } ) requires a packed hostname];
            return;
        }
        $self = bless \$args->{q[NodeID]}, $class;
        $dht{refaddr $self} = $args->{q[DHT]};
        weaken $dht{refaddr $self};
        $packed_host{refaddr $self} = $args->{q[PackedHost]};
        $node_id{refaddr $self}
            = $args->{q[NodeID]} ? $args->{q[NodeID]} : undef;
        $infohashes{refaddr $self} = {};
        $added{refaddr $self}      = time;
        $_last_seen{refaddr $self} = time;    # lie
        $ping_schedule{refaddr $self}
            = $dht{refaddr $self}->_client->_schedule(
                                     {Code   => sub { shift->_query_ping() },
                                      Time   => time,
                                      Object => $self
                                     }
            );
        $query_schedule{refaddr $self}
            = $dht{refaddr $self}->_client->_schedule(
            {   Code => sub {
                    my $s = shift;
                    for my $torrent (
                             values %{$dht{refaddr $self}->_client->torrents})
                    {   if (not $torrent->private) {
                            $s->_query_get_peers($torrent);
                            $s->_query_announce_peer($torrent);
                        }
                    }
                    $s->_query_find_node(pack q[H40],
                                         $dht{refaddr $self}->node_id);
                },
                Time   => time + 1,
                Object => $self
            }
            );
        weaken($REGISTRY{refaddr $self} = $self);
        return $self;
    }
    sub _packed_host { return $packed_host{refaddr + $_[0]}; }

    sub _port {
        return if not $packed_host{refaddr + $_[0]};
        my ($_port, undef)
            = unpack_sockaddr_in($packed_host{refaddr + $_[0]});
        return $_port;
    }

    sub _host {
        return if not defined $packed_host{refaddr + $_[0]};
        my (undef, $addr) = unpack_sockaddr_in($packed_host{refaddr + $_[0]});
        return inet_ntoa($addr);
    }
    sub node_id     { return $node_id{refaddr + $_[0]}; }
    sub _last_seen  { return $_last_seen{refaddr + $_[0]}; }
    sub _infohashes { return [keys %{$infohashes{refaddr + $_[0]}}]; }

    sub _add_infohash {
        my ($self, $infohash) = @_;
        if ($infohash !~ m[^[a-h0-9]{40}$]i) {
            carp
                q[_add_infohash() doesn't know what to do with a bad infohash];
            return;
        }
        return $infohashes{refaddr $self}{lc($infohash)} = 1;
    }

    sub _query_ping {
        my ($self) = @_;
        $ping_schedule{refaddr $self}
            = $dht{refaddr $self}->_client->_schedule(
                                     {Code => sub { shift->_query_ping() },
                                      Time => (time + (60 * 15)),
                                      Object => $self
                                     }
            );

        #
        my $tid = $dht{refaddr $self}->_generate_token_id;
        return
            $dht{refaddr $self}->_send(
                    {Node => $self,
                     t    => $tid,
                     type => q[ping],
                     Packet =>
                         bencode(
                         {t => $tid,
                          y => q[q],
                          q => q[ping],
                          a => {id => $dht{refaddr $self}->node_id},
                          v => sprintf(q[NB:%s], $Net::BitTorrent::VERSION)
                         }
                         )
                    }
            );
    }

    sub _query_find_node {
        my ($self, $target) = @_;
        return if not $target;
        $query_schedule{refaddr $self}
            = $dht{refaddr $self}->_client->_schedule(
            {   Code => sub {
                    my $s = shift;
                    for my $torrent (
                             values %{$dht{refaddr $self}->_client->torrents})
                    {   if (not $torrent->private) {
                            $s->_query_get_peers($torrent);
                        }
                    }
                    $s->_query_find_node(pack q[H40],
                                         $dht{refaddr $self}->node_id);
                },
                Time => (time + (60 * 30)),
                Object => $self
            }
            );
        return
            if scalar(keys %{$dht{refaddr $self}->_routing_table}) >= 300;
        my $tid = $dht{refaddr $self}->_generate_token_id;
        $dht{refaddr $self}->_send(
                    {Node => $self,
                     t    => $tid,
                     type => q[find_node],
                     Packet =>
                         bencode(
                         {t => $tid,
                          y => q[q],
                          q => q[find_node],
                          a => {target => $target,
                                id     => $dht{refaddr $self}->node_id
                          },
                          v => sprintf(q[NB:%s], $Net::BitTorrent::VERSION)
                         }
                         )
                    }
        );
    }

    sub _query_get_peers {
        my ($self, $torrent) = @_;
        return if not defined $torrent;
        return if not $torrent->isa(q[Net::BitTorrent::Torrent]);
        return if $torrent->private;
        my $tid = $dht{refaddr $self}->_generate_token_id;
        $dht{refaddr $self}->_send(
                   {Node => $self,
                    t    => $tid,
                    type => q[get_peers],
                    Packet =>
                        bencode(
                        {t => $tid,
                         y => q[q],
                         q => q[get_peers],
                         a => {
                             info_hash => pack(q[H40], $torrent->infohash),
                             id => $dht{refaddr $self}->node_id
                         },
                         v => sprintf(q[NB:%s], $Net::BitTorrent::VERSION)
                        }
                        )
                   }
        );
    }

    sub _query_announce_peer {
        my ($self, $torrent) = @_;
        if (   (!$torrent)
            || (!blessed $torrent)
            || (!$torrent->isa(q[Net::BitTorrent::Torrent])))
        {   carp
                q[Net::BitTorrent::DHT::Node->_query_announce_peer() requires a Net::BitTorrent::Torrent];
            return;
        }
        if ($torrent->private) {
            warn q[...no announce on private torrents];
            return;
        }
        my $tid = $dht{refaddr $self}->_generate_token_id;
        $dht{refaddr $self}->_send(
                 {Node => $self,
                  t    => $tid,
                  type => q[announce_peer],
                  Packet =>
                      bencode(
                      {t => $tid,
                       y => q[q],
                       q => q[announce_peer],
                       a => {
                           info_hash => pack(q[H40], $torrent->infohash),
                           port  => $dht{refaddr $self}->_client->_tcp_port,
                           token => $_token{refaddr $self}
                       }
                      }
                      )
                 }
        );
        return 1;
    }

    sub _reply_ping {
        my ($self, $packet) = @_;
        return if not defined $packet;
        $_last_seen{refaddr $self} = time;
        $ping_schedule{refaddr $self}
            = $dht{refaddr $self}->_client->_schedule(
                                     {Code => sub { shift->_query_ping() },
                                      Time => (time + (60 * 15)),
                                      Object => $self
                                     }
            );
        $self->node_id($packet->{q[a]}{q[id]});
        $dht{refaddr $self}->_send(
                    {Node => $self,
                     Packet =>
                         bencode(
                         {t => $packet->{q[t]},
                          y => q[r],
                          r => {id => $node_id{refaddr $self}},
                          v => sprintf(q[NB:%s], $Net::BitTorrent::VERSION)
                         }
                         )
                    }
        );
    }

    sub _parse_reply_find_node {
        my ($self, $packet) = @_;
        return if not defined $packet;
        $_last_seen{refaddr $self} = time;
        $ping_schedule{refaddr $self}
            = $dht{refaddr $self}->_client->_schedule(
                                     {Code => sub { shift->_query_ping() },
                                      Time => (time + (60 * 15)),
                                      Object => $self
                                     }
            );

        # xxx - find node by packed_host and update seen, etc.
        for my $new_node (uncompact($packet->{q[r]}{q[values]})) {
            $dht{refaddr $self}->_add_node($new_node);
        }
        return 1;
    }

    sub _parse_reply_get_peers {
        my ($self, $packet) = @_;
        return if not defined $packet;
        $_last_seen{refaddr $self} = time;
        $_token{refaddr $self}     = $packet->{q[r]}{q[token]};
        $ping_schedule{refaddr $self}
            = $dht{refaddr $self}->_client->_schedule(
                                     {Code => sub { shift->_query_ping() },
                                      Time => (time + (60 * 15)),
                                      Object => $self
                                     }
            );
        if (defined $packet->{q[r]}{q[values]}) {
            my $torrent =
                $dht{refaddr $self}->_client->_locate_torrent(unpack q[H*],
                                                   $packet->{q[r]}{q[token]});
            if ($torrent) {
                for my $_node (@{$packet->{q[r]}{q[values]}}) {
                    $torrent->_append_compact_nodes($_node);
                }
            }
            else {

                #warn sprintf
                #    q[...torrent '%s' not found!?!?!?!?!?!?!?!?],
                #    unpack q[H*], $packet->{q[r]}{q[token]};
            }
            return 1;
        }
        elsif (defined $packet->{q[r]}{q[nodes]}) {

            # xxx - find node by packed_host and update seen, etc.
            for my $new_node (uncompact($packet->{q[r]}{q[values]})) {
                $dht{refaddr $self}->_add_node($new_node);
            }
        }
        return 0;
    }

    sub _parse_reply_announce_peer {
        my ($self, $packet) = @_;
        return if not defined $packet;
        $_last_seen{refaddr $self} = time;
        $node_id{refaddr $self} ||= $packet->{q[r]}{q[id]};
        return 0;
    }

    sub _parse_reply_ping {
        my ($self, $packet) = @_;
        $node_id{refaddr $self} ||= $packet->{q[r]}{q[id]};
        return 1;
    }

    sub _parse_query_get_peers {
        my ($self, $packet) = @_;
        return if not defined $packet;
        $_last_seen{refaddr $self} = time;
        $ping_schedule{refaddr $self}
            = $dht{refaddr $self}->_client->_schedule(
                                     {Code => sub { shift->_query_ping() },
                                      Time => (time + (60 * 15)),
                                      Object => $self
                                     }
            );
        my ($_port, $packed_ip)
            = unpack_sockaddr_in($packed_host{refaddr $self});
        my $target = unpack(q[H40], $packet->{q[a]}{q[info_hash]});
        my $torrent = $dht{refaddr $self}->_client->_locate_torrent($target);
        my @nodes;

        if (defined $torrent) {
            @nodes = map {m[.{6}]g} $torrent->_compact_nodes;
        }
        for my $_node (values %{$dht{refaddr $self}->_routing_table}) {
            push @nodes,
                compact(sprintf q[%s:%d], $_node->_host, $_node->_port)
                if grep { $_ eq $target } @{$_node->_infohashes};
        }
        if (scalar @nodes) {
            $dht{refaddr $self}->_send(
                     {Node => $self,
                      Packet =>
                          bencode(
                          {y => q[r],
                           t => $packet->{q[t]},
                           r => {id     => $dht{refaddr $self}->node_id,
                                 token  => $packet->{q[a]}{q[info_hash]},
                                 values => \@nodes
                           },
                           v => sprintf(q[NB%s], $Net::BitTorrent::VERSION)
                          }
                          )
                     }
            );
            return 1;
        }

        # XXX - check our routing table for nodes w/ this torrent
        $dht{refaddr $self}->_send(
                     {Node => $self,
                      Packet =>
                          bencode(
                          {y => q[r],
                           t => $packet->{q[t]},
                           r => {id    => $dht{refaddr $self}->node_id,
                                 token => $packet->{q[a]}{q[id]},
                                 nodes => $dht{refaddr $self}
                                     ->_locate_nodes_near_target($target)
                           },
                           v => sprintf(q[NB%s], $Net::BitTorrent::VERSION)
                          }
                          )
                     }
        );
        return 0;
    }

    sub _parse_query_find_node {
        my ($self, $packet) = @_;
        return if not defined $packet;
        $_last_seen{refaddr $self} = time;
        $ping_schedule{refaddr $self}
            = $dht{refaddr $self}->_client->_schedule(
                                     {Code => sub { shift->_query_ping() },
                                      Time => (time + (60 * 15)),
                                      Object => $self
                                     }
            );
        my $nodes   = q[];
        my $target  = unpack(q[H40], $packet->{q[a]}{q[target]});
        my $torrent = $dht{refaddr $self}->_client->_locate_torrent($target);
        if ($torrent) {
            $nodes = $torrent->_compact_nodes();
        }
        else {
            for my $_node (values %{$dht{refaddr $self}->_routing_table}) {
                $nodes .= $_node->_packed_host
                    if
                    grep { $_ eq unpack(q[H*], $packet->{q[a]}{q[target]}) }
                        @{$_node->_infohashes};
            }
        }
        if (not $nodes) { return; }
        $dht{refaddr $self}->_send(
                     {Node => $self,
                      t    => $packet->{q[t]},
                      Packet =>
                          bencode(
                          {a => {id    => $packet->{q[a]}{q[target]},
                                 nodes => $nodes
                           },
                           y => q[r],
                           t => $packet->{q[t]},
                           r => {id => $dht{refaddr $self}->node_id},
                           v => sprintf(q[NB%s], $Net::BitTorrent::VERSION)
                          }
                          )
                     }
        );
        return 1;
    }

    sub _parse_query_announce_peer {
        my ($self, $packet) = @_;
        return if not defined $packet;
        $_last_seen{refaddr $self} = time;
        $ping_schedule{refaddr $self}
            = $dht{refaddr $self}->_client->_schedule(
                                     {Code => sub { shift->_query_ping() },
                                      Time => (time + (60 * 15)),
                                      Object => $self
                                     }
            );
        $self->_add_infohash(unpack(q[H40], $packet->{q[a]}{q[info_hash]}));
        return
            $dht{refaddr $self}->_send(
                            {Node => $self,
                             Packet =>
                                 bencode(
                                 {y => q[r],
                                  t => $packet->{q[t]},
                                  r => {id => $dht{refaddr $self}->node_id}
                                 }
                                 )
                            }
            );
    }

    sub _parse_query_ping {
        my ($self, $packet) = @_;
        return if not defined $packet;
        $_last_seen{refaddr $self} = time;
        $ping_schedule{refaddr $self}
            = $dht{refaddr $self}->_client->_schedule(
                                     {Code => sub { shift->_query_ping() },
                                      Time => (time + (60 * 15)),
                                      Object => $self
                                     }
            );
        $dht{refaddr $self}->_send(
                            {Node => $self,
                             Packet =>
                                 bencode(
                                 {y => q[r],
                                  t => $packet->{q[t]},
                                  r => {id => $dht{refaddr $self}->node_id}
                                 }
                                 )
                            }
        );
        return 1;
    }

    sub _as_string {
        my ($self, $advanced) = @_;
        my $dump
            = !$advanced
            ? $node_id{refaddr $self} || q[Unknown]
            : sprintf <<'END',
Net::BitTorrent::DHT::Node

Node ID: %s
Address: %s:%d

Last Seen: %s
Infohashes: %d total
    %s
END
            ($node_id{refaddr $self} || q[Unknown]), $self->_host,
            $self->_port,
            scalar(localtime($_last_seen{refaddr $self})),
            scalar(keys %{$infohashes{refaddr $self}}),
            join(qq[\r\n    ], keys %{$infohashes{refaddr $self}});
        return defined wantarray ? $dump : print STDERR qq[$dump\n];
    }

    sub CLONE {
        for my $_oID (keys %REGISTRY) {
            my $_obj = $REGISTRY{$_oID};
            my $_nID = refaddr $_obj;
            for (@CONTENTS) {
                $_->{$_nID} = $_->{$_oID};
                delete $_->{$_oID};
            }
            weaken $dht{$_nID};
            weaken($REGISTRY{$_nID} = $_obj);
            delete $REGISTRY{$_oID};
        }
        return 1;
    }
    DESTROY {
        my ($self) = @_;
        for (@CONTENTS) { delete $_->{refaddr $self}; }
        return delete $REGISTRY{refaddr $self};
    }
}
1;

=pod

=head1 NAME

Net::BitTorrent::DHT::Node - Mainline (bencode) DHT Node

=head1 Constructor

=over 4

=item C<new ( [ARGS] )>

Creates a C<Net::BitTorrent::DHT::Node> object.  This
constructor should not be used directly.

=back

=head1 Methods

=over

=item C<node_id ( )>

Get the Node ID used to identify this
L<node|Net::BitTorrent::DHT::Node> in the DHT swarm and our routing
table.

=back

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
