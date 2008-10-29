#!C:\perl\bin\perl.exe 
package Net::BitTorrent::DHT::Node;
use strict;
use warnings;
{
    use Socket qw[SOL_SOCKET /F_INET/ SOCK_DGRAM SO_REUSEADDR
        /sockaddr_in/ /inet_/
    ];
    use Carp qw[carp];
    use Scalar::Util qw[blessed weaken refaddr];

    #
    use lib q[../../../../lib/];
    use Net::BitTorrent::Util qw[:log :bencode :compact];

    #
    use version qw[qv];    # core as of 5.009
    our $SVN = q[$Id$];
    our $UNSTABLE_RELEASE = 0; our $VERSION = sprintf(($UNSTABLE_RELEASE ? q[%.3f_%03d] : q[%.3f]), (version->new((qw$Rev$)[1])->numify / 1000), $UNSTABLE_RELEASE);
    {
        my (@CONTENTS)
            = \my (%dht,        %packed_host,   %node_id,
                   %added,      %infohashes,    %_token,
                   %_last_seen, %ping_schedule, %query_schedule
            );
        my %REGISTRY;

        sub new {
            my ($class, $args) = @_;
            my $self = undef;
            if (    defined $args->{q[dht]}
                and defined $args->{q[packed_host]})
            {   $self = bless \$args->{q[node_id]}, $class;
                $dht{refaddr $self} = $args->{q[dht]};
                weaken $dht{refaddr $self};
                $packed_host{refaddr $self} = $args->{q[packed_host]};
                $node_id{refaddr $self}     = $args->{q[node_id]};

                #if defined $args->{q[node_id]};
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
                            for my $session (
                                    values
                                    %{$dht{refaddr $self}->_client->sessions})
                            {   if (not $session->_private) {
                                    $s->_query_get_peers($session);
                                    $s->_query_announce_peer($session);
                                }
                            }
                            $s->_query_find_node(pack q[H40],
                                                $dht{refaddr $self}->node_id);
                        },
                        Time   => time + 1,
                        Object => $self
                    }
                    );
            }

            #
            weaken($REGISTRY{refaddr $self} = $self);

            #
            return $self;
        }

        sub _packed_host {
            return if defined $_[1];
            return $packed_host{refaddr + $_[0]};
        }

        sub _port {
            return if defined $_[1];
            return if not $packed_host{refaddr + $_[0]};
            my ($_port, undef)
                = unpack_sockaddr_in($packed_host{refaddr + $_[0]});
            return $_port;
        }

        sub _host {
            return if defined $_[1];
            return if not defined $packed_host{refaddr + $_[0]};
            my (undef, $addr)
                = unpack_sockaddr_in($packed_host{refaddr + $_[0]});
            return inet_ntoa($addr);
        }

        sub node_id {
            return if defined $_[1];
            return $node_id{refaddr + $_[0]};
        }

        sub _last_seen {
            return if defined $_[1];
            return $_last_seen{refaddr + $_[0]};
        }

        sub _infohashes {
            return if defined $_[1];
            return [keys %{$infohashes{refaddr + $_[0]}}];
        }

        sub _add_infohash {
            return if not defined $_[1];
            return $infohashes{refaddr + $_[0]}{$_[1]}++;
        }

        sub _query_ping {
            my ($self) = @_;

        #
        # $dht{refaddr $self}->_client->_cancel($ping_schedule{refaddr $self})
        #    if defined $ping_schedule{refaddr $self};
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
                    {node => $self,
                     t    => $tid,
                     type => q[ping],
                     packet =>
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
                        for my $session (
                             values %{$dht{refaddr $self}->_client->sessions})
                        {   if (not $session->_private) {
                                $s->_query_get_peers($session);
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
                    {node => $self,
                     t    => $tid,
                     type => q[find_node],
                     packet =>
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
            my ($self, $session) = @_;
            return if not defined $session;
            return if not $session->isa(q[Net::BitTorrent::Session]);
            return if $session->_private;
            my $tid = $dht{refaddr $self}->_generate_token_id;
            $dht{refaddr $self}->_send(
                   {node => $self,
                    t    => $tid,
                    type => q[get_peers],
                    packet =>
                        bencode(
                        {t => $tid,
                         y => q[q],
                         q => q[get_peers],
                         a => {
                             info_hash => pack(q[H40], $session->infohash),
                             id => $dht{refaddr $self}->node_id
                         },
                         v => sprintf(q[NB:%s], $Net::BitTorrent::VERSION)
                        }
                        )
                   }
            );
        }

        sub _query_announce_peer {
            my ($self, $session) = @_;
            if (not defined $session) {
                carp
                    q[Net::BitTorrent::DHT::Node->_query_announce_peer() requires a Net::BitTorrent::Session];
                return;
            }
            if (not blessed $session) {
                carp
                    q[Net::BitTorrent::DHT::Node->_query_announce_peer() requires a Net::BitTorrent::Session];
                return;
            }
            if (not $session->isa(q[Net::BitTorrent::Session])) {
                carp
                    q[Net::BitTorrent::DHT::Node->_query_announce_peer() requires a Net::BitTorrent::Session];
                return;
            }
            if ($session->_private) {
                warn q[...no announce on private torrents];
                return;
            }

            #
            my $tid = $dht{refaddr $self}->_generate_token_id;
            $dht{refaddr $self}->_send(
                   {node => $self,
                    t    => $tid,
                    type => q[announce_peer],
                    packet =>
                        bencode(
                        {t => $tid,
                         y => q[q],
                         q => q[announce_peer],
                         a => {
                             info_hash => pack(q[H40], $session->infohash),
                             port  => $dht{refaddr $self}->_client->_port,
                             token => $_token{refaddr $self}
                         }
                        }
                        )
                   }
            );

            #
            return 1;
        }

        sub _reply_ping {
            my ($self, $packet) = @_;
            return if not defined $packet;
            $_last_seen{refaddr $self} = time;

       # $dht{refaddr $self}->_client->_cancel($ping_schedule{refaddr $self});
            $ping_schedule{refaddr $self}
                = $dht{refaddr $self}->_client->_schedule(
                                     {Code => sub { shift->_query_ping() },
                                      Time => (time + (60 * 15)),
                                      Object => $self
                                     }
                );
            $self->node_id($packet->{q[a]}{q[id]});

            #
            $dht{refaddr $self}->_send(
                    {node => $self,
                     packet =>
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

       # $dht{refaddr $self}->_client->_cancel($ping_schedule{refaddr $self});
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

            #
            return 1;
        }

        sub _parse_reply_get_peers {
            my ($self, $packet) = @_;
            return if not defined $packet;
            $_last_seen{refaddr $self} = time;
            $_token{refaddr $self}     = $packet->{q[r]}{q[token]};

       # $dht{refaddr $self}->_client->_cancel($ping_schedule{refaddr $self});
            $ping_schedule{refaddr $self}
                = $dht{refaddr $self}->_client->_schedule(
                                     {Code => sub { shift->_query_ping() },
                                      Time => (time + (60 * 15)),
                                      Object => $self
                                     }
                );
            if (defined $packet->{q[r]}{q[values]}) {
                my $session =
                    $dht{refaddr $self}
                    ->_client->_locate_session(unpack q[H*],
                                               $packet->{q[r]}{q[token]});
                if ($session) {
                    for my $_node (@{$packet->{q[r]}{q[values]}}) {
                        $session->_append_compact_nodes($_node);
                    }

 #warn join q[, ], Net::BitTorrent::Util::uncompact($session->_compact_nodes);
                }
                else {
                    warn sprintf
                        q[...session '%s' not found!?!?!?!?!?!?!?!?],
                        unpack q[H*], $packet->{q[r]}{q[token]};
                }
                return 1;
            }
            elsif (defined $packet->{q[r]}{q[nodes]}) {

                # ...okay, they're not serving this torrent but should we ask
                # them for peers?
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

            #
            $node_id{refaddr $self}
                = $packet->{q[r]}{q[id]};    # XXX - may already be defined...

            #
            return 0;
        }

        sub _parse_reply_ping {
            my ($self, $packet) = @_;

            #
            $node_id{refaddr $self}
                = $packet->{q[r]}{q[id]};    # XXX - may already be defined...

            #
            return 1;
        }

        sub _parse_query_get_peers {
            my ($self, $packet) = @_;
            return if not defined $packet;
            $_last_seen{refaddr $self} = time;

       # $dht{refaddr $self}->_client->_cancel($ping_schedule{refaddr $self});
            $ping_schedule{refaddr $self}
                = $dht{refaddr $self}->_client->_schedule(
                                     {Code => sub { shift->_query_ping() },
                                      Time => (time + (60 * 15)),
                                      Object => $self
                                     }
                );
            my ($_port, $packed_ip)
                = unpack_sockaddr_in($packed_host{refaddr $self});

            #use Data::Dump qw[pp];
            #warn sprintf q[%s:%d says get_peers! %s], inet_ntoa($packed_ip),
            #    $_port,
            #    pp $packet;
            my $target = unpack(q[H40], $packet->{q[a]}{q[info_hash]});
            my $session
                = $dht{refaddr $self}->_client->_locate_session($target);

            #warn q[$target  == ] . $target;
            #warn q[$session == ] . $session;
            my @nodes;
            if (defined $session) {
                @nodes = map {m[.{6}]g} $session->_compact_nodes;
            }
            for my $_node (values %{$dht{refaddr $self}->_routing_table}) {
                push @nodes,
                    compact(sprintf q[%s:%d], $_node->_host, $_node->_port)
                    if grep { $_ eq $target } @{$_node->_infohashes};
            }
            if (scalar @nodes) {
                $dht{refaddr $self}->_send(
                     {  node => $self,
                        packet =>
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
                     {node => $self,
                      packet =>
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

            #
            return 0;
        }

        sub _parse_query_find_node {
            my ($self, $packet) = @_;
            return if not defined $packet;
            $_last_seen{refaddr $self} = time;

       # $dht{refaddr $self}->_client->_cancel($ping_schedule{refaddr $self});
            $ping_schedule{refaddr $self}
                = $dht{refaddr $self}->_client->_schedule(
                                     {Code => sub { shift->_query_ping() },
                                      Time => (time + (60 * 15)),
                                      Object => $self
                                     }
                );

            #
            my $nodes = q[];
            my $target = unpack(q[H40], $packet->{q[a]}{q[target]});
            my $session
                = $dht{refaddr $self}->_client->_locate_session($target);
            if ($session) {
                $nodes = $session->_compact_nodes();
            }
            else {
                for my $_node (values %{$dht{refaddr $self}->_routing_table})
                {   $nodes .= $_node->_packed_host
                        if grep {
                        $_ eq unpack(q[H*], $packet->{q[a]}{q[target]})
                        } @{$_node->_infohashes};
                }
            }
            if (not $nodes) { return; }

            #
            $dht{refaddr $self}->_send(
                     {node => $self,
                      t    => $packet->{q[t]},
                      packet =>
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

            #
            return 1;
        }

        sub _parse_query_announce_peer {
            my ($self, $packet) = @_;
            return if not defined $packet;
            $_last_seen{refaddr $self} = time;

       # $dht{refaddr $self}->_client->_cancel($ping_schedule{refaddr $self});
            $ping_schedule{refaddr $self}
                = $dht{refaddr $self}->_client->_schedule(
                                     {Code => sub { shift->_query_ping() },
                                      Time => (time + (60 * 15)),
                                      Object => $self
                                     }
                );
            $self->_add_infohash(
                               unpack(q[H40], $packet->{q[a]}{q[info_hash]}));
            return
                $dht{refaddr $self}->_send(
                            {node => $self,
                             packet =>
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

       # $dht{refaddr $self}->_client->_cancel($ping_schedule{refaddr $self});
            $ping_schedule{refaddr $self}
                = $dht{refaddr $self}->_client->_schedule(
                                     {Code => sub { shift->_query_ping() },
                                      Time => (time + (60 * 15)),
                                      Object => $self
                                     }
                );

            #
            $dht{refaddr $self}->_send(
                            {node => $self,
                             packet =>
                                 bencode(
                                 {y => q[r],
                                  t => $packet->{q[t]},
                                  r => {id => $dht{refaddr $self}->node_id}
                                 }
                                 )
                            }
            );

            #
            return 1;
        }

        sub _as_string {
            my ($self, $advanced) = @_;
            my $dump = q[TODO];
            return print STDERR qq[$dump\n] unless defined wantarray;
            return $dump;
        }

        sub CLONE {
            for my $_oID (keys %REGISTRY) {

                #  look under oID to find new, cloned reference
                my $_obj = $REGISTRY{$_oID};
                my $_nID = refaddr $_obj;

                #  relocate data
                for (@CONTENTS) {
                    $_->{$_nID} = $_->{$_oID};
                    delete $_->{$_oID};
                }

                # do some silly stuff to avoid user mistakes
                #weaken($_client{$_nID} = $_client{$_oID});
                weaken $dht{$_nID};

                #  update he weak refernce to the new, cloned object
                weaken($REGISTRY{$_nID} = $_obj);
                delete $REGISTRY{$_oID};
            }
            return 1;
        }

        # Destructor
        DESTROY {
            my ($self) = @_;

            #warn q[Goodbye, ] . $$self;
            # Clean all data
            for (@CONTENTS) {
                delete $_->{refaddr $self};
            }
            delete $REGISTRY{refaddr $self};

            #
            return 1;
        }
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
L<node|/Net::BitTorrent::DHT::Node> in the DHT swarm and our routing
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
