#!/usr/bin/perl -w
package Net::BitTorrent::DHT;
{
    use strict;
    use warnings;
    use Digest::SHA qw[sha1];
    use Scalar::Util qw[blessed weaken refaddr];
    use Carp qw[carp];
    use Socket qw[inet_aton pack_sockaddr_in];
    use lib q[../../../lib/];
    use Net::BitTorrent::Util qw[:bencode :compact];
    use Net::BitTorrent::DHT::Node;
    use Net::BitTorrent::Version;
    use version qw[qv];
    our $SVN = q[$Id$];
    our $UNSTABLE_RELEASE = 1; our $VERSION = sprintf(($UNSTABLE_RELEASE ? q[%.3f_%03d] : q[%.3f]), (version->new((qw$Rev$)[1])->numify / 1000), $UNSTABLE_RELEASE);
    my @CONTENTS =
        \my (%_client, %tid, %outstanding_queries, %node_id, %routing_table,
             %nodes, %extra);
    my %REGISTRY;

    sub new {
        my ($class, $args) = @_;
        my $self = undef;
        if (!$args or (ref($args) ne q[HASH])) {
            carp __PACKAGE__
                . q[->new( { ... } ) requires parameters passed as a hashref];
            return;
        }
        if (   !$args->{q[Client]}
            or !blessed($args->{q[Client]})
            or !$args->{q[Client]}->isa(q[Net::BitTorrent]))
        {   carp __PACKAGE__
                . q[->new( { ... } ) requires parameters passed as a hashref];
            return;
        }
        my $node_id = Net::BitTorrent::Version->gen_node_id();
        $self = bless \$node_id, $class;
        $_client{refaddr $self} = $args->{q[Client]};
        weaken $_client{refaddr $self};
        $node_id{refaddr $self}       = $node_id;
        $routing_table{refaddr $self} = {};
        $nodes{refaddr $self}         = q[];
        $tid{refaddr $self}           = qq[\0] x 5;
        $_client{refaddr $self}->_schedule(
                                          {Code   => sub { shift->_pulse() },
                                           Time   => time,
                                           Object => $self
                                          }
        );
        weaken($REGISTRY{refaddr $self} = $self);
        return $self;
    }

    # Accessors | Private
    sub _client {
        return if defined $_[1];
        return $_client{refaddr + $_[0]};
    }

    sub _routing_table {
        return if defined $_[1];
        return $routing_table{refaddr + $_[0]};
    }

    sub _queue_outgoing {
        return if defined $_[1];
        return;
    }

    sub _compact_nodes {
        return if defined $_[1];
        return $nodes{refaddr + $_[0]};
    }

    # Accesors | Public
    sub node_id {
        return if defined $_[1];
        return $node_id{refaddr + $_[0]};
    }

    # Setters | Private
    sub _set_node_id {
        return if not defined $_[1];
        return $node_id{refaddr + $_[0]} = $_[1];
    }

    sub _append_compact_nodes {
        my ($self, $nodes) = @_;
        return if not defined $_client{refaddr $self};
        if (not $nodes) { return; }
        $nodes{refaddr $self} ||= q[];
        return $nodes{refaddr $self}
            = compact(uncompact($nodes{refaddr $self} . $nodes));
    }

    # Methods | Private
    sub _pulse {
        my ($self) = @_;
        {
            my @expired_requests = grep {
                $outstanding_queries{refaddr $self}{$_}->{timestamp}
                    < (time - (60 * 5))
            } keys %{$outstanding_queries{refaddr $self}};
            while (my $packet = shift @expired_requests) {
                delete $outstanding_queries{refaddr $self}{$packet};
            }
        }
        {
            my @expired_nodes = grep {
                my $timestamp = $routing_table{refaddr $self}{$_}->_last_seen;
                ($timestamp < (time - (60 * 30)))
                    or ($timestamp < (time - (60 * 5))
                     and
                     (not defined $routing_table{refaddr $self}{$_}->node_id))
            } keys %{$routing_table{refaddr $self}};
            for my $node (@expired_nodes) {
                $nodes{refaddr $self} .=
                    compact(
                          sprintf(q[%s:%s],
                                  $routing_table{refaddr $self}{$node}->_host,
                                  $routing_table{refaddr $self}{$node}->_port)
                    );
                delete $routing_table{refaddr $self}{$node};
            }
        }
        my @hosts = sort keys %{$routing_table{refaddr $self}};
    NODE: for my $packed_host (@hosts) {
            my $node = $routing_table{refaddr $self}{$packed_host};
            if (    (not defined $node->node_id)
                and ($node->_last_seen < (time - (60 * 5))))
            {   delete $routing_table{refaddr $self}{$packed_host};
                next NODE;
            }
        }
        $_client{refaddr $self}->_schedule({Code => sub { shift->_pulse },
                                            Time   => time + 30,
                                            Object => $self
                                           }
        );
        return;
    }

    sub _locate_nodes_near_target {
        my ($self, $target) = @_;
        return if not defined $target;
        return [sort { ($a->node_id ^ $target) cmp($b->node_id ^ $target) }
                    grep { defined $_->node_id }
                    values %{$routing_table{refaddr $self}}
        ]->[0 .. 8];
    }

    sub _send {
        my ($self, $args) = @_;
        return if not $_client{refaddr $self}->_use_dht();
        if (!$args) {
            carp q[Net::BitTorrent::DHT->_send() requires parameters];
            return;
        }
        if (!$args->{q[Node]}) {
            carp q[Net::BitTorrent::DHT->_send() requires a 'node' parameter];
            return;
        }
        if (   $args->{q[Node]}->node_id
            && $args->{q[Node]}->node_id eq $node_id{refaddr $self})
        {   return;
        }
        if (send($_client{refaddr $self}->_udp(),
                 $args->{q[Packet]},
                 0,
                 $args->{q[Node]}->_packed_host
            ) == length($args->{q[Packet]})
            )
        {   if (defined $args->{q[t]} and defined $args->{q[type]}) {
                $outstanding_queries{refaddr $self}{$args->{q[t]}} = {
                                           size => length($args->{q[Packet]}),
                                           timestamp => time,
                                           node      => $args->{q[Node]},
                                           type      => $args->{q[type]},
                                           args      => $args
                };
                weaken $outstanding_queries{refaddr $self}{$args->{q[t]}}
                    {q[args]}{q[Node]};
            }
            return 1;
        }
        carp sprintf q[Cannot send %d bytes to %s: [%d] %s],
            length($args->{q[Packet]}), $args->{q[Node]}->node_id,
            $^E, $^E;
        return;
    }

    sub _on_data {
        my ($self, $paddr, $data) = @_;
        return if not $_client{refaddr $self}->_use_dht();
        my $node;
        my ($packet, $leftover) = bdecode($data);
        if ((defined $packet) and (ref $packet eq q[HASH])) {
            $routing_table{refaddr $self}{$paddr} ||=
                Net::BitTorrent::DHT::Node->new({DHT        => $self,
                                                 PackedHost => $paddr,
                                                 NodeID     => undef
                                                }
                );
            $node = $routing_table{refaddr $self}{$paddr};
            if (defined $packet->{q[y]}
                and $packet->{q[y]} eq q[q])
            {   my %dispatch = (
                    announce_peer => sub {
                        shift->_parse_query_announce_peer(shift);
                    },
                    get_peers => sub {
                        shift->_parse_query_get_peers(shift);
                    },
                    find_node => sub {
                        shift->_parse_query_find_node(shift);
                    },
                    ping => sub {
                        shift->_parse_query_ping(shift);
                    }
                );
                if (    defined $packet->{q[q]}
                    and defined $dispatch{$packet->{q[q]}})
                {   $dispatch{$packet->{q[q]}}($node, $packet);
                }
                elsif (eval q[require Data::Dump]) {
                    carp q[Unhandled DHT packet: ] . Data::Dump::pp($packet);
                }
            }
            elsif (defined $packet->{q[y]}
                   and $packet->{q[y]} eq q[r])
            {   my %dispatch = (
                    announce_peer => sub {
                        shift->_parse_reply_announce_peer(shift);
                    },
                    get_peers => sub {
                        shift->_parse_reply_get_peers(shift);
                    },
                    ping => sub {
                        shift->_parse_reply_ping(shift);
                    },
                    find_node => sub {
                        shift->_parse_reply_find_node(shift);
                    }
                );
                if (defined $packet->{q[r]}) {
                    if (defined $outstanding_queries{refaddr $self}
                        {$packet->{q[t]}})
                    {   my $type = $outstanding_queries{refaddr $self}
                            {$packet->{q[t]}}{q[type]};
                        if (defined $dispatch{$type}) {
                            $dispatch{$type}($node, $packet);
                        }
                        elsif (eval q[require Data::Dump]) {
                            carp sprintf
                                <<'END',
Unhandled DHT Reply:
     $packet = %s;
$outstanding = %s;
END
                                Data::Dump::pp($packet),
                                Data::Dump::pp(
                                           $outstanding_queries{refaddr $self}
                                               {$packet->{q[t]}});
                            return;
                        }
                        else { return; }
                        delete $outstanding_queries{refaddr $self}
                            {$packet->{q[t]}};
                    }
                }
                elsif (eval q[require Data::Dump]) {
                    warn q[Unhandled DHT Reply: ] . Data::Dump::pp($packet);
                }
            }
        }
        else {    # May be AZ. May be garbage. ...same thing.
        }
        return !!$node;
    }

    sub _generate_token_id {
        return if defined $_[1];
        my ($self) = @_;
        my ($len) = ($tid{refaddr $self} =~ m[^([a-z]+)]);
        $tid{refaddr $self} = (
                    ($tid{refaddr $self} =~ m[^z*(\0*)$])
                    ? ($tid{refaddr $self} =~ m[\0]
                       ? pack(q[a] . (length $tid{refaddr $self}),
                              (q[a] x (length($len || q[]) + 1))
                           )
                       : (q[a] . (qq[\0] x (length($tid{refaddr $self}) - 1)))
                        )
                    : ++$tid{refaddr $self}
        );
        return $tid{refaddr $self};
    }

    sub _add_node {
        my ($self, $node) = @_;
        $self->_append_compact_nodes(compact($node));
        if (scalar(keys %{$routing_table{refaddr $self}}) < 300) {
            my ($host, $port) = split q[:], $node, 2;
            my $packed_host = pack_sockaddr_in($port, inet_aton($host));
            return $routing_table{refaddr $self}{$packed_host} ||=
                Net::BitTorrent::DHT::Node->new(
                                               {DHT        => $self,
                                                PackedHost => $packed_host,
                                                NodeID     => undef
                                               }
                );
        }
        return;
    }

    sub as_string {
        my ($self, $advanced) = @_;
        my $dump = !$advanced ? $node_id{refaddr $self} : sprintf <<'END',
Net::BitTorrent::DHT

Node ID: %s
Routing table: %d active nodes / %d nodes in cache
END
            $node_id{refaddr $self},
            scalar(keys %{$routing_table{refaddr + $_[0]}}),
            (length($routing_table{refaddr + $_[0]}) / 6);
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
            weaken $_client{$_nID};
            delete $outstanding_queries{$_nID};
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

Net::BitTorrent::DHT - Kademlia based Distributed Hash Table

=head1 Constructor

=over

=item C<new ( [ARGS] )>

Creates a C<Net::BitTorrent::DHT> object.  This constructor should not be
used directly.

=back

=head1 Methods

=over

=item C<node_id ( )>

Get the Node ID used to identify this L<client|/Net::BitTorrent> in the
DHT swarm.

=item C<as_string ( [ VERBOSE ] )>

Returns a 'ready to print' dump of the  object's data structure.  If
called in void context, the structure is printed to C<STDERR>.
C<VERBOSE> is a boolean value.

=back

=head1 Bugs

=over

=item *

The routing table is flat.

=back

=head1 Notes

While bandwidth to/from DHT nodes is not limited like other traffic,
it is taken into account and "drained" from the rate limiter.  If
there's a burst of DHT traffic, the peer traffic will be limited to
avoid the total to exceed the global limit.

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
