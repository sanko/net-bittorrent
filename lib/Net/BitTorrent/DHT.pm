#!C:\perl\bin\perl.exe
package Net::BitTorrent::DHT;
{
    use Socket
        qw[/F_INET/ /_DGRAM/ /SOL_SO/ /SO_RE/ /pack_sockaddr_in/ /inet_/];
    use Digest::SHA qw[sha1];
    use Scalar::Util qw[weaken refaddr];
    use Carp qw[carp];

    #
    use lib q[../../../lib/];
    use Net::BitTorrent::Util qw[:log :bencode :compact];
    use Net::BitTorrent::DHT::Node;
    use Net::BitTorrent::Version;

    #
    use version qw[qv];    # core as of 5.009
    our $SVN = q[$Id$];
    our $UNSTABLE_RELEASE = 0; our $VERSION = sprintf(($UNSTABLE_RELEASE ? q[%.3f_%03d] : q[%.3f]), (version->new((qw$Rev$)[1])->numify / 1000), $UNSTABLE_RELEASE);

    # Debugging
    #use Data::Dump qw[pp];
    {
        my (%socket, %_client, %fileno, %tid, %outstanding_queries);
        my (%node_id, %routing_table, %nodes, %extra);
        my %REGISTRY;

        #
        sub new {
            my ($class, $args) = @_;
            if ((defined($args)) and (ref($args) ne q[HASH])) { return; }
            my $self = undef;
            return if not defined $args->{q[Client]};
            return if not $args->{q[Client]}->isa(q[Net::BitTorrent]);
            return if not defined $args->{q[LocalPort]};
            return if not $args->{q[LocalPort]} =~ m[^\d+$];

            # Let the user pick either LocalHost or LocalAddr like
            # IO::Socket::INET.  ...do I really want to do this?
            $args->{q[LocalAddr]} = $args->{q[LocalHost]}
                if exists $args->{q[LocalHost]}
                    && !exists $args->{q[LocalAddr]};
            return if not defined $args->{q[LocalAddr]};
            return
                if not $args->{q[LocalAddr]}
                    =~ m[^(?:(?:(?:25[0-5]|2[0-4][0-9]|[0-1]?[0-9]{1,2})[.]?){4})$];
            if (defined $args->{q[Client]}
                and $args->{q[Client]}->isa(q[Net::BitTorrent]))
            {   my $node_id = Net::BitTorrent::Version->gen_node_id();
                $self = bless \$node_id, $class;
                $_client{refaddr $self} = $args->{q[Client]};
                weaken $_client{refaddr $self};
                my ($socket, $address, $port) = $self->_open_socket(
                    $args->{q[LocalAddr]}, $args->{q[LocalPort]},
                    $args->{q[ReuseAddr]},    # XXX - undocumented
                    $args->{q[ReusePort]}     # XXX - undocumented
                );
                $node_id{refaddr $self}       = $node_id;
                $routing_table{refaddr $self} = {};
                $nodes{refaddr $self}         = q[];
                $tid{refaddr $self} = qq[\0] x 5;    # 26^5 before rollover
                $_client{refaddr $self}->_schedule(
                                          {Code   => sub { shift->_pulse() },
                                           Time   => time,
                                           Object => $self
                                          }
                );
            }

            #
            weaken($REGISTRY{refaddr $self} = $self);

            #
            return $self;
        }

        sub _open_socket {
            my ($self, $host, $port, $reuse_address, $reuse_port) = @_;
            {    # backup
                $extra{refaddr $self}{q[LocalHost]} = $host if defined $host;
                $extra{refaddr $self}{q[LocalPort]} = $port if defined $port;
                $extra{refaddr $self}{q[ReuseAddr]} = $reuse_address
                    if defined $reuse_address;
                $extra{refaddr $self}{q[ReusePort]} = $reuse_port
                    if defined $reuse_port;
            }
            {    # restore from backup
                $host = $extra{refaddr $self}{q[LocalHost]}
                    if not defined $host
                        && defined $extra{refaddr $self}{q[LocalHost]};
                $port = $extra{refaddr $self}{q[LocalPort]}
                    if not defined $port
                        && defined $extra{refaddr $self}{q[LocalPort]};
                $reuse_address = $extra{refaddr $self}{q[ReuseAddr]}
                    if not defined $reuse_address
                        && defined $extra{refaddr $self}{q[ReuseAddr]};
                $reuse_port = $extra{refaddr $self}{q[ReusePort]}
                    if not defined $reuse_port
                        && defined $extra{refaddr $self}{q[ReusePort]};
            }

            # perldoc perlipc
            socket(my ($_socket), PF_INET, SOCK_DGRAM, getprotobyname(q[udp]))
                or return;

        # - What is the difference between SO_REUSEADDR and SO_REUSEPORT?
        #    [http://www.unixguide.net/network/socketfaq/4.11.shtml]
        # - setsockopt - what are the options for ActivePerl under Windows NT?
        #    [http://perlmonks.org/?node_id=63280]
            if ($reuse_address and defined SO_REUSEADDR)
            {    # XXX - undocumented
                setsockopt($_socket, SOL_SOCKET, SO_REUSEADDR, pack(q[l], 1))
                    or return;
            }

           #if ($reuse_port and defined SO_REUSEPORT) {   # XXX - undocumented
           #    setsockopt($_socket, SOL_SOCKET, SO_REUSEPORT, pack(q[l], 1))
           #        or return;
           #}
            bind($_socket, pack_sockaddr_in($port, inet_aton($host)))
                or return;
            $socket{refaddr $self} = $_socket;
            my ($_port, $packed_ip)
                = unpack_sockaddr_in(getsockname($_socket));
            my $ip = gethostbyaddr($packed_ip, AF_INET);

            #$_client{refaddr $self}->_remove_connection($self);
            return ($_socket, inet_ntoa($packed_ip), $_port);
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
        sub _socket { my ($self) = @_; return $socket{refaddr $self}; }

        sub _queue_outgoing {    # no-op
            return if defined $_[1];
            return;
        }

        sub _port {
            return                if defined $_[1];
            $_[0]->_open_socket() if not $socket{refaddr + $_[0]};
            return                if not $socket{refaddr + $_[0]};
            my ($_port, undef)
                = unpack_sockaddr_in(getsockname($socket{refaddr + $_[0]}));
            return $_port;
        }

        sub _host {
            return                if defined $_[1];
            $_[0]->_open_socket() if not $socket{refaddr + $_[0]};
            return                if not $socket{refaddr + $_[0]};
            my (undef, $addr)
                = unpack_sockaddr_in(getsockname($socket{refaddr + $_[0]}));
            return inet_ntoa($addr);
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
        sub _set_node_id {    # XXX - untested
            return if not defined $_[1];
            return $node_id{refaddr + $_[0]} = $_[1];
        }

        sub _append_compact_nodes {    # XXX - untested
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
            $self->_open_socket() if not defined $socket{refaddr $self};
            {    # get rid of old outgoing packets...
                my @expired_requests = grep {
                    $outstanding_queries{refaddr $self}{$_}->{timestamp}
                        < (time - (60 * 5))
                } keys %{$outstanding_queries{refaddr $self}};
                while (my $packet = shift @expired_requests) {
                    delete $outstanding_queries{refaddr $self}{$packet};
                }
            }
            {    # get rid of old, useless DHT nodes...
                my @expired_nodes = grep {
                    my $timestamp
                        = $routing_table{refaddr $self}{$_}->_last_seen;
                    ($timestamp < (time - (60 * 30)))
                        or ($timestamp < (time - (60 * 5))
                            and (not defined $routing_table{refaddr $self}{$_}
                                 ->node_id)
                        )
                } keys %{$routing_table{refaddr $self}};
                for my $node (@expired_nodes) {

              #$nodes{refaddr $self} .=
              #    compact(sprintf(q[%s:%s],
              #                    $routing_table{refaddr $self}{$node}->host,
              #                    $routing_table{refaddr $self}{$node}->port)
              #    );
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
            $_client{refaddr $self}->_schedule(
                                            {Code => sub { shift->_pulse },
                                             Time   => time + 30,
                                             Object => $self
                                            }
            );
            return;
        }

        sub _locate_nodes_near_target {
            my ($self, $target) = @_;
            return if not defined $target;
            return [
                sort {
                    ($a->node_id ^ $target) cmp($b->node_id ^ $target)
                    } grep { defined $_->node_id }
                    values %{$routing_table{refaddr $self}}
            ]->[0 .. 8];
        }

        sub _send {
            my ($self, $args) = @_;
            if (not defined $socket{refaddr $self}) { $self->_open_socket() }
            if (not defined $socket{refaddr $self}) {
                return;
            }
            if (not defined $args) {
                carp q[Net::BitTorrent::DHT->_send() requires parameters];
                return;
            }
            if (not defined $args->{q[node]}) {
                carp
                    q[Net::BitTorrent::DHT->_send() requires a 'node' parameter];
                return;
            }
            if (defined $args->{q[node]}->node_id
                and $args->{q[node]}->node_id eq $node_id{refaddr $self})
            {   carp q[Cannot send packet to ourself!];
                return;
            }
            if (not send($socket{refaddr $self},
                         $args->{q[packet]},
                         0,
                         $args->{q[node]}->_packed_host
                )
                )
            {   carp sprintf q[Cannot send %d bytes to %s: [%d] %s],
                    length($args->{q[packet]}), $args->{q[node]}->node_id,
                    $^E, $^E;
                return;
            }
            if (defined $args->{q[t]} and defined $args->{q[type]})
            {    # don't save replies
                $outstanding_queries{refaddr $self}{$args->{q[t]}} = {
                                           size => length($args->{q[packet]}),
                                           timestamp => time,
                                           node      => $args->{q[node]},
                                           type      => $args->{q[type]},
                                           args      => $args
                };
                weaken $outstanding_queries{refaddr $self}{$args->{q[t]}}
                    {q[args]}{q[node]};
            }

            #
            #use Data::Dump qw[pp];
            #warn sprintf q[Sent | %s:%d => %s],
            #    $args->{q[node]}->_host,
            #    $args->{q[node]}->_port,
            #    pp $args;
            #
            #
            return 1;
        }

        sub _rw {
            my ($self, $read, $write) = @_;
            return if not $read;
            $self->_open_socket() if not defined $socket{refaddr $self};
            my ($actual_read) = 0;
            my $packed_host
                = recv($socket{refaddr $self}, my ($_data), $read, 0);
            if (defined $packed_host) {
                $actual_read = length $_data;
                my ($packet, $leftover)
                    = bdecode($_data);    # TODO: if $leftover, loop
                if ((defined $packet) and (ref $packet eq q[HASH])) {
                    $routing_table{refaddr $self}{$packed_host} ||=
                        Net::BitTorrent::DHT::Node->new(
                                              {dht         => $self,
                                               packed_host => $packed_host,
                                               node_id     => undef
                                              }
                        );
                    my $node = $routing_table{refaddr $self}{$packed_host};

                    #if (ref $packet ne q[HASH])
                    #{    # An attempt to track down a strange bug...
                    #    require Data::Dump;
                    #    warn Data::Dump->Dump([$packet, $leftover],
                    #                            [qw[Packet Leftover]]);
                    #}
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
                        elsif (eval q[require Data::Dump])
                        {    # xxx - do something drastic
                            carp q[Unhandled DHT packet: ]
                                . Data::Dump::pp($packet);
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

               #use Data::Dumper;
               #warn qq[DHT Reply:\n]
               #        . Data::Dumper->Dump(
               #        [$packet,
               #        $outstanding_queries{refaddr $self}{$packet->{q[t]}}],
               #                             [qw[packet for]]);
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
                        elsif (eval q[require Data::Dump])
                        {    # XXX - turn into a callback?
                            warn q[Unhandled DHT Reply: ]
                                . Data::Dump::pp($packet);
                        }
                    }
                }
                else {       # May be AZ. May be garbage. ...same thing.
                             #warn pp $data;

                    # XXX - Do stuff with the node
                }
            }
            return ($actual_read, 0);
        }

        sub _generate_token_id {    # automatic rollover/expansion/etc
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
            if (scalar(keys %{$routing_table{refaddr $self}}) < 300)
            {    # xxx - make this max variable
                my ($host, $port) = split q[:], $node, 2;
                my $packed_host = pack_sockaddr_in($port, inet_aton($host));

                # We figure out what sort of node this is after the 1st packet
                return $routing_table{refaddr $self}{$packed_host} ||=
                    Net::BitTorrent::DHT::Node->new(
                                              {dht         => $self,
                                               packed_host => $packed_host,
                                               node_id     => undef
                                              }
                    );
            }
            return;
        }

        sub _disconnect {    # noop
            return 1;
        }

        sub _as_string {
            my ($self, $advanced) = @_;
            my $dump = q[TODO];
            return defined wantarray ? $dump : print STDERR qq[$dump\n];
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
                weaken $_client{$_nID};
                delete $outstanding_queries{$_nID};

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

=back

=head1 Bugs

=over

=item *

Currently, the routing table is flat.

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
