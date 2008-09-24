package Net::BitTorrent::DHT;

#
use strict;
use warnings;
{

    BEGIN {
        use version qw[qv];
        our $SVN = q[$Id$];
        our $VERSION = sprintf q[%.3f], version->new(qw$Rev 23$)->numify / 1000;
    }
    use Socket
        qw[/F_INET/ /_DGRAM/ /SOL_SO/ /SO_RE/ /pack_sockaddr_in/ /inet_/];
    use Digest::SHA qw[sha1];
    use Scalar::Util qw[weaken];
    use Carp qw[carp];

    #
    use lib q[../../../lib/];
    use Net::BitTorrent::Util qw[:log :bencode :compact];
    use Net::BitTorrent::DHT::Node::Mainline qw[];
    use Net::BitTorrent::DHT::Node::Azureus qw[];

    # Debugging
    #use Data::Dump qw[pp];
    {
        my (%socket, %_client, %fileno, %tid, %outstanding_queries);
        my (%node_id, %routing_table, %backup_nodes, %extra);

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
            {   my $node_id = sha1(
                    (join(
                         q[],
                         map {
                             [q[a] .. q[z], q[A] .. q[Z]]->[rand(52)]
                             } 1 .. 300
                     )
                    )
                );
                $self = bless \$node_id, $class;
                $_client{$self} = $args->{q[Client]};
                weaken $_client{$self};
                my ($socket, $address, $port) = $self->_open_socket(
                    $args->{q[LocalAddr]}, $args->{q[LocalPort]},
                    $args->{q[ReuseAddr]},    # XXX - undocumented
                    $args->{q[ReusePort]}     # XXX - undocumented
                );
                $node_id{$self}       = $node_id;
                $routing_table{$self} = {};
                $backup_nodes{$self}  = q[];
                $tid{$self}           = qq[\0] x 5;    # 26^5 before rollover
                $_client{$self}->_schedule(
                                          {Code   => sub { shift->_pulse() },
                                           Time   => time,
                                           Object => $self
                                          }
                );
            }
            return $self;
        }

        sub _open_socket {
            my ($self, $host, $port, $reuse_address, $reuse_port) = @_;

            {    # backup
                $extra{$self}{q[LocalHost]} = $host if defined $host;
                $extra{$self}{q[LocalPort]} = $port if defined $port;
                $extra{$self}{q[ReuseAddr]} = $reuse_address
                    if defined $reuse_address;
                $extra{$self}{q[ReusePort]} = $reuse_port
                    if defined $reuse_port;
            }
            {    # restore from backup
                $host = $extra{$self}{q[LocalHost]}
                    if not defined $host
                        && defined $extra{$self}{q[LocalHost]};
                $port = $extra{$self}{q[LocalPort]}
                    if not defined $port
                        && defined $extra{$self}{q[LocalPort]};
                $reuse_address = $extra{$self}{q[ReuseAddr]}
                    if not defined $reuse_address
                        && defined $extra{$self}{q[ReuseAddr]};
                $reuse_port = $extra{$self}{q[ReusePort]}
                    if not defined $reuse_port
                        && defined $extra{$self}{q[ReusePort]};
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
            $socket{$self} = $_socket;
            my ($_port, $packed_ip)
                = unpack_sockaddr_in(getsockname($_socket));
            my $ip = gethostbyaddr($packed_ip, AF_INET);

            #$_client{$self}->_remove_connection($self);
            $_client{$self}->_add_connection($self, q[ro]) or return;
            return ($_socket, inet_ntoa($packed_ip), $_port);
        }

        # Accessors | Private
        sub _client {
            return if defined $_[1];
            return $_client{$_[0]};
        }

        sub _routing_table {
            return if defined $_[1];
            return $routing_table{$_[0]};
        }

        sub _socket {
            return if defined $_[1];
            return $socket{$_[0]};
        }

        sub _queue_outgoing {    # no-op
            return if defined $_[1];
            return;
        }

        sub _port {
            return                if defined $_[1];
            $_[0]->_open_socket() if not $socket{$_[0]};
            return                if not $socket{$_[0]};
            my ($_port, undef)
                = unpack_sockaddr_in(getsockname($socket{$_[0]}));
            return $_port;
        }

        sub _host {
            return                if defined $_[1];
            $_[0]->_open_socket() if not $socket{$_[0]};
            return                if not $socket{$_[0]};
            my (undef, $addr)
                = unpack_sockaddr_in(getsockname($socket{$_[0]}));
            return inet_ntoa($addr);
        }

        # Accesors | Public
        sub _node_id {
            return if defined $_[1];
            return $node_id{$_[0]};
        }

        # Methods | Private
        sub _pulse {
            my ($self) = @_;
            $self->_open_socket() if not defined $socket{$self};
            {    # get rid of old outgoing packets...
                my @expired_requests = grep {
                    $outstanding_queries{$self}{$_}->{timestamp}
                        < (time - (60 * 5))
                } keys %{$outstanding_queries{$self}};
                while (my $packet = shift @expired_requests) {
                    delete $outstanding_queries{$self}{$packet};
                }
            }
            {    # get rid of old, useless DHT nodes...
                my @expired_nodes = grep {
                    my $timestamp = $routing_table{$self}{$_}->_last_seen;
                    ($timestamp < (time - (60 * 30)))
                        or ($timestamp < (time - (60 * 5))
                        and (not defined $routing_table{$self}{$_}->_node_id))
                } keys %{$routing_table{$self}};
                for my $node (@expired_nodes) {

                    #$backup_nodes{$self} .=
                    #    compact(sprintf(q[%s:%s],
                    #                    $routing_table{$self}{$node}->host,
                    #                    $routing_table{$self}{$node}->port)
                    #    );
                    delete $routing_table{$self}{$node};
                }
            }
            my @hosts = sort keys %{$routing_table{$self}};
        NODE: for my $packed_host (@hosts) {
                my $node = $routing_table{$self}{$packed_host};
                if (    (not defined $node->_node_id)
                    and ($node->_last_seen < (time - (60 * 5))))
                {   delete $routing_table{$self}{$packed_host};
                    next NODE;
                }
            }
            $_client{$self}->_schedule({Code   => sub { shift->_pulse },
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
                    ($a->_node_id ^ $target) cmp($b->_node_id ^ $target)
                    } grep { defined $_->_node_id }
                    values %{$routing_table{$self}}
            ]->[0 .. 8];
        }

        sub _send {
            my ($self, $args) = @_;
            if (not defined $socket{$self}) { $self->_open_socket() }if (not defined $socket{$self})
             {

                return;
            }if (not defined $args) {
                carp q[Net::BitTorrent::DHT->_send() requires parameters];
                return;
            }
            if (not defined $args->{q[node]}) {
                carp
                    q[Net::BitTorrent::DHT->_send() requires a 'node' parameter];
                return;
            }
            if (defined $args->{q[node]}->_node_id
                and $args->{q[node]}->_node_id eq $node_id{$self})
            {   carp q[Cannot send packet to ourself!];
                return;
            }
            if (not send($socket{$self}, $args->{q[packet]},
                         0,              $args->{q[node]}->_packed_host)
                )
            {   carp sprintf q[Cannot send %d bytes to %s: [%d] %s],
                    length($args->{q[packet]}), $args->{q[node]}->_node_id,
                    $^E, $^E;
                return;
            }
            if (defined $args->{q[t]} and defined $args->{q[type]})
            {    # don't save replies
                $outstanding_queries{$self}{$args->{q[t]}} = {
                                           size => length($args->{q[packet]}),
                                           timestamp => time,
                                           node      => $args->{q[node]},
                                           type      => $args->{q[type]},
                                           args      => $args
                };
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
            $self->_open_socket() if not defined $socket{$self};
            my ($actual_read) = 0;
            my $packed_host = recv($socket{$self}, my ($_data), $read, 0);
            if (defined $packed_host) {
                $actual_read = length $_data;
                my ($packet, $leftover) = bdecode($_data);
                if ($packet) {
                    $routing_table{$self}{$packed_host} ||=
                        Net::BitTorrent::DHT::Node::Mainline->new(
                                              {dht         => $self,
                                               packed_host => $packed_host,
                                               node_id     => undef
                                              }
                        );
                    my $node = $routing_table{$self}{$packed_host};
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
                        else {    # xxx - do something drastic
                            require Data::Dumper;
                            die q[Unhandled DHT packet: ]
                                . Data::Dumper->Dump([$packet], [qw[Packet]]);
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
                                }

#~                                  find_node     => \&_parse_reply_find_node,
                        );
                        if (defined $packet->{q[r]}) {

                           #use Data::Dump qw[pp];
                           #warn sprintf q[Reply!!!!! %s for %s],
                           #    pp($packet),
                           #    pp(
                           #    $outstanding_queries{$self}{$packet->{q[t]}});
                            if (defined $outstanding_queries{$self}
                                {$packet->{q[t]}})
                            {   my $type = $outstanding_queries{$self}
                                    {$packet->{q[t]}}{q[type]};
                                if (defined $dispatch{$type}) {
                                    $dispatch{$type}($node, $packet);
                                }
                                else {
                                    require Data::Dumper;
                                    die q[Unhandled DHT Reply: ]
                                        . Data::Dumper->Dump([$packet],
                                                             [qw[packet]]);
                                }
                                delete $outstanding_queries{$self}
                                    {$packet->{q[t]}};
                            }
                        }
                        elsif (require Data::Dumper)
                        {    # XXX - turn into a callback
                            warn q[Unhandled DHT Reply]
                                . Data::Dumper->Dump([$packet], [qw[packet]]);
                        }
                    }
                }
                else {       # May be AZ. May be garbage. ...same thing.
                             #warn pp $data;
                    if (   not defined $routing_table{$self}{$packed_host}
                        or not $routing_table{$self}{$packed_host}
                        ->isa(q[Net::BitTorrent::DHT::Node::Azureus]))
                    {   $routing_table{$self}{$packed_host}
                            = Net::BitTorrent::DHT::Node::Azureus->new(
                                              {dht         => $self,
                                               packed_host => $packed_host,
                                               node_id     => undef
                                              }
                            );
                    }
                    my $node = $routing_table{$self}{$packed_host};

                    # XXX - Do stuff with the node
                }
            }
            return ($actual_read, 0);
        }

        sub _generate_token_id {    # automatic rollover/expansion/etc
            return if defined $_[1];
            my ($self) = @_;
            my ($len) = ($tid{$self} =~ m[^([a-z]+)]);
            $tid{$self} = (($tid{$self} =~ m[^z*(\0*)$])
                           ? ($tid{$self} =~ m[\0]
                              ? pack(q[a] . (length $tid{$self}),
                                     (q[a] x (length($len || q[]) + 1))
                                  )
                              : (q[a] . (qq[\0] x (length($tid{$self}) - 1)))
                               )
                           : ++$tid{$self}
            );
            return $tid{$self};
        }

        sub _add_node {
            my ($self, $node) = @_;
            if (scalar(keys %{$routing_table{$self}}) < 300)
            {    # xxx - make this max variable
                my ($host, $port) = split q[:], $node, 2;
                my $packed_host = pack_sockaddr_in($port, inet_aton($host));

                # We figure out what sort of node this is after the 1st packet
                return $routing_table{$self}{$packed_host} ||=
                    Net::BitTorrent::DHT::Node::Mainline->new(
                                              {dht         => $self,
                                               packed_host => $packed_host,
                                               node_id     => undef
                                              }
                    );
            }
            return $backup_nodes{$self} .= compact($node);
        }

        sub _disconnect {    # noop
            return 1;
        }

        sub as_string {
            my ($self, $advanced) = @_;
            my $dump = q[TODO];
            return print STDERR qq[$dump\n] unless defined wantarray;
            return $dump;
        }

        #
        DESTROY {
            my ($self) = @_;

            #
            delete $socket{$self};
            delete $fileno{$self};
            delete $node_id{$self};
            delete $_client{$self};
            delete $tid{$self};
            delete $routing_table{$self};
            delete $backup_nodes{$self};
            delete $outstanding_queries{$self};
            delete $extra{$self};

            #
            return 1;
        }
    }
}
1;
__END__

=pod

=head1 NAME

Net::BitTorrent::DHT - Kademlia based Distributed Hash Table

=head1 Constructor

=over 4

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

Returns a 'ready to print' dump of the C<Net::BitTorrent::DHT> object's
data structure.  If called in void context, the structure is printed to
C<STDERR>.

See also: L<Net::BitTorrent|Net::BitTorrent/as_string>

=back

=head1 BUGS/TODO

=over 4

=item *

Currently, the routing table is flat.

=back

=head1 Author

Sanko Robinson <sanko@cpan.org> - http://sankorobinson.com/

CPAN ID: SANKO

=head1 License and Legal

Copyright 2008 by Sanko Robinson E<lt>sanko@cpan.orgE<gt>

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl 5.10 (or higher).  See
http://www.perl.com/perl/misc/Artistic.html or the F<LICENSE> file
included with this distribution.

All POD documentation is covered by the Creative Commons Attribution-
Noncommercial-Share Alike 3.0 License
(http://creativecommons.org/licenses/by-nc-sa/3.0/us/).

Neither this module nor the L<Author|/Author> is affiliated with
BitTorrent, Inc.

=for svn $Id$

=cut
