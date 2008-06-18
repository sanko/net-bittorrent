package Net::BitTorrent::DHT::Node::Mainline;
use strict;
use warnings;
{

    BEGIN {
        use version qw[qv];
        our $SVN = q[$Id$];
        our $VERSION = sprintf q[%.3f], version->new(qw$Rev 0$)->numify / 1000;
    }
    use Socket qw[SOL_SOCKET /F_INET/ SOCK_DGRAM SO_REUSEADDR];
    use Digest::SHA qw[sha1];
    use Carp qw[carp];
    use lib q[../../../../../lib/];
    use Net::BitTorrent::Util qw[:log :bencode :compact];
    {
        my (%dht,            %packed_host,    %node_id,
            %added,          %last_seen,      %last_ping,
            %last_get_peers, %last_find_node, %infohashes
        );

        sub new {
            my ($class, $args) = @_;
            my $self = undef;
            if (    defined $args->{q[dht]}
                and defined $args->{q[packed_host]})
            {   $self = bless \$args->{q[node_id]}, $class;
                $dht{$self}         = $args->{q[dht]};
                $packed_host{$self} = $args->{q[packed_host]};
                $node_id{$self}     = $args->{q[node_id]}
                    if defined $args->{q[node_id]};
                $infohashes{$self}     = {};
                $added{$self}          = time;
                $last_seen{$self}      = time; # ...tell me sweet little lies.
                $last_find_node{$self} = 0;
                $last_get_peers{$self} = 0;
                $last_ping{$self}      = 0;
            }
            return $self;
        }
        sub packed_host    { return $packed_host{$_[0]}; }
        sub host {return join q[.],(unpack(q[SnC4x8], $packed_host{$_[0]}))[2 .. 5];}
        sub port { return (unpack(q[SnC4x8], $packed_host{$_[0]}))[1]; }
        sub node_id        { return $node_id{$_[0]}; }
        sub add_infohash   { return push @{$infohashes{$_[0]}}, $_[1]; }
        sub infohashes     { return keys %{$infohashes{$_[0]}}; }
        sub last_seen      { return $last_seen{$_[0]}; }
        sub last_ping      { return $last_ping{$_[0]}; }
        sub last_get_peers { return $last_get_peers{$_[0]}; }
        sub last_find_node { return $last_find_node{$_[0]}; }

        sub _query_ping {
            my ($self) = @_;
            $last_ping{$self} = time;
            my $tid = $dht{$self}->_generate_token_id;
            return
                $dht{$self}->_send(
                    {node => $self,
                     t    => $tid,
                     type => q[ping],
                     packet =>
                         bencode(
                         {t => $tid,
                          y => q[q],
                          q => q[ping],
                          a => {id => $dht{$self}->node_id},
                          v => sprintf(q[NB:%s], $Net::BitTorrent::VERSION)
                         }
                         )
                    }
                );
        }

        sub _reply_ping {
            my ($self, $packet) = @_;
            $last_seen{$self} = time;
            $self->node_id($packet->{q[a]}{q[id]});
            $dht{$self}->_send(
                    {node => $self,
                     packet =>
                         bencode(
                         {t => $packet->{q[t]},
                          y => q[r],
                          r => {id => $node_id{$self}},
                          v => sprintf(q[NB:%s], $Net::BitTorrent::VERSION)
                         }
                         )
                    }
            );
        }

        sub _query_find_node {
            my ($self, $target) = @_;
            $last_find_node{$self} = time;
            my $tid = $dht{$self}->_generate_token_id;
            $dht{$self}->_send(
                    {node => $self,
                     t    => $tid,
                     type => q[find_node],
                     packet =>
                         bencode(
                         {t => $tid,
                          y => q[q],
                          q => q[find_node],
                          a => {target => $target,
                                id     => $dht{$self}->node_id
                          },
                          v => sprintf(q[NB:%s], $Net::BitTorrent::VERSION)
                         }
                         )
                    }
            );
        }

        sub _query_get_peers {
            my ($self, $session) = @_;
            $last_get_peers{$self} = time;
            return if $session->private;
            my $tid = $dht{$self}->_generate_token_id;
            $dht{$self}->_send(
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
                             id => $dht{$self}->node_id
                         },
                         v => sprintf(q[NB:%s], $Net::BitTorrent::VERSION)
                        }
                        )
                   }
            );
        }

        sub _query_announce_peer {
            my ($self, $session) = @_;
            return if $session->private;
            my $tid = $dht{$self}->_generate_token_id;
            $dht{$self}->_send(
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
                             port => $dht{$self}->client->sockport
                         }
                        }
                        )
                   }
            );
        }

        sub _parse_reply_find_node {
            my ($self, $packet) = @_;
            $last_seen{$self} = time;

            # xxx - find node by packed_host and update seen, etc.
            for my $new_node (uncompact($packet->{q[r]}{q[nodes]})) {
                $dht{$self}->add_node($new_node);
            }
            return 1;
        }

        sub _parse_reply_get_peers {
            my ($self, $packet) = @_;
            $last_seen{$self} = time;
            #use Data::Dump qw[pp];
            #warn pp $packet;
            my $session =
                $dht{$self}->client->_locate_session(unpack q[H*],
                                                   $packet->{q[r]}{q[token]});
            return $session->append_nodes($packet->{q[r]}{q[nodes]})
                if $session;

            # ...okay, we're not serving this torrent but should we store
            # it in case someone else needs these peers?  I'll need to
            # read the spec again...
            return 0;
        }

        sub _parse_query_get_peers {
            my ($self, $packet) = @_;
            $last_seen{$self} = time;
            my (undef, $packed_hostport, @address)
                = unpack(q[SnC4x8], $packed_host{$self});
            #use Data::Dump qw[pp];
            #warn sprintf q[%s.%s.%s.%s:%d says find_node! %s], @address,
            #    $packed_hostport,
            #    pp $packet;
            my $target = $packet->{q[a]}{q[info_hash]};
            my $session
                = $dht{$self}->client->_locate_session(unpack q[H*], $target);
            my @nodes;
            if (defined $session) { @nodes = @{$session->nodes}; }

            # XXX - check our routing table for nodes w/ this torrent
            return
                $dht{$self}->_send(
                     {node => $self,
                      packet =>
                          bencode(
                          {y => q[r],
                           t => $packet->{q[t]},
                           r => {id     => $node_id{$self},
                                 token  => $packet->{q[a]}{q[id]},
                                 values => [map { compact($_) } @nodes]
                           },
                           v => sprintf(q[NB%d], $Net::BitTorrent::VERSION)
                          }
                          )
                     }
                ) if @nodes;

            # XXX - XOR with out current list of nodes
            @nodes = $dht{$self}->find_nodes($target);
            #warn pp \@nodes;
            return
                $dht{$self}->_send(
                     {node => $self,
                      packet =>
                          bencode(
                          {y => q[r],
                           t => $packet->{q[t]},
                           r => {id    => $node_id{$self},
                                 token => $packet->{q[a]}{q[id]},
                                 nodes => [map { compact($_) } @nodes]
                           },
                           v => sprintf(q[NB%d], $Net::BitTorrent::VERSION)
                          }
                          )
                     }
                ) if @nodes;

#{
#  a   => {
#           id => pack("H*","ade4d864fc2e9b77e832728e1698b1b6260c9254"),
#           info_hash => pack("H*","6e59e23cbbfb584745ac2ea74aaf0007a599de0e"),
#         },
#  "q" => "get_peers",
#  t   => 10005645,
#  "y" => "q",
#}
#Response with peers =         {"t":"aa", "y":"r", "r": {"id":"abcdefghij0123456789", "token":"aoeusnth", "values": ["axje.u", "idhtnm"]}}
#bencoded = d1:rd2:id20:abcdefghij01234567895:token8:aoeusnth6:valuesl6:axje.u6:idhtnmee1:t2:aa1:y1:re
#
#Response with closest nodes = {"t":"aa", "y":"r", "r": {"id":"abcdefghij0123456789", "token":"aoeusnth", "nodes": "def456..."}}
#bencoded = d1:rd2:id20:abcdefghij01234567895:nodes9:def456...5:token8:aoeusnthe1:t2:aa1:y1:re
        }

        sub _parse_query_find_node {
            my ($self, $packet) = @_;
            $last_seen{$self} = time;

# Find node is used to find the contact information for a node given its
# ID. "q" == "find_node" A find_node query has two arguments, "id"
# containing the node ID of the querying node, and "target" containing
# the ID of the node sought by the queryer. When a node receives a
# find_node query, it should respond with a key "nodes" and value of a
# string containing the compact node info for the target node or the K
# (8) closest good nodes in its own routing table.
#
#arguments:  {"id" : "<querying nodes id>", "target" : "<id of target node>"}
#
#response:   {"id" : "<queried nodes id>",   "nodes" : "<compact node info>"}
#
#Example Packets:
#find_node Query = {"t":"aa", "y":"q", "q":"find_node", "a": {"id":"abcdefghij0123456789", "target":"mnopqrstuvwxyz123456"}}
#bencoded = d1:ad2:id20:abcdefghij01234567896:target20:mnopqrstuvwxyz123456e1:q9:find_node1:t2:aa1:y1:qe
#
#Response = {"t":"aa", "y":"r", "r": {"id":"0123456789abcdefghij", "nodes": "def456..."}}
#bencoded = d1:rd2:id20:0123456789abcdefghij5:nodes9:def456...e1:t2:aa1:y1:re
#
#{
#  a   => {
#           id => pack("H*","688f71ed729b38f1d0b5f9bf032019e50f45eecc"),
#           target => pack("H*","688f706c52fdd2ee6323b7a943078ba16ceb6217"),
#         },
#  "q" => "find_node",
#  t   => "\2E",
#  v   => "LT\0\r",
#  "y" => "q",
#}
            my (undef, $packed_hostport, @address)
                = unpack(q[SnC4x8], $packed_host{$self});
            #use Data::Dump qw[pp];
            #warn sprintf q[%s.%s.%s.%s:%d says find_node! %s], @address,
            #    $packed_hostport, pp $packet;

   #        $dht{$self}->_send({node => $self, t =>$tid,
   #                      packet      => bencode({
   #                          y => q[r],
   #                          t => $packet->{q[t]},
   #                          r => {id => $node_id{$self}},
   #                          v => sprintf(q[NB%d], $Net::BitTorrent::VERSION)
   #                      }
   #                     })
   #        );
        }

        sub as_string {
            my ($self, $advanced) = @_;
            $dht{$self}->client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            my $dump = q[TODO];
            return print STDERR qq[$dump\n] unless defined wantarray;
            return $dump;
        }
        DESTROY {
            my $self = shift;
            delete $dht{$self};
            delete $packed_host{$self};
            delete $node_id{$self};
            delete $infohashes{$self};
            delete $added{$self};
            delete $last_seen{$self};
            delete $last_find_node{$self};
            delete $last_get_peers{$self};
            delete $last_ping{$self};
            return 1;
        }
    }
}
1;
__END__

=pod

=head1 NAME

Net::BitTorrent::DHT::Node::Mainline - Mainline (bencode) DHT Node

=head1 Constructor

=over 4

=item C<new ( [ARGS] )>

Creates a C<Net::BitTorrent::DHT::Node::Mainline> object.  This
constructor should not be used directly.

=back

=head1 Methods

=over 4

=item C<as_string ( [ VERBOSE ] )>

Returns a 'ready to print' dump of the
C<Net::BitTorrent::DHT::Node::Mainline> object's data structure.  If
called in void context, the structure is printed to C<STDERR>.

See also: L<Net::BitTorrent/as_string>

=back

=head1 BUGS/TODO

=over 4

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
