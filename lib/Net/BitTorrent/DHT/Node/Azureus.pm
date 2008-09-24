package Net::BitTorrent::DHT::Node::Azureus;
use strict;
use warnings;
{

    BEGIN {
        use version qw[qv];
        our $SVN
            = q[$Id$];
        our $VERSION = sprintf q[%.3f], version->new(qw$Rev 23$)->numify / 1000;
    }
    use Socket qw[SOL_SOCKET /F_INET/ SOCK_DGRAM SO_REUSEADDR
        /sockaddr_in/ inet_ntoa
    ];

    use lib q[../../../../../lib/];
    use Net::BitTorrent::Util qw[:log :bencode :compact];
    {
        my (%dht,            %packed_host,    %node_id,
            %added,          %last_seen,      %last_ping,
            %last_peers, %last_find_node, %infohashes
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
                $last_seen{$self}      = time;    # Tell me lies, ...
                $last_find_node{$self} = 0;
                $last_peers{$self} = 0;
                $last_ping{$self}      = 0;
            }
            return $self;
        }

        sub _packed_host {
            return if defined $_[1];
            return $packed_host{$_[0]};
        }

        sub _port {
            return if defined $_[1];
            return if not $packed_host{$_[0]};
            my ($_port, undef) = unpack_sockaddr_in($packed_host{$_[0]});
            return $_port;
        }

        sub _host {
            return if defined $_[1];
            return if not defined $packed_host{$_[0]};
            my (undef, $addr) = unpack_sockaddr_in($packed_host{$_[0]});
            return inet_ntoa($addr);
        }

        sub _node_id {
            return if defined $_[1];
            return $node_id{$_[0]};
        }

        sub _add_infohash {
            return if not defined $_[1];
            return if not @{$infohashes{$_[0]}};
            return push @{$infohashes{$_[0]}}, $_[1];
        }

        sub _infohashes {
            return if defined $_[1];
            return [keys %{$infohashes{$_[0]}}];
        }

        sub _last_seen {
            return if defined $_[1];
            return $last_seen{$_[0]};
        }

        sub _last_ping {
            return if defined $_[1];
            return $last_ping{$_[0]};
        }

        sub _last_peers {
            return if defined $_[1];
            return $last_peers{$_[0]};
        }

        sub _last_find_node {
            return if defined $_[1];
            return $last_find_node{$_[0]};
        }

        sub _query_ping {
            my ($self) = @_;
            $last_ping{$self} = time;
        }

        sub _reply_ping {
            my ($self, $packet) = @_;
            return if not defined $packet;
            $last_seen{$self} = time;
        }

        sub _query_find_node {
            my ($self, $target) = @_;
            return if not defined $target;
            $last_find_node{$self} = time;
        }

        sub _query_peers {
            my ($self, $session) = @_;
            return if not defined $session;
            return if not $session->isa(q[Net::BitTorrent::Session]);
            $last_peers{$self} = time;
        }

        sub _query_announce_peer {
            my ($self, $session) = @_;
            return if not defined $session;
            return if not $session->isa(q[Net::BitTorrent::Session]);
            return if $session->private;
        }

        sub _parse_reply_find_node {
            my ($self, $packet) = @_;
            $last_seen{$self} = time;
        }

        sub _parse_reply_peers {
            my ($self, $packet) = @_;
            $last_seen{$self} = time;
        }

        sub _parse_query_peers {
            my ($self, $packet) = @_;
            $last_seen{$self} = time;
        }

        sub _parse_query_find_node {
            my ($self, $packet) = @_;
            $last_seen{$self} = time;
        }

        sub as_string {
            my ($self, $advanced) = @_;
            $dht{$self}->get_client->_do_callback(q[log], TRACE,
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
            delete $last_peers{$self};
            delete $last_ping{$self};
            return 1;
        }
    }
}
1;
__END__

=pod

=head1 NAME

Net::BitTorrent::DHT::Node::Azureus - Azureus (binary) DHT Node

=head1 Constructor

=over 4

=item C<new ( [ARGS] )>

Creates a C<Net::BitTorrent::DHT::Node::Azureus> object.  This
constructor should not be used directly.

=back

=head1 Methods

=over 4

=item C<as_string ( [ VERBOSE ] )>

Returns a 'ready to print' dump of the
C<Net::BitTorrent::DHT::Node::Azureus> object's data structure.  If
called in void context, the structure is printed to C<STDERR>.

See also: L<Net::BitTorrent|Net::BitTorrent/as_string>

=back

=head1 BUGS/TODO

=over 4

=item *

The specification documentation (and thus this implementation... urm...)
is incomplete.  See
http://www.azureuswiki.com/index.php/Distributed_hash_table

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
