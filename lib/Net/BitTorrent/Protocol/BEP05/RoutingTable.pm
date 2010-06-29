package Net::BitTorrent::Protocol::BEP05::RoutingTable;
{
    use Moose;
    use AnyEvent;
    use lib '../../../../../lib';
    use Net::BitTorrent::Protocol::BEP03::Bencode qw[bdecode];
    use Net::BitTorrent::Protocol::BEP05::Packets qw[:all];
    use Net::BitTorrent::Network::Utility qw[:paddr :sockaddr];
    use Net::BitTorrent::Types;
    use 5.12.0;
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 4; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    #
    has 'tracker' => (isa      => 'Net::BitTorrent::Protocol::BEP05::Tracker',
                      is       => 'ro',
                      init_arg => undef,
                      lazy_build => 1
    );

    sub _build_tracker {
        require Net::BitTorrent::Protocol::BEP05::Tracker;
        Net::BitTorrent::Protocol::BEP05::Tracker->new(
                                                      routing_table => shift);
    }
    has 'nodes' => (isa => 'HashRef[Net::BitTorrent::Protocol::BEP05::Node]',
                    is  => 'ro',
                    init_arg => undef,
                    traits   => ['Hash'],
                    handles  => {
                                add_node     => 'set',
                                get_node     => 'get',
                                del_node     => 'delete',
                                defined_node => 'defined',
                                count_nodes  => 'count',
                                all_nodes    => 'values'
                    },
                    default => sub { {} }
    );
    around 'add_node' => sub {
        my ($code, $self, $node) = @_;
        if (!blessed $node) {
            require Net::BitTorrent::Protocol::BEP05::Node;
            $node =
                Net::BitTorrent::Protocol::BEP05::Node->new(
                                                        host => $node->[0],
                                                        port => $node->[1],
                                                        routing_table => $self
                );
        }
        elsif (!$node->has_routing_table) { $node->_routing_table($self) }
        return $code->($self, $node->sockaddr, $node);
    };
    around 'del_node' => sub {
        my ($code, $self, $node) = @_;
        $code->($self, blessed($node) ? $node->sockaddr : $node);
    };
    after 'del_node' =>
        sub { $_[1]->bucket->_del_node($_[1]) if $_[1]->has_bucket };
    has 'buckets' => (
        isa        => 'ArrayRef[Net::BitTorrent::Protocol::BEP05::Bucket]',
        is         => 'ro',
        lazy_build => 1,
        init_arg   => undef,
        traits     => ['Array'],
        handles    => {
            sort_buckets => [
                'sort_in_place',
                sub {
                    $_[0]->floor->Lexicompare($_[1]->floor);
                    }
            ],
            first_bucket  => 'first',
            grep_buckets  => 'grep',
            count_buckets => 'count',
            add_bucket    => 'push'
        }
    );
    after 'add_bucket' => sub { shift->sort_buckets; };

    sub _build_buckets {
        my ($self) = @_;
        require Net::BitTorrent::Protocol::BEP05::Bucket;
        [Net::BitTorrent::Protocol::BEP05::Bucket->new(routing_table => $self
         )
        ];
    }
    has 'dht' => (isa      => 'Net::BitTorrent::DHT',
                  required => 1,
                  is       => 'ro',
                  weak_ref => 1,
                  handles  => [qw[send]],
                  init_arg => 'dht'
    );

    sub nearest_bucket {
        my ($self, $target) = @_;
        for my $bucket (reverse @{$self->buckets}) {
            return $bucket if $bucket->floor->Lexicompare($target) != 1;
        }
    }
    before 'nearest_bucket' => sub { shift->sort_buckets; };

    sub assign_node {
        my ($self, $node) = @_;
        $self->nearest_bucket($node->nodeid)->add_node($node);
    }

    sub find_node_by_sockaddr {
        my ($self, $sockaddr) = @_;
        my $node = $self->get_node($sockaddr);
        if (!$node) {
            for my $bucket (@{$self->buckets}) {
                $node
                    = $bucket->first_node(sub { $_->sockaddr eq $sockaddr });
                last if $node;
            }
        }
        return $node;
    }

    sub outstanding_add_nodes {
        grep { defined $_ && !$_->has_bucket } $_[0]->all_nodes;
    }
}
1;

=pod

=head1 NAME

Net::BitTorrent::Protocol::BEP05::RoutingTable - A DHT routing table

=head1 Description

TODO

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
