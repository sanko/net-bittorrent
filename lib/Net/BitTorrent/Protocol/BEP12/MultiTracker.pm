package Net::BitTorrent::Protocol::BEP12::MultiTracker;
{
    use Moose;
    use Moose::Util::TypeConstraints;
    use AnyEvent;
    use Carp qw[carp];
    use List::Util qw[shuffle];
    our $MAJOR = 0; our $MINOR = 74; our $DEV = 14; our $VERSION = sprintf('%0d.%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use lib '../../../../';
    use Net::BitTorrent::Protocol::BEP12::Types;
    has metadata => (isa      => 'Net::BitTorrent::Torrent',
                     is       => 'ro',
                     weak_ref => 1,
                     required => 1
    );
    has tiers => (
              traits => ['Array'],
              isa => 'Net::BitTorrent::Protocol::BEP12::Types::Tracker::Tier',
              is  => 'rw',
              coerce  => 1,
              default => sub { [] },
              clearer => '_clear_tiers',
              handles => {_push_tier => 'push', _shuffle_tiers => 'shuffle'}
    );
    my $tier_constraint;

    sub add_tier {
        my ($self, $urls) = @_;
        $tier_constraint //=
            Moose::Util::TypeConstraints::find_type_constraint(
                                     'Net::BitTorrent::Types::Tracker::Tier');
        my $tier = $tier_constraint->coerce($urls);
        $self->_push_tier($tier) or return;
        $self->_shuffle_tiers;
        return $tier;
    }

    #
    for my $type (qw[announce scrape]) {
        has "_${type}_quests" => (isa      => 'ArrayRef[Ref]',
                                  is       => 'ro',
                                  init_arg => undef,
                                  traits   => ['Array'],
                                  handles  => {
                                              "add_${type}_quest" => 'push',
                                              "${type}_quests" => 'elements',
                                              "get_${type}_quest"   => 'get',
                                              "grep_${type}_quests" => 'grep',
                                              "map_${type}_quests"  => 'map'
                                  },
                                  default => sub { [] }
        );
        after "add_${type}_quest" => sub {
            require Scalar::Util;
            Scalar::Util::weaken $_[0]->{"_${type}_quests"}->[-1];
        };
    }

    sub announce {
        my ($self, $event, $code) = @_;
        return if !$self->metadata->can('client');
        return if !$self->metadata->_has_client;
        my %args = (info_hash  => $self->metadata->info_hash->to_Hex,
                    peer_id    => $self->metadata->client->peer_id,
                    port       => $self->metadata->client->port,
                    uploaded   => $self->metadata->uploaded,
                    downloaded => $self->metadata->downloaded,
                    left       => $self->metadata->left
        );
        $args{'info_hash'} =~ s|(..)|\%$1|g;
        my $quest;
        require Scalar::Util;
        Scalar::Util::weaken $self;
        $quest = [
            $event, $code,
            [],
            AE::timer(
                0,
                15 * 60,
                sub {
                    return if !$self;

                    #return if !$self-active;
                    for my $tier (@{$self->tiers}) {
                        $tier->[0]->announce(
                            $event,
                            \%args,
                            sub {
                                my ($announce) = @_;
                                {
                                    my %seen = ();
                                    @{$quest->[2]}
                                        = grep { !$seen{$_->[0]}{$_->[1]}++ }
                                        @{$quest->[2]},
                                        @{$announce->{'peers'}};
                                }
                            }
                        );
                    }
                    $event = undef if $event;
                }
            )
        ];
        $self->add_announce_quest($quest);
        return $quest;
    }

    sub scrape {
        my ($self, $code) = @_;
        require Scalar::Util;
        Scalar::Util::weaken $self;
        my %args = (info_hash => $self->metadata->info_hash->to_Hex);
        $args{'info_hash'} =~ s|(..)|\%$1|g;
        my $quest = [
            0, $code,
            [],
            AE::timer(
                0,
                15 * 60,
                sub {
                    return if !$self;
                    for my $tier (@{$self->tiers}) {
                        $tier->[0]->scrape(\%args, $code);
                    }
                }
            )
        ];
        $self->add_scrape_quest($quest);
        return $quest;
    }
}
1;

=pod

=head1 NAME

Net::BitTorrent::Protocol::BEP12 - Multitracker Metadata Extension

=head1 See Also

=over

=item BEP 12:

http://bittorrent.org/beps/bep_0012.html

=back

=head1 Author

Sanko Robinson <sanko@cpan.org> - http://sankorobinson.com/

CPAN ID: SANKO

=head1 License and Legal

Copyright (C) 2008-2011 by Sanko Robinson <sanko@cpan.org>

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
