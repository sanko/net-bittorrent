package Net::BitTorrent::Torrent::Queued;
{
    use Moose::Role;
    #use Moose::Util::TypeConstraints;
    our $MAJOR = 0; our $MINOR = 74; our $DEV = 3; our $VERSION = sprintf('%0d.%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use lib '../../../';
    use Net::BitTorrent::Protocol::BEP12::MultiTracker;

    #
    has 'client' => (is => 'ro', required => 1);
    has 'tracker' => (is  => 'ro',
                      isa => 'Net::BitTorrent::Protocol::BEP12::MultiTracker',
                      predicate => 'has_tracker',
                      builder   => '_build_tracker'
    );

    sub _build_tracker {
        require Net::BitTorrent::Protocol::BEP12::MultiTracker;
        Net::BitTorrent::Protocol::BEP12::MultiTracker->new(
                                                           metadata => shift);
    }

    after 'BUILDALL' => sub {
        my $s = shift;

        #if ($s->metadata->{'info'}{'private'}) {
        #        require Net::BitTorrent::Protocol::BEP27::Private::Metadata;
        #        Net::BitTorrent::Protocol::BEP27::Private::Metadata->meta
        #            ->apply($s);
        #    }
        #}

        # parse trackers
        $s->tracker->add_tier([$s->announce]) if $s->_has_announce;
        $s->tracker->add_tier($_) for @{ $s->_has_announce_list ? [] :$s->announce_list};
...

    };

=pod

    #use Net::BitTorrent::Types qw[:bencode :torrent];
    #use Net::BitTorrent::Protocol::BEP12::MultiTracker;
    #use Net::BitTorrent::Storage;
    #use File::Spec::Functions qw[rel2abs];
    #use AnyEvent;

    #
    my $bencode_constraint;
    has 'metadata' => (
        isa       => 'NBTypes::Bdecode',
        is        => 'ro',
        writer    => '_set_metadata',
        predicate => '_has_metadata',
        init_arg  => undef,                # cannot set this with new()
        coerce    => 1,
        trigger => sub { shift->_trigger_metadata(@_) },
        default => sub { {} },
        handles => {
            rawdata => sub {
                $bencode_constraint //=
                    Moose::Util::TypeConstraints::find_type_constraint(
                                                          'NBTypes::Bencode');
                my $s = shift;
                return if !$s->_has_metadata;
                return $bencode_constraint->coerce($s->metadata);
                }
        }
    );

    sub _trigger_metadata {   # Subclasses should override this and call super
        my ($s, $n, $o) = @_;
        if (@_ == 2) {

            # May have changed
            $s->tracker->_clear_tiers;
            if ($s->metadata->{'info'}{'private'}) {
                require Net::BitTorrent::Protocol::BEP27::Private::Metadata;
                Net::BitTorrent::Protocol::BEP27::Private::Metadata->meta
                    ->apply($s);
            }
        }

        # parse trackers
        $s->tracker->add_tier([$n->{'announce'}]) if $n->{'announce'};
        if (defined $n->{'announce-list'}) {
            $s->tracker->add_tier($_) for @{$n->{'announce-list'}};
        }

        #warn 'Someone changed the metadata!';
        $s->_reset_info_hash;

        #my $info_hash = $self->info_hash;
        #$self->_reset_info_hash;
        #warn sprintf '%s is now %s', $info_hash->to_Hex,
        #    $self->info_hash->to_Hex;
    }

    #
    has 'info_hash' => (
        is       => 'ro',
        isa      => 'NBTypes::Torrent::Infohash',
        init_arg => undef,                        # cannot set this with new()
        coerce   => 1,                            # Both ways?
        lazy_build => 1,
        builder    => '_build_info_hash',  # returns Torrent::Infohash::Packed
        clearer    => '_reset_info_hash',
        predicate  => '_has_info_hash'
    );

    sub _build_info_hash {
        require Digest::SHA;
        my $s = shift;
        $bencode_constraint //=
            Moose::Util::TypeConstraints::find_type_constraint(
                                                          'NBTypes::Bencode');
        return if !$s->_has_metadata;
        Digest::SHA::sha1(
                         $bencode_constraint->coerce($s->metadata->{'info'}));
    }
    has 'piece_count' => (is         => 'ro',
                          isa        => 'Int',
                          lazy_build => 1,
                          builder    => '_build_piece_count',
                          init_arg   => undef
    );
    sub _build_piece_count { return length(shift->pieces) / 20 }
    has 'tracker' => (is  => 'ro',
                      isa => 'Net::BitTorrent::Protocol::BEP12::MultiTracker',
                      predicate => 'has_tracker',
                      builder   => '_build_tracker'
    );

    sub _build_tracker {
        require Net::BitTorrent::Protocol::BEP12::MultiTracker;
        Net::BitTorrent::Protocol::BEP12::MultiTracker->new(
                                                           metadata => shift);
    }

    # Quick accessors
    sub piece_length { shift->metadata->{'info'}{'piece length'} }
    sub pieces       { shift->metadata->{'info'}{'pieces'} }
    sub private {0}    # overridden by BEP27::Private::Metadata

=cut
    no Moose::Role;
}
1;

=pod

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
