package Net::BitTorrent::Protocol::BEP03::Metadata;
{
    use 5.010;
    use Moose;
    use Moose::Util::TypeConstraints;
    our $MAJOR = 0; our $MINOR = 74; our $DEV = 14; our $VERSION = sprintf('%0d.%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use lib '../../../../';
    use Net::BitTorrent::Protocol::BEP03::Types qw[:bencode :torrent];
    extends 'Net::BitTorrent::Protocol::BEP03::Storage';
    use File::Spec::Functions qw[catdir];

    #
    # XXX - Move this to NBPBEP27::Metadata::_init_metadata
    #if (@_ == 2) {
    #    # May have changed
    #    $s->tracker->clear_tiers;
    #    if ($s->metadata->{'info'}{'private'}) {
    #        require Net::BitTorrent::Protocol::BEP27::Private::Metadata;
    #        Net::BitTorrent::Protocol::BEP27::Private::Metadata->meta
    #            ->apply($s);
    #    }
    #}
    #    # parse trackers
    #    $s->tracker->add_tier([$n->{'announce'}]) if $n->{'announce'};
    #    if (defined $n->{'announce-list'}) {
    #        $s->tracker->add_tier($_) for @{$n->{'announce-list'}};
    #    }
    #warn 'Someone changed the metadata!';
    #    $s->clear_info_hash;
    #my $info_hash = $self->info_hash;
    #$self->_reset_info_hash;
    #warn sprintf '%s is now %s', $info_hash->to_Hex,
    #    $self->info_hash->to_Hex;
    #}
    #
    sub as_string {
        my $s = shift;
        #<<< perltidy will skip this
        state $bencode_constraint //=
        #>>>
            Moose::Util::TypeConstraints::find_type_constraint(
                          'Net::BitTorrent::Protocol::BEP03::Types::Bencode');
        $bencode_constraint->coerce($s->_prepared_metadata);
    }
    has _prepared_metadata => (
                    is  => 'ro',
                    isa => 'Net::BitTorrent::Protocol::BEP03::Types::Bdecode',
                    coerce     => 1,
                    lazy_build => 1
    );

    sub _build__prepared_metadata {
        my $s = shift;
        my $r = {
            ($s->has_announce ? (announce => $s->announce) : ()),
            info => {
                $s->_count_files == 1 && scalar @{$s->files->[0]->path} == 1
                ? (length => $s->size)
                : (files => [
                       map {
                           {path => $_->path, length => $_->length}
                           } @{$s->files}
                   ]
                )
            }
        };
        $r->{'info'}{'piece length'} = $s->piece_length;
        $r->{'info'}{'pieces'} = pack 'H*', join '',
            map { $_->to_Hex } @{$s->pieces};
        $r->{'info'}{'name'} = $s->name;
        return $r;
    }

    #
    has info_hash => (
         isa => 'Net::BitTorrent::Protocol::BEP03::Types::Metadata::Infohash',
         is  => 'ro',
         lazy_build => 1,
         init_arg   => undef,
         coerce     => 1
    );

    sub _build_info_hash {
        my $s = shift;
        #<<< perltidy will skip this
        state $bencode_constraint //=
        #>>>
            Moose::Util::TypeConstraints::find_type_constraint(
                          'Net::BitTorrent::Protocol::BEP03::Types::Bencode');
        require Digest::SHA;
        Digest::SHA::sha1(
               $bencode_constraint->coerce($s->_prepared_metadata->{'info'}));
    }
    has tracker => (is  => 'ro',
                    isa => 'Net::BitTorrent::Protocol::BEP12::MultiTracker',
                    lazy_build => 1
    );

    sub _build_tracker {
        require Net::BitTorrent::Protocol::BEP12::MultiTracker;
        Net::BitTorrent::Protocol::BEP12::MultiTracker->new(
                                                           metadata => shift);
    }

    # Quick accessors
    has name => (isa      => 'Str',
                 is       => 'ro',
                 required => 1
    );
    has announce => (
                 isa => 'Maybe[Net::BitTorrent::Protocol::BEP03::Types::URL]',
                 is  => 'ro',
                 lazy_build => 1
    );
    sub _build_announce { () }
    has pieces => (
           isa => 'Net::BitTorrent::Protocol::BEP03::Types::Metadata::Pieces',
           is  => 'ro',
           required => 1,
           coerce   => 1,
           traits   => ['Array'],
           handles  => {count_pieces => 'count'}
    );

    # Override ::Storage
    override root => sub {
        my $s    = shift;
        my $path = super;
        $s->_count_files == 1 ? $path : catdir $path, $s->name;
    };

    #sub private {0}    # overridden by BEP27::Private::Metadata
    #
    no Moose;
    no Moose::Util::TypeConstraints;
    __PACKAGE__->meta->make_immutable;
}
1;

=pod

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
