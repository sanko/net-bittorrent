package Net::BitTorrent::Protocol::BEP03::Metadata;
{
    use Moose::Role;
    use Moose::Util::TypeConstraints;
    our $MAJOR = 0.075; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use lib '../../../../';
    use Net::BitTorrent::Types qw[:bencode];
    use Net::BitTorrent::Storage;
    use Fcntl ':flock';
    use File::Spec::Functions qw[rel2abs];
    use AnyEvent;

    #
    requires 'client';

    #
    has 'basedir' => (is       => 'ro',
                      isa      => 'Str',
                      required => 1,
                      default  => rel2abs('.'),
                      init_arg => 'BaseDir'
    );
    has 'storage' => (is         => 'ro',
                      required   => 1,
                      isa        => 'Net::BitTorrent::Storage',
                      lazy_build => 1,
                      builder    => '_build_storage',
                      init_arg   => 'Storage',
                      handles    => {
                                  size => 'size',
                                  read => 'read'
                      }
    );

    sub _build_storage {
        Net::BitTorrent::Storage->new(Torrent => $_[0]);
    }
    has 'tracker' => (isa => 'Net::BitTorrent::Protocol::BEP03::Tracker',
                      is  => 'rw',
                      lazy_build => 1,
                      builder    => '_build_tracker'
    );

    sub _build_tracker {
        Net::BitTorrent::Protocol::BEP12::MultiTracker->new(Torrent => $_[0]);
    }
    has 'metadata' => (
        isa      => 'NBTypes::Bdecode',
        is       => 'ro',
        writer   => '_metadata',
        init_arg => undef,                # cannot set this with new()
        coerce   => 1,
        trigger  => sub {
            my ($self, $new_value, $old_value) = @_;
            if (@_ == 2) {       # parse files and trackers
                if (defined $new_value->{'announce-list'}) {
                    require Net::BitTorrent::Protocol::BEP12::MultiTracker;
                    $self->tracker(
                          Net::BitTorrent::Protocol::BEP12::MultiTracker->new(
                                               Torrent => $self,
                                               URL => $new_value->{'announce'}
                          )
                    );
                    $self->tracker->add_tier($_)
                        for @{$new_value->{'announce-list'}};
                }
                elsif (defined $new_value->{'announce'}) {
                    require Net::BitTorrent::Protocol::BEP03::Tracker;
                    $self->tracker(
                               Net::BitTorrent::Protocol::BEP03::Tracker->new(
                                               Torrent => $self,
                                               URL => $new_value->{'announce'}
                               )
                    );
                }
                else {    # .torrent _requires_ DHT
                }

                #
                my @files;
                if (defined $new_value->{'info'}{'files'})
                {    # Multi-file .torrent
                    $self->storage->files($new_value->{'info'}{'files'});
                    $self->storage->root($new_value->{'info'}{'name'});
                }
                else {    # single file torrent; use the name
                    $self->storage->files(
                              [{path   => [$new_value->{'info'}{'name'}],
                                length => $new_value->{'info'}{'length'}
                               }
                              ]
                    );
                }

                #
                if ($_[0]->metadata->{'info'}{'private'}) {
                    require
                        Net::BitTorrent::Protocol::BEP27::Private::Metadata;
                    Net::BitTorrent::Protocol::BEP27::Private::Metadata->meta
                        ->apply($_[0]);
                }
                return 1;
            }
            warn 'Someone changed the metadata!';
        }
    );
    has 'raw_data' => (
        isa        => 'NBTypes::Bencode',
        lazy_build => 1,
        is         => 'ro',
        init_arg   => undef,                # cannot set this with new()
        writer     => '_raw_data',
        coerce     => 1,
        trigger    => sub {
            my ($self, $new_value, $old_value) = @_;
            $self->_metadata($new_value) if @_ == 2;

            # XXX - set the current value back to the old value
        }
    );
    has 'path' => (
        is       => 'ro',
        isa      => 'Str',
        required => 1,
        init_arg => 'Path',
        trigger  => sub {
            my ($self, $new_value, $old_value) = @_;
            if (@_ == 2) {
                open(my ($FH), '<', $new_value)
                    || return !($_[0] = undef);    # exterminate! exterminate!
                flock $FH, LOCK_SH;
                sysread($FH, my ($METADATA), -s $FH) == -s $FH
                    || return !($_[0] = undef);    # destroy!
                $self->_raw_data($METADATA);
                return close $FH;
            }

            # XXX - set the current value back to the old value
        }
    );

    #
    my $bencode_constraint;
    has 'info_hash' => (
        is       => 'ro',
        isa      => 'NBTypes::Torrent::Infohash',
        init_arg => undef,                        # cannot set this with new()
        coerce   => 1,                            # Both ways?
        lazy_build => 1,
        builder    => '_build_info_hash'   # returns Torrent::Infohash::Packed
    );

    sub _build_info_hash {
        require Digest::SHA;
        my ($self) = @_;
        $bencode_constraint //=
            Moose::Util::TypeConstraints::find_type_constraint(
                                                          'NBTypes::Bencode');
        return Digest::SHA::sha1(
                      $bencode_constraint->coerce($self->metadata->{'info'}));
    }
    has 'piece_count' => (is         => 'ro',
                          isa        => 'Int',
                          lazy_build => 1,
                          builder    => '_build_piece_count',
                          init_arg   => undef
    );
    sub _build_piece_count { return length(shift->pieces) / 20 }
    has 'have' => (is         => 'ro',
                   isa        => 'NBTypes::Torrent::Bitfield',
                   lazy_build => 1,
                   coerce     => 1,
                   builder    => '_build_have',
                   init_arg   => undef,
                   writer     => '_have',
                   clearer    => '_clear_have'
    );
    sub _build_have { '0' x $_[0]->piece_count }
    has 'wanted' => (isa        => 'NBTypes::Torrent::Bitfield',
                     is         => 'ro',
                     writer     => '_wanted',
                     coerce     => 1,
                     builder    => '_build_wanted',
                     lazy_build => 1
    );
    sub _build_wanted { '1' x $_[0]->piece_count }
    {
        has 'tracker' => (
                      is  => 'ro',
                      isa => 'Net::BitTorrent::Protocol::BEP12::MultiTracker',
                      writer    => '_tracker',
                      predicate => 'has_tracker'
        );
    }

    # Quick accessors
    sub piece_length { return shift->metadata->{'info'}{'piece length'} }
    sub pieces       { return shift->metadata->{'info'}{'pieces'} }
    sub private {0}    # overridden by BEP27::Private::Metadata

    # Quick methods
    my $pieces_per_hashcheck = 10;    # Max block of pieces in single call

    sub hashcheck {    # Range is split up into $pieces_per_hashcheck blocks
        my ($self, $range) = @_;
        $range
            = defined $range
            ? ref $range
                ? $range
                : [$range]
            : [0 .. $self->piece_count - 1];
        if (scalar @$range <= $pieces_per_hashcheck) {
            $self->_clear_have();
            for my $index (@$range) {
                my $piece = $self->read($index);
                next if !$piece || !$$piece;
                require Digest::SHA;
                vec($bitfield, $index, 1)
                    = Digest::SHA::sha1($$piece) eq
                    substr($self->pieces, ($index * 20), 20);
            }
            $self->_have($bitfield);
        }
        else {
            my $cv = AnyEvent->condvar;
            $cv->begin;
            my (@watchers, @ranges, @this_range, $coderef);
            push @ranges, [splice(@$range, 0, $pieces_per_hashcheck, ())]
                while @$range;
            $coderef = sub {
                shift @watchers if @watchers;
                @this_range = shift @ranges;
                $self->hashcheck(@this_range);
                push @watchers,
                    AE::idle(@ranges ? $coderef : sub { $cv->end });
            };
            push @watchers, AE::idle($coderef);
            $cv->recv;
            shift @watchers;
        }
        return 1;
    }
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
