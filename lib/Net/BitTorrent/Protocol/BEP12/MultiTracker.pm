package Net::BitTorrent::Protocol::BEP12::MultiTracker;
{
    use Moose;
    use Moose::Util::TypeConstraints;
    use Carp qw[carp];
    use List::Util qw[shuffle];
    our $MAJOR = 0.075; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use lib '../../../../';
    use Net::BitTorrent::Types qw[:tracker :bencode];
    my $bdecode_constraint;
    after 'metadata' => sub {
        use Data::Dump;
        my ($self) = @_;
        $bdecode_constraint //=
            Moose::Util::TypeConstraints::find_type_constraint(
                                                          'NBTypes::Bdecode');
        my $tiers
            = $bdecode_constraint->coerce($self->raw_data)->{'announce-list'};
        ddx $tiers;
        die 'MultiTracker!!!!!';
    };

=old
    around 'url' => sub {    # BEP03::Tracker->url is ro but that may change
        my ($code, $self, $args) = @_;
        $code->($self->tiers->[0]->url->[0], $args ? $args : ());
    };
    has 'tiers' => (
         traits => ['Array'],
         isa =>
             'ArrayRef[Net::BitTorrent::Protocol::BEP12::MultiTracker::Tier]',
         is      => 'rw',
         coerce  => 1,
         default => sub { [] },
         handles => {add_tier => 'push',
                     shuffle  => 'shuffle'
         }
    );
    around 'add_tier' => sub {
        my ($code, $self, $trackers) = @_;
        require Net::BitTorrent::Protocol::BEP12::MultiTracker::Tier;
        require Net::BitTorrent::Protocol::BEP03::Tracker;
        return $code->(
            $self,
            Net::BitTorrent::Protocol::BEP12::MultiTracker::Tier->new(
                Torrent  => $self->torrent,
                Trackers => [
                    map {
                        Net::BitTorrent::Protocol::BEP03::Tracker->new(
                                                     URL     => $_,
                                                     Torrent => $self->torrent
                            )
                        } @$trackers
                ]
            )
        );
    };
    after 'add_tier' => sub { $_[0]->shuffle };
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
