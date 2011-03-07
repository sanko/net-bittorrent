package Net::BitTorrent::Protocol::BEP06::Packets;
{
    use 5.010;
    use strict;
    use warnings;

    #use Moose::Util::TypeConstraints;
    our $MAJOR = 0; our $MINOR = 74; our $DEV = 14; our $VERSION = sprintf('%0d.%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use Carp qw[confess];
    use lib '../../../../../lib';
    use Net::BitTorrent::Protocol::BEP03::Types;
    use Net::BitTorrent::Protocol::BEP03::Packets qw[:all];
    use Net::BitTorrent::Protocol::BEP03::Bencode qw[:all];
    use vars qw[@EXPORT_OK %EXPORT_TAGS];
    use Exporter qw[];
    *import = *import = *Exporter::import;
    %EXPORT_TAGS = (
        build => [
            qw[build_suggest build_allowed_fast build_reject build_have_all
                build_have_none]
        ],
        parse => [
            qw[ parse_packet
                _parse_suggest _parse_have_all _parse_have_none
                _parse_reject _parse_allowed_fast]
        ],
        types => [qw[ $SUGGEST $HAVE_ALL $HAVE_NONE $REJECT $ALLOWED_FAST ]]
    );
    @EXPORT_OK = sort map { @$_ = sort @$_; @$_ } values %EXPORT_TAGS;
    $EXPORT_TAGS{'all'} = \@EXPORT_OK;

    #
    our $SUGGEST      = 13;
    our $HAVE_ALL     = 14;
    our $HAVE_NONE    = 15;
    our $REJECT       = 16;
    our $ALLOWED_FAST = 17;

    # Ugly hack
    $Net::BitTorrent::Protocol::BEP03::Packets::parse_packet_dispatch{$SUGGEST
        } = \&_parse_suggest;
    $Net::BitTorrent::Protocol::BEP03::Packets::parse_packet_dispatch{
        $HAVE_ALL} = \&_parse_have_all;
    $Net::BitTorrent::Protocol::BEP03::Packets::parse_packet_dispatch{
        $HAVE_NONE} = \&_parse_have_none;
    $Net::BitTorrent::Protocol::BEP03::Packets::parse_packet_dispatch{$REJECT}
        = \&_parse_reject;
    $Net::BitTorrent::Protocol::BEP03::Packets::parse_packet_dispatch{
        $ALLOWED_FAST} = \&_parse_allowed_fast;

    #
    sub build_suggest {
        my ($index) = @_;
        if ((!defined $index) || ($index !~ m[^\d+$])) {
            confess sprintf '%s::build_suggest() requires an index parameter',
                __PACKAGE__;
            return;
        }
        return pack('NcN', 5, 13, $index);
    }
    sub build_have_all  { return pack('Nc', 1, 14); }
    sub build_have_none { return pack('Nc', 1, 15); }

    sub build_reject {
        my ($index, $offset, $length) = @_;
        if ((!defined $index) || ($index !~ m[^\d+$])) {
            confess sprintf '%s::build_reject() requires an index parameter',
                __PACKAGE__;
            return;
        }
        if ((!defined $offset) || ($offset !~ m[^\d+$])) {
            confess sprintf '%s::build_reject() requires an offset parameter',
                __PACKAGE__;
            return;
        }
        if ((!defined $length) || ($length !~ m[^\d+$])) {
            confess sprintf '%s::build_reject() requires an length parameter',
                __PACKAGE__;
            return;
        }
        my $packed = pack('N3', $index, $offset, $length);
        return pack('Nca*', length($packed) + 1, 16, $packed);
    }

    sub build_allowed_fast {
        my ($index) = @_;
        if ((!defined $index) || ($index !~ m[^\d+$])) {
            confess sprintf
                '%s::build_allowed_fast() requires an index parameter',
                __PACKAGE__;
            return;
        }
        return pack('NcN', 5, 17, $index);
    }

    #
    sub _parse_suggest {
        my ($packet) = @_;
        if ((!$packet) || (length($packet) < 1)) {

            #confess 'Incorrect packet length for SUGGEST';
            return;
        }
        return unpack('N', $packet);
    }
    sub _parse_have_all  { return; }
    sub _parse_have_none { return; }

    sub _parse_reject {
        my ($packet) = @_;
        if ((!$packet) || (length($packet) < 9)) {

            #confess
            #    sprintf(
            #          'Incorrect packet length for REJECT (%d requires >=9)',
            #          length($packet || ''));
            return;
        }
        return ([unpack('N3', $packet)]);
    }

    sub _parse_allowed_fast {
        my ($packet) = @_;
        if ((!$packet) || (length($packet) < 1)) {

            #confess 'Incorrect packet length for FASTSET';
            return;
        }
        return unpack('N', $packet);
    }
}
1;

=pod

=head1 NAME

Net::BitTorrent::Protocol::BEP06::Packets - Packet utilities for the Fast Extension to the BitTorrent Protocol

=head1 Synopsis

    TODO

=head1 Description

What would BitTorrent be without packets?   TCP noise, mostly.

For similar work and links to the specifications behind these packets,
move on down to the L<See Also|/"See Also"> section.

=head1 Imorting from Net::BitTorrent::Protocol::BEP06::Packets

There are three tags available for import.  To get them all in one go,
use the C<:all> tag.

=over

=item C<:types>

Packet types

For more on what these packets actually mean, see the BitTorrent Spec.
This is a list of the currently supported packet types:

=over

=item SUGGEST

=item HAVE_ALL

=item HAVE_NONE

=item REJECT

=item ALLOWED_FAST

=back

=item C<:build>

These create packets ready-to-send to remote peers.  See
L<Building Functions|/"Building Functions">.

=item C<:parse>

These are used to parse unknown data into sensible packets.

=back

=head2 Building Functions

=over

=item C<build_allowed_fast ( INDEX )>

Creates an Allowed Fast packet.

uTorrent never advertises a fast set... why should we?

See also: http://bittorrent.org/beps/bep_0006.html - Fast Extension

=item C<build_suggest ( INDEX )>

Creates a Suggest Piece packet.

Super seeding is not supported by Net::BitTorrent.  Yet.

See also: http://bittorrent.org/beps/bep_0006.html - Fast Extension

=item C<build_reject ( INDEX, OFFSET, LENGTH )>

Creates a Reject Request packet.

See also: http://bittorrent.org/beps/bep_0006.html - Fast Extension

=item C<build_have_all ( )>

Creates a Have All packet.

See also: http://bittorrent.org/beps/bep_0006.html - Fast Extension

=item C<build_have_none ( )>

Creates a Have None packet.

See also: http://bittorrent.org/beps/bep_0006.html - Fast Extension

=back

=head2 Parsing Function(s)

=over

=item C<parse_packet( DATA )>

Attempts to parse any known packet from the data (a scalar ref) passed to it.
On success, the payload and type are returned and the packet is removed from
the incoming data ref.  C<undef> is returned on failure.

=back

=head1 See Also

L<http://bittorrent.org/beps/bep_0006.html|http://bittorrent.org/beps/bep_0006.html> - Fast Extension

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
