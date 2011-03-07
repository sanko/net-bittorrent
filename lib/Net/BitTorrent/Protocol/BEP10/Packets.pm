package Net::BitTorrent::Protocol::BEP10::Packets;
{
    use 5.010;
    use strict;
    use warnings;
    our $MAJOR = 0; our $MINOR = 74; our $DEV = 14; our $VERSION = sprintf('%0d.%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use Carp qw[confess];
    use lib '../../../../../lib';
    use Net::BitTorrent::Protocol::BEP03::Types qw[:bencode];
    use Net::BitTorrent::Protocol::BEP03::Packets qw[:all];
    use Net::BitTorrent::Protocol::BEP03::Bencode qw[:all];
    use vars qw[@EXPORT_OK %EXPORT_TAGS];
    use Exporter qw[];
    *import = *import = *Exporter::import;
    %EXPORT_TAGS = (build => [qw[build_extended]],
                    parse => [qw[parse_packet _parse_extended]],
                    types => [qw[$EXTPROTOCOL]]
    );
    @EXPORT_OK = sort map { @$_ = sort @$_; @$_ } values %EXPORT_TAGS;
    $EXPORT_TAGS{'all'} = \@EXPORT_OK;

    #
    our $EXTPROTOCOL = 20;

    # Ugly hack
    $Net::BitTorrent::Protocol::BEP03::Packets::parse_packet_dispatch{
        $EXTPROTOCOL} = \&_parse_extended;

    #
    sub build_extended {
        my ($msgID, $data) = @_;
        if ((!defined $msgID) || ($msgID !~ m[^\d+$])) {
            confess sprintf
                '%s::build_extended() requires a message id parameter',
                __PACKAGE__;
            return;
        }
        if ((!$data) || (ref($data) ne 'HASH')) {
            confess sprintf '%s::build_extended() requires a payload',
                __PACKAGE__;
            return;
        }
        my $packet = pack('ca*', $msgID, bencode($data));
        return pack('Nca*', length($packet) + 1, 20, $packet);
    }

    sub _parse_extended {
        my ($packet) = @_;
        if ((!$packet) || (!length($packet))) { return; }
        my ($id, $payload) = unpack('ca*', $packet);
        return ([$id, scalar bdecode($payload)]);
    }
}
1;

=pod

=head1 NAME

Net::BitTorrent::Protocol - Packet utilities for the BitTorrent protocol

=head1 Synopsis

    TODO

=head1 Description

What would BitTorrent be without packets?   TCP noise, mostly.

For similar work and links to the specifications behind these packets,
move on down to the L<See Also|/"See Also"> section.

=head1 Exporting from Net::BitTorrent::Protocol::BEP10::Packets

There are three tags available for import.  To get them all in one go,
use the C<:all> tag.

=over

=item C<:types>

Packet types

For more on what these packets actually mean, see the BitTorrent Spec.
This is a list of the currently supported packet types:

=over

=item EXTPROTOCOL

=back

=item C<:build>

These create packets ready-to-send to remote peers.  See
L<Building Functions|/"Building Functions">.

=item C<:parse>

These are used to parse unknown data into sensible packets.

=back

=head2 Building Functions

=over

=item C<build_extended ( DATA )>

Creates an extended protocol packet.

=back

=head2 Parsing Function(s)

=over

=item C<parse_packet( DATA )>

Attempts to parse any known packet from the data (a scalar ref) passed to it.
On success, the payload and type are returned and the packet is removed from
the incoming data ref.  C<undef> is returned on failure.

=back

=head1 See Also

L<http://bittorrent.org/beps/bep_0010.html|<http://bittorrent.org/beps/bep_0010.html>
- Extension Protocol

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
