package Net::BitTorrent::Types;
{
    use Any::Moose;
    use Any::Moose '::Util::TypeConstraints';
    our $MAJOR = 0.075; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use vars qw[@EXPORT_OK %EXPORT_TAGS];
    use Exporter qw[];
    *import = *import = *Exporter::import;
    %EXPORT_TAGS = (
        infohash => [qw[Torrent::Infohash Torrent::Infohash::Packeed]],
        tracker  => [
            qw[Torrent::Tracker Torrent::Tracker::Tier Torrent::Tracker::UDP Torrent::Tracker::HTTP]
        ]
    );
    @EXPORT_OK = sort map { @$_ = sort @$_; @$_ } values %EXPORT_TAGS;
    $EXPORT_TAGS{'all'} = \@EXPORT_OK;    # When you want to import everything

    #
    subtype 'Torrent::Infohash' => as 'Str' =>
        where { length $_ == 40 && /[a-f0-9]/i };
    subtype 'Torrent::Infohash::Packed' => as 'Str' =>
        where { length $_ == 20 };
    coerce 'Torrent::Infohash' => from 'Torrent::Infohash::Packed' =>
        via { uc unpack 'H*', $_ };
    coerce 'Torrent::Infohash::Packed' => from 'Torrent::Infohash' =>
        via { pack 'H*', $_ };

    #
    subtype 'Torrent::Tracker::HTTP' => as
        'Net::BitTorrent::Protocol::BEP03::Tracker::HTTP';
    coerce 'Torrent::Tracker::HTTP' =>
        from subtype(as 'Str' => where {m[^http://]i}) => via {
        require Net::BitTorrent::Protocol::BEP03::Tracker::HTTP;
        return Net::BitTorrent::Protocol::BEP03::Tracker::HTTP->new(
                                                                   URL => $_);
        };
    subtype 'Torrent::Tracker::UDP' => as
        'Net::BitTorrent::Protocol::BEP15::Tracker::UDP';
    coerce 'Torrent::Tracker::UDP' =>
        from subtype(as 'Str' => where {m[^udp://]i}) => via {
        require Net::BitTorrent::Protocol::BEP15::Tracker::UDP;
        return Net::BitTorrent::Protocol::BEP15::Tracker::UDP->new(URL => $_);
        };
}
1;

=pod

=head1 NAME

Net::BitTorrent::Types - Moose Types

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
