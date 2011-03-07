package Net::BitTorrent::Protocol::BEP12::Types;
{
    use strict;
    use warnings;
    use Moose::Util::TypeConstraints;
    use lib '../../../../';
    use Net::BitTorrent::Protocol::BEP03::Types;
    use 5.010;

    #
    our $MAJOR = 0; our $MINOR = 74; our $DEV = 14; our $VERSION = sprintf('%0d.%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    subtype 'Net::BitTorrent::Protocol::BEP12::Types::Tracker::Tier' => as
        'ArrayRef[Net::BitTorrent::Protocol::BEP03::Types::Tracker::UDP|Net::BitTorrent::Protocol::BEP03::Types::Tracker::HTTP]';
    coerce 'Net::BitTorrent::Protocol::BEP12::Types::Tracker::Tier' => from
        'ArrayRef[Str]'                                             => via {
        state $tracker_constraint
            = Moose::Util::TypeConstraints::find_type_constraint(
            'Net::BitTorrent::Protocol::BEP03::Types::Tracker::HTTP|Net::BitTorrent::Protocol::BEP03::Types::Tracker::UDP'
            );
        [map { $tracker_constraint->coerce($_) } @$_];
        };

    #
    no Moose::Util::TypeConstraints;
}
1;

=pod

=head1 NAME

Net::BitTorrent::Protocol::BEP12::Types - Multitracker Moose Types

=head1 Author

=begin :html

L<Sanko Robinson|http://sankorobinson.com/>
<L<sanko@cpan.org|mailto://sanko@cpan.org>> -
L<http://sankorobinson.com/|http://sankorobinson.com/>

CPAN ID: L<SANKO|http://search.cpan.org/~sanko>

=end :html

=begin :text

Sanko Robinson <sanko@cpan.org> - http://sankorobinson.com/

CPAN ID: SANKO

=end :text

=head1 License and Legal

=for :html Copyright (C) 2008-2011 by Sanko Robinson
<L<sanko@cpan.org|mailto://sanko@cpan.org>>

=for :text Copyright (C) 2008-2011 by Sanko Robinson <sanko@cpan.org>

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
