package Net::BitTorrent::Protocol::BEP03::Tracker;
{
    use Any::Moose;
    use Any::Moose '::Util::TypeConstraints';
    use Carp qw[carp];
    use List::Util qw[shuffle];
    our $MAJOR = 0.075; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use lib '../../../../';
    use Net::BitTorrent::Types qw[:tracker];
    has 'url' => (isa => 'Torrent::Tracker::HTTP | Torrent::Tracker::UDP',
                  is  => 'ro',
                  init_arg => 'URL',
                  coerce   => 1,
                  required => 1
    );
    has 'torrent' => (isa      => 'Net::BitTorrent::Torrent',
                      is       => 'ro',
                      required => 1,
                      init_arg => 'Torrent',
                      weak_ref => 1
    );
    has 'last_contact' => (isa        => 'Maybe[Int]',    # Timestamp
                           is         => 'rw',
                           lazy_build => 1,
                           init_arg   => undef
    );
}
1;

=pod

=head1 NAME

Net::BitTorrent::Protocol::BEP03::Tracker - Single tracker torrent

=head1 See Also

=over

=item BEP 03:

http://bittorrent.org/beps/bep_0003.html

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
