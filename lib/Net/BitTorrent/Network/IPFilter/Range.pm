package Net::BitTorrent::Network::IPFilter::Range;
{
    use Moose;
    use Moose::Util::TypeConstraints;
    use 5.012.000;
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use lib '../../../../';
    use Net::BitTorrent::Types qw[:paddr];
    sub BUILD { 1; }
    for my $limit (qw[upper lower]) {
        has $limit => (
            isa      => 'NBTypes::Network::Paddr',
            is       => 'ro',
            required => 1,
            coerce   => 1,
            handles  => {
                $limit . '_as_string' => sub {
                    my $s = shift;
                    require Net::BitTorrent::Network::Utility;
                    Net::BitTorrent::Network::Utility::paddr2ip($s->{$limit});
                    }
            }
        );
    }
    has 'description' => (isa => 'Str', is => 'ro', required => 1);
    has 'access_level' => (isa      => 'Int',
                           is       => 'ro',
                           required => 1,
                           traits   => ['Counter'],
                           handles  => {
                                       set_access_level      => 'set',
                                       increase_access_level => 'inc',
                                       decrease_access_level => 'dec'
                           }
    );

    sub in_range {
        my ($s, $ip) = @_;
        require Net::BitTorrent::Network::Utility;
        $ip = Net::BitTorrent::Network::Utility::ip2paddr($ip);
        return (($s->lower lt $ip && $s->upper gt $ip) ? 1 : 0);
    }

    sub _as_string {
        my $s = shift;
        return join ', ', $s->lower_as_string, $s->upper_as_string,
            $s->access_level,
            $s->description;
    }
}
1;

=pod

=head1 NAME

Net::BitTorrent::Network::IPFilter::Range -

=head1 Description

Nothing to see here.

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
