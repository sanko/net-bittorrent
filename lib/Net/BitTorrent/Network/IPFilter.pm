package Net::BitTorrent::Network::IPFilter;
{
    use Moose;
    use Moose::Util::TypeConstraints;
    use 5.012.000;
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use lib '../../../';
    use Net::BitTorrent::Network::Utility qw[:paddr];
    sub BUILD { 1; }
    has 'ranges' => (
         isa => 'ArrayRef[Net::BitTorrent::Network::IPFilter::Range]' => is =>
             'ro',
         traits   => ['Array'],
         coerce   => 1,
         init_arg => undef,
         default  => sub { [] },
         handles  => {
                     add_range            => 'push',
                     count_ranges         => 'count',
                     is_empty             => 'is_empty',
                     get_range            => 'get',
                     first_range          => 'first',
                     grep_ranges          => 'grep',
                     map_ranges           => 'map',
                     sort_ranges          => 'sort',
                     sort_ranges_in_place => 'sort_in_place',
                     shuffle_ranges       => 'shuffle',
                     clear_ranges         => 'clear',
                     insert_range         => 'insert',
                     delete_range         => 'delete',
                     push_range           => 'push',
                     pop_range            => 'pop'
         }
    );
    around 'add_range' => sub {
        my ($c, $s, $l, $u, $a, $d) = @_;
        $l = blessed $l? $l : sub {
            require Net::BitTorrent::Network::IPFilter::Range;
            Net::BitTorrent::Network::IPFilter::Range->new(lower        => $l,
                                                           upper        => $u,
                                                           access_level => $a,
                                                           description  => $d
            );
            }
            ->();
        return $c->($s, $l) ? $l : ();
    };

    sub load {
        my ($s, $path) = @_;
        open(my $IPFilter, '<', $path) || return;
        for my $line (<$IPFilter>) {
            next if $line =~ m[(?:^#|^$)];
            my ($range, $access_level, $desc)
                = ($line =~ m[^(.+-.+)\s*,\s*(\d+)\s*,\s*(.+)\s*$]);
            next if !$range;
            my ($start, $end) = ($range =~ m[^(.+)\s*-\s*(.+)\s+$]);
            $_ =~ s[\s][]g for $start, $end;
            $s->add_range($start, $end, $access_level, $desc);
        }
        1;
    }

    sub save {
        my ($s, $path) = @_;
        open(my $IPFilter, '>', $path) || return;
        for my $range ($s->sort_ranges(sub { $_[0]->lower cmp $_[1]->lower }))
        {   syswrite $IPFilter, $range->_as_string . "\n";
        }
        return close $IPFilter;
    }

    #
    sub ip_filter {
        my ($s, $ip) = @_;
        return $s->first_range(
            sub {
                $_->in_range($ip) && $_->access_level <= 127;
            }
        ) || ();
    }
}
1;

=pod

=head1 NAME

Net::BitTorrent::Network::IPFilter -

=head1 Description

Nothing to see here.

=head1 See Also

L<Emule Project's ipfilter.dat documentation|http://www.emule-project.net/home/perl/help.cgi?l=1&topic_id=142&rm=show_topic>

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
