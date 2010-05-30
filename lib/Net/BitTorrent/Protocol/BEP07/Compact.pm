package Net::BitTorrent::Protocol::BEP07::Compact;
{
    use strict;
    use warnings;
    use Carp qw[carp];
    use Fcntl ':flock';
    our $MAJOR = 0.075; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use vars qw[@EXPORT_OK %EXPORT_TAGS];
    use Exporter qw[];
    *import = *import = *Exporter::import;
    @EXPORT_OK = qw[compact_ipv6 uncompact_ipv6];
    %EXPORT_TAGS = (all => [@EXPORT_OK], bencode => [@EXPORT_OK]);

    sub uncompact_ipv6 {
        my %peers;
        $peers{sprintf("%X:%X:%X:%X:%X:%X:%X:%X:%s", unpack('n9', $1))}++
            while ($_[0] =~ s[^(.{18})][]g);
        return keys %peers;
    }

    sub compact_ipv6 {
        my (@peers) = @_;
        @peers || return;
        my $return;
        my %seen;
    PEER: for my $peer (grep(defined && !$seen{$_}++, @peers)) {
            next if not $peer;
            my ($ip, $port) = ($peer =~ m[^([\da-f:]+):(\d+)$]i);
            if ($port > 2**16) {
                carp 'Port number beyond ephemeral range: ' . $peer;
            }
            else {
                next PEER unless $ip;
                if ($ip
                    =~ /^(.*):(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})$/)
                {    # mixed hex, dot-quad
                    next PEER if $2 > 255 || $3 > 255 || $4 > 255 || $5 > 255;
                    $ip = sprintf("%s:%X%02X:%X%02X", $1, $2, $3, $4, $5)
                        ;    # convert to pure hex
                }
                my $c;
                next PEER
                    if $ip =~ /[^:0-9a-fA-F]/ ||    # non-hex character
                        (($c = $ip) =~ s/::/x/ && $c =~ /(?:x|:):/)
                        ||                          # double :: ::?
                        $ip =~ /[0-9a-fA-F]{5,}/;   # more than 4 digits
                $c = $ip =~ tr/:/:/;                # count the colons
                next PEER if $c < 7 && $ip !~ /::/;
                if ($c > 7) {    # strip leading or trailing ::
                    next PEER unless $ip =~ s/^::/:/ || $ip =~ s/::$/:/;
                    next PEER if --$c > 7;
                }
                $ip =~ s/::/:::/ while $c++ < 7;    # expand compressed fields
                $ip .= 0 if $ip =~ /:$/;
                my @hex = split(/:/, $ip);
                $hex[$_] = hex($hex[$_] || 0) foreach (0 .. $#hex);
                $return .= uc pack('n9', @hex, $port);
            }
        }
        return $return;
    }
}
1;

=pod

=head1 NAME

Net::BitTorrent::Protocol::BEP07::Compact - Utility functions for BEP07: IPv6 Tracker Extension

=head1 Importing From Net::BitTorrent::Protocol::BEP07::Compact

By default, nothing is exported.

You may import any of the following or use one or more of these tag:

=over

=item C<:all>

Imports the tracker response-related functions
L<compact|/"compact_ipv6 ( LIST )"> and
L<uncompact|/"uncompact_ipv6 ( STRING )">.

=back

=head1 Functions

=over

=item C<compact_ipv6 ( LIST )>

Compacts a list of IPv6:port strings into a single string.

A compact peer is 18 bytes; the first 15 bytes are the host and the last two
bytes are the port.

=item C<uncompact_ipv6 ( STRING )>

Inflates a compacted string of peers and returns a list of IPv6:port strings.

=back

=head1 See Also

=over

=item BEP 07: IPv6 Tracker Extension

http://bittorrent.org/beps/bep_0007.html

=back

=head1 Author

Sanko Robinson <sanko@cpan.org> - http://sankorobinson.com/

CPAN ID: SANKO

=head1 License and Legal

Copyright (C) 2010 by Sanko Robinson <sanko@cpan.org>

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
