package Net::BitTorrent::Protocol::BEP03::Bencode;
{

    use strict;
    use warnings;
    use Carp qw[carp];
    our $MAJOR = 0.075; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use vars qw[@EXPORT_OK %EXPORT_TAGS];
    use Exporter qw[];
    *import = *import = *Exporter::import;
    @EXPORT_OK = qw[bencode bdecode];
    %EXPORT_TAGS = (all     => [@EXPORT_OK],
                    bencode => [@EXPORT_OK],
     );

    sub bencode {
        my ($ref) = @_;
        $ref = defined $ref ? $ref : '';
        if (not ref $ref) {
            return (  (defined $ref and $ref =~ m[^[-+]?\d+$])
                    ? ('i' . $ref . 'e')
                    : (length($ref) . ':' . $ref)
            );
        }
        elsif (ref $ref eq 'ARRAY') {
            return join('', 'l', (map { bencode($_) } @{$ref}),'e');
        }
        elsif (ref $ref eq 'HASH') {
            return
                join('', 'd',
                     (map { bencode($_) . bencode($ref->{$_}) }
                      sort keys %{$ref}
                     ),
                     'e'
                );
        }
        return '';
    }

    sub bdecode {
        my ($string) = @_;
        return if not defined $string;
        my ($return, $leftover);
        if (   $string =~ m[^([1-9]\d*):]s
            or $string =~ m[^(0+):]s)
        {   my $size = $1;
            $return = '' if $1 =~ m[^0+$];
            $string =~ s|^$size:||s;
            $return .= substr($string,0,$size,'');
            return if length $return < $size;
            return wantarray ? ($return, $string) : $return;    # byte string
        }
        elsif ($string =~ s|^i([-+]?\d+)e||s) {                 # integer
            return wantarray ? (int($1), $string) : int($1);
        }
        elsif ($string =~ s|^l(.*)||s) {                        # list
            $leftover = $1;
            while ($leftover and $leftover !~ s|^e||s) {
                (my ($piece), $leftover) = bdecode($leftover);
                push @$return, $piece;
            }
            return wantarray ? (\@$return, $leftover) : \@$return;
        }
        elsif ($string =~ s|^d(.*)||s) {                        # dictionary
            $leftover = $1;
            while ($leftover and $leftover !~ s|^e||s) {
                my ($key, $value);
                ($key, $leftover) = bdecode($leftover);
                ($value, $leftover) = bdecode($leftover) if $leftover;
                $return->{$key} = $value if defined $key;
            }
            return wantarray ? (\%$return, $leftover) : \%$return;
        }
        return;
    }
}

1;
=pod

=head1 NAME

Net::BitTorrent::Protocol::BEP03::Bencode - Utility functions for BEP03: The BitTorrent Protocol Specification

=head1 Importing From Net::BitTorrent::Protocol::BEP03::Bencode

By default, nothing is exported.

You may import any of the following functions by name or with one or more of
these tags:

=over

=item C<:all>

You get the two Bencode-related functions: L<bencode|/"bencode ( ARGS )">
and L<bdecode|/"bdecode ( STRING )">.  For more on Bencoding, see the
BitTorrent Protocol documentation.

=back

=head1 Functions

=over

=item C<bencode ( ARGS )>

Expects a single value (basic scalar, array reference, or hash reference) and
returns a single string.

Bencoding is the BitTorrent protocol's basic serialization and data
organization format. The specification supports integers, lists (arrays),
dictionaries (hashes), and byte strings.

=item C<bdecode ( STRING )>

Expects a bencoded string.  The return value depends on the type of data
contained in the string.

=back

=head1 See Also

=over

=item The BitTorrent Protocol Specification

http://bittorrent.org/beps/bep_0003.html#the-connectivity-is-as-follows

=item Other Bencode related modules:

=over

=item L<Convert::Bencode|Convert::Bencode>

=item L<Bencode|Bencode>

=item L<Convert::Bencode_XS|Convert::Bencode_XS>

=back

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
