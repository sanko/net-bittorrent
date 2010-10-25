package Net::BitTorrent::Protocol::BEP03::Bencode;
{
    use strict;
    use warnings;

    #
    our $MAJOR = 0; our $MINOR = 74; our $DEV = 13; our $VERSION = sprintf('%0d.%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    #
    use Exporter qw[import];
    our %EXPORT_TAGS;
    our @EXPORT_OK = @{$EXPORT_TAGS{'all'}} = qw[bencode bdecode];

    #
    sub bencode {
        my $ref = shift // return;
        return (  ((length $ref) && $ref =~ m[^([-\+][1-9])?\d*$])
                ? ('i' . $ref . 'e')
                : (length($ref) . ':' . $ref)
        ) if !ref $ref;
        return join('', 'l', (map { bencode($_) } @{$ref}), 'e')
            if ref $ref eq 'ARRAY';
        return
            join('', 'd',
                 (map { length($_) . ':' . $_ . bencode($ref->{$_}) }
                  sort keys %{$ref}
                 ),
                 'e'
            ) if ref $ref eq 'HASH';
        return '';
    }

    sub bdecode {
        my $string = shift // return;
        my ($return, $leftover);
        if ($string =~ s[^(0+|[1-9]\d*):][]) {
            my $size = $1;
            $return = '' if $size =~ m[^0+$];
            $return .= substr($string, 0, $size, '');
            return if length $return < $size;
            return $_[0] ? ($return, $string) : $return;    # byte string
        }
        elsif ($string =~ s[^i([-\+]?\d+)e][]) {            # integer
            my $int = $1;
            $int = () if $int =~ m[^-0] || $int =~ m[^0\d+];
            return $_[0] ? ($int, $string) : $int;
        }
        elsif ($string =~ s[^l(.*)][]s) {                   # list
            $leftover = $1;
            while ($leftover and $leftover !~ s[^e][]s) {
                (my ($piece), $leftover) = bdecode($leftover, 1);
                push @$return, $piece;
            }
            return $_[0] ? (\@$return, $leftover) : \@$return;
        }
        elsif ($string =~ s[^d(.*)][]s) {                   # dictionary
            $leftover = $1;
            while ($leftover and $leftover !~ s[^e][]s) {
                my ($key, $value);
                ($key, $leftover) = bdecode($leftover, 1);
                ($value, $leftover) = bdecode($leftover, 1) if $leftover;
                $return->{$key} = $value if defined $key;
            }
            return $_[0] ? (\%$return, $leftover) : \%$return;
        }
        return;
    }
}
1;

=pod

=head1 NAME

Net::BitTorrent::Protocol::BEP03::Bencode - Metadata Utility Functions for BEP03: The BitTorrent Protocol Specification

=head1 Synopsis

    use Net::BitTorrent::Protocol::BEP03::Bencode qw[bencode bdecode];
    use Data::Dump qw[dump];
    printf "bencode: %s\n", bencode [123, [''], 'XXX'];
    printf "bdecode: %s\n", dump bdecode 'd3:fool3:bar4:stube6:numberi123ee';

...prints...

    bencode: li123el0:e3:XXXe
    bdecode: { foo => ["bar", "stub"], number => 123 }

=head1 Description

Taken from L<BEP03|Net::BitTorrent::Protocol::BEP03/"The connectivity is as follows">...

=over

=item * Strings are length-prefixed base ten followed by a colon and the
string. For example C<4:spam> corresponds to 'C<spam>'.

=item * Integers are represented by an 'i' followed by the number in base
C<10> followed by an 'e'. For example C<i3e> corresponds to C<3> and C<i-3e>
corresponds to C<-3>. Integers have no size limitation. C<i-0e> is invalid.
All encodings with a leading zero, such as C<i03e>, are invalid, other than
C<i0e>, which of course corresponds to C<0>.

=item * Lists are encoded as an 'l' followed by their elements (also bencoded)
followed by an 'e'. For example C<l4:spam4:eggse> corresponds to
C<['spam', 'eggs']>.

=item * Dictionaries are encoded as a 'd' followed by a list of alternating
keys and their corresponding values followed by an 'e'. For example,
C<d3:cow3:moo4:spam4:eggse> corresponds to C<{'cow': 'moo', 'spam': 'eggs'}>
and C<d4:spaml1:a1:bee> corresponds to C<{'spam': ['a', 'b']}>. Keys must be
strings and appear in sorted order (sorted as raw strings, not alphanumerics).

=back

=head2 Importing From Net::BitTorrent::Protocol::BEP03::Bencode

By default, nothing is exported.

You may import any of the included functions by name or import everything with
the C<:all> tag.

=head1 Functions

In. Out. That's all there is.

=head2 C<< $string = B<bencode>( $value ) >>

Expects a single C<$value> (which may be a scalar, list, or hash reference)
and returns a single C<$string> value.

Bencoding is the BitTorrent protocol's basic serialization and data
organization format. The specification supports integers, lists (arrays),
dictionaries (hashes), and byte strings.

=head2 C<< $data = B<bdecode>( $string ) >>

Expects a bencoded C<$string>. The return value depends on the type of data
contained in the C<$string>.

=head2 C<< ( $data, $leftovers ) = B<bdecode>( $string, 1 ) >>

Expects a bencoded C<$string>. The return value depends on the type of data
contained in the C<$string>.

This form returns a second value which is any extra data found in the original
C<$string>. Unless your input is malformed, C<$leftovers> will be an empty
string.

=head1 See Also

=over

=item L<The BitTorrent Protocol Specification|http://bittorrent.org/beps/bep_0003.html#the-connectivity-is-as-follows>

=item Other Bencode related modules:

=over

=item L<Convert::Bencode|Convert::Bencode>

=item L<Bencode|Bencode>

=item L<Convert::Bencode_XS|Convert::Bencode_XS>

=back

=back

=head1 TODO

=over

=item I would like to benchmark my versions of bencode and bdecode against the
L<other public versions|/"See Also"> written in Perl.

=back

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

=for :html Copyright (C) 2008-2010 by Sanko Robinson
<L<sanko@cpan.org|mailto://sanko@cpan.org>>

=for :text Copyright (C) 2008-2010 by Sanko Robinson <sanko@cpan.org>

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
