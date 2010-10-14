package t::Net::BitTorrent::Protocol::BEP03::Bencode;
{
    use strict;
    use warnings;

    # Load standard modules
    use Module::Build;
    use Test::More;
    use parent 'Test::Class';

    # Load local context
    -d '_build' ? last : chdir '..' for 1 .. 10;
    my $t_builder = Test::More->builder;
    my $m_builder = Module::Build->current;

    # Load local modules
    use lib '../../../../../../lib', 'lib';
    use Net::BitTorrent::Protocol::BEP03::Bencode qw[:all];

    #
    sub bencode_integer : Test( 9 ) {
        is bencode(4),   'i4e',   'integer';
        is bencode(0),   'i0e',   'zero';
        is bencode(-0),  'i0e',   'zero w/ sign';
        is bencode(-10), 'i-10e', 'negative integer';
        is bencode(+10), 'i10e',  'positive integer';
        is bencode(time x 50), 'i' . (time x 50) . 'e', 'large number';

        # From BEP03
        is bencode(3),  'i3e',  q[bencode 3];
        is bencode(-3), 'i-3e', q[bencode -3];
        is bencode('-0'), '2:-0',
            q[bencode '-0' returns valid string-type (is this okay?)];
    }

    sub bencode_string : Test( 6 ) {
        is bencode('Perl'), '4:Perl', 'string';
        is bencode(''),     '0:',     'null string';
        is bencode(undef),  undef,    'undef';
        is bencode(\undef), '', 'ref to undef';
        is bencode('0:0:'), '4:0:0:', 'odd string (malformed bencoded int)';

        # From BEP03
        is bencode('spam'), '4:spam', q[bencode 'spam'];
    }

    sub bencode_list : Test( 6 ) {
        is bencode([1, 2, 3]), 'li1ei2ei3ee', 'list (integers)';
        is bencode([qw[one two three]]), 'l3:one3:two5:threee',
            'list (strings)';
        is bencode([qw[three 1 2 3 one two]]), 'l5:threei1ei2ei3e3:one3:twoe',
            'list (mixed scalars)';
        is bencode([]), 'le', 'empty list';
        is bencode([[qw[Alice Bob]], [2, 3]]), 'll5:Alice3:Bobeli2ei3eee',
            'list^list';

        # From BEP03
        is bencode(['spam', 'eggs']), 'l4:spam4:eggse',
            q[bencode ['spam', 'eggs']];
    }

    sub bencode_dictionary : Test( 6 ) {
        is bencode({date => {month => 'January', year => 2009}}),
            'd4:dated5:month7:January4:yeari2009eee', 'dictionary';
        is bencode({}), 'de', 'dictionary from empty hash';
        is bencode({age => 25, eyes => 'blue'}), 'd3:agei25e4:eyes4:bluee',
            'dictionary from anon hash';
        is length bencode({join('', map(chr($_), 0 .. 255)) =>
                               join('', map(chr($_), 0 .. 255))
                          }
            ),
            522, 'anon hash with long key/value pair';

        # From BEP03
        is bencode({'cow' => 'moo', 'spam' => 'eggs'}),
            'd3:cow3:moo4:spam4:eggse',
            q[bencode {'cow' => 'moo', 'spam' => 'eggs'}];
        is bencode({'spam' => ['a', 'b']}), 'd4:spaml1:a1:bee',
            q[bencode {'spam'=> ['a', 'b']}];
    }

    sub bencode_complex : Test( 1 ) {
        is bencode(
                 {e => 0, m => {}, p => 48536, v => "\xC2\xB5Torrent 1.7.7"}),
            'd1:ei0e1:mde1:pi48536e1:v15:µTorrent 1.7.7e',
            'bencode complex structure (empty dictionary, "safe" hex chars';
    }

    sub bencode_unsupported : Test( 1 ) {
        is bencode(
            {key => sub { return 'value' }
            }
            ),
            'd3:keye', 'coderefs';
    }

    sub _string_for_bdecode {
        'd7:Integeri42e4:Listl6:item 1i2ei3ee6:String9:The Valuee';
    }

    sub bdecode_integer : Test( 11 ) {
        is bdecode('i4e'),   4,   'integer';
        is bdecode('i-10e'), -10, 'negative integer';
        is_deeply [bdecode('i')],           [], 'aborted integer';
        is_deeply [bdecode('i0')],          [], 'unterminated integer';
        is_deeply [bdecode('ie')],          [], 'empty integer';
        is_deeply [bdecode('i341foo382e')], [], 'malformed integer';
        is_deeply [bdecode('i123')],        [], 'unterminated integer';

        # From BEP03
        is bdecode('i-3e'), -3,    'bdecode i-3e';
        is bdecode('i-0e'), undef, 'bdecode i-0e';
        is bdecode('i03e'), undef, 'bdecode i03e';
        is bdecode('i0e'),  0,     'bdecode i0e';
    }

    sub bdecode_string : Test( 12 ) {
        is bdecode(''),              undef,        'Empty string';
        is bdecode('0:'),            '',           'zero length string';
        is bdecode('3:abc'),         'abc',        'string';
        is bdecode('10:1234567890'), '1234567890', 'integer cast as string';
        is bdecode('02:xy'), undef, 'string with leading zero in length';
        is bdecode('0:0:'), '',
            'trailing junk at end of valid bencoded string';

        # From BEP03
        is bdecode('4:spam'), 'spam', 'bdecode 4:spam';

        # Error handling
        is_deeply [bdecode('35208734823ljdahflajhdf')], [],
            'garbage looking vaguely like a string, with large count';
        is_deeply [bdecode('1:')], [], 'string longer than data';
        is_deeply [bdecode('i6easd', 1)], [6, 'asd'],
            'string with trailing junk';
        is_deeply [bdecode('2:abfdjslhfld', 1)], ['ab', 'fdjslhfld'],
            'string with trailing garbage';
        is_deeply [bdecode('02:xy')], [],
            'string with extra leading zero in count';
    }

    sub bdecode_list : Test( 4 ) {
        is_deeply scalar bdecode(bencode([qw[this that and the other]])),
            [qw[this that and the other]], 'list in scalar context';

        # From BEP03
        is_deeply [bdecode 'l4:spam4:eggse'], [['spam', 'eggs']],
            'bdecode l4:spam4:eggse';

        # Error handling
        is_deeply [bdecode('l')], [[]], 'unclosed empty list';
        is_deeply [bdecode('leanfdldjfh', 1)], [[], 'anfdldjfh'],
            'empty list with trailing garbage';
    }

    sub bdecode_dictionary : Test( 19 ) {
        my $s = shift;
        my ($hashref) = bdecode($s->_string_for_bdecode);
        ok defined $hashref, 'bdecode() returned something';
        is ref $hashref, 'HASH', 'bdecode() returned a valid hash ref';
        ok defined $hashref->{'Integer'}, 'Integer key present';
        is $hashref->{'Integer'}, 42, '  and its the correct value';
        ok defined $hashref->{'String'}, 'String key present';
        is ${$hashref;}{'String'}, 'The Value', '  and its the correct value';
        ok defined $hashref->{'List'}, 'List key present';
        is @{$hashref->{'List'}}, 3, '  list has 3 elements';
        is ${$hashref->{'List'}}[0], 'item 1', '    first element correct';
        is ${$hashref->{'List'}}[1], 2,        '    second element correct';
        is ${$hashref->{'List'}}[2], 3,        '    third element correct';
        my ($encoded_string) = bencode($hashref);
        ok defined $encoded_string, 'bencode() returned something';
        is $encoded_string, $s->_string_for_bdecode,
            '  and it appears to be the correct value';
        is_deeply scalar bdecode(bencode({qw[this that the other]})),
            {qw[this that the other]}, 'dictionary in scalar context';

        # From BEP03
        is_deeply [bdecode 'd3:cow3:moo4:spam4:eggse'],
            [{'cow' => 'moo', 'spam' => 'eggs'}],
            'bdecode d3:cow3:moo4:spam4:eggse';
        is_deeply [bdecode 'd4:spaml1:a1:bee'], [{'spam' => ['a', 'b']}],
            'bdecode d4:spaml1:a1:bee';

        # Error handling
        is_deeply [bdecode('d')], [{}], 'unclosed empty dict';
        is_deeply [bdecode('defoobar', 1)], [{}, 'foobar'],
            'Catch invalid format (empty dictionary w/ trailing garbage)';
        is_deeply [bdecode('d3:fooe', 1)], [{foo => undef}, undef],
            'Catch invalid format (dictionary w/ empty key)';
    }

    sub bdecode_complex : Test( 1 ) {
        is_deeply [bdecode('d1:ei0e1:mde1:pi48536e1:v15:µTorrent 1.7.7e')],
            [{e => 0, m => {}, p => 48536, v => "\xC2\xB5Torrent 1.7.7"}],
            'bdecode complex structure (empty dictionary, "safe" hex chars';
    }

    sub bdecode_unsupported : Test( 1 ) {
        is_deeply [bdecode('relwjhrlewjh')], [], 'complete garbage';
    }

    #
    __PACKAGE__->runtests() if !caller;
}
1;

=pod

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
