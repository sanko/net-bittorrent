package Net::BitTorrent::Protocol::BEP03::Metadata;
{
    use 5.010;
    use Moose;
    our $MAJOR = 0.074; our $MINOR = 1; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    #
    use lib '../../../../';
    use Net::BitTorrent::Types qw[:bencode :metadata :url];

    #
    sub BUILD {1}

    #
    has 'name' => (isa      => 'Str',
                   is       => 'ro',
                   required => 1
    );
    has 'announce' => (isa        => 'Maybe[Net::BitTorrent::Types::URL]',
                       is         => 'ro',
                       lazy_build => 1
    );
    sub _build_announce { () }
    has 'files' => (isa => 'ArrayRef[Net::BitTorrent::Types::Metadata::File]',
                    is  => 'ro',
                    traits   => ['Array'],
                    handles  => {_count_files => 'count'},
                    required => 1
    );
    has 'pieces' => (isa      => 'Net::BitTorrent::Types::Metadata::Pieces',
                     is       => 'ro',
                     required => 1,
                     coerce   => 1,
                     traits   => ['Array'],
                     handles  => {_count_pieces => 'count'}
    );
    after 'BUILD' => sub {
        my $s        = shift;
        my $x        = $s->size / $s->piece_length;
        my $expected = int $x + (int($x) < $x);
        confess sprintf
            'Invalid pieces value: Expected %d pieces but found %d',
            $expected, $s->_count_pieces
            if $s->_count_pieces != $expected;
    };
    has 'piece_length' => (
                      isa => 'Net::BitTorrent::Types::Metadata::Piece_Length',
                      is  => 'ro',
                      required => 1
    );
    has 'info_hash' => (isa => 'Net::BitTorrent::Types::Metadata::Infohash',
                        is  => 'ro',
                        lazy_build => 1,
                        init_arg   => undef,
                        coerce     => 1
    );

    sub _build_info_hash {
        my $s = shift;
        #<<< perltidy will skip this
        state $bencode_constraint //=
        #>>>
            Moose::Util::TypeConstraints::find_type_constraint(
                                           'Net::BitTorrent::Types::Bencode');
        require Digest::SHA;
        Digest::SHA::sha1(
               $bencode_constraint->coerce($s->_prepared_metadata->{'info'}));
    }
    has '_prepared_metadata' => (
        isa        => 'Net::BitTorrent::Types::Bdecode',
        is         => 'ro',
        lazy_build => 1,
        init_arg   => undef,
        coerce     => 1,
        handles    => {
            as_string => sub {
                my $s = shift;
                #<<< perltidy will skip this
                state $bencode_constraint //=
                #>>>
                    Moose::Util::TypeConstraints::find_type_constraint(
                                           'Net::BitTorrent::Types::Bencode');
                $bencode_constraint->coerce($s->_prepared_metadata);
                }
        }
    );

    sub _build__prepared_metadata {
        my $s = shift;
        my $r = {($s->has_announce ? (announce => $s->announce) : ()),
                 info => {($s->_count_files == 1
                               && scalar @{$s->files->[0]->{'path'}} == 1
                           ? (length => $s->files->[0]->{'length'})
                           : (files => $s->files)
                          )
                 }
        };
        $r->{'info'}{'piece length'} = $s->piece_length;
        $r->{'info'}{'pieces'} = pack 'H*', join '',
            map { $_->to_Hex } @{$s->pieces};
        $r->{'info'}{'name'} = $s->name;
        return $r;
    }

    sub size {
        my $s = shift;
        my $x = 0;
        for my $f (@{$s->files}) { $x += $f->{'length'} }
        $x;
    }

    #
    __PACKAGE__->meta->make_immutable;
    no Moose;
}
1;

=pod

=head1 NAME

Net::BitTorrent::Protocol::BEP03::Metadata - Base Class Which Contains the Basic Torrent Metadata as Defined by BEP03

=head1 Synopsis

    use Net::BitTorrent::Protocol::BEP03::Metadata;
    my $torrent = Net::BitTorrent::Protocol::BEP03::Metadata->new(
        announce => 'http://example.com/announce.pl',
        files    => [
                  {path   => [qw[one k file.ext]],
                   length => 1024
                  },
                  {path => [qw[really tiny file.ext]],
                   length => 256
                  }
        ],
        name         => 'My New Torrent',
        piece_length => 2**15,
        pieces       => "\0" x 20
    );
    printf 'Saving %s.torrent... ', $torrent->info_hash->to_Hex; # 106783C6...
    open my $FH, '>', $torrent->info_hash->to_Hex . '.torrent' || die $!;
    syswrite $FH, $torrent->as_string || die $!;
    print close $FH ? 'okay' : 'fail!';

=head1 Description

Taken from L<BEP03|Net::BitTorrent::Protocol::BEP03/"Metainfo files are bencoded dictionaries with the following keys:">...

=over

=item announce

The URL of the tracker.

=item info

This maps to a dictionary, with keys described below.

=over

=item The C<name> key maps to a UTF-8 encoded string which is the suggested
name to save the file (or directory) as. It is purely advisory.

=item C<piece length> maps to the number of bytes in each piece the file is
split into. For the purposes of transfer, files are split into fixed-size
pieces which are all the same length except for possibly the last one which
may be truncated. C<piece length> is almost always a power of two, most
commonly C<2^18 = 256K> (BitTorrent prior to version C<3.2> uses C<2^20 = 1M>
as default).

=item C<pieces> maps to a string whose length is a multiple of C<20>. It is to
be subdivided into strings of length C<20>, each of which is the SHA1 hash of
the piece at the corresponding index.

=item There is also a key C<length> or a key C<files>, but not both or
neither. If C<length> is present then the download represents a single file,
otherwise it represents a set of files which go in a directory structure.

=item In the single file case, C<length> maps to the length of the file in
bytes.

For the purposes of the other keys, the multi-file case is treated as only
having a single file by concatenating the files in the order they appear in
the files list. The files list is the value C<files> maps to, and is a list of
dictionaries containing the following keys:

=over

=item C<length>

The length of the file, in bytes.

=item C<path>

A list of UTF-8 encoded strings corresponding to subdirectory names, the last
of which is the actual file name (a zero length list is an error case).

=back

In the single file case, the name key is the name of a file, in the muliple
file case, it's the name of a directory.

=back

=back

All strings in a .torrent file that contains text must be UTF-8 encoded.

=head1 Methods

In. Out. That's all there is.

=head2 C<< $metadata = Net::BitTorrent::Protocol::BEP03::Metadata->B<new>( ... ) >>

Creates a new object. Supported arguments include:

=over

=item L<C<announce>|/"announce">

This value maps directly to the L<C<announce>|/"announce"> metadata key.

=item C<files> (required)

The constructor expects this to be a list of hashrefs
(C<{ path => [...] , length => ... }>). When the metadata is generated, the
correct type of data is created (multi vs. single file).

=item C<piece_length> (required)

This value maps directly to the L<C<pieces length>|/"pieces length"> metadata
key.

=item C<pieces> (required)

This is a list of L<Bit::Vector> objects. ...but a single string will be
coerced for you.

=item C<name> (required)

This is a string.

=back

=head2 C<< $infohash = $metadata->B<info_hash>( ) >>

Returns a L<Bit::Vector> object which contains the C<20> byte SHA1 hash of the
bencoded form of the info value from the metainfo file. Note that this is a
substring of the metainfo file.

=head2 C<< $torrent = $metadata->B<as_string>( ) >>

Returns the medatada bencoded into a string. This string is ready for storage
as a C<[blah].torrent> file.

=head2 C<< $bytes = $metadata->B<size>( ) >>

Returns the collective size of all included files.

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
