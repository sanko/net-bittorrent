package Net::BitTorrent::Types;
{
    use strict;
    use warnings;
    use Moose::Util::TypeConstraints;
    use 5.010;

    #
    our $MAJOR = 0; our $MINOR = 74; our $DEV = 13; our $VERSION = sprintf('%0d.%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    #
    use Exporter qw[import];
    my %_exports = (

#infohash => [qw[Net::BitTorrent::Types::Infohash Net::BitTorrent::Types::Infohash::Packed]],
        tracker => [
            qw[ Tracker      Tracker::Tier
                Tracker::UDP Tracker::HTTP
                Tracker::HTTP::Event]
        ],

        #file => [
        #    qw[Net::BitTorrent::Types::File::Open::Permission
        #        Net::BitTorrent::Types::File::Path
        #        Net::BitTorrent::Types::File::Path::Absolute
        #        Net::BitTorrent::Types::File::Path::PreExisting
        #        Net::BitTorrent::Types::File::Directory::PreExisting
        #        ]
        #],
        peer => [qw[PeerID]],

        #dht     => [qw[Net::BitTorrent::Types::DHT::NodeID]],
        bencode  => [qw[Bencode Bdecode]],
        metadata => [
            qw[ Metadata::File Metadata::Pieces Metadata::Piece_Length
                Metadata::Infohash]
        ],
        url => [qw[URL URL::HTTP URL::HTTPS URL::UDP]],

#torrent => [
#    qw[Net::BitTorrent::Types::Torrent::Status
#        Net::BitTorrent::Types::Torrent::Bitfield]
#],
#addr => [qw[Net::BitTorrent::Types::Network::Paddr Net::BitTorrent::Types::Network::Addr]]
    );
    our %EXPORT_TAGS = map {
        $_ => [map { 'Net::BitTorrent::Types::' . $_ } @{$_exports{$_}}]
    } keys %_exports;
    our @EXPORT_OK = sort map {@$_} values %EXPORT_TAGS;
    $EXPORT_TAGS{'all'} = \@EXPORT_OK;    # When you want to import everything

    #
    subtype 'Net::BitTorrent::Types::Bencode' => as 'Str';
    subtype 'Net::BitTorrent::Types::Bdecode' => as 'Ref';
    coerce 'Net::BitTorrent::Types::Bencode'  => from
        'Net::BitTorrent::Types::Bdecode'     => via {
        require Net::BitTorrent::Protocol::BEP03::Bencode;
        Net::BitTorrent::Protocol::BEP03::Bencode::bencode($_);
        };
    coerce 'Net::BitTorrent::Types::Bdecode' => from
        'Net::BitTorrent::Types::Bencode'    => via {
        require Net::BitTorrent::Protocol::BEP03::Bencode;
        Net::BitTorrent::Protocol::BEP03::Bencode::bdecode($_);
        };

    #
    subtype 'Net::BitTorrent::Types::Metadata::File' => as 'HashRef' =>
        where {
        keys %$_ >= 2
            && defined $_->{'path'}
            && ref $_->{'path'} eq 'ARRAY'
            && scalar @{$_->{'path'}}
            && defined $_->{'length'}
            && $_->{'length'} !~ m[\D]
            && $_->{'length'} > 0;
        } => message {
        'The file should look like { path => [qw[dir dir name.ext]], length => 1024 }';
        };
    coerce 'Net::BitTorrent::Types::Metadata::File' => from 'Str' => via {
        require File::Spec;
        {
            length   => -s $_,
                path => [File::Spec->splitdir(File::Spec->cannonpath($_))]
        }
    };
    coerce subtype(
        'ArrayRef.Net::BitTorrent::Types::Metadata::File' => as 'ArrayRef') =>
        from 'ArrayRef[Str]' => via {
        #<<< perltidy will skip this
        state $metadata_file_constraint //=
        #>>>
            Moose::Util::TypeConstraints::find_type_constraint(
                                    'Net::BitTorrent::Types::Metadata::File');
        map { $metadata_file_constraint->coerce($_) } @$_;
        };

    #
    subtype 'Net::BitTorrent::Types::Metadata::Piece' => as 'Bit::Vector';
    subtype 'Net::BitTorrent::Types::Metadata::Piece' => as 'Bit::Vector' =>
        where { $_->Size == 160 } =>
        message {'Torrent pieces are 160-bit integers.'};
    coerce 'Net::BitTorrent::Types::Metadata::Piece' =>
        from subtype(as 'Int' => where { length $_ < 40 }) =>
        via { require Bit::Vector; Bit::Vector->new_Dec(160, $_) } =>
        from subtype(as 'Str' => where { length $_ == 40 && /^[a-f\d]+$/i }
        ) => via { require Bit::Vector; Bit::Vector->new_Hex(160, $_) } =>
        from 'Str' => via {
        require Bit::Vector;
        Bit::Vector->new_Hex(160, unpack 'H*', $_);
        };
    subtype 'Net::BitTorrent::Types::Metadata::Pieces' => as
        'ArrayRef[Net::BitTorrent::Types::Metadata::Piece]';
    coerce 'Net::BitTorrent::Types::Metadata::Pieces' => from 'Str' => via {
            #<<< perltidy will skip this
            state $piece_constraint //=
            #>>>
            Moose::Util::TypeConstraints::find_type_constraint(
                                   'Net::BitTorrent::Types::Metadata::Piece');
        [map { $piece_constraint->coerce($_) } shift =~ m[\G(.{20})]g];
    };

    #
    subtype 'Net::BitTorrent::Types::Metadata::Piece_Length' => as 'Int' =>
        where { $_ > 0 } => message {'Value is not a positive integer'};

    #
    subtype 'Net::BitTorrent::Types::URL::HTTP' => as 'Str' =>
        where {m[^http://]i} => message {'HTTP URLs must begin with http://'};
    subtype 'Net::BitTorrent::Types::URL::HTTPS' => as 'Str' =>
        where {m[^https://]i} =>
        message {'HTTPS URLs must begin with https://'};
    subtype 'Net::BitTorrent::Types::URL::UDP' => as 'Str' =>
        where {m[^udp://]i} => message {'UDP URLs must begin with udp://'};
    subtype 'Net::BitTorrent::Types::URL' => as
        'Net::BitTorrent::Types::URL::HTTP|Net::BitTorrent::Types::URL::HTTPS|Net::BitTorrent::Types::URL::UDP'
        => message {'URL does not match any of the supported forms'};

    # Nearly the same as Net::BitTorrent::Types::DHT::NodeID
    subtype 'Net::BitTorrent::Types::Metadata::Infohash' => as
        'Bit::Vector' => where { $_->Size == 160 } =>
        message {'Torrent info_hashes are 160-bit integers.'};
    coerce 'Net::BitTorrent::Types::Metadata::Infohash' =>
        from subtype(as 'Int' => where { length $_ < 40 }) =>
        via { require Bit::Vector; Bit::Vector->new_Dec(160, $_) } =>
        from subtype(as 'Str' => where { length $_ == 40 && /^[a-f\d]+$/i }
        ) => via { require Bit::Vector; Bit::Vector->new_Hex(160, $_) } =>
        from 'Str' => via {
        require Bit::Vector;
        Bit::Vector->new_Hex(160, unpack 'H*', $_);
        };
    subtype 'Net::BitTorrent::Types::Torrent::Bitfield' => as 'Bit::Vector';
    coerce 'Net::BitTorrent::Types::Torrent::Bitfield' =>
        from subtype(as 'Str' => where { $_ =~ m[^(?:[10]+)$] }) => via {
        require Bit::Vector;
        Bit::Vector->new_Bin(length($_), scalar reverse $_);
        },
        from subtype(as 'Str' => where { unpack('b*', $_) =~ m[^(?:[10]+)$] }
        ) => via {
        require Bit::Vector;
        my $unpack = scalar reverse unpack 'b*', $_;
        Bit::Vector->new_Bin(length $unpack, $unpack);
        };

    #
    subtype 'Net::BitTorrent::Types::Tracker::HTTP' => as
        'Net::BitTorrent::Protocol::BEP03::Tracker::HTTP';
    coerce 'Net::BitTorrent::Types::Tracker::HTTP' =>
        from subtype(as 'Str' => where {m[^http://]i}) => via {
        require Net::BitTorrent::Protocol::BEP03::Tracker::HTTP;
        return Net::BitTorrent::Protocol::BEP03::Tracker::HTTP->new(
                                                                   url => $_);
        };
    subtype 'Net::BitTorrent::Types::Tracker::UDP' => as
        'Net::BitTorrent::Protocol::BEP15::Tracker::UDP';
    coerce 'Net::BitTorrent::Types::Tracker::UDP' =>
        from subtype(as 'Str' => where {m[^udp://]i}) => via {
        require Net::BitTorrent::Protocol::BEP15::Tracker::UDP;
        return Net::BitTorrent::Protocol::BEP15::Tracker::UDP->new(url => $_);
        };
    subtype 'Net::BitTorrent::Types::Tracker::Tier' => as
        'ArrayRef[Net::BitTorrent::Types::Tracker::UDP|Net::BitTorrent::Types::Tracker::HTTP]';
    coerce 'Net::BitTorrent::Types::Tracker::Tier' => from 'ArrayRef[Str]' =>
        via {
        state $tracker_constraint
            = Moose::Util::TypeConstraints::find_type_constraint(
            'Net::BitTorrent::Types::Tracker::HTTP|Net::BitTorrent::Types::Tracker::UDP'
            );
        [map { $tracker_constraint->coerce($_) } @$_];
        };

=pod



    enum 'Net::BitTorrent::Types::Tracker::HTTP::Event' => qw[started stopped completed];

    #
    enum 'Net::BitTorrent::Types::File::Open::Permission'  => qw[ro wo rw];
    subtype 'Net::BitTorrent::Types::File::Path'           => as 'Str';
    subtype 'Net::BitTorrent::Types::File::Path::Absolute' => as 'Str' =>
        where { require File::Spec; File::Spec->file_name_is_absolute($_) } =>
        message {'Filename must be absolute.'};
    coerce 'Net::BitTorrent::Types::File::Path::Absolute' => from 'Str' =>
        via { require File::Spec; File::Spec->rel2abs($_); };
    subtype 'Net::BitTorrent::Types::File::Path::PreExisting' => as 'Str' =>
        where { require File::Spec; File::Spec->file_name_is_absolute($_) } =>
        message {'Filename must be absolute.'} => where { -f $_ } =>
        message {'File must be preexisting'};
    subtype 'Net::BitTorrent::Types::File::Directory::PreExisting' => as 'Str' =>
        where { require File::Spec; File::Spec->file_name_is_absolute($_) } =>
        message {'Directory must be absolute.'} => where { -d $_ } =>
        message {'Directory must be preexisting'};
    coerce 'Net::BitTorrent::Types::File::Path::PreExisting' => from 'Str' =>
        via { require File::Spec; File::Spec->rel2abs($_); };
    coerce 'Net::BitTorrent::Types::File::Directory::PreExisting' => from 'Str' =>
        via { require File::Spec; File::Spec->rel2abs($_); };

=cut

    #
    subtype 'Net::BitTorrent::Types::PeerID' => as 'Str' =>
        where { length $_ == 20 } =>
        message {'PeerID is malformed: length != 20'};

=pod

    # Nearly the same as Net::BitTorrent::Types::Torrent::Infohash
    subtype 'Net::BitTorrent::Types::DHT::NodeID' => as 'Bit::Vector' =>
        where { $_->Size == 160 } =>
        message {'DHT NodeIDs are 160-bit integers.'};
    coerce 'Net::BitTorrent::Types::DHT::NodeID' =>
        from subtype(as 'Int' => where { length $_ < 40 }) =>
        via { require Bit::Vector; Bit::Vector->new_Dec(160, $_) } =>
        from subtype(as 'Str' => where { length $_ == 40 && /^[a-f\d]+$/i }
        ) => via { require Bit::Vector; Bit::Vector->new_Hex(160, $_) } =>
        from 'Str' => via {
        require Bit::Vector;
        Bit::Vector->new_Hex(160, unpack 'H*', $_);
        };

    # IPv6 packed address
    subtype 'Net::BitTorrent::Types::Network::Paddr' => as 'Str' =>
        where { length $_ == 16 } =>
        message { sprintf '%s is not 16 bytes', $_ };
    coerce 'Net::BitTorrent::Types::Network::Paddr' => from 'Str' => via {
        require Net::BitTorrent::Network::Utility;
        Net::BitTorrent::Network::Utility::ip2paddr($_);
    };

    #
    subtype 'Net::BitTorrent::Types::Network::Addr' => as 'ArrayRef' =>
        where { $#{$_[0]} == 1 }   => message {'looking for [host, port]'} =>
        where { defined $_[0][0] } => message {'hostname is missing'} =>
        where { defined $_[0][1] } => message {'port is missing'} =>
        where { $_[0][1] =~ m[^\d+$] } => message {'malformed port'};

=cut

    #
    no Moose::Util::TypeConstraints;
}
1;

=pod

=head1 NAME

Net::BitTorrent::Types - Moose Types

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
