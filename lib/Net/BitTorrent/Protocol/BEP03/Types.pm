package Net::BitTorrent::Protocol::BEP03::Types;
{
    use strict;
    use warnings;
    use Moose::Util::TypeConstraints;
    use 5.010;

    #
    our $MAJOR = 0; our $MINOR = 74; our $DEV = 14; our $VERSION = sprintf('%0d.%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    #
    subtype 'Net::BitTorrent::Protocol::BEP03::Types::Bencode' => as 'Str';
    subtype 'Net::BitTorrent::Protocol::BEP03::Types::Bdecode' => as 'Ref';
    coerce 'Net::BitTorrent::Protocol::BEP03::Types::Bencode'  => from
        'Net::BitTorrent::Protocol::BEP03::Types::Bdecode'     => via {
        require Net::BitTorrent::Protocol::BEP03::Bencode;
        Net::BitTorrent::Protocol::BEP03::Bencode::bencode($_);
        };
    coerce 'Net::BitTorrent::Protocol::BEP03::Types::Bdecode' => from
        'Net::BitTorrent::Protocol::BEP03::Types::Bencode'    => via {
        require Net::BitTorrent::Protocol::BEP03::Bencode;
        Net::BitTorrent::Protocol::BEP03::Bencode::bdecode($_);
        };

    #
    subtype 'Net::BitTorrent::Protocol::BEP03::Types::Metadata::File' => as
        'HashRef' => where {
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
    coerce 'Net::BitTorrent::Protocol::BEP03::Types::Metadata::File' => from
        'Str'                                                        => via {
        require File::Spec;
        {
            length   => -s $_,
                path => [File::Spec->splitdir(File::Spec->cannonpath($_))]
        }
        };
    coerce subtype(
         'ArrayRef.Net::BitTorrent::Protocol::BEP03::Types::Metadata::File' =>
             as 'ArrayRef[Object]') => from
        'ArrayRef[Net::BitTorrent::Protocol::BEP03::Types::Metadata::File]' =>
        via {
        require Net::BitTorrent::Protocol::BEP03::Storage::File;
        [map { Net::BitTorrent::Protocol::BEP03::Storage::File->new($_) }
         @$_];
        };

    #
    subtype 'Net::BitTorrent::Protocol::BEP03::Types::Metadata::Piece' => as
        'Bit::Vector';
    subtype 'Net::BitTorrent::Protocol::BEP03::Types::Metadata::Piece' => as
        'Bit::Vector' => where { $_->Size == 160 } =>
        message {'Torrent pieces are 160-bit integers.'};
    coerce 'Net::BitTorrent::Protocol::BEP03::Types::Metadata::Piece' =>
        from subtype(as 'Int' => where { length $_ < 40 }) =>
        via { require Bit::Vector; Bit::Vector->new_Dec(160, $_) } =>
        from subtype(as 'Str' => where { length $_ == 40 && /^[a-f\d]+$/i }
        ) => via { require Bit::Vector; Bit::Vector->new_Hex(160, $_) } =>
        from 'Str' => via {
        require Bit::Vector;
        Bit::Vector->new_Hex(160, unpack 'H*', $_);
        };
    subtype 'Net::BitTorrent::Protocol::BEP03::Types::Metadata::Pieces' => as
        'ArrayRef[Net::BitTorrent::Protocol::BEP03::Types::Metadata::Piece]';
    coerce 'Net::BitTorrent::Protocol::BEP03::Types::Metadata::Pieces' => from
        'Str' => via {
            #<<< perltidy will skip this
            state $piece_constraint //=
            #>>>
            Moose::Util::TypeConstraints::find_type_constraint(
                  'Net::BitTorrent::Protocol::BEP03::Types::Metadata::Piece');
        [map { $piece_constraint->coerce($_) } unpack '(A20)*', $_[0]];
        };

    #
    subtype
        'Net::BitTorrent::Protocol::BEP03::Types::Metadata::Piece_Length' =>
        as 'Int' => where { $_ > 0 } =>
        message {'Value is not a positive integer'};

    #
    subtype 'Net::BitTorrent::Protocol::BEP03::Types::URL::HTTP' => as
        'Str' => where {m[^http://]i} =>
        message {'HTTP URLs must begin with http://'};
    subtype 'Net::BitTorrent::Protocol::BEP03::Types::URL::HTTPS' => as
        'Str' => where {m[^https://]i} =>
        message {'HTTPS URLs must begin with https://'};
    subtype 'Net::BitTorrent::Protocol::BEP03::Types::URL::UDP' => as 'Str' =>
        where {m[^udp://]i} => message {'UDP URLs must begin with udp://'};
    subtype 'Net::BitTorrent::Protocol::BEP03::Types::URL' => as
        'Net::BitTorrent::Protocol::BEP03::Types::URL::HTTP|Net::BitTorrent::Protocol::BEP03::Types::URL::HTTPS|Net::BitTorrent::Protocol::BEP03::Types::URL::UDP'
        => message {'URL does not match any of the supported forms'};

    # Nearly the same as Net::BitTorrent::Protocol::BEP03::Types::DHT::NodeID
    subtype 'Net::BitTorrent::Protocol::BEP03::Types::Metadata::Infohash' =>
        as 'Bit::Vector' => where { $_->Size == 160 } =>
        message {'Torrent info_hashes are 160-bit integers.'};
    coerce 'Net::BitTorrent::Protocol::BEP03::Types::Metadata::Infohash' =>
        from subtype(as 'Int' => where { length $_ < 40 }) =>
        via { require Bit::Vector; Bit::Vector->new_Dec(160, $_) } =>
        from subtype(as 'Str' => where { length $_ == 40 && /^[a-f\d]+$/i }
        ) => via { require Bit::Vector; Bit::Vector->new_Hex(160, $_) } =>
        from 'Str' => via {
        require Bit::Vector;
        Bit::Vector->new_Hex(160, unpack 'H*', $_);
        };
    subtype 'Net::BitTorrent::Protocol::BEP03::Types::Torrent::Bitfield' => as
        'Bit::Vector';
    coerce 'Net::BitTorrent::Protocol::BEP03::Types::Torrent::Bitfield' =>
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
    subtype 'Net::BitTorrent::Protocol::BEP03::Types::Tracker::HTTP' => as
        'Net::BitTorrent::Protocol::BEP03::Tracker::HTTP';
    coerce 'Net::BitTorrent::Protocol::BEP03::Types::Tracker::HTTP' =>
        from subtype(as 'Str' => where {m[^http://]i}) => via {
        require Net::BitTorrent::Protocol::BEP03::Tracker::HTTP;
        return Net::BitTorrent::Protocol::BEP03::Tracker::HTTP->new(
                                                                   url => $_);
        };
    subtype 'Net::BitTorrent::Protocol::BEP03::Types::Tracker::UDP' => as
        'Net::BitTorrent::Protocol::BEP15::Tracker::UDP';
    coerce 'Net::BitTorrent::Protocol::BEP03::Types::Tracker::UDP' =>
        from subtype(as 'Str' => where {m[^udp://]i}) => via {
        require Net::BitTorrent::Protocol::BEP15::Tracker::UDP;
        return Net::BitTorrent::Protocol::BEP15::Tracker::UDP->new(url => $_);
        };
    enum 'Net::BitTorrent::Protocol::BEP03::Types::File::Open::Permission' =>
        qw[ro wo];

=pod



    enum 'Net::BitTorrent::Protocol::BEP03::Types::Tracker::HTTP::Event' => qw[started stopped completed];

    #
=cut
    subtype 'Net::BitTorrent::Protocol::BEP03::Types::File::Path' => as 'Str';
    subtype 'Net::BitTorrent::Protocol::BEP03::Types::File::Path::Absolute' =>
        as 'Str' =>
        where { require File::Spec; File::Spec->file_name_is_absolute($_) } =>
        message {'Filename must be absolute.'};
    coerce 'Net::BitTorrent::Protocol::BEP03::Types::File::Path::Absolute' =>
        from 'Str' => via { require File::Spec; File::Spec->rel2abs($_); };
    subtype
        'Net::BitTorrent::Protocol::BEP03::Types::File::Path::PreExisting' =>
        as 'Str' =>
        where { require File::Spec; File::Spec->file_name_is_absolute($_) } =>
        message {'Filename must be absolute.'} => where { -f $_ } =>
        message {'File must be preexisting'};
    subtype
        'Net::BitTorrent::Protocol::BEP03::Types::File::Directory::PreExisting'
        => as 'Str' =>
        where { require File::Spec; File::Spec->file_name_is_absolute($_) } =>
        message {'Directory must be absolute.'} => where { -d $_ } =>
        message {'Directory must be preexisting'};
    coerce
        'Net::BitTorrent::Protocol::BEP03::Types::File::Path::PreExisting' =>
        from 'Str' => via { require File::Spec; File::Spec->rel2abs($_); };
    coerce
        'Net::BitTorrent::Protocol::BEP03::Types::File::Directory::PreExisting'
        => from 'Str' => via { require File::Spec; File::Spec->rel2abs($_); };

    #
    subtype 'Net::BitTorrent::Protocol::BEP03::Types::PeerID' => as 'Str' =>
        where { length $_ == 20 } =>
        message {'PeerID is malformed: length != 20'};

=pod

    # Nearly the same as Net::BitTorrent::Protocol::BEP03::Types::Torrent::Infohash
    subtype 'Net::BitTorrent::Protocol::BEP03::Types::DHT::NodeID' => as 'Bit::Vector' =>
        where { $_->Size == 160 } =>
        message {'DHT NodeIDs are 160-bit integers.'};
    coerce 'Net::BitTorrent::Protocol::BEP03::Types::DHT::NodeID' =>
        from subtype(as 'Int' => where { length $_ < 40 }) =>
        via { require Bit::Vector; Bit::Vector->new_Dec(160, $_) } =>
        from subtype(as 'Str' => where { length $_ == 40 && /^[a-f\d]+$/i }
        ) => via { require Bit::Vector; Bit::Vector->new_Hex(160, $_) } =>
        from 'Str' => via {
        require Bit::Vector;
        Bit::Vector->new_Hex(160, unpack 'H*', $_);
        };

    # IPv6 packed address
    subtype 'Net::BitTorrent::Protocol::BEP03::Types::Network::Paddr' => as 'Str' =>
        where { length $_ == 16 } =>
        message { sprintf '%s is not 16 bytes', $_ };
    coerce 'Net::BitTorrent::Protocol::BEP03::Types::Network::Paddr' => from 'Str' => via {
        require Net::BitTorrent::Network::Utility;
        Net::BitTorrent::Network::Utility::ip2paddr($_);
    };

    #
    subtype 'Net::BitTorrent::Protocol::BEP03::Types::Network::Addr' => as 'ArrayRef' =>
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

Net::BitTorrent::Protocol::BEP03::Types - Moose Types

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
