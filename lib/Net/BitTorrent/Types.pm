package Net::BitTorrent::Types;
{
    use 5.010;
    #use Moose;
    use Moose::Util::TypeConstraints;
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use vars qw[@EXPORT_OK %EXPORT_TAGS];
    use Exporter qw[];
    *import = *import = *Exporter::import;
    %EXPORT_TAGS = (
        #infohash => [qw[Net::BitTorrent::Type::Infohash Net::BitTorrent::Type::Infohash::Packed]],
        #tracker  => [
        #    qw[ Net::BitTorrent::Type::Tracker      Net::BitTorrent::Type::Tracker::Tier
        #        Net::BitTorrent::Type::Tracker::UDP Net::BitTorrent::Type::Tracker::HTTP
        #        Net::BitTorrent::Type::Tracker::HTTP::Event]
        #],
        #file => [
        #    qw[Net::BitTorrent::Type::File::Open::Permission
        #        Net::BitTorrent::Type::File::Path
        #        Net::BitTorrent::Type::File::Path::Absolute
        #        Net::BitTorrent::Type::File::Path::PreExisting
        #        Net::BitTorrent::Type::File::Directory::PreExisting
        #        ]
        #],
        #client  => [qw[Net::BitTorrent::Type::Client::PeerID]],
        #dht     => [qw[Net::BitTorrent::Type::DHT::NodeID]],
        bencode => [qw[Net::BitTorrent::Type::Bencode Net::BitTorrent::Type::Bdecode]],
        #torrent => [
        #    qw[Net::BitTorrent::Type::Torrent::Status Net::BitTorrent::Type::Torrent::Infohash
        #        Net::BitTorrent::Type::Torrent::Bitfield]
        #],
        #addr => [qw[Net::BitTorrent::Type::Network::Paddr Net::BitTorrent::Type::Network::Addr]]
    );
    @EXPORT_OK = sort map { @$_ = sort @$_; @$_ } values %EXPORT_TAGS;
    $EXPORT_TAGS{'all'} = \@EXPORT_OK;    # When you want to import everything
    subtype 'Net::BitTorrent::Type::Bencode' => as 'Str';
    subtype 'Net::BitTorrent::Type::Bdecode' => as 'Ref';
    coerce 'Net::BitTorrent::Type::Bencode'  => from 'Net::BitTorrent::Type::Bdecode' => via {
        require Net::BitTorrent::Protocol::BEP03::Bencode;
        Net::BitTorrent::Protocol::BEP03::Bencode::bencode($_);
    };
    coerce 'Net::BitTorrent::Type::Bdecode' => from 'Net::BitTorrent::Type::Bencode' => via {
        require Net::BitTorrent::Protocol::BEP03::Bencode;
        Net::BitTorrent::Protocol::BEP03::Bencode::bdecode($_);
    };
=pod
    # Nearly the same as Net::BitTorrent::Type::DHT::NodeID
    subtype 'Net::BitTorrent::Type::Torrent::Infohash' => as 'Bit::Vector' =>
        where { $_->Size == 160 } =>
        message {'Torrent info_hashes are 160-bit integers.'};
    coerce 'Net::BitTorrent::Type::Torrent::Infohash' =>
        from subtype(as 'Int' => where { length $_ < 40 }) =>
        via { require Bit::Vector; Bit::Vector->new_Dec(160, $_) } =>
        from subtype(as 'Str' => where { length $_ == 40 && /^[a-f\d]+$/i }
        ) => via { require Bit::Vector; Bit::Vector->new_Hex(160, $_) } =>
        from 'Str' => via {
        require Bit::Vector;
        Bit::Vector->new_Hex(160, unpack 'H*', $_);
        };
    subtype 'Net::BitTorrent::Type::Torrent::Bitfield' => as 'Bit::Vector';
    coerce 'Net::BitTorrent::Type::Torrent::Bitfield' =>
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
    subtype 'Net::BitTorrent::Type::Tracker::HTTP' => as
        'Net::BitTorrent::Protocol::BEP03::Tracker::HTTP';
    coerce 'Net::BitTorrent::Type::Tracker::HTTP' =>
        from subtype(as 'Str' => where {m[^http://]i}) => via {
        require Net::BitTorrent::Protocol::BEP03::Tracker::HTTP;
        return Net::BitTorrent::Protocol::BEP03::Tracker::HTTP->new(
                                                                   url => $_);
        };
    subtype 'Net::BitTorrent::Type::Tracker::UDP' => as
        'Net::BitTorrent::Protocol::BEP15::Tracker::UDP';
    coerce 'Net::BitTorrent::Type::Tracker::UDP' =>
        from subtype(as 'Str' => where {m[^udp://]i}) => via {
        require Net::BitTorrent::Protocol::BEP15::Tracker::UDP;
        return Net::BitTorrent::Protocol::BEP15::Tracker::UDP->new(url => $_);
        };
    subtype 'Net::BitTorrent::Type::Tracker::Tier' => as
        'ArrayRef[Net::BitTorrent::Type::Tracker::UDP|Net::BitTorrent::Type::Tracker::HTTP]';
    coerce 'Net::BitTorrent::Type::Tracker::Tier' => from 'ArrayRef[Str]' => via {
        state $tracker_constraint
            = Moose::Util::TypeConstraints::find_type_constraint(
                              'Net::BitTorrent::Type::Tracker::HTTP|Net::BitTorrent::Type::Tracker::UDP');
        [map { $tracker_constraint->coerce($_) } @$_];
    };
    enum 'Net::BitTorrent::Type::Tracker::HTTP::Event' => qw[started stopped completed];

    #
    enum 'Net::BitTorrent::Type::File::Open::Permission'  => qw[ro wo rw];
    subtype 'Net::BitTorrent::Type::File::Path'           => as 'Str';
    subtype 'Net::BitTorrent::Type::File::Path::Absolute' => as 'Str' =>
        where { require File::Spec; File::Spec->file_name_is_absolute($_) } =>
        message {'Filename must be absolute.'};
    coerce 'Net::BitTorrent::Type::File::Path::Absolute' => from 'Str' =>
        via { require File::Spec; File::Spec->rel2abs($_); };
    subtype 'Net::BitTorrent::Type::File::Path::PreExisting' => as 'Str' =>
        where { require File::Spec; File::Spec->file_name_is_absolute($_) } =>
        message {'Filename must be absolute.'} => where { -f $_ } =>
        message {'File must be preexisting'};
    subtype 'Net::BitTorrent::Type::File::Directory::PreExisting' => as 'Str' =>
        where { require File::Spec; File::Spec->file_name_is_absolute($_) } =>
        message {'Directory must be absolute.'} => where { -d $_ } =>
        message {'Directory must be preexisting'};
    coerce 'Net::BitTorrent::Type::File::Path::PreExisting' => from 'Str' =>
        via { require File::Spec; File::Spec->rel2abs($_); };
    coerce 'Net::BitTorrent::Type::File::Directory::PreExisting' => from 'Str' =>
        via { require File::Spec; File::Spec->rel2abs($_); };

    #
    subtype 'Net::BitTorrent::Type::Client::PeerID' => as 'Str' =>
        where { length $_ == 20 } =>
        message {'PeerID is malformed: length != 20'};

    # Nearly the same as Net::BitTorrent::Type::Torrent::Infohash
    subtype 'Net::BitTorrent::Type::DHT::NodeID' => as 'Bit::Vector' =>
        where { $_->Size == 160 } =>
        message {'DHT NodeIDs are 160-bit integers.'};
    coerce 'Net::BitTorrent::Type::DHT::NodeID' =>
        from subtype(as 'Int' => where { length $_ < 40 }) =>
        via { require Bit::Vector; Bit::Vector->new_Dec(160, $_) } =>
        from subtype(as 'Str' => where { length $_ == 40 && /^[a-f\d]+$/i }
        ) => via { require Bit::Vector; Bit::Vector->new_Hex(160, $_) } =>
        from 'Str' => via {
        require Bit::Vector;
        Bit::Vector->new_Hex(160, unpack 'H*', $_);
        };

    # IPv6 packed address
    subtype 'Net::BitTorrent::Type::Network::Paddr' => as 'Str' =>
        where { length $_ == 16 } =>
        message { sprintf '%s is not 16 bytes', $_ };
    coerce 'Net::BitTorrent::Type::Network::Paddr' => from 'Str' => via {
        require Net::BitTorrent::Network::Utility;
        Net::BitTorrent::Network::Utility::ip2paddr($_);
    };

    #
    subtype 'Net::BitTorrent::Type::Network::Addr' => as 'ArrayRef' =>
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
