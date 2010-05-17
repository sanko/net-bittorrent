package Net::BitTorrent::Types;
{
    use Moose;
    use Moose::Util::TypeConstraints;
    our $MAJOR = 0.075; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use vars qw[@EXPORT_OK %EXPORT_TAGS];
    use Exporter qw[];
    *import = *import = *Exporter::import;
    %EXPORT_TAGS = (
        infohash => [qw[NBTypes::Infohash NBTypes::Infohash::Packed]],
        tracker  => [
            qw[ NBTypes::Tracker      NBTypes::Tracker::Tier
                NBTypes::Tracker::UDP NBTypes::Tracker::HTTP
                NBTypes::Tracker::HTTP::Event]
        ],
        file   => [qw[NBTypes::Files NBTypes::File::Open::Permission]],
        cache  => [qw[NBTypes::Cache::Packet]],
        client => [qw[NBTypes::Client::PeerID]],
        dht    => [qw[NBTypes::DHT::NodeID]]
    );
    @EXPORT_OK = sort map { @$_ = sort @$_; @$_ } values %EXPORT_TAGS;
    $EXPORT_TAGS{'all'} = \@EXPORT_OK;    # When you want to import everything

    #
    subtype 'NBTypes::Infohash' => as 'Str' =>
        where { length $_ == 40 && /[a-f0-9]/i } => message {
        'Unpacked infohash must be 40 bytes long and contain only hex values'
        };
    subtype 'NBTypes::Infohash::Packed' => as 'Str' =>
        where { length $_ == 20 } =>
        message {'Unpacked infohash must be 20 bytes in length'};
    coerce 'NBTypes::Infohash' => from 'NBTypes::Infohash::Packed' =>
        via { uc unpack 'H*', $_ };
    coerce 'NBTypes::Infohash::Packed' => from 'NBTypes::Infohash' =>
        via { pack 'H*', $_ };

    #
    subtype 'NBTypes::Tracker::HTTP' => as
        'Net::BitTorrent::Protocol::BEP03::Tracker::HTTP';
    coerce 'NBTypes::Tracker::HTTP' =>
        from subtype(as 'Str' => where {m[^http://]i}) => via {
        require Net::BitTorrent::Protocol::BEP03::Tracker::HTTP;
        return Net::BitTorrent::Protocol::BEP03::Tracker::HTTP->new(
                                                                   URL => $_);
        };
    subtype 'NBTypes::Tracker::UDP' => as
        'Net::BitTorrent::Protocol::BEP15::Tracker::UDP';
    coerce 'NBTypes::Tracker::UDP' =>
        from subtype(as 'Str' => where {m[^udp://]i}) => via {
        require Net::BitTorrent::Protocol::BEP15::Tracker::UDP;
        return Net::BitTorrent::Protocol::BEP15::Tracker::UDP->new(URL => $_);
        };

    enum 'NBTypes::Tracker::HTTP::Event' => qw[started stopped completed];

    #
    enum 'NBTypes::File::Open::Permission' => qw[ro wo rw];
    subtype 'NBTypes::Files' => as 'ArrayRef[Net::BitTorrent::Storage::File]';
    coerce 'NBTypes::Files' => from 'ArrayRef[HashRef]' => via {
        require Net::BitTorrent::Storage::File;
        my ($offset, $index) = (0, 0);
        [map {
             my $obj =
                 Net::BitTorrent::Storage::File->new(
                                           Index  => $index++,
                                           Length => $_->{'length'},
                                           Offset => $offset,
                                           Path => [grep {$_} @{$_->{'path'}}]
                 );
             $offset += $_->{'length'};
             $obj
             } @{$_}
        ];
    };
    coerce 'NBTypes::Files' => from 'HashRef' => via {
        require Net::BitTorrent::Storage::File;
        [Net::BitTorrent::Storage::File->new(Length => $_->{'length'},
                                             Path   => $_->{'path'}
         )
        ];
    };

    #
    subtype 'NBTypes::Cache::Packet' => as 'ArrayRef[Int]' =>
        where { scalar @$_ == 2 };

    #
    subtype 'NBTypes::Client::PeerID' => as 'Str' =>
        where { length $_ == 20 } =>
        message {'PeerID is malformed: length != 20'};

    #
    subtype 'NBTypes::DHT::NodeID' => as 'Bit::Vector' =>
        where { $_->Size == 160 } =>
        message {'DHT NodeIDs are 160-bit integers.'};
    coerce 'NBTypes::DHT::NodeID' =>
        from subtype(as 'Int' => where { length $_ <= 40 } ) =>
        via { require Bit::Vector; Bit::Vector->new_Dec(160, $_) } =>
        from subtype(as 'Str' => where {/^[a-f\d]$/i } ) =>
        via { require Bit::Vector; warn $_; Bit::Vector->new_Hex(160, $_) } =>
        from 'Str' => via {
        require Bit::Vector;
        Bit::Vector->new_Hex(160, unpack 'H*', $_);
        };
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
