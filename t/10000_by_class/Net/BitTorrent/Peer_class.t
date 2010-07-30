package t::10000_by_class::Net::BitTorrent::Peer_class;
{
    use strict;
    use warnings;
    use Test::Most;
    use Test::Moose;
    use Test::Exception;
    use parent 'Test::Class';
    use lib '../../../../';
    BEGIN { require 't/10000_by_class/Net/BitTorrent/Peer.t'; }
    use parent-norequire, 't::10000_by_class::Net::BitTorrent::Peer';
    sub new_args { my $s = shift; () }

    # Basic meta/attribute tests
    sub _001_check_attributes_reader : Test( 15 ) {
        my $s = shift;
        has_attribute_ok $s->{'peer'}, $_ for sort qw[
            torrent
            pieces
            interesting remote_interested support_extensions local_connection
            handshake queued on_parole optimistic_unchoke snubbed
            upload_only choked remote_choked connecting
        ];
    }

    sub _002_check_attributes_writer : Test( 27 ) {
        my $s = shift;
        can_ok $s->{'peer'}, $_ for sort qw[
            _set_torrent
            _set_interesting _set_remote_interested _set_support_extensions
            _set_local_connection _set_handshake _set_connecting _set_queued
            _set_on_parole _set_optimistic_unchoke _set_snubbed
            _set_upload_only _unset_interesting _unset_remote_interested
            _unset_support_extensions _unset_local_connection _unset_handshake
            _unset_queued _unset_on_parole _unset_optimistic_unchoke
            _unset_snubbed _unset_upload_only _set_choked _set_remote_choked
            _unset_choked _unset_remote_choked _unset_connecting
        ];
    }

    sub _003_check_attributes_clearer : Test( 1 ) {
        my $s = shift;
        can_ok $s->{'peer'}, $_ for sort qw[
            _clear_pieces
        ];
    }

    sub _004_check_attributes_predicate : Test( 2 ) {
        my $s = shift;
        can_ok $s->{'peer'}, $_ for sort qw[
            _has_torrent
            _has_pieces
        ];
    }

    sub _005_check_attribute_builders : Test( 1 ) {
        my $s = shift;
        can_ok $s->{'peer'}, $_ for sort qw[
            _build_pieces
        ];
    }

    sub _006_check_attribute_triggers : Test( 2 ) {
        my $s = shift;
        can_ok $s->{'peer'}, $_ for sort qw[
            _trigger_torrent
            _trigger_pieces
        ];
    }

    sub _007_check_attribute_initializers : Test( 1 ) {
        my $s = shift;
        can_ok $s->{'peer'}, $_ for sort qw[
            _initializer_torrent
        ];
    }

    sub _008_check_attribute_handlers : Test( 1 ) {
        my $s = shift;
        can_ok $s->{'peer'}, $_ for sort qw[
            seed
        ];
    }

    # Advanced foolishness
    sub _900_set_pieces : Test( 2 ) {
        my $s = shift;
        $s->{'peer'}->_set_pieces(pack 'b*', '000100000011');
        isa_ok $s->{'peer'}->pieces, 'Bit::Vector', 'coerced packed bitfield';
        is $s->{'peer'}->pieces->to_Bin, '0001000000110000',
            '...and has the correct value';
    }

    sub _901_has_piece : Test( 16 ) { # tests that the BV is (fake) zero-based
        my $s = shift;
        diag '...->pieces->to_Bin == ' . $s->{'peer'}->pieces->to_Bin;
        ok $s->{'peer'}->_has_piece($_), sprintf ' ...->_has_piece( %3s )', $_
            for 3, 10, 11;
        ok !$s->{'peer'}->_has_piece($_), sprintf '!...->_has_piece( %3s )',
            $_
            for 0 .. 2, 4 .. 9, 12 .. 15;
    }

    sub _902_set_piece : Test( 1 ) {
        my $s = shift;
        $s->{'peer'}->_set_piece($_) for 0, 2, 8, 15;
        is $s->{'peer'}->pieces->to_Bin, '1011000010110001',
            'updated ...->pieces with ...->_set_piece( ... )';
    }

    sub _903_has_piece : Test( 4 ) {  # tests that the BV is (fake) zero-based
        my $s = shift;
        diag 'updated ...->pieces->to_Bin == ' . $s->{'peer'}->pieces->to_Bin;
        ok $s->{'peer'}->_has_piece($_), sprintf ' ...->_has_piece( %3s )', $_
            for 0, 2, 8, 15;
    }

    sub _904_set_pieces : Test( 3 ) {
        my $s = shift;
        throws_ok sub { $s->{'peer'}->_set_pieces('011001') },
            qr[pieces attribute is alreay set],
            'setting the pieces attribute twice throws exception';
        isa_ok $s->{'peer'}->pieces, 'Bit::Vector',
            'coerced unpacked bitfield';
        is $s->{'peer'}->pieces->to_Bin, '011001',
            '...and has the correct value';
    }

    sub _905_has_piece : Test( 6 ) {  # tests that the BV is (fake) zero-based
        my $s = shift;
        diag '...->pieces->to_Bin == ' . $s->{'peer'}->pieces->to_Bin;
        ok $s->{'peer'}->_has_piece($_), sprintf ' ...->_has_piece( %3s )', $_
            for 1, 2, 5;
        ok !$s->{'peer'}->_has_piece($_), sprintf '!...->_has_piece( %3s )',
            $_
            for 0, 3, 4;
    }

    sub _906_set_piece : Test( 1 ) {
        my $s = shift;
        $s->{'peer'}->_set_piece($_) for 0, 3;
        is $s->{'peer'}->pieces->to_Bin, '111101',
            'updated ...->pieces with ...->_set_piece( ... )';
    }

    sub _907_has_piece : Test( 2 ) {  # tests that the BV is (fake) zero-based
        my $s = shift;
        diag 'updated ...->pieces->to_Bin == ' . $s->{'peer'}->pieces->to_Bin;
        ok $s->{'peer'}->_has_piece($_), sprintf ' ...->_has_piece( %3s )', $_
            for 0, 3;
    }

    sub _908_piece_range : Test( 2 ) {    # beyond range
        my $s = shift;
        throws_ok sub { $s->{'peer'}->_set_piece(100) },
            qr[index out of range],
            ' ...->_set_piece( 100 ) throws "out of range" exception';
        throws_ok sub { $s->{'peer'}->_has_piece(100) },
            qr[index out of range],
            ' ...->_has_piece( 100 ) throws "out of range" exception';
    }

    sub _910_seed : Test( 2 ) {
        my $s = shift;
        diag '...->pieces->to_Bin == ' . $s->{'peer'}->pieces->to_Bin;
        ok !$s->{'peer'}->seed,
            '...->seed() is false when all peer is missing any pieces';
        $s->{'peer'}->_set_piece(4);
        diag 'updated ...->pieces->to_Bin == ' . $s->{'peer'}->pieces->to_Bin;
        ok $s->{'peer'}->seed,
            '...->seed() is true when all peer has all pieces';
    }

    sub _911_set_torrent : Test( 6 ) {
        my $s = shift;
        {

            package Mock::Net::BitTorrent::Torrent;
            use Moose;
            extends 'Net::BitTorrent::Torrent';
            use Net::BitTorrent::Types qw[:torrent];
            has 'info_hash' => (is      => 'ro',
                                isa     => 'NBTypes::Torrent::Infohash',
                                coerce  => 1,
                                default => sub { '0123456789ABCDEF1023' x 2 }
            );
            has 'piece_count' => (is      => 'ro',
                                  isa     => 'Int',
                                  default => 100
            );
            has '+path' => (required => 0);
        }
        {
            my $torrent
                = Mock::Net::BitTorrent::Torrent->new(piece_count => 20);
            explain 'New mock-torrent looks like... ', $torrent;
            ok !$s->{'peer'}->_has_torrent,
                '...->_has_torrent is initially false';
            $s->{'peer'}->_set_torrent($torrent);
            ok $s->{'peer'}->_has_torrent, '...->_has_torrent is now true';
            is $s->{'peer'}->pieces->Size, $torrent->piece_count,
                '...->_set_torrent( ... ) also sets/resizes the pieces attribute';
            is $s->{'peer'}->pieces->to_Bin, '00000000000000111111',
                ' ...without loosing the current data';
            throws_ok sub { $s->{'peer'}->_set_torrent($torrent) },
                qr[torrent attribute is alreay set],
                'setting the torrent attribute twice throws exception';
        }
        is $s->{'peer'}->torrent, undef,
            '...->torrent is only held by a weak ref';
    }

    sub _912_flags_initial_value : Test( 14 ) {
        my $s = shift;

        # True by default
        ok $s->{'peer'}->$_(), sprintf '...->%s() is initially true', $_
            for qw[choked remote_choked connecting];

        # False by default
        ok !$s->{'peer'}->$_(), sprintf '...->%s() is initially false', $_
            for qw[ interesting remote_interested support_extensions
            local_connection handshake queued on_parole optimistic_unchoke
            snubbed upload_only];
    }

    sub _913_flags_set_value : Test( 13 ) {
        my $s     = shift;
        my @flags = qw[ choked remote_choked connecting interesting
            remote_interested support_extensions local_connection handshake
            queued on_parole optimistic_unchoke snubbed upload_only];
        for (@flags) {
            my $set = '_set_' . $_;
            $s->{'peer'}->$set;
        }
        ok $s->{'peer'}->$_(), sprintf '...->%s() is now true', $_ for @flags;
    }

    sub _914_flags_unset_value : Test( 13 ) {
        my $s     = shift;
        my @flags = qw[ choked remote_choked connecting interesting
            remote_interested support_extensions local_connection handshake
            queued on_parole optimistic_unchoke snubbed upload_only];
        for (@flags) {
            my $set = '_unset_' . $_;
            $s->{'peer'}->$set;
        }
        ok !$s->{'peer'}->$_(), sprintf '...->%s() is now false', $_
            for @flags;
    }

    #
    #$ENV{'TEST_VERBOSE'}++;
    __PACKAGE__->runtests() if !caller;
}
1;
