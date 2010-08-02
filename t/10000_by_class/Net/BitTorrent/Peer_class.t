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


    # Basic utility functions/methods
    sub new_args { my $s = shift; () }

    # Basic meta/attribute tests
    sub _0010_check_public_attributes : Test( 15 ) {
        my $s = shift;
        has_attribute_ok $s->{'peer'}, $_ for sort qw[
            torrent
            pieces
            interesting remote_interested support_extensions local_connection
            handshake queued on_parole optimistic_unchoke snubbed
            upload_only choked remote_choked connecting
        ];
    }

    sub _0012_check_private_attributes : Test( 0 ) {
        my $s = shift;
        has_attribute_ok $s->{'peer'}, $_ for sort qw[];
    }

    sub _0015_check_attributes_reader : Test( 15 ) {
        my $s = shift;
        can_ok $s->{'peer'}, $_ for sort qw[
            torrent
            pieces
            interesting remote_interested support_extensions local_connection
            handshake queued on_parole optimistic_unchoke snubbed
            upload_only choked remote_choked connecting
        ];
    }

    sub _0020_check_attributes_writer : Test( 27 ) {
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

    sub _0030_check_attributes_clearer : Test( 1 ) {
        my $s = shift;
        can_ok $s->{'peer'}, $_ for sort qw[
            _clear_pieces
        ];
    }

    sub _0040_check_attributes_predicate : Test( 2 ) {
        my $s = shift;
        can_ok $s->{'peer'}, $_ for sort qw[
            _has_torrent
            _has_pieces
        ];
    }

    sub _0050_check_attribute_builders : Test( 1 ) {
        my $s = shift;
        can_ok $s->{'peer'}, $_ for sort qw[
            _build_pieces
        ];
    }

    sub _0060_check_attribute_triggers : Test( 2 ) {
        my $s = shift;
        can_ok $s->{'peer'}, $_ for sort qw[
            _trigger_torrent
            _trigger_pieces
        ];
    }

    sub _0070_check_attribute_initializers : Test( 1 ) {
        my $s = shift;
        can_ok $s->{'peer'}, $_ for sort qw[
            _initializer_torrent
        ];
    }

    sub _0080_check_attribute_handlers : Test( 1 ) {
        my $s = shift;
        can_ok $s->{'peer'}, $_ for sort qw[
            seed
        ];
    }

    # Advanced foolishness
    sub _9000_set_pieces : Test( 2 ) {
        my $s = shift;
        return $s->skip_setters if $s->skip_setters;
        $s->{'peer'}->_set_pieces(pack 'b*', '000100000011');
        isa_ok $s->{'peer'}->pieces, 'Bit::Vector', 'coerced packed bitfield';
        is $s->{'peer'}->pieces->to_Enum, '3,10,11',
            '...and has the correct value';
    }

    sub _9010_has_piece : Test( 16 )
    {    # tests that the BV is (fake) zero-based
        my $s = shift;
        return $s->skip_setters if $s->skip_setters;
        diag '...->pieces->to_Enum == ' . $s->{'peer'}->pieces->to_Enum;
        ok $s->{'peer'}->_has_piece($_), sprintf ' ...->_has_piece( %3s )', $_
            for 3, 10, 11;
        ok !$s->{'peer'}->_has_piece($_), sprintf '!...->_has_piece( %3s )',
            $_
            for 0 .. 2, 4 .. 9, 12 .. 15;
    }

    sub _9020_set_piece : Test( 1 ) {
        my $s = shift;return $s->skip_setters if $s->skip_setters;
        $s->{'peer'}->_set_piece($_) for 0, 2, 8, 15;
        is $s->{'peer'}->pieces->to_Enum, '0,2,3,8,10,11,15',
            'updated ...->pieces with ...->_set_piece( ... )';
    }

    sub _9030_has_piece : Test( 4 ) { # tests that the BV is (fake) zero-based
        my $s = shift;return $s->skip_setters if $s->skip_setters;
        diag 'updated ...->pieces->to_Enum == '
            . $s->{'peer'}->pieces->to_Enum;
        ok $s->{'peer'}->_has_piece($_), sprintf ' ...->_has_piece( %3s )', $_
            for 0, 2, 8, 15;
    }

    sub _9040_set_pieces : Test( 3 ) {
        my $s = shift;return $s->skip_setters if $s->skip_setters;
        throws_ok sub { $s->{'peer'}->_set_pieces('011001') },
            qr[pieces attribute is already set],
            'setting the pieces attribute twice throws exception';
        isa_ok $s->{'peer'}->pieces, 'Bit::Vector',
            'coerced unpacked bitfield';
        is $s->{'peer'}->pieces->to_Enum, '1,2,5',
            '...and has the correct value';
    }

    sub _9050_has_piece : Test( 6 ) { # tests that the BV is (fake) zero-based
        my $s = shift;return $s->skip_setters if $s->skip_setters;
        diag '...->pieces->to_Enum == ' . $s->{'peer'}->pieces->to_Enum;
        ok $s->{'peer'}->_has_piece($_), sprintf ' ...->_has_piece( %3s )', $_
            for 1, 2, 5;
        ok !$s->{'peer'}->_has_piece($_), sprintf '!...->_has_piece( %3s )',
            $_
            for 0, 3, 4;
    }

    sub _9060_set_piece : Test( 1 ) {
        my $s = shift;return $s->skip_setters if $s->skip_setters;
        $s->{'peer'}->_set_piece($_) for 0, 3;
        is $s->{'peer'}->pieces->to_Enum, '0-3,5',
            'updated ...->pieces with ...->_set_piece( ... )';
    }

    sub _9070_has_piece : Test( 2 ) { # tests that the BV is (fake) zero-based
        my $s = shift;return $s->skip_setters if $s->skip_setters;
        diag 'updated ...->pieces->to_Enum == '
            . $s->{'peer'}->pieces->to_Enum;
        ok $s->{'peer'}->_has_piece($_), sprintf ' ...->_has_piece( %3s )', $_
            for 0, 3;
    }

    sub _9080_piece_range : Test( 2 ) {    # beyond range
        my $s = shift;return $s->skip_setters if $s->skip_setters;
        throws_ok sub { $s->{'peer'}->_set_piece(100) },
            qr[index out of range],
            ' ...->_set_piece( 100 ) throws "out of range" exception';
        throws_ok sub { $s->{'peer'}->_has_piece(100) },
            qr[index out of range],
            ' ...->_has_piece( 100 ) throws "out of range" exception';
    }

    sub _9100_seed : Test( 2 ) {
        my $s = shift;return $s->skip_setters if $s->skip_setters;
        diag '...->pieces->to_Enum == ' . $s->{'peer'}->pieces->to_Enum;
        ok !$s->{'peer'}->seed,
            '...->seed() is false when all peer is missing any pieces';
        $s->{'peer'}->_set_piece(4);
        diag 'updated ...->pieces->to_Enum == '
            . $s->{'peer'}->pieces->to_Enum;
        ok $s->{'peer'}->seed,
            '...->seed() is true when all peer has all pieces';
        $s->{'peer'}->pieces->Bit_Off(4);    # Turn it off before we continue
    }

    sub _9110_set_torrent : Test( 5 ) {
        my $s = shift;return $s->skip_setters if $s->skip_setters;
        require t::80000_mock::Net::BitTorrent::Torrent;
        $s->{'torrent'} = t::80000_mock::Net::BitTorrent::Torrent->new(piece_count => 20);
        explain 'New mock-torrent looks like... ', $s->{'torrent'};
        ok !$s->{'peer'}->_has_torrent,
            '...->_has_torrent is initially false';
        $s->{'peer'}->_set_torrent($s->{'torrent'});
        ok $s->{'peer'}->_has_torrent, '...->_has_torrent is now true';
        is $s->{'peer'}->pieces->Size, $s->{'torrent'}->piece_count,
            '...->_set_torrent( ... ) also sets/resizes the pieces attribute';
        is $s->{'peer'}->pieces->to_Enum, '0-3,5',
            ' ...without loosing the current data';
        throws_ok sub { $s->{'peer'}->_set_torrent($s->{'torrent'}) },
            qr[torrent attribute is already set],
            'setting the torrent attribute twice throws exception';
    }

    sub _9120_wanted : Test( 1 ) {
        my $s = shift;return $s->skip_setters if $s->skip_setters;
        $s->{'torrent'}->_set_piece(3);
        is $s->{'peer'}->_wanted_pieces->to_Enum, '0-2,5';
    }

    sub _9130_torrent_weak_ref : Test( 1 ) {
        my $s = shift;return $s->skip_setters if $s->skip_setters;
        delete $s->{'torrent'};
        is $s->{'peer'}->torrent, undef,
            '...->torrent is only held by a weak ref';
    }

    sub _9140_has_piece : Test( 20 )
    {    # tests that the BV is (fake) zero-based
        my $s = shift;return $s->skip_setters if $s->skip_setters;
        diag '...->pieces->to_Enum == ' . $s->{'peer'}->pieces->to_Enum;
        ok $s->{'peer'}->_has_piece($_), sprintf ' ...->_has_piece( %3s )', $_
            for 0 .. 3, 5;
        ok !$s->{'peer'}->_has_piece($_), sprintf '!...->_has_piece( %3s )',
            $_
            for 4, 6 .. 19;
    }

    sub _9150_flags_initial_value : Test( 13 ) {
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

    sub _9160_flags_set_value : Test( 13 ) {
        my $s     = shift;return $s->skip_setters if $s->skip_setters;
        my @flags = qw[ choked remote_choked connecting interesting
            remote_interested support_extensions local_connection handshake
            queued on_parole optimistic_unchoke snubbed upload_only];
        for (@flags) {
            my $set = '_set_' . $_;
            $s->{'peer'}->$set;
        }
        ok $s->{'peer'}->$_(), sprintf '...->%s() is now true', $_ for @flags;
    }

    sub _9170_flags_unset_value : Test( 13 ) {
        my $s     = shift;return $s->skip_setters if $s->skip_setters;
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
