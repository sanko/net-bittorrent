{

    package Net::BitTorrent::Torrent::PieceSelector::Random;
    use Moose::Role;
    our $MAJOR = 0; our $MINOR = 74; our $DEV = 13; our $VERSION = sprintf('%0d.%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    #
    has 'max_working_pieces' => (    # Hard limit
                                  isa        => 'Int',
                                  is         => 'ro',
                                  lazy_build => 1
    );
    sub _build_max_working_pieces {20}

    #
    sub _select_piece {
        my ($s, $peer) = @_;
        my @incomplete = $s->_unassigned_working_pieces;
        return $incomplete[rand @incomplete] if @incomplete;
        return if $s->_count_working_pieces >= $s->max_working_pieces;
        my @indices=  $peer->_wanted_pieces->Index_List_Read();
        return scalar @indices ? $indices[rand @indices] : ();
    }

    #
    no Moose::Role;
}
1;
