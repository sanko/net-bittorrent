{

    package Net::BitTorrent::Torrent::PieceSelector::Linear;
    use Moose::Role;
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    sub _select_piece {
        my ($s, $peer) = @_;

  # XXX - Make sure this isn't a piece we've requested all blocks from already
        index $peer->_pieces_intersection->to_Bin, '1';
    }
}
1;
