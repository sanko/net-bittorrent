{

    package Net::BitTorrent::Torrent::PieceSelector::Random;
    use Moose::Role;
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    sub _select_piece {
        my ($s, $peer) = @_;
        my @indices = $peer->_wanted_pieces->Index_List_Read();

  #warn 'Wanted pieces: '. join ', ', @indices;
  # XXX - Make sure this isn't a piece we've requested all blocks from already
        return scalar @indices ? $indices[rand @indices] : ();
    }
}
1;
