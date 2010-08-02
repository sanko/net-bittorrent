{

    package Net::BitTorrent::Torrent::PieceSelector::Random;
    use Moose::Role;
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    sub _select_piece {
        my ($s, $peer) = @_;
        my $bitfield = $peer->_pieces_intersection->to_Bin;
        my @index;
        {
            my $i = rindex $bitfield, '1', 0;
            while ($i != -1) {
                push @index, $i;
                $i = rindex $bitfield, '1', $i + 1;
            }
        }

  # XXX - Make sure this isn't a piece we've requested all blocks from already
        return if !scalar @index;
        return $index[rand @index];
    }
}
1;
