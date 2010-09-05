{

    package Net::BitTorrent::Torrent::PieceSelector::Popular;
    use Moose::Role;
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 10; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
}
1;
