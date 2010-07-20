{

    package Net::BitTorrent::Torrent::PieceSelector;
    use Moose;
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    has 'torrent' => (isa      => 'Net::BitTorrent::Torrent',
                      is       => 'ro',
                      required => 1,
                      handles  => [qw[peers]],
                      weak_ref => 1
    );
}
1;
