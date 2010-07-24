{

    package Net::BitTorrent::Torrent::Piece::Block;
    use Moose;
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    has 'offset' => (isa      => 'Int',
                     is       => 'ro',
                     required => 1
    );
    has 'piece' => (isa      => 'Net::BitTorrent::Torrent::Piece',
                    is       => 'ro',
                    required => 1,
                    weak_ref => 1,
                    handles  => [qw[index]]
    );
    has 'length' => (isa      => 'Int',
                     is       => 'ro',
                     required => 1
    );
    has 'peer' => (isa       => 'Net::BitTorrent::Peer',
                   is        => 'ro',
                   writer    => '_set_peer',
                   predicate => '_has_peer',
                   weak_ref  => 1
    );
    has '_complete' => (isa     => 'Defined',
                        is      => 'ro',
                        writer  => '_set_complete',
                        default => 0
    );
}
1;
