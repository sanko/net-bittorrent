{

    package Net::BitTorrent::Protocol::BEP03::Metadata::Piece::Block;
    use Moose;
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    has 'offset' => (isa      => 'Int',
                     is       => 'ro',
                     required => 1
    );
    has 'piece' => (
        isa      => 'Net::BitTorrent::Protocol::BEP03::Metadata::Piece',
        is       => 'ro',
        required => 1,
        weak_ref => 1,
        handles  => {
            index          => 'index',
            piece_selector => 'piece_selector',
            _write         => sub {
                my ($s, $d) = @_;
                return $s->_set_complete
                    if $s->piece_selector->torrent->storage->write($s->index,
                                                              $s->offset, $d);
                }
        }
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
    has '_complete' => (isa     => 'Bool',
                        traits  => ['Bool'],
                        is      => 'ro',
                        handles => {_set_complete => 'set'},
                        default => 0
    );
}
1;
