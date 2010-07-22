{

    package Net::BitTorrent::Torrent::PieceSelector;
    use Moose;
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    sub BUILD {1}
    has 'torrent' => (isa      => 'Net::BitTorrent::Torrent',
                      is       => 'ro',
                      required => 1,
                      handles  => [qw[peers]],
                      weak_ref => 1
    );
    has 'working_pieces' => (
                            isa => 'HashRef[Net::BitTorrent::Torrent::Piece]',
                            is  => 'ro',
                            default => sub { {} },
                            traits  => ['Hash'],
                            handles => {'_add_working_piece'    => 'set',
                                        '_del_working_piece'    => 'delete',
                                        '_has_working_piece'    => 'defined',
                                        '_count_working_pieces' => 'count'
                            }
    );
    has 'strategy' => (isa     => 'RoleName',
                       is      => 'ro',
                       builder => '_build_strategy',
                       trigger => sub { shift->_apply_strategy }
    );

    sub _build_strategy {
        require Net::BitTorrent::Torrent::PieceSelector::Random;
        'Net::BitTorrent::Torrent::PieceSelector::Random';
    }

    sub _apply_strategy {
        my $s = shift;
        require Moose::Util;
        Moose::Util::apply_all_roles($s, $s->strategy);
    }
    after 'BUILD' => sub { shift->_apply_strategy };

    sub select_piece {
        my ($s, $p) = @_;
        my $piece = $s->_select_piece($p);
        return if !$piece;
        if (!blessed $piece) {    # Must be an index
            require Net::BitTorrent::Torrent::Piece;
            $piece =
                Net::BitTorrent::Torrent::Piece->new(selector => $s,
                                                     index    => $piece);
        }
        return $piece;
    }
    sub end_game {0}
}
1;
