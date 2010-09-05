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
          isa => 'HashRef[Net::BitTorrent::Protocol::BEP03::Metadata::Piece]',
          is  => 'ro',
          default => sub { {} },
          traits  => ['Hash'],
          handles => {'_add_working_piece'    => 'set',
                      '_get_working_piece'    => 'get',
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
        return if !defined $piece;
        if (!blessed $piece ) {    # Must be an index
            if ($s->_has_working_piece($piece)) {
                $piece = $s->_get_working_piece($piece);
            }
            else {
                require Net::BitTorrent::Protocol::BEP03::Metadata::Piece;
                $piece =
                    Net::BitTorrent::Protocol::BEP03::Metadata::Piece->new(
                                                         piece_selector => $s,
                                                         index => $piece);
            }
        }

        # Make a note of this object (may already be there...)
        $s->_add_working_piece($piece->index, $piece)
            if !$s->_has_working_piece($piece->index);
        $piece;
    }
    sub end_game {0}
}
1;
