{

    package Net::BitTorrent::Torrent::PieceSelector;
    use Moose;
    our $MAJOR = 0; our $MINOR = 74; our $DEV = 13; our $VERSION = sprintf('%0d.%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    sub BUILD {1}
    has 'torrent' => (isa        => 'Net::BitTorrent::Torrent',
                      is         => 'ro',
                      required   => 1,
                      handles    => [qw[peers]],
                      weak_ref   => 1,
                      lazy_build => 1
    );
    has '_working_pieces' => (
          isa => 'HashRef[Net::BitTorrent::Protocol::BEP03::Metadata::Piece]',
          is  => 'ro',
          lazy_build => 1,
          traits     => ['Hash'],
          handles    => {
                      '_add_working_piece'    => 'set',
                      '_get_working_piece'    => 'get',
                      '_del_working_piece'    => 'delete',
                      'has_working_piece'     => 'defined',
                      '_count_working_pieces' => 'count',
                      'working_pieces'        => 'values'
          }
    );
    sub _build__working_pieces { {} }
    sub _unassigned_working_pieces {
        grep { $_->_first_unassigned_block?1:0 } shift->working_pieces;
    }
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
        $piece // return;
        if (!blessed $piece ) {    # Must be an index
            if ($s->has_working_piece($piece)) {
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
            if !$s->has_working_piece($piece->index);
        $piece;
    }
    sub end_game {0}

    #
    no Moose;
    __PACKAGE__->meta->make_immutable;
}
1;
