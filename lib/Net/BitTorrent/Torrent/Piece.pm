{

    package Net::BitTorrent::Torrent::Piece;
    use Moose;
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    sub BUILD {1}
    has 'index' => (isa      => 'Int',
                    is       => 'ro',
                    required => 1
    );
    has 'selector' => (
                    isa      => 'Net::BitTorrent::Torrent::PieceSelector',
                    is       => 'ro',
                    required => 1,
                    handles => [qw[end_game strategy torrent working_pieces]],
                    weak_ref => 1
    );
    has 'length' => (isa        => 'Int',
                     is         => 'ro',
                     lazy_build => 1
    );

    sub _build_length {
        my $s = shift;
        return $s->torrent->length % $s->torrent->piece_length
            if $s->index == $s->torrent->piece_count;
        return $s->torrent->piece_length;
    }
    has 'priority' => (
            isa     => 'Int',
            is      => 'ro',
            writer  => '_set_priority',
            default => 2                  # 1:low/slow, 2:normal, 3: high/fast
    );
    has 'block_length' => (isa        => 'Int',
                           is         => 'ro',
                           lazy_build => 1
    );

    sub _build_block_length {
        my $s   = shift;
        my $max = 2**14;
        return $max > $s->length ? $s->length : $max;
    }
    has 'blocks' => (
             isa        => 'ArrayRef[Net::BitTorrent::Torrent::Piece::Block]',
             is         => 'ro',
             lazy_build => 1,
             traits     => ['Array'],
             handles    => {
                 _first_unassigned_block => ['first', sub { !$_->_has_peer }],
                 _all_unassigned_blocks  => ['grep',  sub { !$_->_has_peer }]
             }
    );
    after 'BUILD' => sub { shift->blocks };

    sub _build_blocks {
        my $s = shift;
        require Net::BitTorrent::Torrent::Piece::Block;
        my $offset = 0;
        my @blocks = map {
            my $b =
                Net::BitTorrent::Torrent::Piece::Block->new(
                                                    piece  => $s,
                                                    offset => $offset,
                                                    length => $s->block_length
                );
            $offset += $s->block_length;
            $b
        } 1 .. int($s->length / $s->block_length);
        push @blocks,
            Net::BitTorrent::Torrent::Piece::Block->new(
                                  piece  => $s,
                                  offset => $offset,
                                  length => int($s->length % $s->block_length)
            ) if $s->length % $s->block_length;
        return \@blocks;
    }
}
1;
