package Net::BitTorrent::Protocol::BEP03::Metadata::Piece;
{
    use Moose;
    our $MAJOR = 0; our $MINOR = 74; our $DEV = 13; our $VERSION = sprintf('%0d.%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    sub BUILD {1}
    has 'index' => (isa      => 'Int',
                    is       => 'ro',
                    required => 1
    );
    has 'piece_selector' => (
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
        return $s->torrent->size % $s->torrent->piece_length
            if $s->index == $s->torrent->piece_count - 1;
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
        isa =>
            'ArrayRef[Net::BitTorrent::Protocol::BEP03::Metadata::Piece::Block]',
        is         => 'ro',
        lazy_build => 1,
        traits     => ['Array'],
        handles    => {
            _first_unassigned_block => ['first', sub { !$_->has_peer }],
            _all_unassigned_blocks  => ['grep',  sub { !$_->has_peer }],
            _first_incompete_block  => ['first', sub { !$_->complete }],
            _incomplete_blocks      => ['grep',  sub { !$_->complete }],
            _complete_blocks        => ['grep',  sub { $_->complete }],
            _grep_blocks            => 'grep',
            _first_block            => 'first',
            _peers => ['map', sub { $_->has_peer ? $_->peer : () }]
        }
    );

    sub _get_block {
        my ($s, $o, $l) = @_;
        $s->_first_block(sub { $_->offset == $o && $_->length == $l });
    }

    sub _build_blocks {
        my $s = shift;
        require Net::BitTorrent::Protocol::BEP03::Metadata::Piece::Block;
        my $offset = 0;
        my @blocks = map {
            my $b =
                Net::BitTorrent::Protocol::BEP03::Metadata::Piece::Block->new(
                                                    piece  => $s,
                                                    offset => $offset,
                                                    length => $s->block_length
                );
            $offset += $s->block_length;
            $b
        } 1 .. int($s->length / $s->block_length);
        push @blocks,
            Net::BitTorrent::Protocol::BEP03::Metadata::Piece::Block->new(
                                  piece  => $s,
                                  offset => $offset,
                                  length => int($s->length % $s->block_length)
            ) if $s->length % $s->block_length;
        return \@blocks;
    }

    #
    no Moose;
    __PACKAGE__->meta->make_immutable;
}
1;
