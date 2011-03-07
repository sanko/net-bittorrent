package Net::BitTorrent::Protocol::BEP03::Metadata::Piece;
{
    use Moose;
    our $MAJOR = 0; our $MINOR = 74; our $DEV = 14; our $VERSION = sprintf('%0d.%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    #
    use lib '../../../../../../lib';
    use Net::BitTorrent::Protocol::BEP03::Metadata::Block;

    #
    has length => (isa      => 'Int',
                   is       => 'ro',
                   required => 1
    );
    has priority => (
            isa     => 'Int',
            is      => 'ro',
            writer  => '_set_priority',
            default => 2                  # 1:low/slow, 2:normal, 3: high/fast
    );
    has block_length => (isa        => 'Int',
                         is         => 'ro',
                         lazy_build => 1
    );

    sub _build_block_length {
        my $s   = shift;
        my $max = 2**14;
        return $max > $s->length ? $s->length : $max;
    }
    has blocks => (
        isa => 'HashRef[Net::BitTorrent::Protocol::BEP03::Metadata::Block]',
        is  => 'ro',
        lazy_build => 1,
        traits     => ['Hash'],
        handles    => {

            #_first_unassigned_block => ['first', sub { !$_->has_peer }],
            #_all_unassigned_blocks  => ['grep',  sub { !$_->has_peer }],
            #_first_incompete_block  => ['first', sub { !$_->complete }],
            #_incomplete_blocks      => ['grep',  sub { !$_->complete }],
            #_complete_blocks        => ['grep',  sub { $_->complete }],
            #_grep_blocks            => 'grep',
            #_first_block            => 'first',
            #_peers => ['map', sub { $_->has_peer ? $_->peer : () }]
        }
    );

    #sub _get_block {
    #    my ($s, $o, $l) = @_;
    #    $s->_first_block(sub { $_->offset == $o && $_->length == $l });
    #}
    sub _build_blocks {
        my $s      = shift;
        my $offset = 0;
        my %blocks;
        for my $i (1 .. int($s->length / $s->block_length)) {
            $blocks{$offset}
                = Net::BitTorrent::Protocol::BEP03::Metadata::Block->new(
                                                  length => $s->block_length);
            $offset += $s->block_length;
        }
        $blocks{$offset}
            = Net::BitTorrent::Protocol::BEP03::Metadata::Block->new(
                                 length => int($s->length % $s->block_length))
            if $s->length % $s->block_length;
        return \%blocks;
    }

    #
    no Moose;
    __PACKAGE__->meta->make_immutable;
}
1;
