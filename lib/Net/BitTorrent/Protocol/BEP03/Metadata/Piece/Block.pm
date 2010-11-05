{

    package Net::BitTorrent::Protocol::BEP03::Metadata::Piece::Block;
    use Moose;
    our $MAJOR = 0; our $MINOR = 74; our $DEV = 13; our $VERSION = sprintf('%0d.%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    #
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
    has 'peer' => (
        isa      => 'Net::BitTorrent::Peer',
        is       => 'ro',
        writer   => '_set_peer',
        clearer  => 'clear_peer',
        weak_ref => 1,
        handles  => {
                    has_peer => sub { shift->peer // () }
        }
    );
    after 'clear_peer' => sub { shift->clear_timeout };
    after '_set_peer'  => sub { shift->timeout };
    has 'complete' => (isa     => 'Bool',
                       traits  => ['Bool'],
                       is      => 'ro',
                       handles => {_set_complete => 'set'},
                       default => 0
    );
    after '_set_complete' => sub { shift->clear_timeout };
    has 'timeout' => (isa        => 'Ref',
                      is         => 'ro',
                      init_arg   => undef,
                      lazy_build => 1
    );

    sub _build_timeout {
        my $s = shift;
        require AnyEvent;
        require Scalar::Util;
        Scalar::Util::weaken $s;
        AE::timer(
            5*60, 0,
            sub {
                $s // return;
                $s->complete && return;
                $s->has_peer || return;
                $s->peer->_delete_request($s);
                $s->peer->_send_cancel($s);
                $s->clear_peer;
            }
        );
    }

    #
    no Moose;
    __PACKAGE__->meta->make_immutable;
}
1;
