package Net::BitTorrent;
{
    use 5.010;
    use Moose;
    our $MAJOR = 0; our $MINOR = 74; our $DEV = 3; our $VERSION = sprintf('%0d.%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    #
    use lib '../';
    use Net::BitTorrent::Types qw[:peer];
    use Net::BitTorrent::Torrent;

    #
    sub BUILD {1}

    #
    has 'peer_id' => (isa        => 'Net::BitTorrent::Types::PeerID',
                      is         => 'ro',
                      lazy_build => 1,
                      init_arg   => undef
    );

    sub _build_peer_id {
        sprintf 'NB%03d%1s-%7s-%-5s',
            ($MAJOR * 100) + ($MINOR), $DEV > 0 ? 'U' : 'S', join(
            '',
            map {
                ['A' .. 'Z', 'a' .. 'z', 0 .. 9, qw[- . _ ~]]->[rand(66)]
                } 1 .. 7
            ),
            [qw[April KaiLi]]->[rand 2];
    }
    has 'torrents' => (traits  => ['Array'],
                       isa     => 'ArrayRef[Net::BitTorrent::Torrent]',
                       is      => 'ro',
                       lazy    => 1,
                       default => sub { [] },
                       handles => {
                                add_torrent     => 'push',
                                clear_torrents  => 'clear',
                                count_torrents  => 'count',
                                filter_torrents => 'grep',
                                find_torrent    => 'first',
                                has_torrents    => 'count',
                                info_hashes => ['map', sub { $_->info_hash }],
                                map_torrents           => 'map',
                                no_torrents            => 'is_empty',
                                shuffle_torrents       => 'shuffle',
                                sort_torrents          => 'sort',
                                sort_torrents_in_place => 'sort_in_place',
                                torrent                => 'get',
                       }
    );
    around 'add_torrent' => sub {
        my ($code, $self) = (shift, shift);
        my $torrent;
        if (blessed $_[0]) { $torrent = $_[0]; }
        else {
            require Net::BitTorrent::Torrent;
            $torrent = Net::BitTorrent::Torrent->new(@_) or return;
        }
        return
               blessed $torrent
            && $code->($self, $torrent)
            && $torrent->_set_client($self) // 1
            && require Net::BitTorrent::Torrent::Queued
            && Net::BitTorrent::Torrent::Queued->meta->apply($torrent,
                                             client => $self) ? $torrent : ();
    };
    my $info_hash_constraint;
    around 'torrent' => sub ($) {
        my ($code, $self, $index) = @_;
        my $torrent;
        {
            $info_hash_constraint //=
                Moose::Util::TypeConstraints::find_type_constraint(
                                  'Net::BitTorrent::ypes::Torrent::Infohash');
            my $info_hash = $info_hash_constraint->coerce($index);
            $torrent = $self->find_torrent(
                sub {

                    #$_->_has_info_hash &&
                    $_->info_hash->Lexicompare($info_hash) == 0;
                }
            );
        }
        $torrent = $code->($self, $index)
            if !defined $torrent && $index =~ m[^\d$];
        return $torrent;
    };

    #
    __PACKAGE__->meta->make_immutable;
    no Moose;
}
1;
