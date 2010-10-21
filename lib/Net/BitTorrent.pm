package Net::BitTorrent;
{
    use 5.010;
    use Moose;
    our $MAJOR = 0.074; our $MINOR = 1; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    #
    use lib '../';
    use Net::BitTorrent::Types qw[:peer];

    #
    sub BUILD {1}

    #
    has 'peer_id' => (isa        => 'Net::BitTorrent::Types::PeerID',
                      is         => 'ro',
                      lazy_build => 1,
                      required   => 1
    );

    sub _build_peer_id {
        return pack(
            'a20',
            (sprintf(
                 'NB%03d%1s-%8s%-5s',
                 $MAJOR * 1000,
                 ($DEV > 0 ? 'U' : 'S'),
                 (join '',
                  map {
                      ['A' .. 'Z', 'a' .. 'z', 0 .. 9, qw[- . _ ~]]
                      ->[rand(66)]
                      } 1 .. 8
                 ),
                 [qw[KaiLi April]]->[rand 2]
             )
            )
        );
    }

    #
    __PACKAGE__->meta->make_immutable;
    no Moose;
}
1;
