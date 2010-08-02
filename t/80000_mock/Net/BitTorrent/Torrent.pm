        package t::80000_mock::Net::BitTorrent::Torrent;
        {
            use Moose;
            extends 'Net::BitTorrent::Torrent';
            use Net::BitTorrent::Types qw[:torrent];
            has 'info_hash' => (is      => 'ro',
                                isa     => 'NBTypes::Torrent::Infohash',
                                coerce  => 1,
                                default => sub { '0123456789ABCDEF1023' x 2 }
            );
            has 'piece_count' => (is      => 'ro',
                                  isa     => 'Int',
                                  default => 100
            );
            has '+path' => (required => 0);

            sub wanted {
                my $s = shift;
                my $x = $s->have->Clone;
                $x->Flip();
                $x;
            }
        }
        1
