package Net::BitTorrent::Tracker;
{
    use Moose;
    use Net::BitTorrent::Network::Utility qw[server];
    use Net::BitTorrent::Protocol::BEP03::Bencode qw[:all];
    has 'port' => (isa     => 'Int',
                   is      => 'ro',
                   writer  => '_set_port',
                   default => 0
    );
    has 'host' =>
        (isa => 'Str', is => 'ro', writer => '_set_host', default => '::');
    has '_peers' => (
        isa     => 'HashRef[HashRef]',    # By (key ^ info_hash)
        is      => 'ro',
        default => sub { {} },
        traits  => ['Hash'],
        handles => {
            forget_peer => 'delete',
            add_peer    => 'set',
            peers       => 'values'

              #info_hashes   => ['map', sub { $_->{'info_hash'} } ],
              #peer_ids      => ['map', sub { $_->{'peer_id'} } ],
              #find_info_hash => ['map', sub { $_->{'info_hash'} eq $_[0] } ],
        }
    );
    has 'socket' => (isa      => 'Defined',
                     is       => 'ro',
                     init_arg => undef,
                     builder  => '_build_socket'
    );

    sub _build_socket {
        my $s = shift;
        my $x = server(
            $s->host,
            $s->port,
            sub {
                my ($fh, $paddr, $host, $port) = @_;
                my $hdl;
                $hdl = AnyEvent::Handle->new(
                    fh       => $fh,
                    on_drain => sub {
                        $s->on_drain($hdl, $fh, $paddr, $host, $port, @_);
                    },
                    on_read => sub {
                        $s->on_read($hdl, $fh, $paddr, $host, $port, @_);
                    },
                    on_eof => sub { warn 'bye!' }
                );
            },
            sub {
                $s->_set_host($_[1]);
                $s->_set_port($_[2]);
                1;
            },
            $s->protocol
        );
    }
    sub BUILD {1}

    sub on_drain {
        my $s = shift;
        $_[0] = undef;
    }
}
1
