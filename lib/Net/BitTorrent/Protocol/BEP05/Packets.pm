package Net::BitTorrent::Protocol::BEP05::Packets;
{
    use strict;
    use warnings;
    use lib '../../../../../lib';
    use Net::BitTorrent::Protocol::BEP03::Bencode qw[:bencode];
    require Exporter;
    our @ISA = qw[Exporter];
    our %EXPORT_TAGS;
    our @EXPORT_OK = @{$EXPORT_TAGS{'all'}} = sort qw[
        build_dht_reply_get_peers     build_dht_query_get_peers
        build_dht_reply_announce_peer build_dht_query_announce_peer
        build_dht_reply_ping          build_dht_query_ping
        build_dht_reply_find_node     build_dht_query_find_node
        build_dht_reply_error                                      ];
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    my $v = 'NB' . pack 'CC', $MAJOR * 1000, $MINOR;

    #
    sub build_dht_query_ping ($$) {
        my ($tid, $id) = @_;
        return
            bencode({t => $tid,
                     y => 'q',
                     q => 'ping',
                     a => {id => $id},
                     v => $v
                    }
            );
    }

    sub build_dht_query_announce_peer ($$$$$) {
        my ($tid, $id, $infohash, $token, $port) = @_;
        return
            bencode({t => $tid,
                     y => 'q',
                     q => 'announce_peer',
                     a => {id        => $id,
                           port      => $port,
                           info_hash => $infohash,
                           token     => $token
                     },
                     v => $v
                    }
            );
    }

    sub build_dht_query_find_node ($$$) {
        my ($tid, $id, $target) = @_;
        return
            bencode({t => $tid,
                     y => 'q',
                     q => 'find_node',
                     a => {id     => $id,
                           target => $target
                     },
                     v => $v
                    }
            );
    }

    sub build_dht_query_get_peers ($$$) {
        my ($tid, $id, $info_hash) = @_;
        return
            bencode({t => $tid,
                     y => 'q',
                     q => 'get_peers',
                     a => {id => $id, info_hash => $info_hash},
                     v => $v
                    }
            );
    }

    sub build_dht_reply_ping ($$) {
        my ($tid, $id) = @_;
        return bencode({t => $tid, y => 'r', r => {id => $id}, v => $v});
    }

    sub build_dht_reply_announce_peer ($$) {
        my ($tid, $id) = @_;
        return bencode({t => $tid, y => 'r', r => {id => $id}, v => $v});
    }

    sub build_dht_reply_find_node ($$$) {
        my ($tid, $id, $nodes) = @_;
        return
            bencode({t => $tid,
                     y => 'r',
                     r => {id => $id, nodes => $nodes},
                     v => $v
                    }
            );
    }

    sub build_dht_reply_get_peers ($$$$$) {
        my ($tid, $id, $values, $nodes, $token) = @_;
        return
            bencode({t => $tid,
                     y => 'r',
                     r => {id    => $id,
                           token => $token,
                           (@$values ? (values => $values) : ()),
                           ($nodes   ? (nodes  => $nodes)  : ())
                     },
                     v => $v
                    }
            );
    }

    sub build_dht_reply_error ($@) {
        my ($tid, $error) = @_;
        return
            bencode({t => $tid,
                     y => 'e',
                     e => $error,
                     v => $v
                    }
            );
    }
}
1;

=pod

=head1 NAME

Net::BitTorrent::Protocol::BEP05::Packets - DHT Packet Utilities

=head1 Description

TODO

=cut
