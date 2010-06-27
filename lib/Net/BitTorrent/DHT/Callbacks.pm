package Net::BitTorrent::DHT::Callbacks;
{
    use Moose::Role;
    use lib '../../../../lib';
    use Net::BitTorrent::Protocol::BEP03::Bencode qw[bdecode];
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    sub _build_callback_no_op {
        sub {1}
    }
    has "on_dht_$_" => (isa        => 'CodeRef',
                        is         => 'rw',
                        traits     => ['Code'],
                        handles    => {"trigger_dht_$_" => 'execute_method'},
                        lazy_build => 1,
                        builder    => '_build_callback_no_op'
        )
        for qw[
        packet_in           packet_in_okay  packet_in_error
        packet_out          packet_out_okay packet_out_error
        ping_request_out    ping_reply_out
        ping_request_in     ping_reply_in
    ];

    #
    before '_on_data_in' => sub {
        my $code = shift;
        my ($self, $udp, $sock, $paddr, $host, $port, $data, $flags) = @_;
        my $packet = bdecode $data;
        if (!$packet) {
            $self->trigger_dht_packet_in($host, $port, $packet);
        }
        else {
            $self->trigger_dht_packet_in($host, $port, $packet);

            # XXX - other callbacks
        }
        return $code->(@_);
    };
}
1;
