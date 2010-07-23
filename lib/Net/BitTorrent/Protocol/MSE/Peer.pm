package Net::BitTorrent::Protocol::MSE::Peer;
{
    use Moose::Role;
    use Moose::Util::TypeConstraints;
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    has 'handshake_step' =>
        (isa => enum([qw[MSE_ONE MSE_TWO MSE_THREE MSE_FOUR MSE_FIVE]]));

    # XXX - Move MSE-related functions to ::MSE::Packets
    sub CRYPTO_PLAIN {0x01}
    sub CRYPTO_RC4   {0x02}
    sub CRYPTO_XOR   {0x04}    # unimplemented
    sub CRYPTO_AES   {0x08}    # unimplemented
    has '_crypto' => (
         isa      => enum([CRYPTO_PLAIN, CRYPTO_RC4, CRYPTO_XOR, CRYPTO_AES]),
         is       => 'rw',
         default  => CRYPTO_PLAIN,
         init_arg => undef
    );

    #
    sub DH_P {
        require Bit::Vector;
        state $DH_P
            = Bit::Vector->new_Hex('FFFFFFFFFFFFFFFFC90FDAA22168C234C4C66'
                . '28B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404D'
                . 'DEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B5766'
                . '25E7EC6F44C42E9A63A36210000000000090563');
        $DH_P;
    }
    sub DH_G {2}
    sub VC   { "\0" x 8 }

    sub crypto_provide {
        return pack q[N],
            CRYPTO_PLAIN    # | CRYPTO_RC4    #| CRYPTO_XOR | CRYPTO_AES;
    }
}
