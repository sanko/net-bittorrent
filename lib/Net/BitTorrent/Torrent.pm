package Net::BitTorrent::Torrent;
{
    use Any::Moose;
    our $MAJOR = 0.075; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    # Meat
    use lib '../../';
    with 'Net::BitTorrent::Protocol::BEP03::Metadata';
    no Any::Moose;
    __PACKAGE__->meta->make_immutable();
}
1
