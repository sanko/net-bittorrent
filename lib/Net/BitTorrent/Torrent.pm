package Net::BitTorrent::Torrent;
{
    use 5.010;
    use Moose;
    our $MAJOR = 0.074; our $MINOR = 1; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    #
    use lib '../../';
    extends 'Net::BitTorrent::Protocol::BEP03::Metadata';

    #
    has '+_prepared_metadata' =>
        (init_arg => 'prepared_metadata', required => 1);
    around 'BUILDARGS' => sub {
        my $orig  = shift;
        my $class = shift;
        #<<< perltidy will skip this
        state $bedecode_constraint //=
        #>>>
            Moose::Util::TypeConstraints::find_type_constraint(
                                           'Net::BitTorrent::Types::Bdecode');
        if (@_ == 1 && !ref $_[0]) {
            open my $FH, '<', $_[0] || confess 'Failed to open %s: %s', $_[0],
                $!;
            sysread $FH, my $TORRENT, -s $FH;
            $TORRENT = $bedecode_constraint->coerce($TORRENT);
            return
                $class->$orig(
                           files => (
                                $TORRENT->{'info'}{'files'} // [
                                    {path   => [$TORRENT->{'info'}{'name'}],
                                     length => $TORRENT->{'info'}{'length'}
                                    }
                                ]
                           ),
                           name         => $TORRENT->{'info'}{'name'},
                           piece_length => $TORRENT->{'info'}{'piece length'},
                           pieces       => $TORRENT->{'info'}{'pieces'},
                           prepared_metadata => $TORRENT
                );
        }
        return $class->$orig(@_);
    };

    #
    __PACKAGE__->meta->make_immutable;
    no Moose;
}
1;
