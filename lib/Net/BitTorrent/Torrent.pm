package Net::BitTorrent::Torrent;
{
    use 5.010;
    use Moose;
    our $MAJOR = 0; our $MINOR = 74; our $DEV = 13; our $VERSION = sprintf('%0d.%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    #
    use lib '../../';
    extends 'Net::BitTorrent::Protocol::BEP03::Metadata';

    #
    has 'client' => (isa      => 'Net::BitTorrent',
                     is       => 'ro',
                     init_arg => undef,
                     writer   => '_set_client',
                     weak_ref => 1
    );
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
            open(my $FH, '<', $_[0])
                || confess sprintf 'Failed to open %s: %s', $_[0],
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
