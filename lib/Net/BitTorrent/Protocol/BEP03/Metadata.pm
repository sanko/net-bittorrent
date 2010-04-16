package Net::BitTorrent::Protocol::BEP03::Metadata;
{
    use Any::Moose 'Role';
    our $MAJOR = 0.075; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    use lib '../../../../';
    use Net::BitTorrent::Protocol::BEP03 qw[:all];
    use Net::BitTorrent::Storage;

    # Extends Net::BitTorrent::Torrent
    use File::Spec::Functions qw[rel2abs];
    has 'basedir' => (is       => 'ro',
                      isa      => 'Str',
                      required => 1,
                      default  => rel2abs '.',
                      init_arg => 'BaseDir'
    );
    has 'storage' => (
        is  => 'ro',
        isa => 'Net::BitTorrent::Storage',

        #lazy_build => 1,
        default  => sub { Net::BitTorrent::Storage->new() },
        init_arg => 'Storage'
    );
    has 'trackers' => (isa => 'ArrayRef[Net::BitTorrent::Tracker]',
                       is  => 'rw',);
    after 'trackers' => sub {    # All trackers have this torrent as parent
        my ($self, $trackers) = @_;
        foreach my $tracker (@{$trackers || []}) {
            $tracker->torrent($self);
        }
    };
    has 'metadata' => (
        isa      => 'HashRef',
        is       => 'rw',
        init_arg => undef,       # cannot set this with new()
        trigger  => sub {
            my ($self, $new_value, $old_value) = @_;
            if (@_ == 2) {

                # parse files and trackers
                use Data::Dump;
                ddx $new_value;
                if (defined $new_value->{'announce-list'}) {
                }
                else {
                }

                #
                my @files;
                if (defined $new_value->{'info'}{'files'})
                {    # Multi-file .torrent
                    $self->storage->files($new_value->{'info'}{'files'});
                    $self->storage->root($new_value->{'info'}{'name'});
                }
                else {    # single file torrent; use the name
                    $self->storage->files(
                              [{path   => [$new_value->{'info'}{'name'}],
                                length => $new_value->{'info'}{'length'}
                               }
                              ]
                    );
                }
                return 1;
            }
            warn 'Someone changed the metadata!';
        }
    );
    has '_raw_metadata' => (
        isa        => 'Str',
        lazy_build => 1,
        is         => 'rw',
        init_arg   => undef,    # cannot set this with new()
        trigger    => sub {
            my ($self, $new_value, $old_value) = @_;
            return $self->metadata(scalar bdecode $new_value) if @_ == 2;

            # XXX - set the current value back to the old value
        }
    );
    has 'path' => (
        is       => 'ro',
        isa      => 'Str',
        required => 1,
        init_arg => 'Path',
        trigger  => sub {
            my ($self, $new_value, $old_value) = @_;
            if (@_ == 2) {
                open(my ($FH), '<', $new_value)
                    || return !($_[0] = undef);    # exterminate! exterminate!
                sysread($FH, my ($METADATA), -s $FH) == -s $FH
                    || return !($_[0] = undef);    # destroy!
                $self->_raw_metadata($METADATA);
                return close $FH;
            }

            # XXX - set the current value back to the old value
        }
    );

    # Quick accessors
    sub piece_length { return shift->metadata->{'info'}{'piece length'} }
    sub pieces       { return shift->metadata->{'info'}{'pieces'} }
}
1
