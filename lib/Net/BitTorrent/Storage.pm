package Net::BitTorrent::Storage;
{
    use Any::Moose;
    use Any::Moose '::Util::TypeConstraints';
    our $MAJOR = 0.075; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use lib '../../';
    use Net::BitTorrent::Storage::File;
    has 'torrent' => (is       => 'rw',
                      isa      => 'Net::BitTorrent::Torrent',
                      init_arg => 'Torrent'
    );
    subtype 'ArrayRefOfFiles' => as
        'ArrayRef[Net::BitTorrent::Storage::File]';
    coerce 'ArrayRefOfFiles' => from 'ArrayRef[HashRef]' => via {
        my $offset = 0;
        [map {
             my $obj =
                 Net::BitTorrent::Storage::File->new(Length => $_->{'length'},
                                                     Path   => $_->{'path'},
                                                     Offset => $offset
                 );
             $offset += $_->{'length'};
             $obj
             } @{$_}
        ];
    };
    coerce 'ArrayRefOfFiles' => from 'HashRef' => via {
        [Net::BitTorrent::Storage::File->new(Length => $_->{'length'},
                                             Path   => $_->{'path'}
         )
        ];
    };
    has 'files' => (
        is       => 'rw',
        isa      => 'ArrayRefOfFiles',
        init_arg => 'Files',
        coerce   => 1
    );

    has 'root' => (
        is => 'rw',
        isa => 'Str',
        init_arg => 'Root', # ??? - Should this be BaseDir/basedir
        trigger => sub {
            my ($self, $new_root, $old_root) = @_;
            if (@{$self->files} > 1 ) {
                for my $file (@{$self->files}) {
                    my $path  = $file->path;
                    shift @$path if defined $old_root;
                    unshift @$path, $new_root;
                    $file->path($path);
                }
            }
        }
    );
}
1;
