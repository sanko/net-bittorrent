package Net::BitTorrent::Types;
# Custom types
use Type::Tiny;
use Types::Standard qw[ArrayRef CodeRef Enum HashRef Int Ref Str];
my $FILE = Type::Tiny->new(name       => 'File',
                           parent     => Str,
                           constraint => sub { -f $_ },
                           message    => sub {"$_ isn't an existing file"},
);
my $RESERVED = Type::Tiny->new(name       => 'Reserved',
                               parent     => Str,
                               constraint => sub { length $_ == 8 },
                               message => sub {'reserved data is malformed'}
);
my $PEERID = Type::Tiny->new(
    name       => 'PeerID',
    parent     => Str,
    constraint => sub { length $_ == 20 },
    message    => sub {
        'Peer ID must be 20 chars in length';
    }
);
my $INFOHASH = Type::Tiny->new(
    name       => 'Infohash',
    parent     => Str,
    constraint => sub { length $_ == 20 },
    message    => sub {
        'Infohashes are 20 bytes in length';
    }
);
