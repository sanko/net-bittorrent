package t::10000_by_class::Net::BitTorrent::DHT::Standalone;
{
    use strict;
    use warnings;
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 12; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use lib '../', '../../../../../', '../../../../../lib', 'lib';
    BEGIN { require 't/10000_by_class/Net/BitTorrent/DHT.t'; }
    use parent-norequire, 't::10000_by_class::Net::BitTorrent::DHT';
    use Test::More;
    use Test::Moose;

    #
    sub new_args {
        my $t = shift;
        [    #port => [1337 .. 3339, 0],
           boot_nodes => [['router.utorrent.com',   6881],
                          ['router.bittorrent.com', 6881]
           ],
           on_listen_failure => sub {
               my ($s, $a) = @_;
               note $a->{'message'};
               $t->{'cv'}->end if $a->{'protocol'} =~ m[udp];
           },
           on_listen_success =>
               sub { my ($s, $a) = @_; note $a->{'message'}; }
        ];
    }

    sub startup : Tests(startup => no_plan) {
        my $self = shift;
        use_ok $self->class;
        can_ok $self->class, 'new';
        explain $self->new_args;
        $self->{'dht'} = new_ok $self->class, $self->new_args;
    }

    sub check_role : Test( 9 ) {
        my $self = shift;
        does_ok $self->{'dht'}, 'Net::BitTorrent::DHT::Standalone';
        has_attribute_ok $self->{'dht'}, $_ for qw[port
            udp6 udp6_sock udp6_host
            udp4 udp4_sock udp4_host ];
        ok !$self->{'dht'}->has_client,
            '... standalone dht nodes have no client';
    }

    #
    __PACKAGE__->runtests() if !caller;
}
1;
