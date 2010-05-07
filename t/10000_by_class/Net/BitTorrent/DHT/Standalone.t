package t::10000_by_class::Net::BitTorrent::DHT::Standalone;
{
    use strict;
    use warnings;
    use lib '../', '../../../../../', '../../../../../lib', 'lib';
    require 't/10000_by_class/Net/BitTorrent/DHT.t';
    use parent -norequire, 't::10000_by_class::Net::BitTorrent::DHT';
    use parent 'Test::Class';
    use Test::Moose;
    use Test::More;

    #
    sub new_args { () }

    sub check_role : Test( 3 ) {
        my $self = shift;
        does_ok $self->{'dht'},          'Net::BitTorrent::DHT::Standalone';
        has_attribute_ok $self->{'dht'}, 'udp';
        ok !$self->{'dht'}->has_client,
            '... standalone dht nodes have no client';
    }

    #
    __PACKAGE__->runtests() if !caller;
}
1;
