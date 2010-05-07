package t::10000_by_class::Net::BitTorrent::DHT;
{
    use strict;
    use warnings;
    use Test::Most;
    use parent 'Test::Class';
    use lib '../../../../lib', 'lib';

    #
    sub class {'Net::BitTorrent::DHT'}

    sub new_args {
        require Net::BitTorrent;
        [client => Net::BitTorrent->new()];
    }

    #
    sub startup : Tests(startup => 3) {
        my $self = shift;
        use_ok $self->class;
        can_ok $self->class, 'new';
        $self->{'dht'} = new_ok $self->class, $self->new_args;
    }

    sub setup : Test(setup) {
        my $self = shift;
    }

    sub creation : Test {
        my $self = shift;
        isa_ok($self->{dht}, $self->class)
            or $self->FAIL_ALL($self->class . '->new failed');
    }

    sub nodeid : Test {
        my $pig = shift->{dht};
        ok($pig->nodeid, 'nodeid is defined');
    }

    #
    __PACKAGE__->runtests() if !caller;
}
1;
