package t::10000_by_class::Net::BitTorrent::DHT;
{
    use strict;
    use warnings;
    use Test::More;
    use parent 'Test::Class';
    use lib '../../../../lib', 'lib';

    #
    sub class {'Net::BitTorrent::DHT'}

    sub new_args : Tests( 1 ) {
        my $t = shift;
        require Net::BitTorrent;
        [client => new_ok(
             'Net::BitTorrent',
             [port             => [1338, 1339, 0],
              on_listen_failed => sub {
                  my ($s, $a) = @_;
                  diag $a->{'message'};
                  $t->{'cv'}->send if $a->{'protocol'} =~ m[udp];
              },
              on_listen_success => sub {
                  my ($s, $a) = @_;
                  diag $a->{'message'};
                  }
             ],
             'decoy NB client'
         )
        ];
    }

    #
    sub startup : Tests(startup => no_plan) {
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
