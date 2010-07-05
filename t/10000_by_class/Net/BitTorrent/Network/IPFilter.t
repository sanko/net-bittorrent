package t::10000_by_class::Net::BitTorrent::Network::IPFilter;
{
    use strict;
    use warnings;
    use Test::More;
    use parent 'Test::Class';
    use lib '../../../../../lib', 'lib';

    #
    sub class    {'Net::BitTorrent::Network::IPFilter'}
    sub new_args { [] }

    #
    sub startup : Tests(startup => 3) {
        my $self = shift;
        use_ok $self->class;
        can_ok $self->class, 'new';
        $self->{'ip_filter'} = new_ok $self->class, $self->new_args;
    }

    sub setup : Test(setup) {
        my $self = shift;
    }

    sub test_rules : Test( no_plan ) {
        my $s = shift;
        my $f = $s->{'ip_filter'};
        ok $f->is_empty, 'IPFilter has no rules yet';
        isa_ok $f->add_rule('127.0.0.1', '128.32.236.226', 44, 'Test A'),
            'Net::BitTorrent::Network::IPFilter::Rule', 'new rule A';
        is $f->count_rules, 1, 'There is now one rule';
        isa_ok $f->add_rule('127.0.0.1', '128.32.236.226', 21, 'Test B'),
            'Net::BitTorrent::Network::IPFilter::Rule', 'new rule B';
        is $f->count_rules, 2, 'There are now two rules';
        is_deeply $f->rules,
            [bless({access_level => 44,
                    description  => 'Test A',
                    lower        => "\0\0\0\0\0\0\0\0\0\0\0\0\x7F\0\0\1",
                    upper        => "\0\0\0\0\0\0\0\0\0\0\0\0\x80 \xEC\xE2",
                   },
                   'Net::BitTorrent::Network::IPFilter::Rule'
             ),
             bless({access_level => 21,
                    description  => 'Test B',
                    lower        => "\0\0\0\0\0\0\0\0\0\0\0\0\x7F\0\0\1",
                    upper        => "\0\0\0\0\0\0\0\0\0\0\0\0\x80 \xEC\xE2",
                   },
                   'Net::BitTorrent::Network::IPFilter::Rule'
             )
            ],
            'rules were loaded correctly';
    }

    #
    __PACKAGE__->runtests() if !caller;
}
1;
