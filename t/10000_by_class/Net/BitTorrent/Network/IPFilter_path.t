package t::10000_by_class::Net::BitTorrent::Network::IPFilter_path;
{
    use strict;
    use warnings;
    use Test::Most;
    use File::Spec;
    use lib '../', '../../../../../', '../../../../../lib', 'lib';
    BEGIN { require 't/10000_by_class/Net/BitTorrent/Network/IPFilter.t'; }
    use parent-norequire,
        't::10000_by_class::Net::BitTorrent::Network::IPFilter';

    #
    sub path {'./t/90000_data/96000_data/96010_ipfilter.dat'}

    sub startup_add_path : Tests(startup => 1) {
        my $s = shift;
        my $f = $s->{'ip_filter'};
        ok $f->load($s->path), sprintf '->load( \'%s\' )', $s->path;
    }

    sub test_ranges : Test( no_plan ) {
        my $s = shift;
        my $f = $s->{'ip_filter'};
        is $f->count_ranges, 24, 'IPFilter parsed 34 ranges from ' . $s->path;
        #use Data::Dump;
        #ddx $f;

        #isa_ok $f->add_range('127.0.0.1', '128.32.236.226', 44, 'Test A'),
        #    'Net::BitTorrent::Network::IPFilter::Range', 'new range A';
        #is $f->count_ranges, 1, 'There is now one range';
        #isa_ok $f->add_range('127.0.0.1', '128.32.236.226', 21, 'Test B'),
        #    'Net::BitTorrent::Network::IPFilter::Range', 'new range B';
        #is $f->count_ranges, 2, 'There are now two ranges';
        #is_deeply $f->ranges,
        #    [bless({access_level => 0,
        #            description  => 'Test A',
        #            lower        => "\0\0\0\0\0\0\0\0\0\0\0\0\x7F\0\0\1",
        #            upper        => "\0\0\0\0\0\0\0\0\0\0\0\0\x80 \xEC\xE2",
        #           },
        #           'Net::BitTorrent::Network::IPFilter::Range'
        #     ),
        #     bless({access_level => 0,
        #            description  => 'Test B',
        #            lower        => "\0\0\0\0\0\0\0\0\0\0\0\0\x7F\0\0\1",
        #            upper        => "\0\0\0\0\0\0\0\0\0\0\0\0\x80 \xEC\xE2",
        #           },
        #           'Net::BitTorrent::Network::IPFilter::Range'
        #     )
        #    ],
        #    'ranges were loaded correctly';
    }

    sub check_filter : Test( 2 ) {
        my $s = shift;
        my $f = $s->{'ip_filter'};
        ok !$f->is_banned('127.0.0.1'), '127.0.0.1 is not filtered';
        isa_ok($f->is_banned('4.18.55.148'),
               'Net::BitTorrent::Network::IPFilter::Range',
               'EMI is banned');    # EMI Music Publishing
    }

    #
    chdir '../../../../..'  if !-f path();
    __PACKAGE__->runtests() if !caller;
}
1;
