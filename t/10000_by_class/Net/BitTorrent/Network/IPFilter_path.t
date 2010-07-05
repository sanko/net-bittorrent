package t::10000_by_class::Net::BitTorrent::Network::IPFilter_path;
{
    use strict;
    use warnings;
    use Test::More;
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

    sub test_rules : Test( no_plan ) {
        my $s = shift;
        my $f = $s->{'ip_filter'};
        is $f->count_rules, 24, 'IPFilter parsed 24 rules from ' . $s->path;
        require File::Temp;
        my $file = File::Temp->new(TEMPLATE => 'ipfilter_XXXX',
                                   SUFFIX   => '.dat');
        ok $f->save($file->filename), 'saving rules to ' . $file->filename;
        my $r = new_ok $s->class, [],
            'empty instance which will slurp rules from ' . $file->filename;
        ok $r->load($file->filename), 'reading rules from ' . $file->filename;
        is_deeply $r, $f, 'rules were loaded correctly';
    }

    sub check_filter : Test( 2 ) {
        my $s = shift;
        my $f = $s->{'ip_filter'};
        ok !$f->is_banned('127.0.0.1'), '127.0.0.1 is not filtered';
        isa_ok($f->is_banned('4.18.55.148'),
               'Net::BitTorrent::Network::IPFilter::Rule',
               'EMI is banned');    # EMI Music Publishing
    }

    #
    chdir '../../../../..'  if !-f path();
    __PACKAGE__->runtests() if !caller;
}
1;
