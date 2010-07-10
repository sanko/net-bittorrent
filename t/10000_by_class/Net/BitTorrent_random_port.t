package t::10000_by_class::Net::BitTorrent_random_port;
{
    use strict;
    use warnings;
    use lib '../', '../../../', '../../../lib', 'lib';
    BEGIN { require 't/10000_by_class/Net/BitTorrent.t'; }
    use parent-norequire, 't::10000_by_class::Net::BitTorrent';
    use Test::More;
    use Test::Moose;

    #
    sub port {0}

    #
    __PACKAGE__->runtests() if !caller;
}
1;
