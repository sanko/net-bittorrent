package t::10000_by_class::Net::BitTorrent_single_port;
{
    use strict;
    use warnings;
    use lib '../', '../../../', '../../../lib', 'lib';
    BEGIN { require 't/10000_by_class/Net/BitTorrent.t'; }
    use parent-norequire, 't::10000_by_class::Net::BitTorrent';
    use Test::More;
    use Test::Moose;
    use 5.10.0;

    #
    sub port { state $x = int rand 65000 }

    #
    __PACKAGE__->runtests() if !caller;
}
1;
