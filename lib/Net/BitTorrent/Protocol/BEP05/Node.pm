package Net::BitTorrent::Protocol::BEP05::Node;
{
    use Moose;
    use Moose::Util::TypeConstraints;
    use AnyEvent;
    use lib '../../../../../lib';
    use Net::BitTorrent::Types qw[NBTypes::DHT::NodeID];
    use Net::BitTorrent::Protocol::BEP05::Packets qw[:all];
    use 5.10.0;
    our $MAJOR = 0.075; our $MINOR = 0; our $DEV = -1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    #
    sub BUILD {1}

    #
    has 'port'     => (isa => 'Int', is => 'ro', required   => 1);
    has 'host'     => (isa => 'Str', is => 'ro', required   => 1);
    has 'sockaddr' => (isa => 'Str', is => 'ro', lazy_build => 1);

    sub _build_sockaddr {
        require Net::BitTorrent::Network::Utility;
        Net::BitTorrent::Network::Utility::sockaddr($_[0]->host, $_[0]->port);
    }
    has 'ipv6' => (isa => 'Bool', is => 'ro', lazy_build => 1);
    sub _build_ipv6 { length shift->sockaddr == 28 }
    has 'v' =>
        (isa => 'Str', is => 'ro', writer => '_v', predicate => '_has_v');
    has 'seen' => (isa       => 'Bool',
                   is        => 'ro',
                   writer    => '_seen',
                   predicate => '_has_seen'
    );
    has 'bucket' => (isa       => 'Net::BitTorrent::Protocol::BEP05::Bucket',
                     is        => 'ro',
                     writer    => 'assign_bucket',
                     weak_ref  => 1,
                     predicate => 'has_bucket'
    );
    has 'routing_table' => (
                      isa => 'Net::BitTorrent::Protocol::BEP05::RoutingTable',
                      is  => 'ro',
                      predicate => 'has_routing_table',
                      writer    => '_routing_table',
                      required  => 1,
                      weak_ref  => 1,
                      handles => {send => 'send', local_node => 'local_node'}
    );
    around 'send' => sub {
        my ($code, $self, $packet) = @_;
        $code->($self, $self, $packet);
    };
    has 'nodeid' => (isa       => 'NBTypes::DHT::NodeID',
                     is        => 'ro',
                     writer    => '_nodeid',
                     predicate => 'has_nodeid',
                     coerce    => 1
    );
    after '_nodeid' => sub { $_[0]->routing_table->assign_node($_[0]) };
    has 'outstanding_requests' => (isa     => 'HashRef[HashRef]',
                                   is      => 'ro',
                                   traits  => ['Hash'],
                                   handles => {add_request    => 'set',
                                               get_request    => 'get',
                                               del_request    => 'delete',
                                               expire_request => 'delete',
                                               is_expecting   => 'defined'
                                   },
                                   init_arg => undef,
                                   default  => sub { {} }
    );
    after 'expire_request' => sub { shift->miss };
    around 'add_request'   => sub {
        my ($code, $self, $tid, $args) = @_;
        require Scalar::Util;
        Scalar::Util::weaken $self;
        $args->{'timeout'} //= AE::timer(
            20, 0,
            sub {    #warn 'Tired of waiting for reply!';
                $self->expire_request($tid);    # May ((poof)) $self
            }
        );
        $code->($self, $tid, $args);
    };
    has 'ping_timer' => (isa      => 'ArrayRef',
                         builder  => '_build_ping_timer',
                         is       => 'ro',
                         init_arg => undef,
                         writer   => '_ping_timer',
                         clearer  => 'touch'
    );
    has 'prev_get_peers' =>
        (isa => 'Int', is => 'rw', default => 0, lazy => 1);
    sub _build_ping_timer {
        my ($self) = @_;
        require Scalar::Util;
        Scalar::Util::weaken $self;
        AE::timer(60 * 15, 60 * 15, sub { $self->ping });
    }
    after 'BUILD' => sub {
        my ($self) = @_;
        require Scalar::Util;
        Scalar::Util::weaken $self;
        $self->_ping_timer(AE::timer(5, 0, sub { $self->ping }));
    };
    has 'birth' => (
        is => 'ro',
        isa=>'Int',
        init_arg=>undef,
        default => sub { time }
    );
    sub ping {
        my ($self) = @_;
        state $tid = 'a';
        my $packet =
            build_dht_query_ping('p_' . $tid,
                                 pack('H*', $self->local_node->nodeid->to_Hex
                                 )
            );
        my $sent = $self->send($packet);
        return $self->miss() if !$sent;
        $self->add_request('p_' . $tid, {type => 'ping'});
        $tid++;
    }

    sub _reply_ping {
        my ($self, $tid) = @_;
        warn sprintf 'Sending pong! to %s', $self->host;
        my $packet =
            build_dht_reply_ping($tid,
                                 pack('H*', $self->local_node->nodeid->to_Hex
                                 )
            );
        my $sent = $self->send($packet);
        $self->miss() if !$sent;
        return $sent;
    }

    sub find_node {
        my ($self, $target) = @_;
        state $tid = 'a';
        my $packet =
            build_dht_query_find_node('fn_' . $tid,
                                      pack('H*',
                                           $self->local_node->nodeid->to_Hex),
                                      $target
            );
        my $sent = $self->send($packet);
        return $self->miss() if !$sent;
        $self->add_request('fn_' . $tid,
                           {type => 'find_node', target => $target});
        $tid++;
    }

    sub get_peers {
        my ($self, $info_hash) = @_;
        return if $self->prev_get_peers() > time - 120;
        state $tid = 'a';
        my $packet =
            build_dht_query_get_peers('gp_' . $tid,
                                      pack('H*',
                                           $self->local_node->nodeid->to_Hex),
                                      pack('H*', $info_hash->to_Hex)
            );
        my $sent = $self->send($packet);
        return $self->miss() if !$sent;
        $self->add_request('gp_' . $tid,
                           {type => 'get_peers', info_hash => $info_hash});
        $tid++;
        $self->prev_get_peers(time);
    }
    has 'fail' => (
        isa      => 'Int',
        traits   => ['Counter'],
        default  => 0,
        is       => 'ro',
        handles  => {miss => 'inc',},
        init_arg => undef,
        trigger  => sub {
            my ($self, $new, $old) = @_;
            $self->routing_table->del_node($self) if $new == 5;
        }
    );
}
1;
