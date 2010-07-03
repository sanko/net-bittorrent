package Net::BitTorrent::DHT::Standalone;
{
    use Moose::Role;
    use lib '../../../../lib';
    use Net::BitTorrent::Protocol::BEP03::Bencode qw[bdecode];
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    has 'port' => (is      => 'ro',
                   isa     => 'Int|ArrayRef[Int]',
                   builder => '_build_port',
                   writer  => '_set_port'
    );

    sub _build_port {
        my $s = shift;
        $s->has_client ? $s->client->port : 0;
    }
    my %_sock_types = (4 => '0.0.0.0', 6 => '::');
    for my $ipv (keys %_sock_types) {
        has 'udp'
            . $ipv => (is         => 'ro',
                       init_arg   => undef,
                       isa        => 'Object',
                       lazy_build => 1,
                       writer     => '_set_udp' . $ipv,
                       predicate  => '_has_udp' . $ipv
            );
        has 'udp' 
            . $ipv
            . '_sock' => (is         => 'ro',
                          init_arg   => undef,
                          isa        => 'GlobRef',
                          lazy_build => 1,
                          weak_ref   => 1,
                          writer     => '_set_udp' . $ipv . '_sock',
                          predicate  => '_has_udp' . $ipv . '_sock'
            );
        has 'udp' 
            . $ipv
            . '_host' => (is        => 'ro',
                          isa       => 'Str',
                          default   => $_sock_types{$ipv},
                          writer    => '_set_udp' . $ipv . '_host',
                          predicate => '_has_udp' . $ipv . '_host'
            );
    }

    sub _build_udp6 {
        my $s = shift;
        my $server;
        for my $port (ref $s->port ? @{$s->port} : $s->port) {
            require Net::BitTorrent::Network::Utility;
            $server = Net::BitTorrent::Network::Utility::server(
                $s->udp6_host,
                $port,
                sub { $s->_on_udp6_in(@_); },
                sub {
                    my ($actual_socket, $actual_host, $actual_port) = @_;

                    #if ($self->port != $port) { ...; }
                    $s->_set_udp6_sock($actual_socket);
                    $s->_set_udp6_host($actual_host);
                    $s->_set_port($actual_port);
                },
                'udp'
            );
            last if defined $server;
        }
        return $server if $server;
        $s->trigger_listen_failed(
            {protocol => 'udp6',
             severity => 'fatal',
             event    => 'listen_failed',
             message => 'Failed to open IPv6 port to the outside world: ' . $!
            }
        );
        return;
    }

    sub _build_udp4 {
        my $s = shift;
        my $server;
        for my $port (ref $s->port ? @{$s->port} : $s->port) {
            require Net::BitTorrent::Network::Utility;
            $server = Net::BitTorrent::Network::Utility::server(
                $s->udp4_host,
                $port,
                sub { $s->_on_udp4_in(@_); },
                sub {
                    my ($actual_socket, $actual_host, $actual_port) = @_;

                    #if ($self->port != $port) { ...; }
                    $s->_set_udp4_sock($actual_socket);
                    $s->_set_udp4_host($actual_host);
                    $s->_set_port($actual_port);
                },
                'udp'
            );
            last if defined $server;
        }
        return $server if $server;
        $s->trigger_listen_failed(
            {protocol => 'udp4',
             severity => 'fatal',
             event    => 'listen_failed',
             message => 'Failed to open IPv4 port to the outside world: ' . $!
            }
        );
        return;
    }
}
1;

=pod

=head1 NAME

Net::BitTorrent::DHT::Standalone

=head1 Description

This role is applied automatically when the Net::BitTorrent::DHT constructor
is called without a blessed Net::BitTorrent object in the C<client> parameter.
For API documentation, see L<Net::BitTorrent::DHT>.

Standalone DHT nodes may be useful for statistical purposes.

=head1 Methods

Aside from the public L<constructor|/"Net::BitTorrent::DHT->new( )">, the API
L<Net::BitTorrent::DHT::Standalone|Net::BitTorrent::DHT::Standalone> is
exactly the same as the L<Net::BitTorrent::DHT|Net::BitTorrent::DHT>.

=head2 Net::BitTorrent::DHT->new( )

This creates a new standalone DHT node. Random ports will be opened to
incoming UDP connections via IPv4 and IPv6.

    use Net::BitTorrent::DHT;
    my $node = Net::BitTorrent::DHT->new( );

=head1 Author

Sanko Robinson <sanko@cpan.org> - http://sankorobinson.com/

CPAN ID: SANKO

=head1 License and Legal

Copyright (C) 2008-2010 by Sanko Robinson <sanko@cpan.org>

This program is free software; you can redistribute it and/or modify it under
the terms of
L<The Artistic License 2.0|http://www.perlfoundation.org/artistic_license_2_0>.
See the F<LICENSE> file included with this distribution or
L<notes on the Artistic License 2.0|http://www.perlfoundation.org/artistic_2_0_notes>
for clarification.

When separated from the distribution, all original POD documentation is
covered by the
L<Creative Commons Attribution-Share Alike 3.0 License|http://creativecommons.org/licenses/by-sa/3.0/us/legalcode>.
See the
L<clarification of the CCA-SA3.0|http://creativecommons.org/licenses/by-sa/3.0/us/>.

Neither this module nor the L<Author|/Author> is affiliated with BitTorrent,
Inc.

=for rcs $Id$

=cut
