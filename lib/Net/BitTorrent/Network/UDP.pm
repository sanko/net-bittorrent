package Net::BitTorrent::Network::UDP;
{
    use Moose::Role;
    use AnyEvent;
    use lib '../../../../lib';
    use Net::BitTorrent::Network::Utility qw[server];
    our $MAJOR = 0; our $MINOR = 74; our $DEV = 13; our $VERSION = sprintf('%0d.%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    #
    has '_port' => (is       => 'ro',
                    isa      => 'Int',
                    init_arg => 'port',
                    default  => 0         # random
    );
    my %_sock_types = (4 => '0.0.0.0', 6 => '::');
    for my $ipv (keys %_sock_types) {
        has 'udp'
            . $ipv => (is         => 'ro',
                       init_arg   => undef,
                       isa        => 'Object',
                       lazy_build => 1
            );
        has 'udp' 
            . $ipv
            . '_sock' => (is         => 'ro',
                          init_arg   => undef,
                          isa        => 'GlobRef',
                          lazy_build => 1,
                          weak_ref   => 1,
                          writer     => '_udp' . $ipv . '_sock'
            );
        has 'udp' 
            . $ipv
            . '_host' => (is         => 'ro',
                          isa        => 'Str',
                          writer     => '_udp' . $ipv . '_host',
                          lazy_build => 1,
                          builder    => sub { $_sock_types{$ipv} }
            );
        has 'udp' . $ipv . '_port' => (
            is         => 'ro',
            isa        => 'Int',
            writer     => '_udp' . $ipv . '_port',
            lazy_build => 1,
            builder    => sub {0},
            trigger    => sub {
                my ($self, $new, $old) = @_;
                if (defined $old && $old != $new) {

                    # XXX - Something's not right.
                }
            }
        );
    }
    {

        sub _build_udp6 {
            my ($self) = @_;
            my $port
                = $self->has_udp6_port ? $self->udp6_port
                : $self->has_udp4_port ? $self->udp4_port
                :                        $self->_port;
            warn $port;
            return server(
                $self->udp6_host,
                $port,
                sub { $self->_on_udp6_in(@_); },
                sub {
                    my ($sock, $host, $port) = @_;
                    if ($self->udp6_port && $self->udp6_port != $port) {
                        ...;
                    }
                    $self->_udp6_sock($sock);
                    $self->_udp6_host($host);
                    $self->_udp6_port($port);
                    warn 'udp6 :' . $port;
                },
                'udp'
            );
        }

        sub _build_udp4 {
            my ($self) = @_;
            my $port
                = $self->has_udp4_port ? $self->udp4_port
                : $self->has_udp6_port ? $self->udp6_port
                :                        $self->_port;
            warn $port;
            return server(
                $self->udp4_host,
                $port,
                sub { $self->_on_udp4_in(@_); },
                sub {
                    my ($sock, $host, $port) = @_;
                    if ($self->udp4_port && $self->udp4_port != $port) {
                        ...;
                    }
                    $self->_udp4_sock($sock);
                    $self->_udp4_host($host);
                    $self->_udp4_port($port);
                    warn 'udp4 :' . $port;
                },
                'udp'
            );
        }
    }
}
1;

=pod

=head1 NAME

Net::BitTorrent::Network::UD - Moose role for instances/classes who require direct access to a bound UDP port

=head1 Description

Nothing to see here.

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
