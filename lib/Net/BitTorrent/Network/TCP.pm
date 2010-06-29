package Net::BitTorrent::Network::TCP;
{
    use Moose::Role;
    use AnyEvent;
    use lib '../../../../lib';
    use Net::BitTorrent::Network::Utility qw[server];
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 2; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    #
    my %_sock_types = (4 => '0.0.0.0', 6 => '::');
    for my $ipv (keys %_sock_types) {
        has 'tcp'
            . $ipv => (is         => 'ro',
                       init_arg   => undef,
                       isa        => 'Object',
                       lazy_build => 1
            );
        has 'tcp' 
            . $ipv
            . '_sock' => (is         => 'ro',
                          init_arg   => undef,
                          isa        => 'GlobRef',
                          lazy_build => 1,
                          weak_ref   => 1,
                          writer     => '_tcp' . $ipv . '_sock'
            );
        has 'tcp' 
            . $ipv
            . '_host' => (is      => 'ro',
                          isa     => 'Str',
                          default => $_sock_types{$ipv},
                          writer  => '_tcp' . $ipv . '_host'
            );
        has 'tcp' . $ipv . '_port' => (
            is      => 'ro',
            isa     => 'Int',
            writer  => '_tcp' . $ipv . '_port',
            default => 0,                         # random
            trigger => sub {
                my ($self, $new, $old) = @_;
                if (defined $old && $old != $new) {

                    # XXX - Something's not right.
                }
            }
        );
        after 'BUILD' => sub {
            my ($self, $args) = @_;
            ...;
            if (  !defined $args->{'tcp' . $ipv . '_port'}
                && defined $args->{'port'})
            {   my $call = '_tcp' . $ipv . '_port';
                $self->$call($args->{'port'});
            }
            {    # Open up!
                my $call = 'tcp' . $ipv;
                $self->$call()
                    if !defined $args->{'disable_tcp' . $ipv};
            }
        };
    }
    {

        sub _build_tcp6 {
            my ($self) = @_;
            return server(
                $self->tcp6_host,
                $self->tcp6_port,
                sub { $self->_on_tcp6_in(@_); },
                sub {
                    my ($sock, $host, $port) = @_;
                    if ($self->tcp6_port && $self->tcp6_port != $port) {
                        ...;
                    }
                    $self->_tcp6_sock($sock);
                    $self->_tcp6_host($host);
                    $self->_tcp6_port($port);
                },
                'tcp'
            );
        }

        sub _build_tcp4 {
            my ($self) = @_;
            return server(
                $self->tcp4_host,
                $self->tcp4_port,
                sub { $self->_on_tcp4_in(@_); },
                sub {
                    my ($sock, $host, $port) = @_;
                    if ($self->tcp4_port && $self->tcp4_port != $port) {
                        ...;
                    }
                    $self->_tcp4_sock($sock);
                    $self->_tcp4_host($host);
                    $self->_tcp4_port($port);
                },
                'tcp'
            );
        }
    }
}
1;

=pod

=head1 NAME

Net::BitTorrent::Network::TCP - Moose role for instances/classes who require direct access to a listing TCP port

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
