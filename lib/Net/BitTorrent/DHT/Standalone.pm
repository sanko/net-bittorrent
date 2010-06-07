package Net::BitTorrent::DHT::Standalone;
{
    use Moose::Role;
    use Net::BitTorrent::Network::UDP;
    our $MAJOR = 0.075; our $MINOR = 0; our $DEV = -1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    has 'port' => (is      => 'ro',
                   lazy    => 1,
                   default => 0,
                   isa     => 'Int',
                   writer  => '_port'
    );
    has 'udp' => (init_arg   => undef,
                  is         => 'ro',
                  isa        => 'Net::BitTorrent::Network::UDP',
                  lazy_build => 1
    );

    sub _build_udp {
        my ($self) = @_;
        Net::BitTorrent::Network::UDP->new(
            port                => $self->port,
            ipv4_on_data_in     => sub { $self->_ipv4_on_data_in(@_) },
                ipv6_on_data_in => sub { $self->_ipv6_on_data_in(@_) }
        );
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

