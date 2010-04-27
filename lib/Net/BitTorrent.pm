package Net::BitTorrent;
{
    use Moose;
    use Moose::Util::TypeConstraints;
    our $MAJOR = 0.075; our $MINOR = 0; our $DEV = -1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use AnyEvent;

    #
    sub timer { shift; AnyEvent->timer(@_) }
    sub run { AnyEvent->condvar->recv }
    has 'torrents' => (traits  => ['Array'],
                       isa     => 'ArrayRef[Net::BitTorrent::Torrent]',
                       is      => 'ro',
                       reader  => '_torrents',
                       default => sub { [] },
                       coerce  => 1,
                       handles => {
                                  add_torrent     => 'push',
                                  clear_torrents  => 'clear',
                                  count_torrents  => 'count',
                                  filter_torrents => 'grep',
                                  find_torrent    => 'first',
                                  has_torrents    => 'count',
                                  infohashes => ['map', sub { $_->infohash }],
                                  map_torrents     => 'map',
                                  no_torrents      => 'is_empty',
                                  shuffle_torrents => 'shuffle',
                                  sort_torrents    => 'sort',
                                  torrent          => 'get',
                       }
    );
    around 'add_torrent' => sub {
        my ($code, $self) = (shift, shift);
        my $torrent;
        if (blessed $_[0]) { $torrent = $_[0]; }
        else {
            require Net::BitTorrent::Torrent;
            $torrent = Net::BitTorrent::Torrent->new(@_);
        }
        return
               blessed $torrent
            && $code->($self, $torrent)
            && $torrent->client($self);
    };

    #
    has 'port' => (
        isa      => 'Int',
        is       => 'ro',
        init_arg => 'Port',
        writer   => '_port',
        default  => 0,
        trigger  => sub {
            my ($self, $new, $old) = @_;
            if (defined $old && $new && $old) {
                warn "TODO: Re-open servers";
            }
        }
    );

    #
    my @sockets = qw[tcp_ipv6 tcp_ipv4 udp_ipv6 udp_ipv4];
    has [@sockets] => (init_arg   => undef,
                       is         => 'ro',
                       isa        => 'Object',
                       lazy_build => 1
    );

    sub BUILD {
        my ($self, $args) = @_;
        {    # Non-blocking socket creation
            my @sock_types = grep {
                my ($ipv) = (m[(ipv\d)$]);
                $args->{'_no_' . $ipv}    # !!! - Disable IPv4 or IPv6
                    ? ()
                    : $_
            } @sockets;
            my $cv = AnyEvent->condvar;
            $cv->begin;
            my (@watchers, $coderef);
            $coderef = sub {
                shift @watchers if @watchers;
                my $sock_type = shift @sock_types;
                $self->$sock_type();
                push @watchers,
                    AE::idle(@sock_types ? $coderef : sub { $cv->end });
            };
            push @watchers, AE::idle($coderef);
            $cv->recv;
            shift @watchers;
        }
    }
    {
        use Socket qw[/SOCK_/ /F_INET/ inet_aton /sockaddr_in/ inet_ntoa
            SOL_SOCKET SO_REUSEADDR
        ];

        sub ip2paddr ($) {    # snagged from NetAddr::IP::Util
            my ($ipv6) = @_;
            return undef unless $ipv6;
            return inet_aton($1) if $ipv6 =~ m[^(?:::ffff:)?([^:]+)$]i; # IPv4
            if ($ipv6 =~ /^(.*:)(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})$/)
            {    # mixed hex, dot-quad
                return undef if $2 > 255 || $3 > 255 || $4 > 255 || $5 > 255;
                $ipv6 = sprintf("%s%X%02X:%X%02X", $1, $2, $3, $4, $5)
                    ;    # convert to pure hex
            }
            my $c;
            return undef
                if $ipv6 =~ /[^:0-9a-fA-F]/ ||    # non-hex character
                    (($c = $ipv6) =~ s/::/x/ && $c =~ /(?:x|:):/)
                    ||                            # double :: ::?
                    $ipv6 =~ /[0-9a-fA-F]{5,}/;   # more than 4 digits
            $c = $ipv6 =~ tr/:/:/;                # count the colons
            return undef if $c < 7 && $ipv6 !~ /::/;
            if ($c > 7) {    # strip leading or trailing ::
                return undef
                    unless $ipv6 =~ s/^::/:/
                        || $ipv6 =~ s/::$/:/;
                return undef if --$c > 7;
            }
            while ($c++ < 7) {    # expand compressed fields
                $ipv6 =~ s/::/:::/;
            }
            $ipv6 .= 0 if $ipv6 =~ /:$/;
            my @hex = split(/:/, $ipv6);
            foreach (0 .. $#hex) {
                $hex[$_] = hex($hex[$_] || 0);
            }
            return pack("n8", @hex);
        }

        sub paddr2ip ($) {        # snagged from NetAddr::IP::Util
            return inet_ntoa($_[0]) if length $_[0] == 4;    # ipv4
            return unless length($_[0]) == 16;
            my @hex = (unpack('n8', $_[0]));
            $hex[9] = $hex[7] & 0xff;
            $hex[8] = $hex[7] >> 8;
            $hex[7] = $hex[6] & 0xff;
            $hex[6] >>= 8;
            my $return = sprintf("%X:%X:%X:%X:%X:%X:%D:%D:%D:%D", @hex);
            $return =~ s/(0+:)+/:/;
            $return =~ s/^0+//;
            $return =~ s/^:+/::/;
            $return =~ s/::0+/::/;
            return $return;
        }

        sub pack_sockaddr {
            my ($port, $packed_host) = @_;
            return length $packed_host == 4
                ? sockaddr_in($port, $packed_host)
                : pack('SnLa16L', PF_INET6, $port, 0, $packed_host, 0);
        }

        sub unpack_sockaddr {
            my ($packed_host) = @_;
            return
                length $packed_host == 28
                ? (unpack('SnLa16L', $packed_host))[1, 3]
                : unpack_sockaddr_in($packed_host);
        }

        sub _server {
            my ($host, $port, $callback, $prepare, $proto) = @_;
            my $_packed_host = ip2paddr($host);
            my $type = length $_packed_host == 4 ? PF_INET : PF_INET6;
            $port ||= 0;
            $port =~ m[^(\d+)$];
            $port = $1;
            socket my ($socket), $type,
                $proto eq 'udp' ? SOCK_DGRAM : SOCK_STREAM,
                getprotobyname($proto)
                || return;

        # - What is the difference between SO_REUSEADDR and SO_REUSEPORT?
        #    [http://www.unixguide.net/network/socketfaq/4.11.shtml]
        # - setsockopt - what are the options for ActivePerl under Windows NT?
        #    [http://perlmonks.org/?node_id=63280]
        #      setsockopt($_tcp, SOL_SOCKET, SO_REUSEADDR, pack(q[l], 1))
        #         or return;
        # SO_REUSEPORT is undefined on Win32... Boo...
            return
                if !setsockopt $socket, SOL_SOCKET, SO_REUSEADDR,
                pack('l', 1);
            return if !bind $socket, pack_sockaddr($port, $_packed_host);
            if (defined $prepare) {
                my ($_port, $packed_ip) = unpack_sockaddr getsockname $socket;
                $prepare->($socket, paddr2ip($packed_ip), $_port);
            }
            return if $proto ne 'udp' && !listen($socket, 8);
            return AE::io(
                $socket, 0,
                $proto eq 'udp'
                ? sub {
                    warn 'Mehhhhhhh...';
                    my $flags = 0;
                    if ($socket
                        && (my $peer = recv $socket, my ($data), 1024, $flags)
                        )
                    {   my ($service, $host) = unpack_sockaddr $peer;
                        $callback->($socket, paddr2ip($host), $service, $data,
                                    $flags
                        );
                    }
                    }
                : sub {
                    warn 'EHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH';
                    while ($socket
                           && (my $peer = accept my $fh, $socket))
                    {   fh_nonblocking $fh, 1;
                        my ($service, $host)
                            = unpack_sockaddr getsockname $peer;
                        $callback->($fh, paddr2ip($host), $service, $peer);

             #if ($state->{fh} && (accept my ($peer), $state->{fh})) {
             #    my ($service, $host) = unpack_sockaddr getsockname $peer;
             #    $callback->($state->{fh}, paddr2ip($host), $service, $peer);
             #}
                    }
                }
            );
        }
    }

    sub _build_tcp_ipv4 {
        my $self = shift;
        my $port = $self->port;
        return _server(
            '0.0.0.0',
            $port,
            sub { die 'Accept!'; },
            sub {
                my ($_fh, $_host, $_port) = @_;
                $self->_port($_port) if $port != $_port;
            },
            'tcp'
        );
    }

    sub _build_udp_ipv4 {
        my $self = shift;
        my $port = $self->port;
        return _server(
            '0.0.0.0',
            $port,
            sub { die 'Accept!'; },
            sub {
                my ($_fh, $_host, $_port) = @_;
                $self->_port($_port) if $port != $_port;
            },
            'udp'
        );
    }

    sub _build_tcp_ipv6 {
        my $self = shift;
        my $port = $self->port;
        return _server(
            '::', $port,
            sub { die 'Accept!'; },
            sub {
                my ($_fh, $_host, $_port) = @_;
                $self->_port($_port) if $port != $_port;
            },
            'tcp'
        );
    }

    sub _build_udp_ipv6 {
        my $self = shift;
        my $port = $self->port;
        return _server(
            '::', $port,
            sub { die 'Accept!'; },
            sub {
                my ($_fh, $_host, $_port) = @_;
                $self->_port($_port) if $port != $_port;
            },
            ,
            'udp'
        );
    }
}
1;

=pod

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
