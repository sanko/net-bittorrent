package Net::BitTorrent::Network::Utility;
{
    use strict;
    use warnings;
    use Moose;
    use AnyEvent;
    use Socket qw[/SOCK_/ /F_INET/ inet_aton /sockaddr_in/ inet_ntoa
        SOL_SOCKET SO_REUSEADDR
    ];
    my %cache;
    require Exporter;
    our @ISA = qw[Exporter];
    our %EXPORT_TAGS = (socket   => [qw[client server]],
                        paddr    => [qw[ip2paddr paddr2ip]],
                        sockaddr => [qw[pack_sockaddr unpack_sockaddr]],
                        vars     => [qw[%cache]]
    );
    our @EXPORT_OK = @{$EXPORT_TAGS{'all'}}
        = sort map {@$_} values %EXPORT_TAGS;

    sub ip2paddr ($) {    # snagged from NetAddr::IP::Util
        my ($ipv6) = @_;
        return undef unless defined $ipv6;
        return $cache{$ipv6} if defined $cache{$ipv6};
        if (!defined $cache{$ipv6}) {
            if ($ipv6 =~ m[^(?:::ffff:)?([^:]+)$]i) {    # IPv4
                $cache{$ipv6} = inet_aton($1);
                return $cache{$ipv6};
            }
            if ($ipv6 =~ /^(.*:)(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})$/)
            {                                            # mixed hex, dot-quad
                return undef if $2 > 255 || $3 > 255 || $4 > 255 || $5 > 255;
                $ipv6 = sprintf("%s%X%02X:%X%02X", $1, $2, $3, $4, $5)
                    ;                                    # convert to pure hex
            }
            my $c;
            return undef
                if $ipv6 =~ /[^:0-9a-fA-F]/ ||           # non-hex character
                    (($c = $ipv6) =~ s/::/x/ && $c =~ /(?:x|:):/)
                    ||                                   # double :: ::?
                    $ipv6 =~ /[0-9a-fA-F]{5,}/;          # more than 4 digits
            $c = $ipv6 =~ tr/:/:/;                       # count the colons
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
            $cache{$ipv6} = pack("n8", @hex);
        }
        return $cache{$ipv6};
    }

    sub paddr2ip ($) {    # snagged from NetAddr::IP::Util
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
        my $return
            = length $packed_host == 4
            ? sockaddr_in($port, $packed_host)
            : pack('SnLa16L', PF_INET6, $port, 0, $packed_host, 0);
        return $return;
    }

    sub unpack_sockaddr {
        my ($packed_host) = @_;
        return
            length $packed_host == 28
            ? (unpack('SnLa16L', $packed_host))[1, 3]
            : unpack_sockaddr_in($packed_host);
    }

    sub connect {
        my ($host, $port, $ready, $prepare) = @_;
        my $packed_host = ip2paddr($host) || return;
        my $type = length $packed_host == 4 ? PF_INET : PF_INET6;
        socket(my ($socket), $type, SOCK_STREAM, getprotobyname('tcp'))
            || return;
        my $_inet_aton = inet_aton($host) || return;    # TODO: Cache this
        my $pack_sockaddr_in = pack_sockaddr_in($port, $_inet_aton)
            || return;
        require AnyEvent::Util;
        AnyEvent::Util::fh_nonblocking $socket, 1;
        connect($socket, $pack_sockaddr_in);            # Nonblocking

        if (defined $prepare) {
            my ($_port, $packed_ip) = unpack_sockaddr getsockname $socket;
            $prepare->($socket, $host, $_port);
        }
        return AE::io($socket, 1, sub { $ready->($socket, $host, $port) });
    }

    sub server {
        my ($host, $port, $callback, $prepare, $proto) = @_;
        my $_packed_host = ip2paddr($host);
        my $type = length $_packed_host == 4 ? PF_INET : PF_INET6;
        socket my ($socket), $type,
            $proto eq 'udp' ? SOCK_DGRAM : SOCK_STREAM, getprotobyname($proto)
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
        require AnyEvent::Util;
        AnyEvent::Util::fh_nonblocking $socket, 1;
        return if $proto ne 'udp' && !listen($socket, 8);
        return AE::io(
            $socket, 0,
            $proto eq 'udp'
            ? sub {
                warn 'Mehhhhhhh...';
                my $flags = 0;
                if ($socket
                    && (my $peer = recv $socket, my ($data), 1024, $flags))
                {   my ($service, $host) = unpack_sockaddr $peer;
                    $callback->($socket, $peer, paddr2ip($host), $service, $data,
                                $flags
                    );
                }
                }
            : sub {
                warn 'EHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH';
                while ($socket
                       && (my $peer = accept my $fh, $socket))
                {   require AnyEvent::Util;
                    AnyEvent::Util::fh_nonblocking $fh, 1;
                    my ($service, $host) = unpack_sockaddr getsockname $peer;
                    $callback->($fh, $peer, paddr2ip($host), $service, $peer);

             #if ($state->{fh} && (accept my ($peer), $state->{fh})) {
             #    my ($service, $host) = unpack_sockaddr getsockname $peer;
             #    $callback->($state->{fh}, paddr2ip($host), $service, $peer);
             #}
                }
            }
        );
    }
}
1;
