#!perl -w
# $Id$
# Stolen from libwww-perl
use strict;
use warnings;
require IO::Socket;
if (@ARGV >= 2 && $ARGV[0] eq q[--port]) {
    my $port = $ARGV[1];
    require Sys::Hostname;
    my $host = Sys::Hostname::hostname();
    if (my $socket =
        IO::Socket::INET->new(PeerAddr => qq[$host:$port],
                              Timeout  => 5)
        )
    {   require IO::Select;
        if (IO::Select->new($socket)->can_read(1)) {
            my ($n, $buf);
            if ($n = sysread($socket, $buf, 512)) {
                exit if $buf eq qq[Hi there!\n];
                die
                    qq[Seems to be talking to the wrong server at $host:$port, got "$buf"\n];
            }
            elsif (defined $n) {
                die qq[Immediate EOF from server at $host:$port\n];
            }
            else {
                die qq[Can't read from server at $host:$port: $!];
            }
        }
        die qq[No response from server at $host:$port\n];
    }
    die qq[Can't connect: $@\n];
}

# server code
my $socket = IO::Socket::INET->new(Listen => 1, Timeout => 5);
my $port = $socket->sockport;
open(CLIENT, qq("$^X" "$0" --port $port |)) || die qq[Can't run $^X $0: $!\n];
if (my $client = $socket->accept) {
    print $client qq[Hi there!\n];
    close($client) || die qq[Can't close socket: $!];
}
else {
    warn qq[Test server timeout\n];
}
exit if close(CLIENT);
die qq[Can't wait for client: $!] if $!;
die qq[The can-we-talk-to-ourself test failed.\n];
