# -*- perl -*-

# t/data/etc/http_tracker.pl - Used by old 500 tests
# $Id$

use strict;
use warnings;
use Socket;

use lib q[../../../lib];
use lib q[./lib];
use Net::BitTorrent::Util qw[:compact :bencode];

$|++;

my %tracker_data;

my $port = 800;
my $host = q[127.0.0.1];

socket( my ($httpd), PF_INET, SOCK_STREAM, getprotobyname(q[tcp]) )
    || die(q[socket]);
setsockopt( $httpd, SOL_SOCKET, SO_REUSEADDR, 1 )
    || die(q[setsockopt]);

bind( $httpd,
      pack( q[Sna4x8],
            &AF_INET, $port,
            ( join q[], map { chr $_ } ( $host =~ m[(\d+)]g ) ) )
) || die(q[bind]);
listen( $httpd, SOMAXCONN ) || die(q[listen]);

( undef, $port, undef ) = unpack( q[SnC4x8], getsockname($httpd) );

while ( my $paddr = accept( my ($client), $httpd ) ) {
    my $gotten = q[];
    while ( sysread( $client, my ($data), 1024 ) ) {
        $gotten .= $data;
        if ( $data =~ m[^GET\s+(/(announce|scrape)\?([^\s]*))] ) {
            my $type = $2;
            my %hash = split m[[=&]], $3;
            $hash{q[info_hash]}
                =~ s|\%([a-f0-9]{2})|pack(q[C], hex($1))|ieg;
            my ( undef, undef, @address )
                = unpack( q[SnC4x8], getsockname($client) );
            my %reply;
            if ( $type eq q[announce] ) {
                $hash{q[peer_id]}
                    =~ s|\%([a-f0-9]{2})|pack(q[C], hex($1))|ieg;
                %reply = (
                     interval => 1500,
                     peers => $tracker_data{ $hash{info_hash} } || q[]
                );
                $tracker_data{ $hash{info_hash} }
                    = compact( ( ( join q[.], @address ) . q[:]
                                     . $hash{port}
                               ),
                               uncompact(
                                     $tracker_data{ $hash{info_hash} }
                               )
                    );
            }
            else {    # TODO: scrape
            }
            use Data::Dump qw[pp];
            warn pp \%hash;
            warn pp \%tracker_data;
            syswrite( $client,
                      sprintf
                          <<END, scalar gmtime, bencode( \%reply ) ); }
HTTP/1.0 200 Here ya go!
Date: %s GMT
Server: Net::BitTorrent test tracker/1.0
Content-type: text/plain

%s
END
        else {
            syswrite( $client, sprintf <<END, scalar gmtime ); }
HTTP/1.0 404 Go away
Date: %s GMT
Server: Net::BitTorrent test tracker/1.0
Content-type: text/plain

Bye.
END
        last if $data =~ m[\015?\012\015?\012];
    }
    close $client;
}
