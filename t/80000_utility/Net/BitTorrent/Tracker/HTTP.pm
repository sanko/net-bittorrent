package Net::BitTorrent::Tracker::HTTP;
{
    use Net::BitTorrent::Protocol::BEP03::Bencode qw[:all];
    use Net::BitTorrent::Protocol::BEP07::Compact qw[:all];    # IPv4
    use Net::BitTorrent::Protocol::BEP23::Compact qw[:all];    # IPv6
    use Moose;
    our $MAJOR = 0; our $MINOR = 74; our $DEV = 13; our $VERSION = sprintf('%0d.%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);

    extends 'Net::BitTorrent::Tracker';
    sub protocol {'tcp'}

    sub on_read {
        my ($s, $h, $fh, $paddr, $ip, $port) = @_;
        my ($status, $body) = ('404 EH!?', 'Sorry. Play again.');
        if ($h->rbuf =~ s[^GET (.+?)(?:\?(.+))? HTTP/1\.(\d)\015\012][]) {
            my ($path, $args, $ver) = ($1, $2, $3);
            my %args = map { m[^(.+?)(?:=(.*))?$]; $1 => $2; }
                split qr[[&;]], $args;
            my %headers = map { m[^(.+?)\s*:\s*(.*)?$]; $1 => $2; }
                split qr[\015\012], $h->rbuf;
            if ($path eq '/announce.pl') {
                my $tracker_id = $args{'tracker id'} || pack 'H*',
                    int rand(time);
                my $max_peers = $args{'max_peers'} || 50;
                my $info_hash = $args{'info_hash'};
                $info_hash =~ s[%(..)][chr hex $1]eg;
                $s->add_peer(pack('H*', $args{'key'}) ^ $info_hash ^
                                 pack('B*', $args{'peer_id'}),
                             {address => [
                                    $args{'ip'} || $ip, $args{'port'} || $port
                              ],
                              downloaded => $args{'downloaded'},
                              event      => $args{'event'} || undef,
                              info_hash  => $info_hash,
                              key        => $args{'key'},
                              left       => $args{'left'},
                              peer_id    => $args{'peer_id'},
                              tracker_id => $tracker_id,
                              uploaded   => $args{'uploaded'},
                              touch      => time
                             }
                );
                $status = '200 Alright';
                my $num_peers = 0;
                my @peers     = grep {
                           $_->{'info_hash'} eq $info_hash
                        && $num_peers++ < $max_peers
                } $s->peers;
                $body = {
                    'min interval' => 60 * 5,
                    interval       => 60 * 10,
                    'tracker id'   => $tracker_id,
                    peers          => (
                        $args{'compact'}
                        ? (compact_ipv4 map { $_->{'address'} } @peers)
                        : (map {
                               {peer_id => $_->{'peer_id'},
                                ip      => $_->{'address'}->[0],
                                port    => $_->{'address'}->[1]
                               }
                               } @peers
                        )
                    )
                };
            }

            #warn bencode $s->get_info_hash($info_hash);
            elsif ($path eq '/scrape.pl') { warn 'Scrape!' }
            else                          { warn 'NFI!' }
        }

        #die $info_hash;
        $h->rbuf = '';
        $body = bencode $body if ref $body;
        $h->push_write(sprintf <<'END', $status, length($body), $body)
HTTP/1.0 %s
Content-Type: text/plain
Content-Length: %d
Connection: close

%s
END
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
