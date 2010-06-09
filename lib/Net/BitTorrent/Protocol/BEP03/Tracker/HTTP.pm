package Net::BitTorrent::Protocol::BEP03::Tracker::HTTP;
{
    use Moose;
    use Moose::Util::TypeConstraints;
    use List::Util qw[shuffle];
    our $MAJOR = 0.075; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use lib '../../../../../';
    use Net::BitTorrent::Types qw[:tracker :bencode];
    use Net::BitTorrent::Network::Utility qw[client];
    use Net::BitTorrent::Protocol::BEP23::Compact qw[uncompact_ipv4];
    has 'url' => (isa      => subtype(as Str => where {m[^http://.+]}),
                  is       => 'ro',
                  required => 1,
    );
    my $bdecode_constraint;

    sub scrape {
        my ($self, @infohash) = @_;
        use Data::Dump;
        ddx \@infohash;
        ddx $self;
        warn $self->torrent->infohash;
        return if $self->url !~ m[^(.+)/announce(\b(?:[^/])*)$];
        warn sprintf '%s/scrape%s', $1, $2 || '';
        die 'scrape! ' . $self->url;
        my $url = $self->url;

        if ($url =~ m[(.+)/announce(\b(?:[^/])*)]) {
        }
        use Data::Dump;
    }

    sub announce {
        my ($self, $event, $args, $code) = @_;
        use Scalar::Util;
        Scalar::Util::weaken $self;
        Scalar::Util::weaken $code;
        my $query;
        my %query_hash = (
                 info_hash  => $args->{'info_hash'},
                 peer_id    => $args->{'peer_id'},
                 port       => $args->{'port'},
                 uploaded   => $args->{'uploaded'},
                 downloaded => $args->{'downloaded'},
                 left       => $args->{'left'},
                 key        => $^T,
                 numwant    => 200,
                 compact    => 1,
                 no_peer_id => 1,
                 (defined $event && $event =~ m[^(?:st(?:art|opp)|complet)ed$]
                  ? (event => $event)
                  : ()
                 )
        );
        my $url = $self->url . '?' . join '&',
            map { $_ . '=' . $query_hash{$_} } keys %query_hash;
        my ($host, $port, $path)
            = $url =~ m{^https?://([^/:]*)(?::(\d+))?(/.*)$};
        $port //= 80;
        my $http;
        $http = client(
            $host, $port,
            sub {
                my $data = '';
                my ($sock, $_host, $_port) = @_;
                return if !$sock;
                syswrite($sock,
                         join "\015\012",
                         "GET $path HTTP/1.0",
                         'Connection: close',
                         "Host: $host:$port",
                         'Accept: text/plain',
                         'Accept-Encoding:',
                         'User-Agent: Net::BitTorrent/'
                             . $Net::BitTorrent::VERSION,
                         '',
                         ''
                );
                shutdown $sock, 1;
                $http = AE::io(
                    $sock, 0,    # read
                    sub {
                        if (!sysread($sock, $data, 1024 * 128, length $data))
                        {        # We must be finished. Parse it out.
                            shutdown $sock, 0;
                            $http = undef;
                            close $sock;
                            my ($header, $content)
                                = $data =~ m[^(.+)?(?:\015?\012){2}(.+)$]s;
                            $bdecode_constraint //=
                                Moose::Util::TypeConstraints::find_type_constraint(
                                                          'NBTypes::Bdecode');
                            my $announce
                                = $bdecode_constraint->coerce($content);
                            $announce->{'peers'}
                                = [uncompact_ipv4($announce->{'peers'})]
                                if $announce->{'peers'};
                            $query->[1]->($announce)    # if $code;
                        }
                    }
                );
            }
        );
        $query = [\%query_hash, $code, [], $http];
        return $query;
    }
    1;
}

=pod

=head1 NAME

Net::BitTorrent::Protocol::BEP03::Tracker::HTTP - A single HTTP-based tracker

=head1 See Also

=over

=item BEP 03:

http://bittorrent.org/beps/bep_0003.html

=back

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
