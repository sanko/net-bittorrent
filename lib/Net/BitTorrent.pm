package Net::BitTorrent;
{
    use Any::Moose;
    use Any::Moose '::Util::TypeConstraints';
    our $MAJOR = 0.075; our $MINOR = 0; our $DEV = -1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use AnyEvent;

    #
    sub timer { shift; AnyEvent->timer(@_) }
    sub run { AnyEvent->condvar->recv }
    has 'torrents' => (
        traits  => ['Hash'],
        isa     => 'HashRef[Net::BitTorrent::Torrent]',    # ??? - by infohash
        is      => 'ro',
        reader  => '_torrents',
        default => sub { {} },
        coerce  => 1,
        handles => {add_torrent     => 'set',
                    torrent         => 'get',
                    delete_torrent  => 'delete',
                    find_torrent    => 'exists',
                    has_no_torrents => 'is_empty',
                    each            => 'kv',
                    count_torrents  => 'count',
                    torrents        => 'values',
                    infohashes      => 'keys'
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
            && $code->($self, $torrent->infohash, $torrent)
            && $torrent->client($self);
    };

    #
    for my $protocol (qw[tcp udp]) { # XXX - Involved
        for my $ipv (4, 6) {
            has $protocol . '_ipv' . $ipv . '_port' => (
                isa      => 'Int',
                is       => 'ro',
                init_arg => 'Port',
                writer   => '_' . $protocol . '_ipv' . $ipv . '_port',
                trigger  => sub {
                    my ($self, $new, $old) = @_;
                    if (defined $old && !$old && $new) {
                        warn "Re-open ${protocol}_ipv${ipv}_server";
                    }
                }
            );
            has $protocol . '_ipv' . $ipv . '_host' => (
                isa => (
                    $ipv == 4
                    ? (subtype(
                           as 'Str' => where {
                               m[^(\d{1,3}\.){3}(\d{1,3})$];
                           }
                       )
                        )
                    : 'Str'
                ),
                is       => 'ro',
                init_arg => 'Host',
                writer   => '_' . $protocol . '_ipv' . $ipv . '_host',
                trigger  => sub {
                    my ($self, $new, $old) = @_;
                    if (defined $old && !$old && $new) {
                        warn "Re-open ${protocol}_ipv${ipv}_server";
                    }
                }
            );
        }
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
