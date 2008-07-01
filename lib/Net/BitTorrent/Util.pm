package Net::BitTorrent::Util;
use strict;
use warnings;
{

    BEGIN {
        use version qw[qv];
        our $SVN = q[$Id$];
        our $VERSION = sprintf q[%.3f], version->new(qw$Rev$)->numify / 1000;
    }
    use List::Util qw[min max shuffle sum];
    use vars qw[@EXPORT_OK %EXPORT_TAGS];
    use Exporter qw[];
    *import    = *Exporter::import;
    @EXPORT_OK = qw[bencode bdecode
        compact uncompact
        min max shuffle sum
        TRACE FATAL ERROR WARN INFO DEBUG
    ];
    %EXPORT_TAGS = (all     => [@EXPORT_OK],
                    bencode => [qw[bencode bdecode]],
                    compact => [qw[compact uncompact]],
                    list    => [qw[min max shuffle sum]],
                    log     => [qw[TRACE FATAL ERROR WARN INFO DEBUG]]
    );

    # http://tech.puredanger.com/2008/03/25/log-levels/
    # http://www.sip-communicator.org/index.php/Documentation/LogLevels
    sub TRACE { return 32 }
    sub DEBUG { return 16 }
    sub INFO  { return 7 }
    sub WARN  { return 4 }
    sub ERROR { return 2 }
    sub FATAL { return 1 }

    sub bencode {
        if (defined $_[0] and not ref $_[0]) {
            return (  ($_[0] =~ m[^[-+]?\d+$])
                    ? (q[i] . $_[0] . q[e])
                    : (length($_[0]) . q[:] . $_[0])
            );
        }
        elsif (ref $_[0] eq q[ARRAY]) {
            return join(q[], q[l], (map { bencode($_) } @{$_[0]}), q[e]);
        }
        elsif (ref $_[0] eq q[HASH]) {
            return
                join(q[], q[d],
                     (map { bencode($_) . bencode($_[0]->{$_}) }
                          sort keys %{$_[0]}
                     ),
                     q[e]
                );
        }
        $@ = q[invalid format];
        return q[];
    }

    sub bdecode {    # needs new benchmark
        my ($string) = @_;
        return if not $string;
        if ($string =~ m[^([1-9]\d*):]s) {
            my $return = q[];
            my $size   = $1;
            $string =~ s|^$size:||s;
            while ($size) {
                my $this_time = min($size, 32766);
                $string =~ s|^(.{$this_time})||s;
                return if not $1;
                $return .= $1;
                $size = max(0, ($size - $this_time));
            }
            return wantarray ? ($return, $string) : $return;    # byte string
        }
        elsif ($string =~ s|^i([-+]?\d+)e||s) {                 # integer
            return wantarray ? (int($1), $string || undef) : int($1);
        }
        elsif ($string =~ s|^l(.*)||s) {                        # list
            my @return   = ();
            my $leftover = $1;
            while ($leftover and $leftover !~ s|^e||s) {
                (my ($piece), $leftover) = bdecode($leftover);
                push @return, $piece;
            }
            return wantarray ? (\@return, $leftover || undef) : \@return;
        }
        elsif ($string =~ s|^d(.*)||s) {                        # dictionary
            my %return   = ();
            my $leftover = $1;
            while ($leftover and $leftover !~ s|^e||s) {
                (my ($key),   $leftover) = bdecode($leftover);
                (my ($value), $leftover) = bdecode($leftover);
                $return{$key} = $value;
            }
            return wantarray ? (\%return, $leftover || undef) : \%return;
        }
        $@ = sprintf q[Bad bencoded data: %s], ($string||q[]);
        return;
    }

    sub compact {    # IPv4 only.  For now.
        my (@peers) = @_;
        if (not @peers) {
            $@ = q[Not enough parameters for compact(ARRAY)];
            return;
        }
        my $return = q[];
        my %seen;
    PEER: for my $peer (grep(!$seen{$_}++, @peers)) {
            next if not $peer;
            my ($ip, $port) = ( # ...sigh, some (old) trackers do crazy things
                ref $peer eq q[HASH]
                ? ($peer->{q[ip]}, $peer->{q[port]})
                : split(q[:], $peer, 2)
            );
            if (grep { $_ > 0xff }
                       ($ip =~ m[^([\d]+)\.([\d]+)\.([\d]+)\.([\d]+)$])
                    or ($ip !~ m[^([\d]+)\.([\d]+)\.([\d]+)\.([\d]+)$]))
            {   $@ = q[Invalid IP address: ] . $peer;
            }
            elsif ($port =~ m[[^\d]]) {
                $@ = q[Malformed port number: ] . $peer;
            }
            elsif ($port > 2**16) {    #
                $@ = q[Port number beyond ephemeral range: ] . $peer;
            }
            else {
                $return .= pack q[C4n],
                    ($ip =~ m[^([\d]+)\.([\d]+)\.([\d]+)\.([\d]+)$]),
                    int $port;
            }
        }
        return $return;
    }

    sub uncompact {
        my $string = shift;
        if (not defined $string) { return; }
        my %peers;
        while ($string =~ m|(....)(..)|g) {
            $peers{
                sprintf(q[%d.%d.%d.%d:%d],
                        unpack(q[C4], $1),
                        unpack(q[n],  $2))
                }++;
        }
        return (shuffle(%peers ? keys %peers : ()));
    }
}
1;
__END__

=pod

=head1 NAME

Net::BitTorrent::Util - BitTorrent Related Utility Functions

=head1 Importing From Net::BitTorrent::Util

By default, nothing is exported.

You may import any of the following or use one or more of these tags:

=over 2

=item C<:all>

Everything is imported into your namespace.

=item C<:bencode>

You get the two Bencode-related functions: C<bencode> and C<bedecode>.
For more on Bencoding, see the BitTorrent Protocol documentation.

=item C<:compact>

C<compact>, C<uncompact>

These are tracker response-related functions.

=item C<:list>

C<min>, C<max>, C<shuffle>, C<sum>

Net::BitTorrent::Util imports these from List::Util.

=item C<:log>

Net::BitTorrent's log callback uses these:

=over 2

=item C<TRACE>

=item C<FATAL>

=item C<ERROR>

=item C<WARN>

=item C<INFO>

=item C<DEBUG>

=back

=back

=head1 Functions

=over 4

=item C<bencode ( ARGS )>

Expects a single value (basic scalar, array reference, or hash
reference) and returns a single string.

Bencoding is the BitTorrent protocol's basic serialization and
data organization format.  The specification supports integers,
lists (arrays), dictionaries (hashes), and byte strings.

See Also: L<Convert::Bencode>, L<Bencode>, L<Convert::Bencode_XS>

=item C<bdecode ( STRING )>

Expects a bencoded string.  The return value depends on the type of
data contained in the string.

See Also: L<Convert::Bencode>, L<Bencode>, L<Convert::Bencode_XS>

=item C<compact ( LIST )>

Compacts a list of IPv4:port strings into a single string.

A compact peer is 6 bytes;  the first four bytes are the host (in
network byte order), the last two bytes are the port (again in network
byte order).

=item C<uncompact ( STRING )>

Inflates a compacted string of peers and returns a list of IPv4:port
strings.

=back

=head1 Log Levels

=over 4

=item C<FATAL>

C<FATAL> errors usually mean something really wrong has taken place.
You should restart the application to restore normal operation.

=item C<ERROR>

C<ERROR> is used for logging general errors that prevent the
L<Net::BitTorrent> from functioning as expected.

=item C<WARN>

C<WARN> is used for logging any unusual situation that is, for the
moment, not preventing normal operation.

=item C<INFO>

C<INFO> level messages include any interesting piece of information
that helps to give context to a log, often when things are starting or
stopping.

=item C<DEBUG>

C<DEBUG> level includes anything that you’d like to be in the logs
when trying to understand why the application didn’t work as expected.

=item C<TRACE>

Indicates a level of logging that shows the control flow of the
program.  Among the things that you’d like to log with a C<TRACE>
level are: entry and exit of a method, loop, if statement or other
control flow statements.

=back

=head1 Author

Sanko Robinson <sanko@cpan.org> - http://sankorobinson.com/

CPAN ID: SANKO

=head1 License and Legal

Copyright 2008 by Sanko Robinson E<lt>sanko@cpan.orgE<gt>

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

See [http://www.perl.com/perl/misc/Artistic.html] or the LICENSE file
included with this module.

Neither this module nor the L<Author|/Author> is affiliated with
BitTorrent, Inc.

=for svn $Id$

=cut
