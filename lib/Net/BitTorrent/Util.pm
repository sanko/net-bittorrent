package Net::BitTorrent::Util;
{
    use strict;      # core as of perl 5
    use warnings;    # core as of perl 5.006

    #
    use Carp qw[carp];                         # core as of perl 5
    use List::Util qw[min max shuffle sum];    # core as of 5.007003

    #
    use version qw[qv];                        # core as of 5.009
    our $SVN = q[$Id$];
    our $VERSION = sprintf q[%.3f], version->new(qw$Rev: 24 $)->numify / 1000;

    #
    use vars                                   # core as of perl 5.002
        qw[@EXPORT_OK %EXPORT_TAGS];
    use Exporter                               # core as of perl 5
        qw[];
    *import = *import = *Exporter::import;
    @EXPORT_OK = qw[bencode bdecode
        compact uncompact
        min max shuffle sum
        TRACE DEBUG INFO WARN ERROR FATAL
    ];
    %EXPORT_TAGS = (all     => [@EXPORT_OK],
                    bencode => [qw[bencode bdecode]],
                    compact => [qw[compact uncompact]],
                    list    => [qw[min max shuffle sum]],
                    log     => [qw[TRACE DEBUG INFO WARN ERROR FATAL]]
    );

    #
    # http://tech.puredanger.com/2008/03/25/log-levels/
    # http://www.sip-communicator.org/index.php/Documentation/LogLevels
    sub TRACE { return 32 }
    sub DEBUG { return 16 }
    sub INFO  { return 8 }
    sub WARN  { return 4 }
    sub ERROR { return 2 }
    sub FATAL { return 1 }

    sub bencode {
        my ($ref) = @_;
        if (defined $ref and not ref $ref) {
            return (  ($ref =~ m[^[-+]?\d+$])
                    ? (q[i] . $ref . q[e])
                    : (length($ref) . q[:] . $ref)
            );
        }
        elsif (ref $ref eq q[ARRAY]) {
            return join(q[], q[l], (map { bencode($_) } @{$ref}), q[e]);
        }
        elsif (ref $ref eq q[HASH]) {
            return
                join(q[], q[d],
                     (map { bencode($_) . bencode($ref->{$_}) }
                          sort keys %{$ref}
                     ),
                     q[e]
                );
        }
        return q[]; # $ref == undef
    }

    sub bdecode {    # needs work
        return if not @_;
        my $string = shift;
        my ($return, $leftover);
        if ($string =~ m[^([1-9]\d*):]s) {
            my $size = $1;
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
            return wantarray ? (int($1), $string) : int($1);
        }
        elsif ($string =~ s|^l(.*)||s) {                        # list
            $leftover = $1;
            while ($leftover and $leftover !~ s|^e||s) {
                (my ($piece), $leftover) = bdecode($leftover);
                push @$return, $piece;
            }
            return wantarray ? (\@$return, $leftover) : \@$return;
        }
        elsif ($string =~ s|^d(.*)||s) {                        # dictionary
            $leftover = $1;
            while ($leftover and $leftover !~ s|^e||s) {
                (my ($key),   $leftover) = bdecode($leftover);
                (my ($value), $leftover) = bdecode($leftover);
                $return->{$key} = $value;
            }
            return wantarray ? (\%$return, $leftover) : \%$return;
        }
        #carp sprintf(q[Bad bencoded data: '%s'], $string);
        return;
    }

    sub compact {
        my (@peers) = @_;
        if (not @peers) {
            carp q[Not enough parameters for compact(ARRAY)];
            return;
        }
        my $return;
        my %seen;
    PEER: for my $peer (grep(defined && !$seen{$_}++, @peers)) {
            next if not $peer;
            my ($ip, $port) = ( # ...sigh, some (old) trackers do crazy things
                ref $peer eq q[HASH]
                ? ($peer->{q[ip]}, $peer->{q[port]})
                : split(q[:], $peer, 2)
            );
            if ($peer
                !~ m[^(?:(?:(?:25[0-5]|2[0-4][0-9]|[0-1]?[0-9]{1,2})[.]?){4}):\d+$]
                )
            {   carp q[Invalid IP address: ] . $peer;
            }
            elsif ($port =~ m[[^\d]]) {
                carp q[Malformed port number: ] . $peer;
            }
            elsif ($port > 2**16) {    #
                carp q[Port number beyond ephemeral range: ] . $peer;
            }
            else {
                $return .= pack q[C4n],    # XXX - use inet_aton
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
    1;
}

=head1 Author

Sanko Robinson <sanko@cpan.org> - http://sankorobinson.com/

CPAN ID: SANKO

=head1 License and Legal

Copyright (C) 2008 by Sanko Robinson E<lt>sanko@cpan.orgE<gt>

This program is free software; you can redistribute it and/or modify
it under the terms of The Artistic License 2.0.  See the F<LICENSE>
file included with this distribution or
http://www.perlfoundation.org/artistic_license_2_0.  For
clarification, see http://www.perlfoundation.org/artistic_2_0_notes.

When separated from the distribution, all POD documentation is covered
by the Creative Commons Attribution-Share Alike 3.0 License.  See
http://creativecommons.org/licenses/by-sa/3.0/us/legalcode.  For
clarification, see http://creativecommons.org/licenses/by-sa/3.0/us/.

Neither this module nor the L<Author|/Author> is affiliated with
BitTorrent, Inc.

=for svn $Id$

=cut
