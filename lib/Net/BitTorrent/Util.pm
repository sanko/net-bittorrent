package Net::BitTorrent::Util;
use strict;
use warnings;
{

    BEGIN {
        use vars qw[$VERSION];
        use version qw[qv];
        our $SVN
            = q[$Id$];
        our $VERSION = sprintf q[%.3f], version->new(qw$Rev$)->numify / 1000;
    }
    use List::Util qw[min max shuffle sum];
    use Carp qw[carp];
    use base qw[Exporter];
    use vars qw[@ISA @EXPORT_OK %EXPORT_TAGS];
    @ISA       = qw[Exporter];
    @EXPORT_OK = qw[bencode bdecode
        compact uncompact
        min max shuffle sum
        TRACE FATAL ERROR WARN INFO DEBUG
    ];
    %EXPORT_TAGS = (all     => [@EXPORT_OK],
                    bencode => [qw(bencode bdecode)],
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
        if (not ref $_[0]) {
            return (  ($_[0] =~ m[^[-+]?\d+$])
                    ? (q[i] . $_[0] . q[e])
                    : (length($_[0]) . q[:] . $_[0])
            );
        }
        elsif (ref $_[0] eq q[ARRAY]) {
            return
                join(q[], q[l], (map { bencode($_) } @{$_[0]}), q[e]);
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

=pod

=begin benchmark

Benchmark: timing 10000 iterations of POE, [cpan://Bencode], [cpan://Convert::Bencode], [cpan://Convert::Bencode_XS], mine, torrenttool...
       POE: 94 wallclock secs (70.02 usr +  0.01 sys = 70.03 CPU) @ 142.80/s (n=10000)
[cpan://Bencode]: 11 wallclock secs ( 9.14 usr +  0.00 sys =  9.14 CPU) @ 1093.97/s (n=10000)
[cpan://Convert::Bencode]: 11 wallclock secs ( 8.52 usr +  0.02 sys =  8.53 CPU) @ 1172.06/s (n=10000)
[cpan://Convert::Bencode_XS]:  2 wallclock secs ( 1.22 usr +  0.00 sys =  1.22 CPU) @ 8203.45/s (n=10000)
      mine: 11 wallclock secs ( 7.28 usr +  0.03 sys =  7.31 CPU) @ 1367.61/s (n=10000)
torrenttool: 16 wallclock secs (11.23 usr +  0.00 sys = 11.23 CPU) @ 890.15/s (n=10000)
                               Rate   POE torrenttool [cpan://Bencode] [cpan://Convert::Bencode] mine [cpan://Convert::Bencode_XS]
POE                           143/s    --        -84%             -87%                      -88% -90%                         -98%
torrenttool                   890/s  523%          --             -19%                      -24% -35%                         -89%
[cpan://Bencode]             1094/s  666%         23%               --                       -7% -20%                         -87%
[cpan://Convert::Bencode]    1172/s  721%         32%               7%                        -- -14%                         -86%
mine                         1368/s  858%         54%              25%                       17%   --                         -83%
[cpan://Convert::Bencode_XS] 8203/s 5645%        822%             650%                      600% 500%                           --
===========================================================
key:
 torrenttool = http://meroko.lindesign.se/torrenttool
 POE         = http://www.xs4all.nl/~thospel/ASIS/lib/POE/Component/BitTorrent.pm

=end benchmark

=cut

    sub bdecode {    # needs new benchmark
        my ($string) = @_;
        return if not $string;
        my ($return);
        if ($string =~ m[^(\d+):]) {    # byte string
             #$' =~ m[^(.{$1})]s; # large .torrents (>=8200 pieces) will have
             #return ($1, $');    # byte strings longer than perl's regex
             #                    # quantifier limit.
            my $blah = $';    # this new code is untested
            my $before = substr($blah, 0, $1, q[]);
            $@ =
                sprintf(q[Not enough data for byte string (%d vs %d)],
                        $1, length($blah))
                if length($blah) < $1;
            $@ = sprintf(q[Trailing garbage at %d (%d bytes)],
                         length($1), length($blah))
                if $blah;
            return wantarray ? ($before, $blah) : $before;
        }
        elsif ($string =~ m[^i([-+]?\d+)e]) {    # integer
            $@ = sprintf(q[Trailing garbage at %d (%d bytes)],
                         length($1), length($'))
                if $';
            return wantarray ? (int($1), $') : int($1);
        }

        #elsif ( $string =~ m[^l(.+e)] ) {          # list
        #    $string = $1;
        elsif ($string =~ m[^l]) {               # dictionary
            $string = $';
            do {
                (my ($value), $string) = bdecode($string);
                push @$return, $value;
            } while ($string and $string !~ m[^e]);
            $@ = sprintf(q[Trailing garbage at %d (%d bytes)],
                         length($`), length($'))
                if $';
            return wantarray ? ($return, $') : ($return);
        }

        #elsif ( $string =~ m[^d(.+e)] ) {          # dictionary
        #    $string = $1;
        elsif ($string =~ m[^d]) {    # dictionary
            $string = $';
            do {
                (my ($key),   $string) = bdecode($string);
                (my ($value), $string) = bdecode($string);
                $return->{$key} = $value if $key;
            } while ($string and $string !~ m[^e]);
            $@ = sprintf(q[Trailing garbage at %d (%d bytes)],
                         length($'), length($'))
                if $';
            return wantarray ? ($return, $') : ($return);
        }
        else {
            $@ = q[Bad bencoded data];
        }
        return;
    }

=pod

=begin benchmark

Benchmark: timing 1000 iterations of POE, [cpan://Bencode], [cpan://Convert::Bencode], [cpan://Convert::Bencode_XS], cae, icir, mine, torrenttool...
       POE:  5 wallclock secs ( 4.08 usr +  0.02 sys =  4.09 CPU) @ 244.26/s (n=1000)
[cpan://Bencode]:  8 wallclock secs ( 6.17 usr +  0.05 sys =  6.22 CPU) @ 160.80/s (n=1000)
[cpan://Convert::Bencode]: 91 wallclock secs (76.30 usr +  1.33 sys = 77.63 CPU) @ 12.88/s (n=1000)
[cpan://Convert::Bencode_XS]:  0 wallclock secs ( 0.19 usr +  0.03 sys =  0.22 CPU) @ 4566.21/s (n=1000)
            (warning: too few iterations for a reliable count)
       cae:  7 wallclock secs ( 5.42 usr +  1.25 sys =  6.67 CPU) @ 149.88/s (n=1000)
      icir: 84 wallclock secs (76.80 usr +  0.75 sys = 77.55 CPU) @ 12.90/s (n=1000)
      mine: 43 wallclock secs (30.27 usr + 10.70 sys = 40.97 CPU) @ 24.41/s (n=1000)
torrenttool:  2 wallclock secs ( 1.86 usr +  0.11 sys =  1.97 CPU) @ 508.13/s (n=1000)
                               Rate [cpan://Convert::Bencode]   icir   mine   cae [cpan://Bencode]   POE torrenttool [cpan://Convert::Bencode_XS]
[cpan://Convert::Bencode]    12.9/s                        --    -0%   -47%  -91%             -92%  -95%        -97%                        -100%
icir                         12.9/s                        0%     --   -47%  -91%             -92%  -95%        -97%                        -100%
mine                         24.4/s                       89%    89%     --  -84%             -85%  -90%        -95%                         -99%
cae                           150/s                     1063%  1062%   514%    --              -7%  -39%        -71%                         -97%
[cpan://Bencode]              161/s                     1148%  1147%   559%    7%               --  -34%        -68%                         -96%
POE                           244/s                     1796%  1794%   901%   63%              52%    --        -52%                         -95%
torrenttool                   508/s                     3844%  3840%  1982%  239%             216%  108%          --                         -89%
[cpan://Convert::Bencode_XS] 4566/s                    35345% 35310% 18607% 2947%            2740% 1769%        799%                           --
===========================================================
key:
 torrenttool = http://meroko.lindesign.se/torrenttool
 icir        = http://www.icir.org/christian/downloads/bdecode
 POE         = http://www.xs4all.nl/~thospel/ASIS/lib/POE/Component/BitTorrent.pm
 cae         = http://web.archive.org/web/20061013191718/http://www.bklyn.org/~cae/Bencode.pm

=end benchmark

=begin comment

True, my bdecode is painfully slow, but we only run it once per .torrent and
once on tracker scrape/announce and I hate the thought of adding another prereq
just for that.  I'll work on it.

=end comment

=cut

    sub compact {
        my (@peers) = @_;
        if (not @peers) {

            #warn(q[Not enough parameters for compact(ARRAY)]);
            return;
        }
        my $return = q[];
        my %saw;
    PEER: for my $peer (grep(!$saw{$_}++, @peers)) {
            my ($ip, $port)
                = (    # ...sigh, some (old) trackers do crazy stuff
                ref $peer eq q[HASH]
                ? ($peer->{q[ip]}, $peer->{q[port]})
                : split(q[:], $peer, 2)
                );
            if (grep { $_ > 0xff }
                ($ip =~ m[^([\d]+)\.([\d]+)\.([\d]+)\.([\d]+)$])
                    or
                ($ip !~ m[^([\d]+)\.([\d]+)\.([\d]+)\.([\d]+)$]))
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

Net::BitTorrent::Util - Utility functions

=head1 METHODS

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

=head1 LOG LEVELS

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

=head1 AUTHOR

Sanko Robinson <sanko@cpan.org> - [http://sankorobinson.com/]

=head1 LICENSE AND LEGAL

Copyright 2008 by Sanko Robinson E<lt>sanko@cpan.orgE<gt>

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

See [http://www.perl.com/perl/misc/Artistic.html] or the LICENSE file
included with this module.

Neither this module nor the L<AUTHOR|/AUTHOR> is affiliated with
BitTorrent, Inc.

=for svn $Id$

=cut
