{

    package Net::BitTorrent::Util;

    BEGIN {
        use vars qw[$VERSION];
        use version qw[qv];
        our $SVN = q[$Id$];
        our $VERSION = sprintf q[%.3f], version->new(qw$Rev$)->numify / 1000;
    }

    use strict;
    use warnings 'all';
    use List::Util qw[min max shuffle sum];
    use Carp qw[carp croak];

    sub bencode {
        if (not ref $_[0]) {
            return (
                  ($_[0] =~ m[^\d+$])
                ? (q[i] . $_[0] . q[e])
                : (length($_[0]) . q[:] . $_[0])
            );
        }
        elsif (ref $_[0] eq q[ARRAY]) {
            return join(q[], q[l], (map { bencode($_) } @{$_[0]}), q[e]);
        }
        return join(q[],
            q[d],
            (map { bencode($_) . bencode($_[0]->{$_}) } sort keys %{$_[0]}),
            q[e]);
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

    sub bdecode {
        my ($string) = @_;
        my ($return);
        if ($string =~ m[^(\d+):]) {    # byte string
                #$' =~ m[^(.{$1})]s; # large .torrents (>=8200 pieces) will have
                #return ($1, $');    # byte strings longer than perl's regex
                #                    # quantifier limit.
            my $blah = $';                           # this new code is untested
            my $before = substr($blah, 0, $1, q[]);  # needs new benchmark
            return ($before, $blah);
        }
        elsif ($string =~ m[^i(\d*)e]) {             # integer
            return ($1, $');
        }
        elsif ($string =~ m[^l]) {                   # list
            $string = $';

            do {
                (my ($value), $string) = bdecode($string);
                push @$return, $value;
            } while ($string !~ m[^e]);
            return ($return, $');
        }
        elsif ($string =~ m[^d]) {                   # dictionary
            $string = $';
            do {
                (my ($key),   $string) = bdecode($string);
                (my ($value), $string) = bdecode($string);
                $return->{$key} = $value;
            } while ($string !~ m[^e]);
            return ($return, $');
        }
        else {
            warn q[Bad bencoded data];
        }
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

            #croak(q[Not enough parameters for compact(ARRAY)]);
            return;
        }
        my $return = q[];
        for my $peer (@{$peers[0]}) {
            my ($ip, $port) = (    # ...sigh, some (old) trackers do crazy stuff
                ref $peer eq q[HASH]
                ? ($peer->{q[ip]}, $peer->{q[port]})
                : split(q[:], $peer)
            );
            $return .= pack q[C4n], ($ip =~ m[(\d+)]g), $port;
        }
        return $return;
    }

    sub uncompact {
        my $string = shift;
        if (not defined $string) { return; }
        my %peers;
        while ($string =~ m|(....)(..)|g) {
            $peers{
                sprintf(q[%d.%d.%d.%d:%d], unpack(q[C4], $1), unpack(q[n], $2))
              }++;
        }
        return (shuffle(%peers ? keys %peers : ()));
    }

    1;
}

