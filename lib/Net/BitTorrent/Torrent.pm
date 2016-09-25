package Net::BitTorrent::Torrent;
use 5.014;
use Moo;
use strictures 2;
use namespace::clean;
#
use IO::Async::Loop;
#
use File::Spec;
use File::Path;
use Fcntl qw[/SEEK_/ /O_/ :flock];
use Types::Standard qw[ArrayRef CodeRef Enum HashRef Int Ref Str];
use Try::Tiny;
#
use Net::BitTorrent::Protocol qw[:all];
#
use Digest::SHA qw[sha1];
use Scalar::Util qw[/weak/];
#
our $VERSION = "0.75";
#
has client => (is        => 'ro',
               isa       => sub { $_[0]->isa('Net::BitTorrent') },
               predicate => '_has_client',
               writer    => '_set_client',
               handles   => [qw[loop]]
);
#
has path => (
          is  => 'ro',
          isa => sub { die $_[0] . ' is not an existing file.' if !-f $_[0] },
          required => 1,
          coerce   => sub { File::Spec->rel2abs($_[0]) }
);
has basedir => (is       => 'ro',
                isa      => Str,
                required => 1,
                default  => sub { File::Spec->curdir },
                coerce   => sub { File::Spec->rel2abs($_[0]) }
);
has metadata => (
    init_arg => undef,
    is       => 'ro',
    isa      => HashRef,
    lazy     => 1,
    default  => sub {
        open my $fh, '<', shift->path;
        sysread $fh, my $raw, -s $fh;
        bdecode $raw;
    }
);

# Generated
has bitfield => (is      => 'ro',
                 isa     => Str,
                 lazy    => 1,
                 default => sub { pack 'b*', "\0" x shift->piece_count },
                 writer  => '_set_bitfield'
);
has peers => (is => 'ro', isa => Str, lazy => 1);
has infohash => (is      => 'ro',
                 isa     => Str,
                 lazy    => 1,
                 default => sub { sha1(bencode(shift->metadata->{info})) }
);
has status => (is => 'ro', isa => Str, lazy => 1, default => 1)
    ;    # 0:stopped/1:active/2:paused
has size => (
    is       => 'ro',
    isa      => Int,
    init_arg => undef,
    lazy     => 1,
    default  => sub {
        my $ret = 0;
        $ret += $_->{length} for @{shift->files};
        $ret;
    }
);
has files => (
    is       => 'ro',
    lazy     => 1,
    isa      => ArrayRef [HashRef],
    init_arg => undef,
    clearer  => '_clear_files',
    default  => sub {
        my $s = shift;
        defined $s->metadata->{info}{files} ?
            [
            map {
                {priority => 1,
                 fh       => undef,
                 mode     => 'c',
                 length   => $_->{length},
                 timeout  => undef,
                 path =>
                     File::Spec->rel2abs(
                                      File::Spec->catfile(
                                          $s->basedir, $s->name, @{$_->{path}}
                                      )
                     )
                }
            } @{$s->metadata->{info}{files}}
            ]
            : [{priority => 1,
                fh       => undef,
                mode     => 'c',
                length   => $s->metadata->{info}{length},
                timeout  => undef,
                path =>
                    File::Spec->rel2abs(
                                    File::Spec->catfile($s->basedir, $s->name)
                    )
               }
            ];
    }
);

# Callback system
has on_complete        => (is => 'ro');
has on_file_complete   => (is => 'ro');
has on_piece_complete  => (is => 'ro');
has on_block_complete  => (is => 'ro');
has on_peer_connect    => (is => 'ro');
has on_peer_disconnect => (is => 'ro');
has on_tracker_scrape  => (is => 'ro');
has on_tracker_anounce => (is => 'ro');
has on_tracker_failure => (is => 'ro');

# From metadata
sub name         { shift->metadata->{info}{name} }
sub pieces       { shift->metadata->{info}{pieces} }
sub piece_length { shift->metadata->{info}{'piece length'} }

sub piece_count {
    my $s     = shift;
    my $count = $s->size / $s->piece_length;
    int($count) + (($count == int $count) ? 1 : 0);
}

# Eh, who cares
has block_size => (
    is      => 'ro',
    isa     => Int,
    lazy    => 1,
    default => 2**
        14 #int($IO::Async::Stream::READLEN /3) # XXX - Change val in IO::Async?
);

# See status attribute
sub stop  { shift->_set_status(0) }   # XXX - Disconnect peers and close files
sub start { shift->_set_status(1) }
sub pause { shift->_set_status(2) }
sub is_stopped { shift->status == 0 }
sub is_started { shift->status == 1 }
sub is_paused  { shift->status == 2 }
sub is_active  { shift->status >= 1 }

# Taken from AnyEvent::BitTorrent
has working_pieces => (is       => 'ro',
                       lazy     => 1,
                       isa      => HashRef,
                       init_arg => undef,
                       default  => sub { {} }
);

=cut
my $prev;

around working_pieces => sub {
    my ($code, $s) = @_;
    use Carp;
    carp 'Called!';
    my $pieces = $code->($s);
    use Data::Printer;
    use Data::Diff qw(Diff);
    warn np Diff( $prev, $pieces) if $prev;
    $prev = $pieces;
    $pieces;
};
=cut

sub wanted {
    my $s      = shift;
    my $wanted = '';
    for my $findex (0 .. $#{$s->files}) {
        my $prio = !!$s->files->[$findex]{priority};
        for my $index ($s->_file_to_range($findex)) {
            vec($wanted, $index, 1) = $prio && !vec($s->bitfield, $index, 1);
        }
    }
    $wanted;
}

sub _left {
    my $s = shift;
    $s->piece_length * scalar grep {$_} split '',
        substr unpack('b*', $s->wanted), 0, $s->piece_count + 1;
}

sub _file_to_range {
    my ($s, $file) = @_;
    my $start = 0;
    for (0 .. $file - 1) {
        $start += $s->files->[$_]->{length};
    }
    my $end = $start + $s->files->[$file]->{length};
    $start = $start / $s->piece_length;
    $end   = $end / $s->piece_length;
    (int($start) .. int $end + ($end != int($end) ? 0 : +1));
}

has piece_cache => (is => 'ro', isa => HashRef, default => sub { {} });

sub _cache_path {
    my $s = shift;
    File::Spec->catfile($s->basedir,
                        (scalar @{$s->files} == 1 ? () : $s->name),
                        '~ABPartFile_-'
                            . uc(substr(unpack('H*', $s->infohash), 0, 10))
                            . '.dat'
    );
}

sub _write_cache {
    my ($s, $f, $o, $d) = @_;
    my $path = $s->_cache_path;
    #AE::log
    #    debug =>
    #    'Attempting to store %d bytes to cache file (%s) [$f=%s, $o=%s]',
    #    length($d), $path, $f, $o;
    my @split = File::Spec->splitdir($path);
    pop @split;    # File name itself
    my $dir = File::Spec->catdir(@split);
    File::Path::mkpath($dir) if !-d $dir;
    sysopen(my ($fh), $path, O_WRONLY | O_CREAT)
        || return;
    flock $fh, LOCK_EX;
    my $pos = sysseek $fh, 0, SEEK_CUR;
    my $w = syswrite $fh, $d;
    flock $fh, LOCK_UN;
    close $fh;
    $s->piece_cache->{$f}{$o} = $pos;
    #AE::log debug => 'Wrote %d bytes to cache file', $w;
    return $w;
}

sub _read_cache {
    my ($s, $f, $o, $l) = @_;
    $s->piece_cache->{$f} // return;
    $s->piece_cache->{$f}{$o} // return;
    my $path = $s->_cache_path;
    #AE::log
    #    debug =>
    #    'Attempting to read %d bytes from cache file (%s) [$f=%s, $o=%s]',
    #    $l, $path, $f, $o;
    sysopen(my ($fh), $path, O_RDONLY)
        || return;
    flock $fh, LOCK_SH;
    sysseek $fh, $s->piece_cache->{$f}{$o}, SEEK_SET;
    my $w = sysread $fh, my ($d), $l;
    flock $fh, LOCK_UN;
    close $fh;
    return $d;
}
sub _open {
    my ($s, $i, $m) = @_;

    #warn sprintf 'Opening file #%d (%s) for %s',
    #    $i, $s->files->[$i]->{path}, $m;
    return 1 if $s->files->[$i]->{mode} eq $m;
    if (defined $s->files->[$i]->{fh}) {
        warn sprintf 'Closing %s', $s->files->[$i]->{fh};
        flock $s->files->[$i]->{fh}, LOCK_UN;
        close $s->files->[$i]->{fh};
        $s->files->[$i]->{fh} = ();
    }
    if ($m eq 'r') {

        #    warn sprintf 'Opening %s to read', $s->files->[$i]->{path};
        sysopen($s->files->[$i]->{fh}, $s->files->[$i]->{path}, O_RDONLY)
            || return;
        flock($s->files->[$i]->{fh}, LOCK_SH) || return;
        weaken $s unless isweak $s;
        my $x = $i;
        if ($s->_has_client) {
            $s->files->[$x]->{timeout}
                = IO::Async::Timer::Countdown->new(
                         delay     => 500,
                         on_expire => sub { $s // return; $s->_open($x, 'c') }
                );
            $s->loop->add($s->files->[$x]->{timeout});
            $s->files->[$x]->{timeout}->start;
        }
    }
    elsif ($m eq 'w') {

        #    warn sprintf 'Opening %s to write', $s->files->[$i]->{path};
        my @split = File::Spec->splitdir($s->files->[$i]->{path});
        pop @split;    # File name itself
        my $dir = File::Spec->catdir(@split);
        File::Path::mkpath($dir) if !-d $dir;
        sysopen($s->files->[$i]->{fh},
                $s->files->[$i]->{path},
                O_WRONLY | O_CREAT)
            || return;
        flock $s->files->[$i]->{fh}, LOCK_EX;
        truncate $s->files->[$i]->{fh}, $s->files->[$i]->{length}
            if -s $s->files->[$i]->{fh}
            != $s->files->[$i]->{length};    # XXX - pre-allocate files
        weaken $s unless isweak $s;
        my $x = $i;

        if ($s->_has_client) {
            $s->files->[$x]->{timeout}
                = IO::Async::Timer::Countdown->new(
                         delay     => 500,
                         on_expire => sub { $s // return; $s->_open($x, 'c') }
                );
            $s->loop->add($s->files->[$x]->{timeout});
            $s->files->[$x]->{timeout}->start;
        }
    }
    elsif ($m eq 'c') { $s->files->[$i]->{timeout} = () }
    else              {return}
    return $s->files->[$i]->{mode} = $m;
}

sub _write {
    my ($s, $index, $offset, $data) = @_;

    #warn sprintf
    #    'Attempting to write %d bytes from piece %d starting at %d bytes',
    #    length($data), $index, $offset;
    my $file_index = 0;
    my $total_offset = int(($index * $s->piece_length) + ($offset || 0));

    #warn sprintf '...calculated offset == %d', $total_offset;
SEARCH:
    while ($total_offset > $s->files->[$file_index]->{length}) {
        $total_offset -= $s->files->[$file_index]->{length};
        $file_index++;

    #    warn sprintf
    #        'Searching for location... $total_offset = %d, $file_index = %d',
    #        $total_offset, $file_index;
        last SEARCH    # XXX - return?
            if not defined $s->files->[$file_index]->{length};
    }
    my $i = 1;
WRITE: while ((defined $data) && (length $data > 0)) {
        my $this_write
            = (($total_offset + length $data)
               >= $s->files->[$file_index]->{length})
            ?
            ($s->files->[$file_index]->{length} - $total_offset)
            : length $data;

  #    warn sprintf
  #        'Attempt #%d to write %d bytes from file #%d (%s), starting at %d',
  #        $i++,
  #        $this_write,
  #        $file_index, $s->files->[$file_index]->{path}, $total_offset;
        if ($s->files->[$file_index]->{priority} == 0) {
            $s->_write_cache($file_index, $total_offset, substr $data, 0,
                             $this_write, '');

            #        warn 'Wrote data to cache...';
        }
        else {
            $s->_open($file_index, 'w');
            sysseek $s->files->[$file_index]->{fh}, $total_offset, SEEK_SET;
            my $w = syswrite $s->files->[$file_index]->{fh}, substr $data, 0,
                $this_write, '';

       #        warn sprintf 'Wrote %d bytes of data to file (%d bytes left)',
       #            $w, length $data;
            weaken $s unless isweak $s;
            my $x = $file_index;
            if ($s->_has_client) {
                $s->files->[$x]->{timeout}
                    = IO::Async::Timer::Countdown->new(
                         delay     => 120,
                         on_expire => sub { $s // return; $s->_open($x, 'c') }
                    );
                $s->loop->add($s->files->[$x]->{timeout});
                $s->files->[$x]->{timeout}->start;
            }
        }
        $file_index++;
        last WRITE if not defined $s->files->[$file_index];
        $total_offset = 0;
    }
    return length $data;
}

sub _read {
    my ($s, $index, $offset, $length) = @_;

    #warn sprintf
    #    'Attempting to read %d bytes from piece %d starting at %d bytes',
    #    $length, $index, $offset;
    my $data         = '';
    my $file_index   = 0;
    my $total_offset = ($index * $s->piece_length) + $offset;
SEARCH:
    while ($total_offset > $s->files->[$file_index]->{length}) {
        $total_offset -= $s->files->[$file_index]->{length};
        $file_index++;

        #warn sprintf
        #    'Searching for location... $total_offset = %d, $file_index = %d',
        #    $total_offset, $file_index;
        last SEARCH    # XXX - return?
            if not defined $s->files->[$file_index]->{length};
    }
READ: while ((defined $length) && ($length > 0)) {
        my $this_read
            = (
              ($total_offset + $length) >= $s->files->[$file_index]->{length})
            ?
            ($s->files->[$file_index]->{length} - $total_offset)
            : $length;

        #warn sprintf
        #    'Attempting to read %d bytes from file #%d (%s), starting at %d',
        #    $this_read,
        #    $file_index, $s->files->[$file_index]->{path}, $total_offset;
        if (   (!-f $s->files->[$file_index]->{path})
            || (!$s->_open($file_index, 'r')))
        {   $data .= $s->_read_cache($file_index, $total_offset, $this_read)
                // ("\0" x $this_read);
            #warn sprintf 'Failed to open file. Using null chars instead.';
        }
        else {
            sysseek $s->files->[$file_index]->{fh}, $total_offset, SEEK_SET;
            sysread $s->files->[$file_index]->{fh}, my ($_data), $this_read;
            $data .= $_data if $_data;

           #warn sprintf
           #    'Read %d bytes of data from file (%d bytes collected so far)',
           #    length $_data, length $data;
            weaken $s unless isweak $s;
            my $x = $file_index;
            if ($s->_has_client) {
                $s->files->[$x]->{timeout}
                    = IO::Async::Timer::Countdown->new(
                         delay     => 500,
                         on_expire => sub { $s // return; $s->_open($x, 'c') }
                    );
                $s->loop->add($s->files->[$x]->{timeout});
                $s->files->[$x]->{timeout}->start;
            }
        }
        $file_index++;
        $length -= $this_read;

        #warn sprintf 'Still need to read %d bytes', $length;
        last READ if not defined $s->files->[$file_index];
        $total_offset = 0;
    }

    #warn sprintf 'Returning %d bytes of data', length $data;
    return $data;
}

# Cheap callback system
has on_hash_pass => (
    is      => 'rw',
    isa     => CodeRef,
    default => sub {
        sub { !!1 }
    },
    clearer => '_no_hash_pass'
);
sub _trigger_hash_pass { shift->on_hash_pass()->(@_) }
has on_hash_fail => (
    is      => 'rw',
    isa     => CodeRef,
    default => sub {
        sub { !!1 }
    },
    clearer => '_no_hash_fail'
);
sub _trigger_hash_fail { shift->on_hash_fail()->(@_) }
#
sub hashcheck (;@) {
    my $s = shift;
    my @indexes = @_ ? @_ : (0 .. $s->piece_count);
    my $bitfield   = $s->bitfield;    # Makes sure it's built too
    my $total_size = $s->size;
    for my $index (@indexes) {
        next if $index < 0 || $index > $s->piece_count;
        my $piece = $s->_read($index,
                              0,
                              $index == $s->piece_count
                              ?
                                  $total_size % $s->piece_length
                              : $s->piece_length
        );
        my $expected = substr($s->pieces, $index * 20, 20);
        my $reality  = sha1($piece);
        my $ok       = defined($piece) && ($expected eq $reality);
        vec($bitfield, $index, 1) = $ok;

        #warn sprintf "Validate piece #%06d %s, Expected: %s\n"
        #    .        "                             Reality:  %s",
        #    $index, ($ok ? 'PASS' : 'FAIL'), unpack('H*', $expected),
        #    unpack('H*', $reality);
        $ok ?
            $s->_trigger_hash_pass($index)
            : $s->_trigger_hash_fail($index);
    }
    $s->_set_bitfield($bitfield);
}
#
has $_ => (is       => 'ro',
           isa      => Int,
           init_arg => undef,
           default  => sub {0},
           writer   => '_set_' . $_
) for qw[uploaded downloaded];
my $shuffle;
has trackers => (is       => 'ro',
                 lazy     => 1,
                 builder  => '_build_trackers',
                 isa      => ArrayRef [HashRef],
                 init_arg => undef
);

sub _build_trackers {
    my $s = shift;
    $shuffle //= sub {
        my $deck = shift;    # $deck is a reference to an array
        return unless @$deck;    # must not be empty!
        my $i = @$deck;
        while (--$i) {
            my $j = int rand($i + 1);
            @$deck[$i, $j] = @$deck[$j, $i];
        }
    };
    my $trackers = [
        map {
            my $ref = {
                urls       => $_,
                complete   => 0,
                incomplete => 0,
                peers      => '',
                peers6     => '',
                announcer  => undef,
                ticker     => IO::Async::Timer::Periodic->new(
                    first_interval => 1,
                    interval       => 15 * 60,
                    on_tick        => sub {
                        state $state = 'started';
                        return if $s->is_stopped;
                        $s->announce($state);
                        $state = () if $state;
                    }
                ),
                failures => 0
            };
            $s->loop->add($ref->{ticker});
            $ref->{ticker}->start;
            $ref
            } defined $s->metadata->{announce} ? [$s->metadata->{announce}]
        : (),
        defined $s->metadata->{'announce-list'}
        ? @{$s->metadata->{'announce-list'}}
        : ()
    ];
    $shuffle->($trackers);
    $shuffle->($_->{urls}) for @$trackers;
    $trackers;
}

sub announce {
    my ($s, $e) = @_;

    #return if $a++ > 10;    # Retry attempts
    for my $tier (@{$s->trackers}) {
        $tier->{announcer} //= try { $s->_announce_tier($e, $tier)->get};
    }
}

# Single, global instanance
my $http;

sub _announce_tier {
    my ($s, $e, $tier) = @_;
    $http //= sub {
        require Net::Async::HTTP;
        my $h = Net::Async::HTTP->new();
        $s->loop->add($h);
        $h;
        }
        ->();
    my @urls = grep {m[^https?://]}
        @{$tier->{urls}};    # XXX - Only using http trackers for now
    return if $tier->{failures} > 5;
    return if $#{$tier->{urls}} < 0;                 # Empty tier?
    return if $tier->{urls}[0] !~ m[^https?://.+];

    #local $AnyEvent::HTTP::USERAGENT
    #    = 'Net::BitTorrent/' . $AnyEvent::BitTorrent::VERSION;
    my $_url = $tier->{urls}[0] . '?info_hash=' . sub { # encode
        local $_ = shift;
s/([^A-Za-z0-9])/sprintf("%%%2.2X", ord($1))/ge;
#s/([\W])/"%" . uc(sprintf("%2.2x",ord($1)))/eg;
        $_;
        }
        ->($s->infohash)
        . ('&peer_id=' . $s->client->peerid)
        . ('&uploaded=' . $s->uploaded)
        . ('&downloaded=' . $s->downloaded)
        . ('&left=' . $s->_left)
        . ('&port=' . $s->client->port)
        . '&compact=1'
        . ($e ? '&event=' . $e : '');
    #use Carp;
    #carp 'Announce URL: ' . $_url;
    warn $_url;
    $http->GET($_url)->on_done(
        sub {
            my $response = shift;
            use Data::Printer;
            warn 'Announce response: ' . np($response);
            $tier->{announcer} = ();
            my $reply = bdecode($response->content);
            if (defined $reply->{'failure reason'}) {    # XXX - Callback?
                push @{$tier->{urls}}, shift @{$tier->{urls}};
                $s->_announce_tier($e, $tier)->get();
                $tier->{'failure reason'} = $reply->{'failure reason'};
                $tier->{failures}++;
            }
            else {                                       # XXX - Callback?
                $tier->{failures} = $tier->{'failure reason'} = 0;
                $tier->{peers}
                    = compact_ipv4(
                             uncompact_ipv4($tier->{peers} . $reply->{peers}))
                    if $reply->{peers};
                $tier->{peers6}
                    = compact_ipv6(
                           uncompact_ipv6($tier->{peers6} . $reply->{peers6}))
                    if $reply->{peers6};
                $tier->{complete}   = $reply->{complete};
                $tier->{incomplete} = $reply->{incomplete};
                $tier->{ticker} = IO::Async::Timer::Countdown->new(
                    delay => $reply->{interval} // (15 * 60),
                    on_expire => sub {
                        return if $s->is_stopped;
                        try { $s->_announce_tier($e, $tier)->get; }
                    }
                );
                $s->loop->add($tier->{ticker});
                $tier->{ticker}->start;

                use Data::Printer;
                warn np $tier;
            }
        }
        )->on_fail(
        sub {
            $tier->{'failure reason'} = 'HTTP Error: ' . shift;
            $tier->{failures}++;
            push @{$tier->{urls}}, shift @{$tier->{urls}};
            $tier->{ticker} = IO::Async::Timer::Countdown->new(
                    delay => 30,
                    on_expire => sub {
                        return if $s->is_stopped;
                        try { $s->_announce_tier($e, $tier)->get; }
                    }
                );
                $s->loop->add($tier->{ticker});
                $tier->{ticker}->start;
        }
        );
}

#
1;
__END__

=encoding utf-8

=head1 NAME

Net::BitTorrent -  BitTorrent peer-to-peer protocol class

=head1 SYNOPSIS

    use Net::BitTorrent;

    my $client = Net::BitTorrent->new();
    my $torrent = $client->add_torrent('totally.legal.torrent');
    $client->loop;

=head1 DESCRIPTION

Net::BitTorrent is ...

=head1 Methods


=head2 C<new( ... )>

=head2 C<state( )>

=head2 C<start( )>

=head2 C<pause( )>

=head2 C<stop( )>

=head2 C<hashcheck( [...] )>

=head2 C<size( )>

=head2 C<infohash( )>

=head2 C<name( )>

=head1 Callbacks


=head2 C<on_hash_pass>


=head2 C<on_hash_fail>

=head2 C<on_complete>

=head2 C<on_block_complete>

=head2 C<on_file_complete>

=head1 LICENSE

Copyright (C) Sanko Robinson.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=head1 AUTHOR

Sanko Robinson E<lt>sanko@cpan.orgE<gt>

=cut

