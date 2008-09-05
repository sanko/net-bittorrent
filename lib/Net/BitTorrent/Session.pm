package Net::BitTorrent::Session;
{
    use strict;      # core as of perl 5
    use warnings;    # core as of perl 5.006

    #
    use Digest::SHA qw[sha1_hex];                   # core as of perl 5.009003
    use Carp qw[confess confess];                   # core as of perl 5
    use Cwd qw[cwd];                                # core as of perl 5
    use File::Spec::Functions qw[rel2abs catfile];  # core as of perl 5.00504
    use Scalar::Util qw[blessed weaken];            # core as of perl 5.007003
    use List::Util qw[sum shuffle];                 # core as of perl 5.007003
    use Fcntl qw[O_RDONLY];                         # core as of perl 5

    #
    use version qw[qv];                             # core as of 5.009
    our $SVN = q[$Id$];
    our $VERSION = sprintf q[%.3f], version->new(qw$Rev: 24 $)->numify / 1000;

    #
    use lib q[../../../lib];
    use Net::BitTorrent::Util qw[:bencode :compact];
    use Net::BitTorrent::Session::File;
    use Net::BitTorrent::Session::Tracker;
    use Net::BitTorrent::Peer;

    # Debugging
    #use Data::Dump qw[pp];

    #
    my (%_client, %path, %basedir);    # new() params (basedir is optional)
    my (%size,          %files,  %trackers, %infohash,
        %_private,      %pieces, %uploaded, %downloaded,
        %_piece_length, %nodes,  %bitfield, %working_pieces
    );
    my (%clutter);    # creation date, encoding, created by, etc.

    # Constructor
    sub new {

        # Creates a new N::B::Session object
        # Accepts parameters as key/value pairs in a hash reference
        # Required parameters:
        #  - Client  (blessed N::B object)
        #  - Path    (.torrent)
        # Optional parameters
        #  - BaseDir (root directory for related files; defaults to cwd)
        # Returns
        #    - a new blessed object on success
        #    - undef on failure
        # MO
        # - validate incoming parameters
        # - read .torrent
        # - bdecode data
        # - validate pieces string
        # - get infohash
        # - verify infohash is valid
        # - bless $self
        # - set client
        # - set clutter
        # - if multifile
        #   - loop through each file
        #     - add size to toal length
        #     - create new N::B::S::File object
        # - else
        #   - length is total length
        #   - create new N::B::S::File object
        # - set tracker hash
        # - set pieces hash
        # -
        # -
        # -
        # -
        # -
        # - return $self
        my ($class, $args) = @_;
        my $self;

        # Param validation... Ugh...
        if (not defined $args) {
            confess q[Net::BitTorrent::Session->new({}) requires ]
                . q[parameters a set of parameters];
            return;
        }
        if (ref($args) ne q[HASH]) {
            confess q[Net::BitTorrent::Session->new({}) requires ]
                . q[parameters to be passed as a hashref];
            return;
        }
        if (not defined $args->{q[Path]}) {
            confess q[Net::BitTorrent::Session->new({}) requires a ]
                . q['Path' parameter];
            return;
        }
        if (not -f $args->{q[Path]}) {
            confess
                sprintf(q[Net::BitTorrent::Session->new({}) cannot find '%s'],
                        $args->{q[Path]});
            return;
        }
        if (not defined $args->{q[Client]}) {
            confess q[Net::BitTorrent::Session->new({}) requires a ]
                . q['Client' parameter];
            return;
        }
        if (not blessed $args->{q[Client]}) {
            confess q[Net::BitTorrent::Session->new({}) requires a ]
                . q[blessed 'Client' object];
            return;
        }
        if (not $args->{q[Client]}->isa(q[Net::BitTorrent])) {
            confess q[Net::BitTorrent::Session->new({}) requires a ]
                . q[blessed Net::BitTorrent object in the 'Client' parameter];
            return;
        }

        # Tidy up some rough edges here
        $args->{q[Path]} = rel2abs($args->{q[Path]});
        $args->{q[BaseDir]} = rel2abs(
                  defined($args->{q[BaseDir]}) ? $args->{q[BaseDir]} : cwd());

        # Here comes the real work...
        my ($TORRENT_FH, $TORRENT_RAW, $TORRENT_DATA);

        # Open .torrent
        if (not sysopen($TORRENT_FH, $args->{q[Path]}, O_RDONLY)) {
            confess    # difficult to trigger for a test suite
                sprintf(
                 q[Net::BitTorrent::Session->new({}) could not open '%s': %s],
                 $args->{q[Path]}, $!);
            return;
        }

        # No need to lock it...
        # Read .torrent
        if (sysread($TORRENT_FH, $TORRENT_RAW, -s $args->{q[Path]})
            != -s $args->{q[Path]})
        {   confess    # difficult to trigger for a test suite
                sprintf(
                q[Net::BitTorrent::Session->new({}) could not read all %d bytes of '%s' (Read %d instead)],
                -s $args->{q[Path]},
                $args->{q[Path]}, length($TORRENT_DATA)
                );
            return;
        }

        # Close .torrent
        if (not close($TORRENT_FH)) {
            confess    # difficult to trigger for a test suite
                sprintf(
                q[Net::BitTorrent::Session->new({}) could not close '%s': %s],
                $args->{q[Path]}, $!);
            return;
        }

        #
        undef $TORRENT_FH;

        # bdecode data
        $TORRENT_DATA = bdecode($TORRENT_RAW);
        undef $TORRENT_RAW;

        #warn pp $TORRENT_DATA->{q[info]}{q[files]};
        #warn $args->{q[Path]};
        # parse pieces string and...
        #   - verify pieces string > 40
        #   - verify pieces string % 40 == 0
        if (length(unpack(q[H*], $TORRENT_DATA->{q[info]}{q[pieces]})) < 40)
        {    # TODO: Create bad .torrent to trigger this
                #$_client{$self}
                #    ->_do_callback(q[log], ERROR, q[Broken torrent]);
            return;
        }
        if (length(unpack(q[H*], $TORRENT_DATA->{q[info]}{q[pieces]})) % 40)
        {       # TODO: Create bad .torrent to trigger this
                #$_client{$self}
                #    ->_do_callback(q[log], ERROR, q[Broken torrent]);
            return;
        }

        # Get infohash
        my $infohash = sha1_hex(bencode($TORRENT_DATA->{q[info]}));

        # Verify infohash is valid
        if ($infohash !~ m[^([0-9a-f]{40})$]) {

            # Could this ever really happen?
            #$_client{$self}->_do_callback(q[log], ERROR,
            #                             q[Improper info_hash]);
            return;
        }

        # Bless $self
        $self = bless \$infohash, $class;

        # Store required and extra data
        #
        # What data should I store in its own hash?
        #  - Yes
        #    - infohash
        #    - info/private
        #    - info/piece length
        #    - info/pieces
        #    - announce || announce-list
        #  - Possible (until I decide, they are put into %clutter)
        #    - comment
        #    - encoding
        #    - created by
        #    - creation date
        #    ? info/name
        #    -
        $_client{$self} = $args->{q[Client]};
        weaken $_client{$self};
        $infohash{$self}      = $infohash;
        $_private{$self}      = $TORRENT_DATA->{q[info]}{q[private]} ? 1 : 0;
        $_piece_length{$self} = $TORRENT_DATA->{q[info]}{q[piece length]};
        $pieces{$self}        = $TORRENT_DATA->{q[info]}{q[pieces]};
        $bitfield{$self}      = pack(q[b*], qq[\0] x $self->_piece_count);
        $path{$self}          = $args->{q[Path]};

        #
        $nodes{$self} = q[];

        #warn pp $TORRENT_DATA;
        $clutter{$self} = {
            q[created by]    => $TORRENT_DATA->{q[created by]},
            q[creation date] => $TORRENT_DATA->{q[creation date]},
            q[comment]       => $TORRENT_DATA->{q[comment]},
            q[encoding]      => $TORRENT_DATA->{q[encoding]},
            q[nodes]         => $TORRENT_DATA->{q[nodes]},           # DHT
            q[sources]   => $TORRENT_DATA->{q[sources]},     # Depthstrike
            q[url-list]  => $TORRENT_DATA->{q[url-list]},    # GetRight
            q[httpseeds] => $TORRENT_DATA->{q[httpseeds]}    # BitTornado
        };

        #warn pp \%clutter;
        # Files
        $size{$self} = 0;
        if (defined $TORRENT_DATA->{q[info]}{q[files]}) { # multifile .torrent
            for my $file (@{$TORRENT_DATA->{q[info]}{q[files]}}) {
                $size{$self} += $file->{q[length]};
                my $filename = catfile(
                    $args->{q[BaseDir]},
                    (    #defined($TORRENT_DATA->{q[info]}{q[name.utf-8]})
                           #? $TORRENT_DATA->{q[info]}{q[name.utf-8]}
                           #:
                       $TORRENT_DATA->{q[info]}{q[name]}
                    ),
                    @{  (    #defined($file->{q[path.utf-8]})
                             #? $file->{q[path.utf-8]}
                             #:
                         $file->{q[path]}
                        )
                        }
                );
                if (    defined $TORRENT_DATA->{q[encoding]}
                    and $TORRENT_DATA->{q[encoding]} !~ m[^utf-?8$]i
                    and not utf8::is_utf8($filename)
                    and require Encode)
                {  # some clients do a poor/incomplete job with encoding so we
                       # work around it by upgrading and setting the utf8 flag
                    $filename =
                        Encode::decode(Encode::find_encoding(
                                                  $TORRENT_DATA->{q[encoding]}
                                           )->name,
                                       $filename
                        );
                }
                push(@{$files{$self}},
                     Net::BitTorrent::Session::File->new(
                                         {Size    => $file->{q[length]},
                                          Path    => $filename,
                                          Session => $self,
                                          Index   => scalar(@{$files{$self}})
                                         }
                     )
                );
            }
        }
        else {    # single file .torrent
            my $filename = catfile(
                $args->{q[BaseDir]},
                (    #defined($TORRENT_DATA->{q[info]}{q[name.utf-8]})
                       #? $TORRENT_DATA->{q[info]}{q[name.utf-8]}
                       #:
                   $TORRENT_DATA->{q[info]}{q[name]}
                )
            );
            if (    defined $TORRENT_DATA->{q[encoding]}
                and $TORRENT_DATA->{q[encoding]} !~ m[^utf-?8$]i
                and not utf8::is_utf8($filename)
                and require Encode)
            {    # some clients do a poor/incomplete job with encoding so we
                    # work around it by upgrading and setting the utf8 flag
                $filename =
                    Encode::decode(Encode::find_encoding(
                                                  $TORRENT_DATA->{q[encoding]}
                                       )->name,
                                   $filename
                    );
            }
            warn sprintf q['%s' is utf? %d], $filename,
                utf8::is_utf8($filename);
            push(@{$files{$self}},
                 Net::BitTorrent::Session::File->new(
                              {Size    => $TORRENT_DATA->{q[info]}{q[length]},
                               Path    => $filename,
                               Session => $self,
                               Index   => 0
                              }
                 )
            );
            $size{$self} = $TORRENT_DATA->{q[info]}{q[length]};
        }

        # Trackers
        if (defined $TORRENT_DATA->{q[announce-list]}) {    # Multitracker
            for my $tier (@{$TORRENT_DATA->{q[announce-list]}}) {
                push(@{$trackers{$self}},
                     Net::BitTorrent::Session::Tracker->new(
                                             {Session => $self, URLs => $tier}
                     )
                );
            }
        }
        elsif (defined $TORRENT_DATA->{q[announce]}) {      # Single tracker
            push(@{$trackers{$self}},
                 Net::BitTorrent::Session::Tracker->new(
                                    {Session => $self,
                                     URLs    => [$TORRENT_DATA->{q[announce]}]
                                    }
                 )
            );
        }
        else {    # No trackers; requires DHT
            $trackers{$self} = [];
            if ($_private{$self}) {  # I'm not sure how to handle this.  We...
                 # could resort to Webseeding but... why would anyone do this?
                confess
                    q[This torrent does not contain any trackers and does ]
                    . q[not allow DHT];
                return;
            }
        }

        #
        #warn pp \%size;
        #warn pp \%files;
        #warn pp \%trackers;
        $_client{$self}->schedule({Time   => time + 15,
                                   Code   => sub { shift->_new_peer },
                                   Object => $self
                                  }
        );
        return $self;
    }

    # Accessors | Public
    sub infohash { return $infohash{+shift} }
    sub complete { die; return undef }          # TODO
    sub trackers { return $trackers{+shift} }
    sub bitfield { return $bitfield{+shift} }
    sub path     { return $path{+shift} }
    sub files    { return $files{+shift} }
    sub size     { return $size{+shift} }

    # Accessors | Private
    sub _client       { return $_client{+shift}; }
    sub _uploaded     { return $uploaded{+shift} || 0; }
    sub _downloaded   { return $downloaded{+shift} || 0; }
    sub _piece_length { return $_piece_length{+shift}; }
    sub _private      { return $_private{+shift}; }

    sub _piece_count {
        return int(length(unpack(q[H*], $pieces{+shift})) / 40);
    }
    sub _compact_nodes { return $nodes{+shift}; }

    sub _wanted {
        my ($self) = @_;
        my $wanted = q[0] x $self->_piece_count;
        my $p_size = $_piece_length{$self};
        my $offset = 0;
        for my $file (@{$files{$self}}) {

        #    warn sprintf q[[i%d|p%d|s%d] %s ], $file->index, $file->priority,
        #    $file->size, $$file;
            my $start = ($offset / $p_size);
            my $end   = (($offset + $file->size) / $p_size);

            #warn sprintf q[%d .. %d | %d], ($start + ($start > int($start))),
            #    ($end + ($end > int($end))), ($end - $start + 1);
            if ($file->priority ? 1 : 0) {
                substr($wanted, $start,
                       ($end - $start + 1),
                       (($file->priority ? 1 : 0) x ($end - $start + 1)));
            }

            #warn $wanted;
            $offset += $file->size;
        }

        #my $relevence = $peer->_bitfield | $_wanted ^ $_wanted;
        return (pack(q[b*], $wanted) | $bitfield{$self} ^ $bitfield{$self});
    }

    # Methods | Public
    # Methods | Private

    sub _add_uploaded{ my ($self, $amount)=@_; $uploaded{$self}+=$amount;}
    sub _add_downloaded{my ($self, $amount)=@_; $downloaded{$self}+=$amount;}

    sub _append_compact_nodes {
        my ($self, $nodes) = @_;
        if (not $nodes) {
            warn q[No nodes!];
            return;
        }
        return $nodes{$self} = compact(uncompact($nodes{$self} . $nodes));
    }

    sub _new_peer {
        my ($self) = @_;

        #
        $_client{$self}->schedule({Time   => time + 15,
                                   Code   => sub { shift->_new_peer },
                                   Object => $self
                                  }
        );

        #
        if (not $nodes{$self}) { return; }

        #
        my @nodes = uncompact($nodes{$self});

        #
        for (1 .. (30 - scalar @{$self->_peers})) {
            last if not @nodes;

            #
            my $node = shift @nodes;

            #
            my $ok
                = $_client{$self}->_event(q[ip_filter], {Address => $node});
            if (defined $ok and $ok == 0) { next; }

            #
            my $peer =
                Net::BitTorrent::Peer->new({Address => $node,
                                            Session => $self
                                           }
                );
        }

        #
        return 1;
    }

    sub _peers {
        my ($self) = @_;

        #
        my @return = map {
            (    ($_->{q[Object]}->isa(q[Net::BitTorrent::Peer]))
             and ($_->{q[Object]}->_session)
             and ($_->{q[Object]}->_session eq $self))
                ? $_->{q[Object]}
                : ()
        } values %{$_client{$self}->_connections};

        #
        return \@return;
    }

    sub _piece_by_index {
        my ($self, $index) = @_;

        #
        if (not defined $index) {
            confess
                q[Net::BitTorrent::Session->_piece_by_index() requires an index];
            return;
        }

        #
        if ($index !~ m[^\d+$]) {
            confess
                q[Net::BitTorrent::Session->_piece_by_index() requires a positive integer];
            return;
        }

        #
        if (defined $working_pieces{$self}{$index}) {
            return $working_pieces{$self}{$index};
        }

        #
        return;
    }

    sub _pick_piece {
        my ($self, $peer) = @_;

        # TODO: param validation
        if (not defined $peer) {
            confess
                q[Net::BitTorrent::Session->_pick_piece(PEER) requires a peer];
        }
        if (not blessed $peer) {
            confess
                q[Net::BitTorrent::Session->_pick_piece(PEER) requires a blessed peer];
        }
        if (not $peer->isa(q[Net::BitTorrent::Peer])) {
            confess
                q[Net::BitTorrent::Session->_pick_piece(PEER) requires a peer object];
        }

        #
        #warn pp $working_pieces{$self};
        #
        my $piece;

        # pieces this peer has and we need
        my $_wanted   = $self->_wanted;
        my $relevence = $peer->_bitfield & $_wanted;

        #
        return if unpack(q[b*], $relevence) !~ m[1];

        #
        my $endgame = (    # XXX - make this a percentage variable
            (sum(split(q[], unpack(q[b*], $_wanted)))
                 <= (length(unpack(q[b*], $_wanted)) * 0.7)
            )
            ? 1
            : 0
        );

    #
    # block_per_piece = 35
    # pieces          = 5
    # slots           = 20
    # unchoked_peers  = 10
    #
    # (block_size * pieces) <= (peers * 20)
    # (((blocks_per_piece * pieces) / slots) * peers) = 5
    # working = ((blocks_per_piece / slots)  * peers)
    #
    #  There should be, at least, ($slots * $peers) blocks.
    #  $working = int ( ( $slots * $unchoked_peers ) / $blocks_per_piece ) + 1
    #  $working = int ( ( 20     * 8               ) / 35                ) + 1
    #
        my $slots = int(((2**21) / $_piece_length{$self}));    # ~2M/peer
        my $unchoked_peers
            = scalar(grep { $_->_peer_choking == 0 } @{$self->_peers});
        my $blocks_per_piece = int($_piece_length{$self} / (
                                               ($_piece_length{$self} < 2**14)
                                               ? $_piece_length{$self}
                                               : 2**14
                                   )
        );
        my $max_working_pieces
            = int(($slots * $unchoked_peers) / $blocks_per_piece) + 1;

        #warn sprintf q[$max_working_pieces: %d], $max_working_pieces;
        #
        if (scalar(grep {$_->{q[Slow]} == 0} values %{$working_pieces{$self}}) >= $max_working_pieces) {

            #warn sprintf q[%d>=%d], (scalar(keys %{$working_pieces{$self}})),
            #    $max_working_pieces;
            my @indexes =
            grep {$working_pieces{$self}{$_}->{q[Slow]} == 0}
            keys %{$working_pieces{$self}};

            #warn sprintf q[indexes: %s], (join q[, ], @indexes);
            for my $index (@indexes) {
                if (vec($relevence, $index, 1) == 1) {
                    if (index((  $endgame
                               ? $working_pieces{$self}{$index}
                                   {q[Blocks_Recieved]}
                               : $working_pieces{$self}{$index}
                                   {q[Blocks_Requested]}
                              ),
                              0, 0
                        ) != -1
                        )
                    {   $piece = $working_pieces{$self}{$index};
                        last;
                    }
                }
            }
        }
        else {

            #warn sprintf q[%d<%d], (scalar(keys %{$working_pieces{$self}})),
            #    $max_working_pieces;
            my @wanted;
            for my $i (0 .. ($self->_piece_count - 1))
            {    # XXX - Far from efficient...
                push @wanted, $i if vec($relevence, $i, 1);
            }
            @wanted = shuffle @wanted;    # XXX - use priorities...
        TRY: for my $try (1 .. 10) {
                my $index = shift @wanted;
                next TRY if vec($relevence, $index, 1) == 0;
                my $_piece_length = (    # XXX - save some time and store this
                    ($index == int($size{$self} / $_piece_length{$self}))
                    ? ($size{$self} % $_piece_length{$self})
                    : ($_piece_length{$self})
                );

                #
                my $block_length = (  ($_piece_length{$self} < 2**14)
                                    ? ($_piece_length{$self})
                                    : (2**14)
                );
                my $block_length_last
                    = ($_piece_length{$self} % $_piece_length);

                #die $block_length_last;
                # XXX - may not be balanced
                my $block_count
                    = (int($_piece_length / $block_length)
                           + ($block_length_last ? 1 : 0));

                #
                $piece = {
                    Index    => $index,
                    Priority => 2,        # Get from file
                    Blocks_Requested => [map {0} 1 .. $block_count],
                    Blocks_Recieved => [map {0} 1 .. $block_count],
                    Block_Length    => $block_length,
                    Block_Length_Last => $block_length_last,
                    Block_Count       => $block_count,
                    Length            => $_piece_length,
                    Endgame           => $endgame,
                    Slow => 0,
                    Touch => 0
                };
                last TRY;
            }
        }

        #
        if ($piece) {
            if (not defined $working_pieces{$self}{$piece->{q[Index]}}) {
                $working_pieces{$self}{$piece->{q[Index]}} = $piece;
            }
        }
        return $piece;
    }

    sub _write_data {
        my ($self, $index, $offset, $data) = @_;

        # TODO: param validation
        if ((length($$data) + (($_piece_length{$self} * $index) + $offset))
            > $size{$self})
        {   confess q[Too much data or bad offset data for this torrent];
            return;
        }

        #
        my $file_index = 0;
        my $total_offset
            = int((($index * $_piece_length{$self})) + ($offset || 0));

        #warn sprintf q[Write I:%d O:%d L:%d TOff:%d], $index, $offset,
        #    length($$data), $total_offset;
    SEARCH:
        while ($total_offset > $files{$self}->[$file_index]->size) {
            $total_offset -= $files{$self}->[$file_index]->size;
            $file_index++;
            last SEARCH    # XXX - should this simply return?
                if not defined $files{$self}->[$file_index]->size;
        }
    WRITE: while (length $$data > 0) {
            my $this_write
                = ($total_offset + length $$data
                   > $files{$self}->[$file_index]->size)
                ? $files{$self}->[$file_index]->size - $total_offset
                : length $$data;
            $files{$self}->[$file_index]->_open(q[w]) or return;
            $files{$self}->[$file_index]->_sysseek($total_offset);
            $files{$self}->[$file_index]
                ->_write(substr($$data, 0, $this_write, q[]))
                or return;
            $file_index++;
            last WRITE if not defined $files{$self}->[$file_index];
            $total_offset = 0;
        }

        #
        return 1;
    }

    sub _read_data {
        my ($self, $index, $offset, $length) = @_;

        #
        confess q[Bad index!]  if not defined $index  || $index !~ m[^\d+$];
        confess q[Bad offset!] if not defined $offset || $offset !~ m[^\d+$];
        confess q[Bad length!] if not defined $length || $length !~ m[^\d+$];

        #
        my $data = q[];
        if (($length + (($_piece_length{$self} * $index) + $offset))
            > $size{$self})
        {   confess q[Too much or bad offset data for this torrent];
            return;
        }

        #
        my $file_index = 0;
        my $total_offset
            = int((($index * $_piece_length{$self})) + ($offset || 0));

        #warn sprintf q[Read  I:%d O:%d L:%d TOff:%d], $index, $offset,
        #    $length, $total_offset;
    SEARCH:
        while ($total_offset > $files{$self}->[$file_index]->size) {
            $total_offset -= $files{$self}->[$file_index]->size;
            $file_index++;
            last SEARCH    # XXX - should this simply return?
                if not defined $files{$self}->[$file_index]->size;
        }
    READ: while (length($data) < $length) {
            my $this_read
                = (
                 $total_offset + $length > $files{$self}->[$file_index]->size)
                ? $files{$self}->[$file_index]->size - $total_offset
                : $length - (length($data));

            #warn sprintf q[Reading %d bytes from '%s'],
            #    $this_read, $files{$self}->[$file_index]->path;
            $files{$self}->[$file_index]->_open(q[r]) or return;
            $files{$self}->[$file_index]->_sysseek($total_offset);
            $data .= $files{$self}->[$file_index]->_read($this_read)
                or return;

            #
            $file_index++;
            last READ if not defined $files{$self}->[$file_index];
            $total_offset = 0;
        }

        #
        return \$data;
    }

    sub hashcheck {
        my ($self) = @_;
        for my $index (0 .. ($self->_piece_count - 1)) {
            $self->_check_piece_by_index($index);
        }
        return 1;
    }

    sub _check_piece_by_index {
        my ($self, $index) = @_;

        #
        if (not defined $index) {
            confess
                q[Net::BitTorrent::Session->_check_piece_by_index( INDEX ) ]
                . q[requires an index.];
            return;
        }
        if ($index !~ m[^\d+$]) {
            confess
                q[Net::BitTorrent::Session->_check_piece_by_index( INDEX ) ]
                . q[requires an integer index.];
            return;
        }

        #
        if (defined $working_pieces{$self}{$index}) {
            delete $working_pieces{$self}{$index};
            #if (keys %{$working_pieces{$self}}) {
            #    warn q[Remaining working pieces: ]
            #        . pp $working_pieces{$self};
            #}
        }

        #
        my $data = $self->_read_data($index, 0,
                                     ($index == ($self->_piece_count - 1)
                                      ? ($size{$self} % $_piece_length{$self})
                                      : $_piece_length{$self}
                                     )
        );

        #
        #warn sprintf q[%s vs %s],
        #     sha1_hex($data),
        #     substr(unpack(q[H*], $pieces{$self}), $index * 40, 40);
        if ((not $data)
            or (sha1_hex($$data) ne
                substr(unpack(q[H*], $pieces{$self}), $index * 40, 40))
            )
        {   vec($bitfield{$self}, $index, 1) = 0;
            $_client{$self}->_event(q[piece_hash_fail],
                                    {Session => $self, Index => $index});
            return;
        }

        #
        if (vec($bitfield{$self}, $index, 1) == 0) {
            # Only trigger event if piece is 'new'
            vec($bitfield{$self}, $index, 1) = 1;
            $_client{$self}->_event(q[piece_hash_pass],
                                    {Session => $self, Index => $index});
        }

        #
        return 1;
    }

    # Destructor
    DESTROY {
        my ($self) = @_;

        #warn q[Goodbye, ] . $$self;
        delete $_client{$self};
        delete $path{$self};
        delete $basedir{$self};
        delete $size{$self};
        delete $files{$self};
        delete $trackers{$self};
        delete $infohash{$self};
        delete $_private{$self};
        delete $pieces{$self};
        delete $clutter{$self};
        delete $uploaded{$self};
        delete $downloaded{$self};
        delete $_piece_length{$self};
        delete $nodes{$self};
        delete $bitfield{$self};
        delete $working_pieces{$self};

        #
        return 1;
    }

=pod

=head1 NAME

Net::BitTorrent::Session - Class Representing a Single .torrent File

=head1 Description

=head1 Constructor

=over 4

=item C<new ( { [ARGS] } )>

Creates a C<Net::BitTorrent::Session> object.  This constructor is
called by
L<Net::BitTorrent::add_session( )|Net::BitTorrent/add_session ( { ... } )>
and should not be used directly.

C<new( )> accepts arguments as a hash, using key-value pairs:

=over 4

=item C<BaseDir>

The root directory used to store the files related to this session.  This
directory is created if not preexisting.

This is the only optional parameter.

Default: C<./> (Current working directory)

=item C<Client>

The L<Net::BitTorrent|Net::BitTorrent> object this session will
eventually be served from.

=item C<Path>

Filename of the .torrent file to load.

=back

=back

=head1 Incompatable Changes

=over

=item *

C<block_size> is no longer a supported parameter for
L<new( )|/"new ( { [ARGS] } )">.

=item *

L<new( )|/"new ( { [ARGS] } )"> parameter C<base_dir> has been renamed
C<BaseDir>.

=item *

C<get_infohash( )> has been renamed (yeah, yeah, I know...) to
L<infohash( )|/infohash( )>.  The old C<get_infohash( )> accessor will
work for at least the next few versions, but it will eventually be
depricated.

=back

























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
    1;
}
