#!C:\perl\bin\perl.exe
package Net::BitTorrent::Torrent;
{
    use strict;      # core as of perl 5
    use warnings;    # core as of perl 5.006

    #
    use Digest::SHA qw[sha1_hex];                   # core as of perl 5.009003
    use Carp qw[carp carp];                         # core as of perl 5
    use Cwd qw[cwd];                                # core as of perl 5
    use File::Spec::Functions qw[rel2abs catfile];  # core as of perl 5.00504
    use Scalar::Util qw[blessed weaken refaddr];    # core as of perl 5.007003
    use List::Util qw[sum shuffle max];             # core as of perl 5.007003
    use Fcntl qw[O_RDONLY];                         # core as of perl 5

    #
    use lib q[../../../lib];
    use Net::BitTorrent::Util qw[:bencode :compact];
    use Net::BitTorrent::Torrent::File;
    use Net::BitTorrent::Torrent::Tracker;
    use Net::BitTorrent::Peer;

    #
    use version qw[qv];                             # core as of 5.009
    our $SVN = q[$Id$];
    our $UNSTABLE_RELEASE = 0; our $VERSION = sprintf(($UNSTABLE_RELEASE ? q[%.3f_%03d] : q[%.3f]), (version->new((qw$Rev$)[1])->numify / 1000), $UNSTABLE_RELEASE);

    #
    # Debugging
    #use Data::Dump qw[pp];
    #
    my %REGISTRY = ();
    my @CONTENTS = \my (
        %_client, %path, %basedir,    # new() params (path is required)
        %size,          %files,  %trackers,  %infohash,
        %_private,      %pieces, %_uploaded, %_downloaded,
        %_piece_length, %nodes,  %bitfield,  %working_pieces,
        %_block_length,
        %_raw_data,    # creation date, encoding, created by, etc.
        %status        # (201=Started, 961=Force Download, 1000=finished)
                       # 1   = Started
                       # 2   = Checking
                       # 4   = Start after check
                       # 8   = Checked
                       # 16  = Error
                       # 32  = Paused
                       # 64  = Queued
                       # 128 = Loaded
    );

    # Constructor
    sub new {

        # Creates a new N::B::Torrent object
        # Accepts parameters as key/value pairs in a hash reference
        # Required parameters:
        #  - Path    (.torrent)
        # Optional parameters
        #  - Client  (blessed N::B object)
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
        # - set _raw_data
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
        my $self = bless \$class, $class;

        # Param validation... Ugh...
        if (not defined $args) {
            carp q[Net::BitTorrent::Torrent->new({}) requires ]
                . q[a set of parameters];
            return;
        }
        if (ref($args) ne q[HASH]) {
            carp q[Net::BitTorrent::Torrent->new({}) requires ]
                . q[parameters to be passed as a hashref];
            return;
        }
        if (not defined $args->{q[Path]}) {
            carp q[Net::BitTorrent::Torrent->new({}) requires a ]
                . q['Path' parameter];
            return;
        }
        if (not -f $args->{q[Path]}) {
            carp
                sprintf(q[Net::BitTorrent::Torrent->new({}) cannot find '%s'],
                        $args->{q[Path]});
            return;
        }
        if (defined $args->{q[Client]}) {
            if (not blessed $args->{q[Client]}) {
                carp q[Net::BitTorrent::Torrent->new({}) requires a ]
                    . q[blessed 'Client' object];
                return;
            }
            if (not $args->{q[Client]}->isa(q[Net::BitTorrent])) {
                carp q[Net::BitTorrent::Torrent->new({}) requires a ]
                    . q[blessed Net::BitTorrent object in the 'Client' parameter];
                return;
            }
        }
        if (defined $args->{q[BlockLength]}) {    # Undocumented
            if ($args->{q[BlockLength]} !~ m[^\d+$]) {
                carp q[Net::BitTorrent::Torrent->new({}) requires an ]
                    . q[integer 'BlockLength' parameter];
                delete $args->{q[BlockLength]};
            }
        }
        if (defined $args->{q[Status]}) {         # Undocumented
            if ($args->{q[Status]} !~ m[^\d+$]) {
                carp q[Net::BitTorrent::Torrent->new({}) requires an ]
                    . q[integer 'Status' parameter];
                delete $args->{q[Status]};
            }
        }

        # Tidy up some rough edges here
        $args->{q[Path]} = rel2abs($args->{q[Path]});
        $args->{q[BaseDir]} = rel2abs(
                  defined($args->{q[BaseDir]}) ? $args->{q[BaseDir]} : cwd());

        # Here comes the real work...
        my ($TORRENT_FH, $TORRENT_RAW);

        # Open .torrent
        if (not sysopen($TORRENT_FH, $args->{q[Path]}, O_RDONLY)) {
            carp    # difficult to trigger for a test suite
                sprintf(
                 q[Net::BitTorrent::Torrent->new({}) could not open '%s': %s],
                 $args->{q[Path]}, $!);
            return;
        }

        # No need to lock it...
        # Read .torrent
        if (sysread($TORRENT_FH, $TORRENT_RAW, -s $args->{q[Path]})
            != -s $args->{q[Path]})
        {   carp    # difficult to trigger for a test suite
                sprintf(
                q[Net::BitTorrent::Torrent->new({}) could not read all %d bytes of '%s' (Read %d instead)],
                -s $args->{q[Path]},
                $args->{q[Path]}, length($TORRENT_RAW)
                );
            return;
        }

        # Close .torrent
        if (not close($TORRENT_FH)) {
            carp    # difficult to trigger for a test suite
                sprintf(
                q[Net::BitTorrent::Torrent->new({}) could not close '%s': %s],
                $args->{q[Path]}, $!);
            return;
        }

        # bdecode data
        $_raw_data{refaddr $self} = bdecode($TORRENT_RAW);

        # Keep it clean...
        undef $TORRENT_RAW;
        undef $TORRENT_FH;

        #
        if (not defined $_raw_data{refaddr $self}) {
            carp q[Malformed .torrent];
            return;
        }

        #warn pp $_raw_data{refaddr $self}{q[info]}{q[files]};
        #warn $args->{q[Path]};
        # parse pieces string and...
        #   - verify pieces string > 40
        #   - verify pieces string % 40 == 0
        if (length(
                  unpack(q[H*], $_raw_data{refaddr $self}{q[info]}{q[pieces]})
            ) < 40
            )
        {    # TODO: Create bad .torrent to trigger this for tests
                #$_client{refaddr $self}
             #    ->_event(q[log], {Level=>ERROR, Msg=>q[Broken torrent: Pieces hash is less than 40 bytes]})
             #  if defined $_client{refaddr $self};
            return;
        }
        if (length(
                  unpack(q[H*], $_raw_data{refaddr $self}{q[info]}{q[pieces]})
            ) % 40
            )
        {    # TODO: Create bad .torrent to trigger this for tests
                #$_client{refaddr $self}
             #    ->_event(q[log], {Level=>ERROR, Msg=>q[Broken torrent: Pieces hash will not break apart into even, 40 byte segments]})
             # if defined $_client{refaddr $self};
            return;
        }

        # Get infohash
        my $infohash = sha1_hex(bencode($_raw_data{refaddr $self}{q[info]}));

        # Verify infohash is valid
        if ($infohash !~ m[^([0-9a-f]{40})$]) {

            # Could this ever really happen?
            #$_client{refaddr $self}->_event(q[log], {Level=>ERROR,
            #                             Msg=>q[Improper info_hash]})
            # if defined $_client{refaddr $self};
            return;
        }

        # Set scalar content for blessed $self
        $$self = $infohash;

        # Store required and extra data
        #
        # What data should I store in its own hash?
        #  - Yes
        #    - infohash
        #    - info/private
        #    - info/piece length
        #    - info/pieces
        #    - announce || announce-list
        #  - Possible (until I decide, they are put into %_raw_data)
        #    - comment
        #    - encoding
        #    - created by
        #    - creation date
        #    ? info/name
        #    -
        if (defined $args->{q[Client]}) {
            $_client{refaddr $self} = $args->{q[Client]};
            weaken $_client{refaddr $self};
        }
        $infohash{refaddr $self} = $infohash;
        $_private{refaddr $self}
            = $_raw_data{refaddr $self}{q[info]}{q[private]} ? 1 : 0;
        $_piece_length{refaddr $self}
            = $_raw_data{refaddr $self}{q[info]}{q[piece length]};
        $path{refaddr $self}           = $args->{q[Path]};
        $basedir{refaddr $self}        = $args->{q[BaseDir]};
        $working_pieces{refaddr $self} = {};
        $_block_length{refaddr $self} = (defined $args->{q[BlockLength]}
                                         ? $args->{q[BlockLength]}
                                         : (2**14)
        );
        $nodes{refaddr $self} = q[];
        ${$bitfield{refaddr $self}}
            = pack(q[b*], qq[\0] x $self->_piece_count);

        # (201=Started, 961=Force Download, 1000=finished)
        #     1 = Started
        #     2 = Checking
        #     4 = Start after check
        #     8 = Checked
        #    16 = Error
        #    32 = Paused
        #    64 = Queued
        #   128 = Loaded
        #   256 =
        #   512 = Force
        #  1000 = Complete
        ${$status{refaddr $self}} = (defined $args->{q[Status]}
                                     ? $args->{q[Status]}
                                     : (1 | 64)
        );
        ${$status{refaddr $self}} |= 128;
        ${$status{refaddr $self}} |= 64
            if defined $_client{refaddr $self};

     #q[nodes]         => $_raw_data{refaddr $self}{q[nodes]},           # DHT
     #q[sources]   => $_raw_data{refaddr $self}{q[sources]},     # Depthstrike
     #q[url-list]  => $_raw_data{refaddr $self}{q[url-list]},    # GetRight
     #q[httpseeds] => $_raw_data{refaddr $self}{q[httpseeds]},    # BitTornado
     #
     #q[name] => $_raw_data{refaddr $self}{q[info]}{q[name]}
     #warn pp \%_raw_data;
     # Files
        my @_files;
        if (defined $_raw_data{refaddr $self}{q[info]}{q[files]})
        {    # multifile .torrent
            for my $file (@{$_raw_data{refaddr $self}{q[info]}{q[files]}}) {
                push @_files, [
                    catfile(
                        $basedir{refaddr $self},
                        ( #defined($_raw_data{refaddr $self}{q[info]}{q[name.utf-8]})
                            #? $_raw_data{refaddr $self}{q[info]}{q[name.utf-8]}
                            #:
                           $_raw_data{refaddr $self}{q[info]}{q[name]}
                        ),
                        @{  (    #defined($file->{q[path.utf-8]})
                                 #? $file->{q[path.utf-8]}
                                 #:
                             $file->{q[path]}
                            )
                            }
                    ),
                    $file->{q[length]}
                ];
            }
        }
        else {
            push @_files, [
                catfile(
                    $basedir{refaddr $self},
                    ( #defined($_raw_data{refaddr $self}{q[info]}{q[name.utf-8]})
                          #? $_raw_data{refaddr $self}{q[info]}{q[name.utf-8]}
                          #:
                       $_raw_data{refaddr $self}{q[info]}{q[name]}
                    )
                ),
                $_raw_data{refaddr $self}{q[info]}{q[length]}
            ];
        }
        $size{refaddr $self} = 0;
        for my $_file (@_files) {
            my ($path, $size) = @$_file;
            {             # XXX - an attempt to make paths safe. Needs work.
                $path =~ s[\.\.][]g;
                $path =~ m[(.+)];      # Mark it as untainted
                $path = $1;
            }
            if (    defined $_raw_data{refaddr $self}{q[encoding]}
                and $_raw_data{refaddr $self}{q[encoding]} !~ m[^utf-?8$]i
                and not utf8::is_utf8($path)
                and require Encode)
            {    # some clients do a poor/incomplete job with encoding so we
                    # work around it by upgrading and setting the utf8 flag
                $path =
                    Encode::decode(Encode::find_encoding(
                                        $_raw_data{refaddr $self}{q[encoding]}
                                       )->name,
                                   $path
                    );
            }
            push(@{$files{refaddr $self}},
                 Net::BitTorrent::Torrent::File->new(
                                 {Size    => $size,
                                  Path    => $path,
                                  Torrent => $self,
                                  Index   => scalar(@{$files{refaddr $self}})
                                 }
                 )
            );
            $size{refaddr $self} += $size;
        }

        # Trackers
        if (defined $_raw_data{refaddr $self}{q[announce-list]})
        {    # Multitracker
            for my $tier (@{$_raw_data{refaddr $self}{q[announce-list]}}) {
                push(@{$trackers{refaddr $self}},
                     Net::BitTorrent::Torrent::Tracker->new(
                                             {Torrent => $self, URLs => $tier}
                     )
                );
            }
        }
        elsif (defined $_raw_data{refaddr $self}{q[announce]})
        {    # Single tracker
            push(@{$trackers{refaddr $self}},
                 Net::BitTorrent::Torrent::Tracker->new(
                          {Torrent => $self,
                           URLs    => [$_raw_data{refaddr $self}{q[announce]}]
                          }
                 )
            );
        }
        else {    # No trackers; requires DHT
            $trackers{refaddr $self} = [];
            if ($_private{refaddr $self})
            {     # I'm not sure how to handle this.  We...
                 # could resort to Webseeding but... why would anyone do this?
                carp q[This torrent does not contain any trackers and does ]
                    . q[not allow DHT];
                return;
            }
        }

        #
        #warn pp \%size;
        #warn pp \%files;
        #warn pp \%trackers;
        $_client{refaddr $self}->_schedule(
                                  {Time   => time + 15,
                                   Code   => sub { shift->_new_peer if @_; },
                                   Object => $self
                                  }
        ) if defined $_client{refaddr $self};

        # threads stuff
        weaken($REGISTRY{refaddr $self} = $self);
        if ($threads::shared::threads_shared)
        {    # allows non-blocking hashcheck
            threads::shared::share($bitfield{refaddr $self});
            threads::shared::share($status{refaddr $self});
        }

        #
        return $self;
    }

    # Mutators | Private
    sub _set_bitfield {
        my ($self, $new_value) = @_;
        return if ${$status{refaddr $self}} & 2;    # hashchecking
        return if length ${$bitfield{refaddr $self}} != length $new_value;

        # XXX - make sure bitfield conforms to what we expect it to be
        return ${$bitfield{refaddr $self}} = $new_value;
    }

    sub _set_status {
        my ($self, $new_value) = @_;
        return if ${$status{refaddr $self}} & 2;    # hashchecking
             # XXX - make sure status conforms to what we expect it to be
        return ${$status{refaddr $self}} = $new_value;
    }

    # Accessors | Public
    sub infohash { return $infohash{refaddr +shift}; }
    sub trackers { return $trackers{refaddr +shift}; }
    sub bitfield { return ${$bitfield{refaddr +shift}}; }
    sub path     { return $path{refaddr +shift}; }
    sub files    { return $files{refaddr +shift}; }
    sub size     { return $size{refaddr +shift}; }
    sub status   { return ${$status{refaddr +shift}}; }

    # Accessors | Private
    sub _client       { return $_client{refaddr +shift}; }
    sub _uploaded     { return $_uploaded{refaddr +shift} || 0; }
    sub _downloaded   { return $_downloaded{refaddr +shift} || 0; }
    sub _piece_length { return $_piece_length{refaddr +shift}; }
    sub _private      { return $_private{refaddr +shift}; }
    sub _block_length { return $_block_length{refaddr +shift} }
    sub _raw_data     { return $_raw_data{refaddr +shift} }

    sub _complete {
        my ($self) = @_;
        return if ${$status{refaddr $self}} & 2;    # hashchecking
        return ((substr(unpack(q[b*], $self->_wanted), 0, $self->_piece_count)
                     !~ 1
                )
                ? 1
                : 0
        );
    }

    sub _piece_count {    # XXX - could use a cache...
        my ($self) = @_;
        return
            int(
              length(
                  unpack(q[H*], $_raw_data{refaddr $self}{q[info]}{q[pieces]})
                  ) / 40
            );
    }
    sub _compact_nodes { return $nodes{refaddr +shift}; }

    sub _wanted {
        my ($self) = @_;
        return if ${$status{refaddr $self}} & 2;    # hashchecking
        my $wanted = q[0] x $self->_piece_count;
        my $p_size = $_piece_length{refaddr $self};
        my $offset = 0;
        for my $file (@{$files{refaddr $self}}) {

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
        return (
             pack(q[b*], $wanted)
                 | ${$bitfield{refaddr $self}} ^ ${$bitfield{refaddr $self}});
    }

    # Methods | Public
    # ...None yet?
    # Methods | Private
    sub _add_uploaded {
        my ($self, $amount) = @_;
        return if not defined $_client{refaddr $self};
        return if ${$status{refaddr $self}} & 2;         # hashchecking
        $_uploaded{refaddr $self} += (($amount =~ m[^\d+$]) ? $amount : 0);
    }

    sub _add_downloaded {
        my ($self, $amount) = @_;
        return if not defined $_client{refaddr $self};
        return if ${$status{refaddr $self}} & 2;         # hashchecking
        $_downloaded{refaddr $self} += (($amount =~ m[^\d+$]) ? $amount : 0);
    }

    sub _append_compact_nodes {                          # XXX - untested
        my ($self, $nodes) = @_;
        return if not defined $_client{refaddr $self};
        if (not $nodes) { return; }
        $nodes{refaddr $self} ||= q[];
        return $nodes{refaddr $self}
            = compact(uncompact($nodes{refaddr $self} . $nodes));
    }

    sub _new_peer {
        my ($self) = @_;
        return if not defined $_client{refaddr $self};
        return if ${$status{refaddr $self}} & 2;         # hashchecking
                                                         #
        $_client{refaddr $self}->_schedule(
                                         {Time   => time + 15,
                                          Code   => sub { shift->_new_peer },
                                          Object => $self
                                         }
        );

 #
 #         warn sprintf q[Half open peers: %d | Total: %d], scalar(
 #~             grep {
 #~                 $_->{q[Object]}->isa(q[Net::BitTorrent::Peer])
 #~                     and not defined $_->{q[Object]}->peerid()
 #~                 } values %{$_client{refaddr $self}->_connections}
 #~             ),
 #~             scalar(grep { $_->{q[Object]}->isa(q[Net::BitTorrent::Peer]) }
 #~                    values %{$_client{refaddr $self}->_connections});
 #
        if (scalar(
                grep {
                    $_->{q[Object]}->isa(q[Net::BitTorrent::Peer])
                        and not defined $_->{q[Object]}->peerid
                    } values %{$_client{refaddr $self}->_connections}
            ) >= 8
            )
        {   return;
        }    # half open
        if ($self->_complete)          { return; }
        if (not $nodes{refaddr $self}) { return; }

        #
        my @nodes = uncompact($nodes{refaddr $self});

        #
        for (1 .. (30 - scalar @{$self->_peers})) {
            last if not @nodes;

            #
            my $node = shift @nodes;

            #
            my $ok = $_client{refaddr $self}
                ->_event(q[ip_filter], {Address => $node});
            if (defined $ok and $ok == 0) { next; }

            #
            my $peer =
                Net::BitTorrent::Peer->new({Address => $node,
                                            Torrent => $self
                                           }
                );
            last
                if scalar(
                grep {
                    $_->{q[Object]}->isa(q[Net::BitTorrent::Peer])
                        and not defined $_->{q[Object]}->peerid
                    } values %{$_client{refaddr $self}->_connections}
                ) >= 8;
        }

        #
        return 1;
    }

    sub _peers {
        my ($self) = @_;
        return if not defined $_client{refaddr $self};

        #
        my $_connections = $_client{refaddr $self}->_connections;
        my @return       = map {
            (    ($_->{q[Object]}->isa(q[Net::BitTorrent::Peer]))
             and ($_->{q[Object]}->_torrent)
             and ($_->{q[Object]}->_torrent eq $self))
                ? $_->{q[Object]}
                : ()
        } values %$_connections;

        #
        return \@return;
    }

    sub _add_tracker {
        my ($self, $tier) = @_;
        return if not defined $_client{refaddr $self};
        carp q[Please, pass new tier in an array ref...]
            unless ref $tier eq q[ARRAY];
        return
            push(@{$trackers{refaddr $self}},
                 Net::BitTorrent::Torrent::Tracker->new(
                                             {Torrent => $self, URLs => $tier}
                 )
            );
    }

    sub _piece_by_index {
        my ($self, $index) = @_;

        #
        if (not defined $index) {
            carp
                q[Net::BitTorrent::Torrent->_piece_by_index() requires an index];
            return;
        }

        #
        if ($index !~ m[^\d+$]) {
            carp
                q[Net::BitTorrent::Torrent->_piece_by_index() requires a positive integer];
            return;
        }

        #
        if (defined $working_pieces{refaddr $self}{$index}) {
            return $working_pieces{refaddr $self}{$index};
        }

        #
        return;
    }

    sub _pick_piece {
        my ($self, $peer) = @_;
        return if not defined $_client{refaddr $self};
        return if ${$status{refaddr $self}} & 2;      # hashchecking
                                                      # TODO: param validation
        if (not defined $peer) {
            carp
                q[Net::BitTorrent::Torrent->_pick_piece(PEER) requires a peer];
        }
        if (not blessed $peer) {
            carp
                q[Net::BitTorrent::Torrent->_pick_piece(PEER) requires a blessed peer];
        }
        if (not $peer->isa(q[Net::BitTorrent::Peer])) {
            carp
                q[Net::BitTorrent::Torrent->_pick_piece(PEER) requires a peer object];
        }

        #
        #use Data::Dump qw[pp];
        #warn q[_pick_piece ] . pp $working_pieces{refaddr $self};
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
                 <= (length(unpack(q[b*], $_wanted)) * 0.01)
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
        my $slots = int(((2**23) / $_piece_length{refaddr $self})); # ~8M/peer
        my $unchoked_peers
            = scalar(grep { $_->_peer_choking == 0 } @{$self->_peers});
        my $blocks_per_piece = int($_piece_length{refaddr $self} / (
                                       ($_piece_length{refaddr $self} < 2**14)
                                       ? $_piece_length{refaddr $self}
                                       : 2**14
                                   )
        );
        my $max_working_pieces
            = max(8, int(($slots * $unchoked_peers) / $blocks_per_piece) + 1);

        #warn sprintf q[$max_working_pieces: %d], $max_working_pieces;
        #
        if (scalar(grep { $_->{q[Slow]} == 0 }
                       values %{$working_pieces{refaddr $self}}
            ) >= $max_working_pieces
            )
        {    #warn sprintf q[%d>=%d],
                #    (scalar(keys %{$working_pieces{refaddr $self}})),
                #    $max_working_pieces;
            my @indexes
                = grep { $working_pieces{refaddr $self}{$_}{q[Slow]} == 0 }
                keys %{$working_pieces{refaddr $self}};

            #warn sprintf q[indexes: %s], (join q[, ], @indexes);
            for my $index (@indexes) {
                if (vec($relevence, $index, 1) == 1) {
                    if (($endgame
                         ? index($working_pieces{refaddr $self}{$index}
                                     {q[Blocks_Recieved]},
                                 0,
                                 0
                         )
                         : scalar grep { scalar keys %$_ }
                         @{  $working_pieces{refaddr $self}{$index}
                                 {q[Blocks_Requested]}
                         }
                        ) != -1
                        )
                    {   $piece = $working_pieces{refaddr $self}{$index};
                        last;
                    }
                }
            }
        }
        else {

     #warn sprintf q[%d<%d], (scalar(keys %{$working_pieces{refaddr $self}})),
     #    $max_working_pieces;
            my %weights;
            for my $i (0 .. ($self->_piece_count - 1))
            {    # XXX - Far from efficient...
                if (vec($relevence, $i, 1)) {
                    $weights{$i} = 1;
                }
            }
            return if not keys %weights;

            # [id://230661]
            my $total    = sum values %weights;
            my $rand_val = $total * rand;
            my $index;
            for my $i (reverse sort keys %weights) {
                $rand_val -= $weights{$i};
                if ($rand_val <= 0) { $index = $i; last; }
            }
            return if not defined $index;
            my $_piece_length = (    # XXX - save some time and store this?
                ($index == int(
                          $size{refaddr $self} / $_piece_length{refaddr $self}
                 )
                )
                ? ($size{refaddr $self} % $_piece_length{refaddr $self})
                : ($_piece_length{refaddr $self})
            );

            #
            my $block_length = (
                ($_piece_length{refaddr $self} < $_block_length{refaddr $self}
                )
                ? ($_piece_length{refaddr $self})
                : $_block_length{refaddr $self}
            );
            my $block_length_last
                = ($_piece_length{refaddr $self} % $_piece_length);

            #die $block_length_last;
            # XXX - may not be balanced
            my $block_count
                = (int($_piece_length / $block_length)
                       + ($block_length_last ? 1 : 0));

            #
            $piece = {Index             => $index,
                      Priority          => $weights{$index},
                      Blocks_Requested  => [map { {} } 1 .. $block_count],
                      Blocks_Recieved   => [map {0} 1 .. $block_count],
                      Block_Length      => $block_length,
                      Block_Length_Last => $block_length_last,
                      Block_Count       => $block_count,
                      Length            => $_piece_length,
                      Endgame           => $endgame,
                      Slow              => 0,
                      Touch             => 0
            };
        }

        #
        if ($piece) {
            if (not
                defined $working_pieces{refaddr $self}{$piece->{q[Index]}})
            {   $working_pieces{refaddr $self}{$piece->{q[Index]}} = $piece;
            }
        }

        #
        return $piece
            ? $working_pieces{refaddr $self}{$piece->{q[Index]}}
            : ();
    }

    sub _write_data {
        my ($self, $index, $offset, $data) = @_;
        return if not defined $_client{refaddr $self};
        return if ${$status{refaddr $self}} & 2;         # hashchecking

        # TODO: param validation
        if ((length($$data)
             + (($_piece_length{refaddr $self} * $index) + $offset)
            ) > $size{refaddr $self}
            )
        {   carp q[Too much data or bad offset data for this torrent];
            return;
        }

        #
        my $file_index = 0;
        my $total_offset = int(
                 (($index * $_piece_length{refaddr $self})) + ($offset || 0));

        #warn sprintf q[Write I:%d O:%d L:%d TOff:%d], $index, $offset,
        #    length($$data), $total_offset;
    SEARCH:
        while ($total_offset > $files{refaddr $self}->[$file_index]->size) {
            $total_offset -= $files{refaddr $self}->[$file_index]->size;
            $file_index++;
            last SEARCH    # XXX - should this simply return?
                if not defined $files{refaddr $self}->[$file_index]->size;
        }
    WRITE: while (length $$data > 0) {
            my $this_write
                = ($total_offset + length $$data
                   > $files{refaddr $self}->[$file_index]->size)
                ? $files{refaddr $self}->[$file_index]->size - $total_offset
                : length $$data;
            $files{refaddr $self}->[$file_index]->_open(q[w]) or return;
            $files{refaddr $self}->[$file_index]->_sysseek($total_offset);
            $files{refaddr $self}->[$file_index]
                ->_write(substr($$data, 0, $this_write, q[]))
                or return;
            $file_index++;
            last WRITE if not defined $files{refaddr $self}->[$file_index];
            $total_offset = 0;
        }

        #
        return 1;
    }

    sub _read_data {
        my ($self, $index, $offset, $length) = @_;

        #
        carp q[Bad index!]  if not defined $index  || $index !~ m[^\d+$];
        carp q[Bad offset!] if not defined $offset || $offset !~ m[^\d+$];
        carp q[Bad length!] if not defined $length || $length !~ m[^\d+$];

        #
        my $data = q[];
        if (($length + (($_piece_length{refaddr $self} * $index) + $offset))
            > $size{refaddr $self})
        {   carp q[Too much or bad offset data for this torrent];
            return;
        }

        #
        my $file_index = 0;
        my $total_offset = int(
                 (($index * $_piece_length{refaddr $self})) + ($offset || 0));

        #warn sprintf q[Read  I:%d O:%d L:%d TOff:%d], $index, $offset,
        #    $length, $total_offset;
    SEARCH:
        while ($total_offset > $files{refaddr $self}->[$file_index]->size) {
            $total_offset -= $files{refaddr $self}->[$file_index]->size;
            $file_index++;
            last SEARCH    # XXX - should this simply return?
                if not defined $files{refaddr $self}->[$file_index]->size;
        }
    READ: while ($length > 0) {
            my $this_read
                = (($total_offset + $length)
                   >= $files{refaddr $self}->[$file_index]->size)
                ? ($files{refaddr $self}->[$file_index]->size - $total_offset)
                : $length;

     #warn sprintf q[Reading %d (%d) bytes from '%s'],
     #    $this_read, $this_write, $files{refaddr $self}->[$file_index]->path;
            $files{refaddr $self}->[$file_index]->_open(q[r]) or return;
            $files{refaddr $self}->[$file_index]->_sysseek($total_offset);
            my $_data
                = $files{refaddr $self}->[$file_index]->_read($this_read);
            $data .= $_data if $_data;

            #
            $file_index++;
            $length -= $this_read;
            last READ if not defined $files{refaddr $self}->[$file_index];
            $total_offset = 0;
        }

        #
        return \$data;
    }

    sub hashcheck {
        my ($self) = @_;

        # (201=Started, 961=Force Download, 1000=finished)
        #     1 = Started
        #     2 = Checking
        #     4 = Start after check
        #     8 = Checked
        #    16 = Error
        #    32 = Paused
        #    64 = Queued
        #   128 = Loaded
        #   256 =
        #   512 = Force
        #  1000 = Complete
        ${$status{refaddr $self}} |= 2;

        # close peers
        for my $_peer (@{$self->_peers}) {
            $_peer->_disconnect(
                              q[Removing torrent from local client]);
        }
        for my $index (0 .. ($self->_piece_count - 1)) {
            $self->_check_piece_by_index($index);
        }
        ${$status{refaddr $self}} ^= 2;
        return 1;
    }

    sub _check_piece_by_index {
        my ($self, $index) = @_;

        #
        if (not defined $index) {
            carp q[Net::BitTorrent::Torrent->_check_piece_by_index( INDEX ) ]
                . q[requires an index.];
            return;
        }
        if ($index !~ m[^\d+$]) {
            carp q[Net::BitTorrent::Torrent->_check_piece_by_index( INDEX ) ]
                . q[requires an integer index.];
            return;
        }

        #
        if (defined $working_pieces{refaddr $self}{$index}) {
            delete $working_pieces{refaddr $self}{$index};

            #if (keys %{$working_pieces{refaddr $self}}) {
            #    warn q[Remaining working pieces: ]
            #        . pp $working_pieces{refaddr $self};
            #}
        }

        #
        my $data = $self->_read_data(
                     $index, 0,
                     ($index == ($self->_piece_count - 1)
                      ? ($size{refaddr $self} % $_piece_length{refaddr $self})
                      : $_piece_length{refaddr $self}
                     )
        );

#
#warn sprintf q[%s vs %s],
#     sha1_hex($data),
#     substr(unpack(q[H*], $_raw_data{refaddr $self}{q[info]}{q[pieces]}), $index * 40, 40);
        if ((not $data)
            or (sha1_hex($$data) ne substr(
                             unpack(
                                 q[H*],
                                 $_raw_data{refaddr $self}{q[info]}{q[pieces]}
                             ),
                             $index * 40,
                             40
                )
            )
            )
        {   vec(${$bitfield{refaddr $self}}, $index, 1) = 0;
            $_client{refaddr $self}->_event(q[piece_hash_fail],
                                          {Torrent => $self, Index => $index})
                if defined $_client{refaddr $self};
            return 0;
        }

        #
        if (vec(${$bitfield{refaddr $self}}, $index, 1) == 0)
        {    # Only if pass is 'new'
            vec(${$bitfield{refaddr $self}}, $index, 1) = 1;
            $_client{refaddr $self}->_event(q[piece_hash_pass],
                                          {Torrent => $self, Index => $index})
                if defined $_client{refaddr $self};
        }

        #
        return 1;
    }

    sub _as_string {
        my ($self, $advanced) = @_;
        my $dump
            = !$advanced
            ? $self->infohash
            : sprintf <<END,
Torrent: %s
 Path:       %s
 Storage:    %s
 Infohash:   %s
 Size:       %d bytes
 Status:     %d
 --
 Num Pieces: %d
 Piece Size: %d bytes
 Working:    %s
 --
 Files:      %s
 --
 Trackers:   %s
 --
 DHT:        %s

q[TODO]
END
            $_raw_data{refaddr $self}{q[info]}{q[name]},
            $self->path(),
            $basedir{refaddr $self},
            $self->infohash(),
            $self->size(),
            $self->status(),
            $self->_piece_count(),
            $_piece_length{refaddr $self},
            join(q[, ], (keys %{$working_pieces{refaddr $self}}) || q[N/A]),

# %_client, %path, %basedir,    # new() params (path is required)
#        %size,          %files,  %trackers,  %infohash,
#        %_private,      %pieces, %_uploaded, %_downloaded,
#        %_piece_length, %nodes,  %bitfield,  %working_pieces,
#        %_block_length,
#        %_raw_data,    # creation date, encoding, created by, etc.
#        %status        # (201=Started, 961=Force Download, 1000=finished)
#                       # 1   = Started
#                       # 2   = Checking
#                       # 4   = Start after check
#                       # 8   = Checked
#                       # 16  = Error
#                       # 32  = Paused
#                       # 64  = Queued
#                       # 128 = Loaded
#
#    sub trackers { return $trackers{refaddr +shift}; }
#    sub bitfield { return ${$bitfield{refaddr +shift}}; }
#    sub files    { return $files{refaddr +shift}; }
#    sub _client       { return $_client{refaddr +shift}; }
#    sub _uploaded     { return $_uploaded{refaddr +shift} || 0; }
#    sub _downloaded   { return $_downloaded{refaddr +shift} || 0; }
#    sub _block_length { return $_block_length{refaddr +shift} }
#    sub _raw_data     { return $_raw_data{refaddr +shift} }
#    sub _complete {
#        my ($self) = @_;
#        return if ${$status{refaddr $self}} & 2;    # hashchecking
#        return ((substr(unpack(q[b*], $self->_wanted), 0, $self->_piece_count)
#                     !~ 1
#                )
#                ? 1
#                : 0
#        );
#    }
#    sub _compact_nodes { return $nodes{refaddr +shift}; }
#    sub _wanted {
# Files
            (map { qq[\n\t] . $_->_as_string($advanced) }
             @{$files{refaddr $self}}),

            # Trackers
            (map { qq[\n\t] . $_->_as_string($advanced) }
             @{$trackers{refaddr $self}}),

            # DHT
            ($_private{refaddr $self}
             ? q[[Private - DHT/Peer EXchange disabled]]
             : q[]);
        return print STDERR qq[$dump\n] unless defined wantarray;
        return $dump;
    }

    sub CLONE {
        for my $_oID (keys %REGISTRY) {

            #  look under oID to find new, cloned reference
            my $_obj = $REGISTRY{$_oID};
            my $_nID = refaddr $_obj;

            #  relocate data
            for (@CONTENTS) {
                $_->{$_nID} = $_->{$_oID};
                delete $_->{$_oID};
            }
            weaken $_client{$_nID};

            #  update he weak refernce to the new, cloned object
            weaken($REGISTRY{$_nID} = $_obj);
            delete $REGISTRY{$_oID};
        }
        return 1;
    }

    # Destructor
    DESTROY {
        my ($self) = @_;
        for (@CONTENTS) {
            delete $_->{refaddr $self};
        }

        #warn q[Goodbye, ] . $$self;
        delete $REGISTRY{refaddr $self};

        #
        return 1;
    }
    1;
}

=pod

=head1 NAME

Net::BitTorrent::Torrent - Class Representing a Single .torrent File

=head1 Description

=head1 Constructor

=over

=item C<new ( { [ARGS] } )>

Creates a C<Net::BitTorrent::Torrent> object.  This constructor is
called by
L<Net::BitTorrent::add_torrent( )|Net::BitTorrent/add_torrent ( { ... } )>
and should not be used directly.

C<new( )> accepts arguments as a hash, using key-value pairs:

=over

=item C<BaseDir>

The root directory used to store the files related to this torrent.  This
directory is created if not preexisting.

This is an optional parameter.

Default: C<./> (Current working directory)

=item C<Client>

The L<Net::BitTorrent|Net::BitTorrent> object this torrent will
eventually be served from.

This is an optional parameter.

No default.  Without a defined parent client, his object is very limited
in capability.  Basic informaion and <hash checking|/hashcheck> only.

=item C<Path>

Filename of the .torrent file to load.

This is the only required parameter.

=back

=back

=head1 Methods

=over

=item C<bitfield>

Returns a bitfield representing the pieces that have been successfully
downloaded.

=item C<files>

Returns a list of
L<Net::BitTorrent::Torrent::File|Net::BitTorrent::Torrent::File> objects
representing all files contained in the related .torrent file.

=item C<hashcheck>

Verifies the integrity of all L<files|Net::BitTorrent::Torrent::File>
associated with this torrent.

This is a blocking method; all processing will stop until this function
returns.

=item C<infohash>

Returns the 20 byte SHA1 hash used to identify this torrent internally,
with trackers, and with remote peers.

=item C<path>

Returns the L<filename|/Path> of the torrent this object represents.

=item C<size>

Returns the total size of all files listed in the .torrent file.

=item C<trackers>

Returns a list of all
L<Net::BitTorrent::Torrent::Tracker|Net::BitTorrent::Torrent::Tracker>
objects related to the torrent.

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
