package Net::BitTorrent::Session;
use strict;
use warnings;
{

    BEGIN {
        use version qw[qv];
        our $SVN
            = q[$Id$];
        our $VERSION = sprintf q[%.3f], version->new(qw$Rev$)->numify / 1000;
    }
    use Digest::SHA qw[];
    use File::Spec qw[];
    use Fcntl qw[O_RDONLY];
    use lib q[../../../lib];
    use Net::BitTorrent::Util qw[:bencode :compact sum :log];
    use Net::BitTorrent::Session::Piece;
    use Net::BitTorrent::Session::File;
    use Net::BitTorrent::Session::Tracker;
    {
        my (%path,     %base_dir,   %private,    %piece_count,
            %infohash, %block_size, %piece_size, %total_size,
            %uploaded, %downloaded, %trackers,   %client,
            %files,    %pieces,     %nodes,      %endgame,
            %name
        );

        sub new {
            my ($class, $args) = @_;
            my $self = undef;
            if (defined $args->{q[client]}) {
                if (not defined $args->{q[path]}) {
                    $args->{q[client]}->_do_callback(q[log], WARN,
                          q[Cannot create session; missing 'path' parameter]);
                }
                elsif (not -e $args->{q[path]}) {
                    $args->{q[client]}->_do_callback(q[log], WARN,
                          sprintf(q[File not found: '%s'], $args->{q[path]}));
                }
                elsif (not -f $args->{q[path]}) {
                    $args->{q[client]}->_do_callback(q[log], WARN,
                            sprintf(q['%s' is not a file], $args->{q[path]}));
                }
                else {
                    my $sysopen
                        = sysopen(my ($FH), $args->{q[path]}, O_RDONLY);
                    if (not $sysopen or not defined $FH) {
                        $args->{q[client]}->callback_log(
                                               {message =>
                                                    sprintf(
                                                       q[Can't open '%s': %s],
                                                       $args->{q[path]}, $^E
                                                    )
                                               }
                        );
                        return;
                    }
                    sysread($FH, my ($_data), ((stat($FH))[7]))
                        == ((stat($FH))[7])
                        or return;    # error
                    close $FH;
                    my $_content = bdecode($_data);

                    #use Data::Dump qw[pp];warn pp $_content;
                    my $infohash = Digest::SHA::sha1_hex(
                                               bencode($_content->{q[info]}));
                    if ($infohash !~ m[^([0-9a-f]{40})$]) {
                        $client{$self}->_do_callback(q[log], ERROR,
                                                     q[Improper info_hash]);
                        return;
                    }
                    elsif (length(
                                unpack(q[H*], $_content->{q[info]}{q[pieces]})
                           ) < 40
                        )
                    {   $client{$self}
                            ->_do_callback(q[log], ERROR, q[Broken torrent]);
                        return;
                    }
                    $self = bless \$infohash, $class;
                    $client{$self}   = $args->{q[client]};
                    $infohash{$self} = $infohash;
                    $path{$self}     = $args->{q[path]};
                    $nodes{$self}    = q[];
                    if ($_content->{q[info]}{q[files]}) {
                        my $_size = 0;
                        for my $_f (@{$_content->{q[info]}{q[files]}}) {
                            $_size += $_f->{q[length]};
                        }
                        $total_size{$self} = $_size;
                    }
                    else {
                        $total_size{$self} = $_content->{q[info]}{q[length]};
                    }
                    $base_dir{$self}
                        = defined $args->{q[base_dir]}
                        ? $args->{q[base_dir]}
                        : q[./];
                    $piece_count{$self}
                        = int(
                            length(
                                unpack(q[H*], $_content->{q[info]}{q[pieces]})
                                ) / 40
                        ) - 1;
                    $piece_size{$self}
                        = $_content->{q[info]}{q[piece length]};
                    $private{$self}
                        = $_content->{q[info]}{q[private]} ? 1 : 0;
                    $name{$self} = (   $_content->{q[info]}{q[name.utf-8]}
                                    || $_content->{q[info]}{q[name]});
                    my $x = 0;
                    $pieces{$self} = [
                        map {
                            Net::BitTorrent::Session::Piece->new(
                                                          {hash    => $_,
                                                           index   => $x++,
                                                           session => $self
                                                          }
                                )
                            } (
                             $_content->{q[info]}{q[pieces]} =~ m[\G(.{20})]gs
                            )
                    ];

                    # defaults
                    $client{$self}->_set_pulse($self, time + 5);
                    $downloaded{$self} = 0;
                    $uploaded{$self}   = 0;
                    $block_size{$self} = (
                                   (        (defined $args->{q[block_length]})
                                        and ($args->{q[block_length]} > 2**15)
                                   )
                                   ? $args->{q[block_length]}
                                   : 2**14
                    );
                    if (defined($_content->{q[nodes]})
                        and ref $_content->{q[nodes]} eq q[ARRAY])
                    {   for my $node (@{$_content->{q[nodes]}}) {
                            $client{$self}
                                ->get_dht->add_node(join q[:], @$node);
                        }
                    }
                    if (defined($_content->{q[announce-list]})
                        and ref $_content->{q[announce-list]} eq q[ARRAY])
                    {   $trackers{$self} = [
                            map {
                                Net::BitTorrent::Session::Tracker->new(
                                               {session => $self, urls => $_})
                                } @{$_content->{q[announce-list]}}
                        ];
                    }
                    elsif (defined $_content->{q[announce]}) {
                        $trackers{$self} = [
                                   Net::BitTorrent::Session::Tracker->new(
                                       {urls    => [$_content->{q[announce]}],
                                        session => $self
                                       }
                                   )
                        ];
                    }
                    else {    # This seems to be a trackerless torrent
                              # but we support DHT so it should work out.
                              # ...unless the torrent is also private.
                        $trackers{$self} = [];
                    }
                    my $i = 0;
                    $files{$self} = exists $_content->{q[info]}{q[length]}
                        ? (
                        [Net::BitTorrent::Session::File->new(
                             {   size => $_content->{q[info]}{q[length]},
                                 path => File::Spec
                                     ->catfile(    # XXX - not happy with this
                                     $self->get_base_dir,
                                     $_content->{q[info]}{q[name.utf-8]}
                                         || $_content->{q[info]}{q[name]}
                                     ),
                                 session => $self,
                                 index   => $i++
                             }
                         )
                        ]
                        )
                        : (
                        [map {
                             Net::BitTorrent::Session::File->new(
                                 {   size => $_->{q[length]},
                                     path => File::Spec
                                         ->catfile( # XXX - not happy with this
                                         $self->get_base_dir,
                                         ($_content->{q[info]}{q[name.utf-8]}
                                              || $_content->{q[info]}{q[name]}
                                         ),
                                         @{         $_->{q[path.utf-8]}
                                                 || $_->{q[path]}
                                             }
                                         ),
                                     session => $self,
                                     index   => $i++
                                 }
                                 )
                             } @{$_content->{q[info]}{q[files]}}
                        ]
                        );
                    if ($files{$self} eq []) {
                        $client{$self}->_do_callback(q[log], ERROR,
                             q[Broken torrent: No files contained in .torrent]
                        );
                        return;
                    }
                }
            }
            return $self;
        }
        sub get_path { return $path{$_[0]}; }
        sub get_name { return $name{$_[0]}; }

        sub get_base_dir {
            return File::Spec->rel2abs($base_dir{$_[0]});
        }
        sub get_private         { return $private{$_[0]}; }
        sub get_infohash        { return $infohash{$_[0]}; }
        sub get_client          { return $client{$_[0]}; }
        sub get_pieces          { return $pieces{$_[0]}; }
        sub get_trackers        { return $trackers{$_[0]}; }
        sub get_files           { return $files{$_[0]}; }
        sub get_piece_count     { return $piece_count{$_[0]}; }
        sub get_piece_size      { return $piece_size{$_[0]}; }
        sub get_total_size      { return $total_size{$_[0]}; }
        sub _get_endgame_status { return $endgame{$_[0]}; }

        sub add_tracker {    # should be add_tracker_tier
            my ($self, @urls) = @_;
            return push @{$trackers{$self}},
                Net::BitTorrent::Session::Tracker->new(
                                           {urls => @urls, session => $self});
        }

        sub hash_check {
            return
                scalar grep { $_->get_verified_integrity } @{$pieces{$_[0]}};
        }

        sub get_block_size {
            my ($self) = @_;
            return $block_size{$self};
        }

        sub set_block_size {
            my ($self, $value) = @_;
            $self->get_client->_do_callback(q[log], WARN,
                                            q[block_size is malformed])
                and return
                unless $value =~ m[^\d+$]
                    and $value
                    < ($client{$self}->get_max_buffer_per_conn + 12);
            return $block_size{$self} = $value;
        }

        sub _inc_uploaded {
            my ($self, $value) = @_;
            $client{$self}
                ->_do_callback(q[log], FATAL, q[uploaded is protected])
                and die
                unless caller->isa(q[Net::BitTorrent::Session::Peer]);
            return $uploaded{$self} += $value;
        }
        sub get_uploaded { return $uploaded{$_[0]}; }

        sub _inc_downloaded {
            my ($self, $value) = @_;
            $client{$self}
                ->_do_callback(q[log], FATAL, q[downloaded is protected])
                and die
                unless caller->isa(q[Net::BitTorrent::Session::Peer]);
            return $downloaded{$self} += $value;
        }
        sub get_downloaded { return $downloaded{$_[0]}; }

        sub append_nodes {
            my ($self, $new_nodes) = @_;
            $client{$self}->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            return $nodes{$self}
                = compact(uncompact($nodes{$self}), uncompact($new_nodes));
        }
        sub get_nodes         { return uncompact($nodes{$_[0]}); }
        sub get_compact_nodes { return $nodes{$_[0]}; }

        sub _pulse {
            my ($self) = @_;
            $client{$self}->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));

    # TODO: review the following block ===================================
    #for my $file (@{$files{$self}}) {
    #$file->close if ((time - $file->get_touch_timestamp) > 600);
    #}
    #if (not $self->get_complete) {
    #grep {
    #	$_->_disconnect(q[We're in pull mode and this peer has nothing for us.])
    #	not $_->get_is_interesting
    #} @peers;
    # Remove peers we're not interested it.  Evil, but...
    # well, survival is key. We can seed later.  Right?  Right.
    #}
    # TODO: review the above block =======================================
    # TODO: remember TCP/IP holds old connections ~120s and you'll
    #      get caught with too many open sockets if they pile up
            $self->_connect_to_new_peers();
            $client{$self}->_set_pulse($self, time + 3);
        }

        sub _connect_to_new_peers {
            my ($self)    = @_;
            my @peers     = $self->get_peers;
            my $new_peers = 0;
            if ($self->get_nodes
                and
                (scalar(@peers) < $self->get_client->get_conns_per_session)
                and (
                    scalar(
                        grep {
                            $_->isa(q[Net::BitTorrent::Session::Peer])
                                and not $_->_get_connected
                            } values %{$self->get_client->_get_connections}
                    ) < $self->get_client->get_max_halfopen
                )
                )
            {   my @nodes = $self->get_nodes;
                while (
                     scalar(@nodes)
                     and (
                         scalar(grep { not $_->get_peer_id } $self->get_peers)
                         < $self->get_client->get_max_halfopen)
                    )
                {   my $new_peer =
                        Net::BitTorrent::Session::Peer->new(
                                                 {address => shift(@nodes),
                                                  session => $self
                                                 }
                        );
                    if ($new_peer) {
                        $self->get_client->_add_connection($new_peer);
                        $new_peers++;
                    }
                }
            }
            return $new_peers;
        }

        sub _check_endgame_status {
            my ($self) = @_;
            $client{$self}->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            return unless $downloaded{$self};
            if ((scalar(
                     grep {
                         not $_->get_cached_integrity
                             and $_->get_priority
                         } @{$pieces{$self}}
                 )
                ) <= 10    # TODO: make this number a variable
                )
            {   $endgame{$self} = 1;
            }
            else { $endgame{$self} = 0 }
            return $endgame{$self};
        }

        sub _pick_piece {
            my ($self, $peer) = @_;
            $client{$self}->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            return if $self->get_complete;
            return if not defined $peer->get_bitfield;
            my $piece = undef;

            # TODO: needs work...
            my $free_blocks = 0;
            my $free_slots  = 0;
            for (grep { $_->get_working } @{$pieces{$self}}) {
                $free_blocks += scalar grep { scalar $_->get_peers == 0 }
                    values %{$_->get_blocks};
            }
            for ($self->get_peers) {
                $free_slots += (
                    8    # XXX - This value MAY be set via DHT/ext.prot
                        - scalar(@{$_->get_outgoing_requests})
                ) if not $_->get_is_choking;
            }
            my $max_working = (($free_blocks < $free_slots)
                      + (scalar(grep { $_->get_working } @{$pieces{$self}})));
            my @weights = (
                (scalar(grep { $_->get_working } @{$pieces{$self}})
                     < $max_working    # TODO: Make this ratio variable
                )
                ? (grep {
                               not $_->get_cached_integrity
                           and $_->get_priority
                           and vec($peer->get_bitfield, $_->get_index, 1)
                           and ($_->get_working
                                ? (scalar grep { scalar $_->get_peers == 0 }
                                   values %{$_->get_blocks})
                                : 1
                           )
                       } @{$pieces{$self}}
                    )
                : (grep {
                       $_->get_working    # and $pieces{$self}->[$_->index]
                           and (scalar grep { scalar $_->get_peers == 0 }
                                values %{$_->get_blocks})
                       } @{$pieces{$self}}
                )
            );
            return if not @weights;

            # [id://230661]
            my $total    = sum map { $_->get_priority } @weights;
            my $rand_val = $total * rand;
            my $i        = -1;
            while ($rand_val > 0) {
                $rand_val -= $weights[++$i]->get_priority;
            }
            $piece = $weights[$i];
            if ($piece) {
                $piece->set_working(1);
                return $piece;
            }
            return;
        }

        sub get_peers {
            my ($self) = @_;
            $client{$self}->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            return (
                grep {
                            ref $_ eq q[Net::BitTorrent::Session::Peer]
                        and defined $_->get_session
                        and $_->get_session eq $self
                    } values %{$self->get_client->_get_connections}
            );
        }

        sub get_complete {
            my ($self) = @_;
            $client{$self}->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            for my $piece (@{$pieces{$self}}) {
                if (not $piece->get_cached_integrity and $piece->get_priority)
                {   return 0;
                }
            }
            return 1;
        }

        sub get_bitfield {
            my ($self) = @_;
            $client{$self}->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            return pack q[B*], join q[],    # dec bit order
                map { $_->get_cached_integrity ? 1 : 0 } @{$pieces{$self}};
        }

        sub close_files {
            my ($self) = @_;
            $client{$self}->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
            my $return = 0;
            for my $file (@{$files{$self}}) {
                $return++ if $file->_close;
            }
            return $return;
        }
        DESTROY {
            my ($self) = @_;
            $client{$self}->_del_pulse($self)
                if defined $client{$self};
            delete $path{$self};
            delete $base_dir{$self};
            delete $private{$self};
            delete $infohash{$self};
            delete $piece_count{$self};
            delete $piece_size{$self};
            delete $block_size{$self};
            delete $total_size{$self};
            delete $uploaded{$self};
            delete $downloaded{$self};
            delete $trackers{$self};
            delete $client{$self};
            delete $files{$self};
            delete $pieces{$self};
            delete $endgame{$self};
            delete $nodes{$self};
            delete $name{$self};
            return 1;
        }

        sub as_string {
            my ($self, $advanced) = @_;
            my %hash = (private  => $private{$self},
                        trackers => $trackers{$self},
                        files    => $files{$self},
                        ($advanced ? (pieces => $pieces{$self}) : ()),
                        nodes => $nodes{$self}
            );
            my @values = ($infohash{$self},
                          $path{$self},
                          $base_dir{$self},
                          $total_size{$self},
                          ((((scalar grep { $_->get_cached_integrity }
                                  @{$pieces{$self}}
                             ) / (scalar @{$pieces{$self}})
                            )
                           ) * 100
                          ),
                          $uploaded{$self},
                          $downloaded{$self}
            );
            s/(^[-+]?\d+?(?=(?>(?:\d{3})+)(?!\d))|\G\d{3}(?=\d))/$1,/g
                for @values[3, 5, 6];
            my $dump = sprintf( <<'END', @values);
Net::BitTorrent::Session (%40s)
===================================================================
Basic Information
  Path:             %s
  Base directory:   %s
  Total size:       %s bytes
  Percent complete: %3.2f%%
  Uploaded:         %s bytes
  Downloaded:       %s bytes
END
            if ($advanced) {
                {
                    my @adv_values_file_info = (scalar(@{$files{$self}}),
                                                (scalar(@{$files{$self}}) == 1
                                                 ? q[]
                                                 : q[s]
                                                )
                    );
                    $dump .= sprintf( <<'END', @adv_values_file_info);

Advanced Information:
  File Information:
    File list: (%d file%s)
END
                    $dump .= join qq[\n], map {
                        my $file = $_->as_string($advanced);
                        $file =~ s|\n|\n    |g;
                        q[ ] x 4 . $file
                    } @{$files{$self}};
                }
                {
                    my @adv_values_piece_info = (
                        $piece_count{$self},
                        $piece_size{$self},

                        #$block_size{$self},
                        scalar(grep { $_->get_working } @{$pieces{$self}}),
                        join(q[, ],
                             map      { $_->get_index }
                                 grep { $_->get_working } @{$pieces{$self}})
                    );
                    s/(^[-+]?\d+?(?=(?>(?:\d{3})+)(?!\d))|\G\d{3}(?=\d))/$1,/g
                        for @adv_values_piece_info[0 .. 2];
                    $dump .= sprintf( <<'END', @adv_values_piece_info);

  Piece Information
    Piece count:    %s
    Piece size:     %s bytes
    Working pieces: %d
      %s

END
                }
                {
                    my @adv_values_tracker_info = (
                        scalar(@{$trackers{$self}}),

                    #(join(q[, ], map { $_->as_string } @{$trackers{$self}})),
                        (join(
                             qq[\n],
                             map {
                                 my $tracker = $_->as_string($advanced);
                                 $tracker =~ s|\n|\n      |g;
                                 q[ ] x 6 . $tracker
                                 } @{$trackers{$self}}
                         )
                        )
                    );
                    s/(^[-+]?\d+?(?=(?>(?:\d{3})+)(?!\d))|\G\d{3}(?=\d))/$1,/g
                        for @adv_values_tracker_info[0,];
                    $dump .= sprintf( <<'END', @adv_values_tracker_info);

  Tracker Information:
    Tiers: %d
    Tier list:
%s
END
                }
                {
                    my @peers = $self->get_peers;
                    my @adv_values_peer_info = (
                        scalar(@peers),

                    #(join(q[, ], map { $_->as_string } @{$trackers{$self}})),
                        (join(
                             qq[\n],
                             map {
                                 my $peer = $_->as_string($advanced);
                                 $peer =~ s|\n|\n      |g;
                                 q[ ] x 6 . $peer
                                 } @peers
                         )
                        )
                    );
                    s/(^[-+]?\d+?(?=(?>(?:\d{3})+)(?!\d))|\G\d{3}(?=\d))/$1,/g
                        for @adv_values_peer_info[0,];
                    $dump .= sprintf( <<'END', @adv_values_peer_info);

  Peer Information:
    Peers: %d
    Peer list:
%s
END
                }
            }
            return print STDERR qq[$dump\n]
                unless defined wantarray;
            return $dump;
        }
    }
}
1;
__END__

=pod

=head1 NAME

Net::BitTorrent::Session - Class Representing a Single .torrent File

=head1 Constructor

=over 4

=item C<new ( { [ARGS] } )>

Creates a C<Net::BitTorrent::Session> object.  This constructor is
called by
L<Net::BitTorrent::add_session()|Net::BitTorrent/add_session ( { ... } )>
and should not be used directly.  C<new( )> accepts arguments as a
hash, using key-value pairs:

=over 4

=item C<path>

Filename of the .torrent file to load.

This is the only required parameter.

=item C<base_dir>

Base directory used to store the files related to this session.  This
directory is created if not preexisting.

Default: C<./> (Current working directory)

=item C<block_length>

Length of blocks we request from peers of this session.  This should
not be changed as it can greatly affect performance.

Default: 32768 (C<2**15>)

=back

=back

=head1 Methods

Unless stated, all methods return either a C<true> or C<false> value,
with C<true> meaning that the operation was a success.  When a method
states that it returns a value, failure will result in C<undef> or an
empty list.

=over 4

=item C<add_tracker ( URLS )>

Add a new
L<Net::BitTorrent::Session::Tracker|Net::BitTorrent::Session::Tracker>
tier to the session.  Accepts a list of URLs.

See also: L<get_trackers ( )|/get_trackers ( )>

=item C<get_base_dir>

Returns the L<base directory|/"base_dir"> used by the files contained in
this session.

=item C<get_path ( )>

Returns the L<filename|/"path"> of the torrent this object represents.

=item C<append_nodes ( STRING )>

Adds a string of compacted nodes to the list of potential peers.

I<This method may be renamed in a future version.>

See also:
L<compact_nodes ( )|/"compact_nodes ( )">, L<nodes ( )|/"nodes ( )">,
L<Net::BitTorrent::Util::compact( )|Net::BitTorrent::Util/"compact ( LIST )">

=item C<as_string ( [ VERBOSE ] )>

Returns a 'ready to print' dump of the C<Net::BitTorrent::Session>
object's data structure.  If called in void context, the structure is
printed to C<STDERR>.

See also:
L<Net::BitTorrent|Net::BitTorrent/"as_string ( [ VERBOSE ] )">

=item C<get_bitfield ( )>

Returns a bitfield representing the pieces that have been successfully
downloaded.

=item C<get_block_size ( )>

Get the size used when requesting data from peers.

See also: L<set_block_size ( )|/"set_block_size ( NEWVAL )">

=item C<set_block_size ( NEWVAL )>

Set the size used when requesting data from peers.  Use with care.

See also:
L<Net::BitTorrent::set_max_buffer_per_conn( )|Net::BitTorrent/set_max_buffer_per_conn ( NEWVAL )>,
theory.org (http://tinyurl.com/32k7wu), discussion
(http://tinyurl.com/4ekea2)

=item C<get_client ( )>

Returns the L<Net::BitTorrent|Net::BitTorrent> object related to this
session.

=item C<close_files ( )>

Forces the closure of all related
L<Net::BitTorrent::Session::File|Net::BitTorrent::Session::File>
objects with open file handles.

Under normal circumstances, this should not be called in clients.

=item C<get_compact_nodes ( )>

Returns a list of potential peers as a compacted string.

See also:
L<nodes ( )|/nodes ( )>,
L<Net::BitTorrent::Util::compact( )|Net::BitTorrent::Util/compact ( LIST )>
L<Net::BitTorrent::Util::uncompact( )|Net::BitTorrent::Util/uncompact ( STRING )>

=item C<get_complete ( )>

Returns a boolean value indicating whether or not we have finished
downloading all pieces we want.

See also:
L<Net::BitTorrent::Session::Piece|Net::BitTorrent::Session::Piece>

=item C<get_downloaded ( )>

Returns the total amount of data downloaded during the current
session.

See also: L<get_uploaded ( )|/get_uploaded ( )>

=item C<get_files ( )>

Returns a list of
L<Net::BitTorrent::Session::File|Net::BitTorrent::Session::File>
objects representing all files contained in the related .torrent file.

=item C<hash_check ( )>

Verifies the integrity of all files associated with this session.

This is a blocking method; all processing will stop until this
function returns.

See also:
L<Net::BitTorrent::Session::Piece|Net::BitTorrent::Session::Piece/"get_verified_integrity ( )">

=item C<get_infohash ( )>

Returns the 20 byte SHA1 hash used to identify this session
internally, with trackers, and with remote peers.

=item C<get_name ( )>

For multi-file F<.torrent>s, returns the filename of the directory in
which to store all the files according to the F<.torrent>'s metadata.
For single file F<.torrent>s, returns the filename of the file according
to the .torrent's metadata.

=item C<get_nodes ( )>

Returns a list of potential peers.

To receive a compacted list of nodes (to use in a quick resume system,
for example), see L<compact_nodes( )|/compact_nodes ( )>.

=item C<get_peers ( )>

Returns a list of all related
L<Net::BitTorrent::Session::Peer|Net::BitTorrent::Session::Peer>
objects.

=item C<get_piece_count ( )>

Returns the number of
L<Net::BitTorrent::Session::Piece|Net::BitTorrent::Session::Piece>
objects related to this session.

=item C<get_piece_size ( )>

Returns the piece size defined in the .torrent file.

=item C<get_pieces ( )>

Returns a list of
L<Net::BitTorrent::Session::Piece|Net::BitTorrent::Session::Piece>
objects.

=item C<get_private ( )>

Returns a bool value indicating whether or not this session is allowed
to use DHT and other Peer Exchange protocols.

=item C<get_total_size ( )>

Returns the total size of all files listed in the .torrent file.

=item C<get_trackers ( )>

Returns a list of all
L<Net::BitTorrent::Session::Tracker|Net::BitTorrent::Session::Tracker>
objects related to the session.

See also: L<add_tracker ( )|/add_tracker ( URLS )>

=item C<get_uploaded ( )>

Returns the total amount of data uploaded during the current session.

See also: L<get_downloaded ( )|/get_downloaded ( )>

=back

=head1 AUTHOR

Sanko Robinson <sanko@cpan.org> - http://sankorobinson.com/

CPAN ID: SANKO

=head1 License and Legal

Copyright 2008 by Sanko Robinson E<lt>sanko@cpan.orgE<gt>

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl 5.10 (or higher).  See
http://www.perl.com/perl/misc/Artistic.html or the F<LICENSE> file
included with this distribution.

All POD documentation is covered by the Creative Commons Attribution-
Noncommercial-Share Alike 3.0 License
(http://creativecommons.org/licenses/by-nc-sa/3.0/us/).

Neither this module nor the L<Author|/Author> is affiliated with
BitTorrent, Inc.

=for svn $Id$

=cut
