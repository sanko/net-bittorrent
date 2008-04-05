package Net::BitTorrent::Session;
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
    use Digest::SHA qw[];
    use File::Spec qw[];
    use Carp qw[carp];
    use Net::BitTorrent::Util
        qw[bdecode bencode uncompact compact sum];
    use Net::BitTorrent::Session::Piece;
    use Net::BitTorrent::Session::File;
    use Net::BitTorrent::Session::Tracker;
    {
        my ( %next_pulse,  %path,     %base_dir,   %private,
             %piece_count, %infohash, %block_size, %piece_size,
             %total_size,  %uploaded, %downloaded, %trackers,
             %client,      %files,    %pieces,     %nodes,
             %endgame
        );

        sub new {
            my ( $class, $args ) = @_;
            my $self = undef;
            if ( defined $args->{q[client]} ) {
                if ( not defined $args->{q[path]} ) {
                    $args->{q[client]}->_do_callback( q[log],
                        q[Cannot create session; missing 'path' parameter]
                    );
                }
                elsif ( not -e $args->{q[path]} ) {
                    $args->{q[client]}->_do_callback(
                                     q[log],
                                     sprintf( q[File not found: '%s'],
                                              $args->{q[path]} )
                    );
                }
                elsif ( not -f $args->{q[path]} ) {
                    $args->{q[client]}->_do_callback(
                                       q[log],
                                       sprintf( q['%s' is not a file],
                                                $args->{q[path]} )
                    );
                }
                else {
                    my $sysopen # I don't care about Macs. see [perldoc://sysopen]
                        = sysopen( my ($FH), $args->{q[path]}, 0 );
                    if ( not $sysopen or not defined $FH ) {
                        $args->{q[client]}->callback_log(
                                        { message =>
                                              sprintf(
                                               q[Can't open '%s': %s],
                                               $args->{q[path]}, $^E
                                              )
                                        }
                        );
                        return;
                    }
                    sysread( $FH, my ($_data), ( ( stat($FH) )[7] ) )
                        == ( ( stat($FH) )[7] )
                        or return;    # error
                    close $FH;
                    my ($_content) = bdecode($_data);

                    my $infohash = Digest::SHA::sha1_hex(
                                    bencode( $_content->{q[info]} ) );

                    if ( $infohash !~ m[^([0-9a-f]{40})$] ) {
                        carp(q[Improper info_hash]);
                        return;
                    }
                    elsif (
                        length(
                            unpack(
                                q[H*], $_content->{q[info]}{q[pieces]}
                            )
                        ) < 40
                        )
                    {
                        carp(q[Broken torrent]);
                        return;
                    }
                    $self = bless \$infohash, $class;
                    $client{$self}   = $args->{q[client]};
                    $infohash{$self} = $infohash;
                    $path{$self}     = $args->{q[path]};
                    $nodes{$self}    = q[];
                    if ( $_content->{q[info]}{q[files]} ) {
                        my $_size = 0;
                        for my $_f (
                                 @{ $_content->{q[info]}{q[files]} } )
                        {
                            $_size += $_f->{q[length]};
                        }
                        $total_size{$self} = $_size;
                    }
                    else {
                        $total_size{$self}
                            = $_content->{q[info]}{q[length]};
                    }
                    $base_dir{$self}
                        = defined $args->{q[base_dir]}
                        ? $args->{q[base_dir]}
                        : q[./];
                    $piece_count{$self}
                        = int(
                        length(
                            unpack(
                                q[H*], $_content->{q[info]}{q[pieces]}
                            )
                            ) / 40
                        ) - 1;
                    $piece_size{$self}
                        = $_content->{q[info]}{q[piece length]};
                    $private{$self} = $_content->{q[private]} ? 1 : 0;
                    my $x = 0;
                    $pieces{$self} = [
                        map {
                            Net::BitTorrent::Session::Piece->new(
                                                   { hash    => $_,
                                                     index   => $x++,
                                                     session => $self
                                                   }
                                )
                            } ( $_content->{q[info]}{q[pieces]}
                                    =~ m[\G(.{20})]gs
                            )
                    ];

                    # defaults
                    $next_pulse{$self} = time + 5;
                    $downloaded{$self} = 0;
                    $uploaded{$self}   = 0;
                    $block_size{$self} = (
                           ( ( defined $args->{q[block_length]} )
                                 and
                                 ( $args->{q[block_length]} > 2**15 )
                           )
                           ? $args->{q[block_length]}
                           : 2**14
                    );
                    if ( defined( $_content->{q[nodes]} )
                         and ref $_content->{q[nodes]} eq q[ARRAY] )
                    {
                        $nodes{$self}
                            = compact( map { join q[:], @$_ }
                                       @{ $_content->{q[nodes]} } );
                    }
                    if ( defined( $_content->{q[announce-list]} )
                         and ref $_content->{q[announce-list]} eq
                         q[ARRAY] )
                    {
                        $trackers{$self} = [
                            map {
                                Net::BitTorrent::Session::Tracker
                                    ->new(
                                    { session => $self, urls => $_ } )
                                } @{ $_content->{q[announce-list]} }
                        ];
                    }
                    elsif ( defined $_content->{q[announce]} ) {
                        $trackers{$self} = [
                              Net::BitTorrent::Session::Tracker->new(
                                  { urls =>
                                        [ $_content->{q[announce]} ],
                                    session => $self
                                  }
                              )
                        ];
                    }
                    else {    # This seems to be a trackerless torrent
                              # and we don't support DHT yet.
                        $trackers{$self} = [];
                    }
                    {
                        my $_index = 0;
                        $files{$self}
                            = exists $_content->{q[info]}{q[length]}
                            ? ([ Net::BitTorrent::Session::File->new(
                                       { size => $_content->{q[info]}
                                             {q[length]},
                                         path =>
                                             File::Spec->catfile(
                                                  $_content->{q[info]}
                                                      {q[name]}
                                             ),
                                         session => $self,
                                         index   => $_index++
                                       }
                                 )
                               ]
                            )
                            : (
                            [  map {
                                   Net::BitTorrent::Session::File
                                       ->new(
                                        { size => $_->{q[length]},
                                          path =>
                                              File::Spec->catfile(
                                               ( $_content->{q[info]}
                                                     {q[name]}
                                               ),
                                               @{ $_->{q[path]} }
                                              ),
                                          session => $self,
                                          index   => $_index++
                                        }
                                       )
                                   } @{ $_content->{q[info]}
                                       {q[files]} }
                            ]
                            );
                    }
                }
            }
            return $self;
        }

        sub hash_check {
            my ($self) = @_;
            for my $piece ( @{ $pieces{$self} } ) { $piece->verify }
            return 1;
        }

        sub _next_pulse {
            my ($self) = @_;
            return $next_pulse{$self};
        }
        sub path { my ($self) = @_; return $path{$self}; }

        sub base_dir {
            my ($self) = @_;
            return File::Spec->rel2abs( $base_dir{$self} );
        }
        sub private  { my ($self) = @_; return $private{$self}; }
        sub infohash { my ($self) = @_; return $infohash{$self}; }
        sub client   { my ($self) = @_; return $client{$self}; }
        sub pieces   { my ($self) = @_; return $pieces{$self}; }
        sub trackers { my ($self) = @_; return $trackers{$self}; }

        sub add_tracker {    # should be add_tracker_tier
            my ( $self, @urls ) = @_;
            return push @{ $trackers{$self} },
                Net::BitTorrent::Session::Tracker->new(
                                { urls => @urls, session => $self } );
        }
        sub files { my ($self) = @_; return $files{$self}; }

        sub piece_count {
            my ($self) = @_;
            return $piece_count{$self};
        }

        sub piece_size {
            my ($self) = @_;
            return $piece_size{$self};
        }

        sub block_size {
            my ( $self, $value ) = @_;
            return (
                defined $value
                ? do {
                    $self->client->_do_callback( q[on_log],
                                          q[block_size is malformed] )
                        and return
                        unless $value =~ m[^\d+$]
                            and $value < (
                                   $client{$self}->maximum_buffer_size
                                       + 12
                            );
                    $block_size{$self} = $value;
                    }
                : $block_size{$self}
            );
        }

        sub total_size {
            my ($self) = @_;
            return $total_size{$self};
        }
        sub _endgame { my ($self) = @_; return $endgame{$self}; }

        sub _inc_uploaded {
            my ( $self, $value ) = @_;
            carp q[uploaded is protected]
                unless caller->isa(q[Net::BitTorrent::Session::Peer]);
            return $uploaded{$self} += $value;
        }
        sub uploaded { my ($self) = @_; return $uploaded{$self}; }

        sub _inc_downloaded {
            my ( $self, $value ) = @_;
            carp q[downloaded is protected]
                unless caller->isa(q[Net::BitTorrent::Session::Peer]);
            return $downloaded{$self} += $value;
        }

        sub downloaded {
            my ($self) = @_;
            return $downloaded{$self};
        }

        sub append_nodes {
            my ( $self, $new_nodes ) = @_;
            return $nodes{$self} =
                compact( uncompact( $nodes{$self} ),
                         uncompact($new_nodes) );
        }

        sub nodes {
            my ($self) = @_;
            return uncompact( $nodes{$self} );
        }

        sub compact_nodes {
            my ($self) = @_;
            return $nodes{$self};
        }

        sub _pulse {
            my ($self) = @_;
            for my $tier ( @{ $trackers{$self} } ) {
                $tier->_pulse if $tier->_next_pulse < time;
            }
            my @peers = $self->peers;
            for my $peer (@peers) {
                $peer->_pulse if $peer->_next_pulse < time;
            }

# TODO: review the following block ===================================
#for my $file (@{$files{$self}}) {
#$file->close if ((time - $file->touch_timestamp) > 600);
#}
#if (not $self->complete) {
#grep {
#	$_->_disconnect(q[We're in pull mode and this peer has nothing for us.])
#	not $_->is_interesting
#} @peers;
# Remove peers we're not interested it.  Evil, but...
# well, survival is key. We can seed later.  Right?  Right.
#}
# TODO: review the above block =======================================

        # TODO: remember TCP/IP holds old connections ~120s and you'll
        #      get caught with too many open sockets if they pile up
            if ($self->nodes
                and ( scalar(@peers)
                      < $self->client->maximum_peers_per_session )
                and (
                    scalar(
                        grep {
                            $_->isa(q[Net::BitTorrent::Session::Peer])
                                and not $_->_connected
                            } $self->client->_connections
                    ) < $self->client->maximum_peers_half_open
                )
                )
            {
                my @nodes = $self->nodes;
                while ( scalar(@nodes)
                        and (
                            scalar(
                                 grep { not $_->peer_id } $self->peers
                            ) < $self->client->maximum_peers_half_open
                        )
                    )
                {
                    my $new_peer =
                        Net::BitTorrent::Session::Peer->new(
                                          { address => shift(@nodes),
                                            session => $self
                                          }
                        );
                    $self->client->_add_connection($new_peer)
                        if $new_peer;
                }
            }
            return $next_pulse{$self} = time + 3;
        }

        sub _check_endgame_status {
            my ($self) = @_;
            return unless $downloaded{$self};
            if ((  scalar( grep { not $_->check and $_->priority }
                               @{ $pieces{$self} } )
                ) <= 10    # TODO: make this number a variable
                )
            {
                $endgame{$self} = 1;
            }
            else { $endgame{$self} = 0 }
            return $endgame{$self};
        }

        sub _pick_piece {
            my ( $self, $peer ) = @_;
            return if $self->complete;
            return if not defined $peer->bitfield;
            my $piece = undef;

# TODO:  needs work... ###############################################
            my $free_blocks = 0;
            my $free_slots  = 0;
            grep {
                $free_blocks
                    += scalar grep { scalar $_->peers == 0 }
                    values %{ $_->blocks }
                }
                grep { $_->working } @{ $pieces{$self} };
            grep {
                $free_slots
                    += ( $client{$self}->maximum_requests_per_peer
                         - scalar( @{ $_->outgoing_requests } ) )
                    if not $_->is_choking
            } $self->peers;

# ...major work ######################################################

            my $max_working = (
                      ( $free_blocks < $free_slots ) + (
                          scalar(
                              grep { $_->working } @{ $pieces{$self} }
                          )
                      )
            );

            my @weights = (
                (  scalar( grep { $_->working } @{ $pieces{$self} } )
                       < $max_working # TODO: Make this ratio variable
                )
                ? ( grep {
                                not $_->check
                            and $_->priority
                            and vec( $peer->bitfield, $_->index, 1 )
                            and (
                            $_->working
                            ? ( scalar grep {
                                    scalar $_->peers == 0
                                    } values %{ $_->blocks }
                            )
                            : 1
                            )
                        } @{ $pieces{$self} }
                    )
                : ( grep {
                        $_->working  # and $pieces{$self}->[$_->index]
                            and
                            ( scalar grep { scalar $_->peers == 0 }
                              values %{ $_->blocks } )
                        } @{ $pieces{$self} }
                )
            );
            return if not @weights;

            # [id://230661]
            my $total    = sum map { $_->priority } @weights;
            my $rand_val = $total * rand;
            my $i        = -1;
            while ( $rand_val > 0 ) {
                $rand_val -= $weights[ ++$i ]->priority;
            }
            $piece = $weights[$i];
            if ($piece) {
                $piece->working(1);
                return $piece;
            }
            return;
        }

        sub peers {
            my ($self) = @_;
            return (
                grep {
                    ref $_ eq q[Net::BitTorrent::Session::Peer]
                        and defined $_->session
                        and $_->session eq $self
                    } $self->client->_connections
            );
        }

        sub complete {
            my ($self) = @_;
            for my $piece ( @{ $pieces{$self} } ) {
                if ( not $piece->check and $piece->priority ) {
                    return 0;
                }
            }
            return 1;
        }

        sub bitfield {
            my ($self) = @_;
            return pack q[B*], join q[],
                map { $_->check ? 1 : 0 } @{ $pieces{$self} };
        }

        sub close_files {
            my ($self) = @_;
            my $return = 0;
            for my $file ( @{ $files{$self} } ) {
                $return++ if $file->_close;
            }
            return $return;
        }
        DESTROY {
            my ($self) = @_;
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
            delete $next_pulse{$self};
            return 1;
        }

        sub as_string {
            my ( $self, $advanced ) = @_;
            my %hash = (
                    private  => $private{$self},
                    trackers => $trackers{$self},
                    files    => $files{$self},
                    ( $advanced ? ( pieces => $pieces{$self} ) : () ),
                    nodes => $nodes{$self}
            );
            my @values = ( $infohash{$self},
                           $path{$self},
                           $base_dir{$self},
                           $total_size{$self},
                           ( ( ( ( scalar grep { $_->check }
                                       @{ $pieces{$self} }
                                 ) / ( scalar @{ $pieces{$self} } )
                               )
                             ) * 100
                           ),
                           $uploaded{$self},
                           $downloaded{$self}
            );
            s/(^[-+]?\d+?(?=(?>(?:\d{3})+)(?!\d))|\G\d{3}(?=\d))/$1,/g
                for @values[ 3, 5, 6 ];
            my $dump = sprintf( <<'END', @values );
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
                    my @adv_values_file_info = (
                                  scalar( @{ $files{$self} } ),
                                  ( scalar( @{ $files{$self} } ) == 1
                                    ? q[]
                                    : q[s]
                                  )
                    );
                    $dump
                        .= sprintf( <<'END', @adv_values_file_info );

Advanced Information:
  File Information:
    File list: (%d file%s)
END
                    $dump .= join qq[\n], map {
                        my $file = $_->as_string($advanced);
                        $file =~ s|\n|\n    |g;
                        q[ ] x 4 . $file
                    } @{ $files{$self} };
                }
                {
                    my @adv_values_piece_info = (
                        $piece_count{$self},
                        $piece_size{$self},

                        #$block_size{$self},
                        scalar(
                              grep { $_->working } @{ $pieces{$self} }
                        ),
                        join( q[, ],
                              map      { $_->index }
                                  grep { $_->working }
                                  @{ $pieces{$self} } )
                    );
                    s/(^[-+]?\d+?(?=(?>(?:\d{3})+)(?!\d))|\G\d{3}(?=\d))/$1,/g
                        for @adv_values_piece_info[ 0 .. 2 ];
                    $dump
                        .= sprintf( <<'END', @adv_values_piece_info );

  Piece Information
    Piece count:    %s
    Piece size:     %s bytes
    Working pieces: %d
      %s

END
                }
                {
                    my @adv_values_tracker_info = (
                        scalar( @{ $trackers{$self} } ),

            #(join(q[, ], map { $_->as_string } @{$trackers{$self}})),
                        (  join(
                               qq[\n],
                               map {
                                   my $tracker
                                       = $_->as_string($advanced);
                                   $tracker =~ s|\n|\n      |g;
                                   q[ ] x 6 . $tracker
                                   } @{ $trackers{$self} }
                           )
                        )
                    );
                    s/(^[-+]?\d+?(?=(?>(?:\d{3})+)(?!\d))|\G\d{3}(?=\d))/$1,/g
                        for @adv_values_tracker_info[ 0, ];
                    $dump .= sprintf(
                                  <<'END', @adv_values_tracker_info );

  Tracker Information:
    Tiers: %d
    Tier list:
%s
END
                }
                {
                    my @peers = $self->peers;
                    my @adv_values_peer_info = (
                        scalar(@peers),

            #(join(q[, ], map { $_->as_string } @{$trackers{$self}})),
                        (  join(
                               qq[\n],
                               map {
                                   my $peer
                                       = $_->as_string($advanced);
                                   $peer =~ s|\n|\n      |g;
                                   q[ ] x 6 . $peer
                                   } @peers
                           )
                        )
                    );
                    s/(^[-+]?\d+?(?=(?>(?:\d{3})+)(?!\d))|\G\d{3}(?=\d))/$1,/g
                        for @adv_values_peer_info[ 0, ];
                    $dump
                        .= sprintf( <<'END', @adv_values_peer_info );

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

Net::BitTorrent::Session - Single .torrent session

=head1 CONSTRUCTOR

=over 4

=item C<new ( { [ARGS] } )>

Creates a C<Net::BitTorrent::Session> object.  This constructor is
called by
L<Net::BitTorrent::add_session()|Net::BitTorrent/add_session ( { ... } )>
and should not be used directly.  C<new ( )> accepts arguments as a
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

=head1 METHODS

Unless stated, all methods return either a C<true> or C<false> value,
with C<true> meaning that the operation was a success.  When a method
states that it returns a value, failure will result in C<undef> or an
empty list.

=over 4

=item C<add_tracker ( URLS )>

Add a new
L<Net::BitTorrent::Session::Tracker|Net::BitTorrent::Session::Tracker>
tier to the session.  Accepts a list of URLs.

See also: L<trackers ( )|/trackers ( )>

=item C<append_nodes ( STRING )>

Adds a string of compacted nodes to the list of potential peers.

See also:
L<compact_nodes ( )|/compact_nodes ( )>, L<nodes ( )|/nodes ( )>,
L<Net::BitTorrent::Util::compact( )|Net::BitTorrent::Util/compact ( LIST )>

=item C<as_string ( [ VERBOSE ] )>

Returns a 'ready to print' dump of the C<Net::BitTorrent::Session>
object's data structure.  If called in void context, the structure is
printed to C<STDERR>.

See also: [id://317520],
L<Net::BitTorrent::as_string()|Net::BitTorrent/as_string ( [ VERBOSE ] )>

=item C<bitfield ( )>

Returns a bitfield representing the pieces that have been successfully
downloaded.

=item C<block_size ( [NEWVAL] )>

Mutator to get and/or set the size used when requesting data from
peers.  Use with care.

See also:
L<Net::BitTorrent::maximum_buffer_size( )|Net::BitTorrent/maximum_buffer_size ( [NEW VALUE] )>,
theory.org (L<http://tinyurl.com/32k7wu>), discussion
(L<http://tinyurl.com/4ekea2>)

=item C<client ( )>

Returns the L<Net::BitTorrent|Net::BitTorrent> object related to this
session.

=item C<close_files ( )>

Forces the closure of all related
L<Net::BitTorrent::Session::File|Net::BitTorrent::Session::File>
objects with open file handles.

Under normal circumstances, this should not be called in clients.

=item C<compact_nodes ( )>

Returns a list of potential peers as a compacted string.

See also:
L<nodes ( )|/nodes ( )>,
L<Net::BitTorrent::Util::compact( )|Net::BitTorrent::Util/compact ( LIST )>
L<Net::BitTorrent::Util::uncompact( )|Net::BitTorrent::Util/uncompact ( STRING )>

=item C<complete ( )>

Boolean value indicating whether or not we have finished downloading
all pieces we want.

See also:
L<Net::BitTorrent::Session::Piece|Net::BitTorrent::Session::Piece>

=item C<downloaded ( )>

Returns the total amount of data downloaded during the current
session.

See also: L<uploaded ( )|/uploaded ( )>

=item C<files ( )>

Returns a list of
L<Net::BitTorrent::Session::File|Net::BitTorrent::Session::File>
objects representing all files contained in the related .torrent file.

=item C<hash_check ( )>

Verifies the integrity of all files associated with this session.

This is a blocking method; all processing will stop until this
function returns.

See also:
L<Net::BitTorrent::Session::Piece::verify( )|Net::BitTorrent::Session::Piece/verify( )>

=item C<infohash ( )>

Returns the 20 byte SHA1 hash used to identify this session
internally, with trackers, and with remote peers.

=item C<nodes ( )>

Returns a list of potential peers.

To receive a compacted list of nodes (to use in a quick resume system,
for example), see L<compact_nodes( )|/compact_nodes ( )>.

=item C<peers ( )>

Returns a list of all related
L<Net::BitTorrent::Session::Peer|Net::BitTorrent::Session::Peer>
objects.

=item C<piece_count ( )>

Returns the number of
L<Net::BitTorrent::Session::Piece|Net::BitTorrent::Session::Piece>
objects related to this session.

=item C<piece_size ( )>

Returns the piece size defined in the .torrent file.

=item C<pieces ( )>

Returns a list of
L<Net::BitTorrent::Session::Piece|Net::BitTorrent::Session::Piece>
objects.

=item C<private ( )>

Returns a bool value indicating whether or not this session is allowed
to use DHT and other Peer Exchange protocols.

=item C<total_size ( )>

Returns the total size of all files listed in the .torrent file.

=item C<trackers ( )>

Returns a list of all
L<Net::BitTorrent::Session::Tracker|Net::BitTorrent::Session::Tracker>
objects related to the session.

See also: L<add_tracker ( )|/add_tracker ( URLS )>

=item C<uploaded ( )>

Returns the total amount of data uploaded during the current session.

See also: L<downloaded ( )|/downloaded ( )>

=back

=head1 AUTHOR

Sanko Robinson <sanko@cpan.org> - L<http://sankorobinson.com/>

CPAN ID: SANKO

ProperNoun on Freenode

=head1 LICENSE AND LEGAL

Copyright 2008 by Sanko Robinson E<lt>sanko@cpan.orgE<gt>

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.  See
L<http://www.perl.com/perl/misc/Artistic.html> or the F<LICENSE> file
included with this module.

All POD documentation is covered by the Creative Commons
Attribution-Noncommercial-Share Alike 3.0 License
(L<http://creativecommons.org/licenses/by-nc-sa/3.0/us/>).

Neither this module nor the L<AUTHOR|/AUTHOR> is affiliated with
BitTorrent, Inc.

=for svn $Id$

=cut
