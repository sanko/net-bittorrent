package Net::BitTorrent::Session::File;
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
    use Fcntl qw[/O_/ /SEEK/];
    use File::Spec;
    use Carp qw[carp];
    my (%size,    %path,   %index,
        %session, %handle, %unicode_filehandle,
        %open_timestamp, %open_mode, %touch_timestamp, %piece_range

    );
    {
        sub new {
            my ( $class, $args ) = @_;
            my $self = undef;
            if (    caller->isa(q[Net::BitTorrent::Session])
                and defined $args->{q[session]}
                and
                $args->{q[session]}->isa(q[Net::BitTorrent::Session])
                and defined $args->{q[path]}

                #and $args->{q[path]} =~ m[^\w$]
                and defined $args->{q[size]}
                and $args->{q[size]} =~ m[^\d+$]
                and defined $args->{q[index]}
                and $args->{q[index]} =~ m[^\d+$]
                )
            {
                $self = bless \$args->{q[path]}, $class;
                $size{$self}      = $args->{q[size]};
                $path{$self}      = $args->{q[path]};
                $index{$self}     = $args->{q[index]};
                $session{$self}   = $args->{q[session]};
                $open_mode{$self} = undef;
            }
            return $self;
        }
        sub size { my ($self) = @_; return $size{$self} }

        sub path {
            my $self = shift;
            return
                File::Spec->catfile( $session{$self}->base_dir,
                                     $path{$self} );
        }
        sub index   { my ($self) = @_; return $index{$self} }
        sub session { my ($self) = @_; return $session{$self} }

        sub client {
            my ($self) = @_;
            return $session{$self}->client;
        }
        sub _handle { my ($self) = @_; return $handle{$self} }

        sub _use_unicode_handle {
            my ($self) = @_;
            return $unicode_filehandle{$self};
        }    # used only on win32

        sub open_timestamp {
            my ($self) = @_;
            return $open_timestamp{$self};
        }
        sub open_mode { my ($self) = @_; return $open_mode{$self} }

        sub touch_timestamp {
            my ($self) = @_;
            return $touch_timestamp{$self};
        }

        sub _open {
            my ( $self, $mode ) = @_;
            if ( $mode !~ m[^[rw]$] ) {
                $self->client->_do_callback( q[file_error], $self,
                             sprintf( q[Bad open mode: %s], $mode ) );
                return;
            }
            if ( $handle{$self} and $open_mode{$self} ) {
                if ( $mode eq $open_mode{$self} ) {
                    return 1;
                }
                else {
                    $self->_sysclose;
                }
            }
            {    # TODO: Is File::Spec unicode safe?
                my ( $vol, $dir, $file )
                    = File::Spec->splitpath( $self->path );
                if ( not -d File::Spec->catpath( $vol, $dir, q[] ) )
                {    # TODO: -d is certainly not safe :(
                    if ( $mode =~ m[w] ) {
                        require File::Path;
                        my @created
                            = File::Path::mkpath(
                            File::Spec->catpath( $vol, $dir, q[] ),
                            { verbose => 0 } )    # or one?
                            or return;
                        grep {
                            $self->client->_do_callback( q[on_log],
                                              q[mkpath created $_\n] )
                        } @created;
                    }
                }
            }
            if ($self->client->use_unicode
                ? do {
                    $self->_sysopen( ( $mode eq q[r]
                                       ? O_RDONLY
                                       : O_WRONLY | O_CREAT
                                     ),
                                     oct 777
                    );
                }
                : do {
                    sysopen( $handle{$self},
                             $self->path,
                             (  $mode eq q[r]
                                ? O_RDONLY
                                : O_WRONLY | O_CREAT
                             ),
                             oct 777
                    );
                }
                )
            {
                $open_mode{$self}
                    = $mode eq q[r] ? O_RDONLY : O_WRONLY;
                $open_timestamp{$self} = time;
                $self->client->_do_callback( q[file_open], $self );
                return 1;
            }
            return;
        }

        sub _seek {
            my ( $self, $position ) = @_;
            if ( not $handle{$self} ) {
                $self->client->_do_callback( q[file_error],
                                             q[File not open] );
            }
            elsif ( $position > $size{$self} ) {
                $self->client->_do_callback(
                      q[file_error],
                      sprintf(
                          q[Cannot seek beyond end of file (%d > %d)],
                          $position, $size{$self}
                      )
                );
            }
            else {
                return sysseek( $handle{$self}, $position, SEEK_SET );
            }
        }
        sub _systell { sysseek( $_[0]->_handle, 0, SEEK_CUR ) }

        sub _read {
            my ( $self, $length ) = @_;
            my $data = q[];
            if ( not $handle{$self} ) {
                $self->client->_do_callback( q[file_error],
                                             q[File not open] );
            }
            elsif ( $open_mode{$self} != O_RDONLY ) {
                $self->client->_do_callback( q[file_error],
                                          q[File not open for read] );
            }
            elsif ( $self->_systell + $length > $size{$self} ) {
                $self->client->_do_callback( q[file_error],
                                  q[Cannot read beyond end of file] );
            }
            else {
                truncate( $handle{$self}, $size{$self} )
                    if -s $handle{$self} != $size{$self};
                my $real_length
                    = sysread( $handle{$self}, $data, $length );
                if ( $real_length or ( $real_length == $length ) ) {
                    $touch_timestamp{$self} = time;
                    $self->client->_do_callback( q[file_read], $self,
                                                 $real_length );
                }
            }
            return $data;
        }

        sub _write {
            my ( $self, $data ) = @_;
            my $real_length;
            if ( not $handle{$self} ) {
                $self->client->_do_callback( q[file_error],
                                             q[File not open] );
            }
            elsif ( $open_mode{$self} != O_WRONLY ) {
                $self->client->_do_callback( q[file_error],
                                         q[File not open for write] );
            }
            elsif ( $self->_systell + length($data) > $size{$self} ) {
                $self->client->_do_callback( q[file_error],
                                 q[Cannot write beyond end of file] );
            }
            else {
                truncate( $handle{$self}, $size{$self} )
                    if -s $handle{$self} != $size{$self};
                $real_length = syswrite( $handle{$self}, $data,
                                         length($data) );
                if ( not defined $real_length
                     or $real_length != length($data) )
                {
                    $touch_timestamp{$self} = time;
                    $self->client->_do_callback( q[file_write], $self,
                                                 $real_length );
                }
            }
            return $real_length;
        }

        sub _close {
            my ($self) = @_;
            $self->session->client->use_unicode
                ? do { $self->_sysclose( $handle{$self} ) }
                : do { CORE::close( $handle{$self} ) };
            delete $handle{$self};
            $open_mode{$self} = undef;
            $self->client->_do_callback( q[file_close], $self );
            return 1;
        }

        sub piece_range {    # cache this only when needed
            my ($self) = @_;
            if ( not defined $piece_range{$self} ) {
                my $offset = 0;
                for my $_index ( 0 .. $self->index - 1 ) {
                    $offset += $self->session->files->[$_index]->size;
                }
                $piece_range{$self} = [
                          int( $offset / $self->session->piece_size ),
                          int( ( $offset + $size{$self} )
                               / $self->session->piece_size
                          )
                ];
            }
            return $piece_range{$self};
        }

        sub pieces {
            my ($self) = @_;
            if ( not defined $piece_range{$self} ) {
                $self->piece_range;
            }
            return
                map { $self->session->pieces->[$_] }
                ( $piece_range{$self}[0] .. $piece_range{$self}[1] );
        }

        sub priority {
            my ( $self, $value ) = @_;

       # TODO: if $value == 0, we shouldn't write to this file
       # TODO: let's not set the priority for the piece if it overlaps
            return (
                defined $value
                ? do {
                    carp(q[priority is malformed]) and return
                        unless $value =~ m[^\d+$];
                    map { $_->priority($value) } $self->pieces;
                    }
                : map { $_->priority } $self->pieces
            );
        }

        sub as_string {
            my ( $self, $advanced ) = @_;
            my @values = (
                   $index{$self},
                   ( q[=] x ( 25 + length( $index{$self} ) ) ),
                   $self->path,
                   $size{$self},
                   (  (  (  (  scalar grep { $_->check } $self->pieces
                            ) / ( scalar $self->pieces )
                         )
                      ) * 100
                   ),
                   @{ $self->piece_range }
            );
            s/(^[-+]?\d+?(?=(?>(?:\d{3})+)(?!\d))|\G\d{3}(?=\d))/$1,/g
                for @values[ 3, ];    # no 'better way' warning...
            my $dump = sprintf( <<'END', @values );
Net::BitTorrent::File (#%d)
%s
Basic Information
    Path:             %s
    Size:             %s bytes
    Percent complete: %3.2f%%
    Piece range:      %d .. %d
END

=pod

=begin future

            if ($advanced) {
                my @adv_values = (scalar(@{$sessions{$self}}));

                $dump .= sprintf(<<'END', @adv_values);
Advanced Information
  Loaded sessions: (%d)
END
                $dump .= join qq[\n], map {
                    my $session = $_->as_string($advanced);
                    $session =~ s|\n|\n    |g;
                    q[ ] x 4 . $session
                } @{$sessions{$self}};
            }

=end future

=cut

            return print STDERR qq[$dump\n] unless defined wantarray;
            return $dump;
        }
        DESTROY {
            my $self = shift;
            delete $size{$self};
            delete $path{$self};
            delete $piece_range{$self};
            delete $session{$self};
            delete $handle{$self};
            delete $unicode_filehandle{$self};
            delete $open_timestamp{$self};
            delete $open_mode{$self};
            delete $touch_timestamp{$self};
            return 1;
        }

        sub _sysopen {
            my ( $self, $mode, $perms ) = @_;

 # Unicode, and other extended charsets are a pain...
 # [http://groups.google.com/group/perl.unicode/msg/86ab5af239975df7],
 # [id://538097], [id://229642], [id://445883], and others
            if ( $self->session->client->use_unicode
                 and ( $^O eq q[MSWin32]
                       and ( utf8::is_utf8( $self->path ) ) )
                )
            {
                require Win32API::File;
                Win32API::File->import(qw[:ALL]);
                require Encode;
                Encode->import(qw[find_encoding encode]);
                my $h = CreateFileW(
                        encode( q[UTF-16LE], $self->path . qq[\0] ),
                        ( ( $mode &= O_WRONLY )
                          ? ( GENERIC_WRITE(),
                              FILE_SHARE_READ() | FILE_SHARE_WRITE(),
                              [], OPEN_ALWAYS()
                              )
                          : ( GENERIC_READ(),
                              FILE_SHARE_READ() | FILE_SHARE_WRITE(),
                              [], OPEN_EXISTING()
                          )
                        ),
                        0,
                        []
                );
                my $fd = OsFHandleOpenFd( $h, $mode );
                return if $fd < 0;
                $self->_use_unicode_handle($h);
                return
                    CORE::open( $handle{$self},
                                (  ( $mode &= O_WRONLY )
                                   ? q[>&]
                                   : q[<&]
                                ),
                                $fd
                    );
            }
            sysopen( $handle{$self}, $self->path, $mode, $perms );
        }

        sub _sysclose {
            my ($self) = @_;
            if ( $self->session->client->use_unicode
                 and defined $self->_use_unicode_handle )
            {
                require Win32API::File;
                Win32API::File->import(qw[:ALL]);
                my $OS_FH = CloseHandle( $self->_use_unicode_handle );
                delete $unicode_filehandle{$self};
            }
            return CORE::close( $handle{$self} )
                and delete $handle{$self}
                and $open_mode{$self} = undef;
        }
    }
}
1;
__END__

=pod

=head1 NAME

Net::BitTorrent::Session::File - Single file class

=head1 CONSTRUCTOR

=over 4

=item C<new ( { [ARGS] } )>

Creates a C<Net::BitTorrent::Session::File> object.  This constructor
should not be used directly.

=back

=head1 METHODS

=over 4

=item C<as_string ( [ VERBOSE ] )>

Returns a 'ready to print' dump of the
C<Net::BitTorrent::Session::File> object's data structure.  If called
in void context, the structure is printed to C<STDERR>.

See also: [id://317520],
L<Net::BitTorrent::as_string()|Net::BitTorrent/as_string ( [ VERBOSE ] )>

=item C<client ( )>

Returns the L<Net::BitTorrent|Net::BitTorrent> object related to this
file.

=item C<index ( )>

Returns the zero based index of this file according to the related
L<Net::BitTorrent::Session|Net::BitTorrent::Session> object's file
list.

=item C<open_mode ( )>

Returns a C<Fcntl> value representing if and how the related file
handle is open.  Possible values:

    O_RDONLY - Read
    O_WRONLY - Write
    undef    - Closed

See also: L<Fcntl|Fcntl>

=item C<open_timestamp ( )>

Returns when the file was opened.

=item C<path ( )>

Returns the absolute path of the related file.

=item C<piece_range ( )>

Returns the indexes of the first and last
L<Net::BitTorrent::Session::Piece|Net::BitTorrent::Session::Piece>
objects covered by this file.

=item C<pieces ( )>

Returns a list of
L<Net::BitTorrent::Session::Piece|Net::BitTorrent::Session::Piece>
objects.

=item C<priority ( [NEWVAL] )>

Mutator to set/get the download priority of this file.

By default, all files begin with a level two priority with the intent
being on a C<0> (skip), C<1> (low), C<2> (normal), C<3> (high)
priority scale but you may use any scale you want.  For example, you
could set a file's priority to say... C<1,000,000>, leave everything
else at the default C<2> and and be positive we'll work on it first.
To avoid downloading this file, set priority to zero.

See also:
L<Net::BitTorrent::Session::Piece::priority ( )|Net::BitTorrent::Session::Piece/priority ( [NEWVAL] )>

NOTE: Setting the priority to zero will tell C<Net::BitTorrrent> not
to bother requesting these pieces but the file will still be created
on disk if a piece we want overlaps onto this file.  Just give me some
time to work on an intermediate .piece file and this problem will go
away.

=item C<session ( )>

Returns the L<Net::BitTorrent::Session|Net::BitTorrent::Session>
object related to this file.

=item C<size ( )>

Returns the size of the file represented by this object.

=item C<touch_timestamp ( )>

Returns when the file was last written to.

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
