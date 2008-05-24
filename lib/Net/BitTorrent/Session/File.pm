package Net::BitTorrent::Session::File;
use strict;
use warnings;
{

    BEGIN {
        use version qw[qv];
        our $SVN = q[$Id$];
        our $VERSION = sprintf q[%.3f], version->new(qw$Rev$)->numify / 1000;
    }
    use Fcntl qw[/O_/ /SEEK/];
    use File::Spec;
    use Carp qw[carp];
    use Net::BitTorrent::Util qw[:log];
    my (%size,        %path,           %index,     %session,
        %handle,      %open_timestamp, %open_mode, %touch_timestamp,
        %piece_range, %win32_handle
    );

    sub new {
        my ($class, $args) = @_;
        my $self = undef;
        if (    caller->isa(q[Net::BitTorrent::Session])
            and defined $args->{q[session]}
            and $args->{q[session]}->isa(q[Net::BitTorrent::Session])
            and defined $args->{q[path]}

            #and $args->{q[path]} =~ m[^\w$]
            and defined $args->{q[size]}
            and $args->{q[size]} =~ m[^\d+$]
            and defined $args->{q[index]}
            and $args->{q[index]} =~ m[^\d+$]
            )
        {   $self = bless \$args->{q[path]}, $class;
            $size{$self} = $args->{q[size]};
            $path{$self} = $args->{q[path]};
            utf8::decode($path{$self});    # Just in case...
            $index{$self}     = $args->{q[index]};
            $session{$self}   = $args->{q[session]};
            $open_mode{$self} = undef;
        }
        return $self;
    }
    sub size            { return $size{$_[0]}; }
    sub path            { return $path{$_[0]}; }
    sub index           { return $index{$_[0]}; }
    sub session         { return $session{$_[0]}; }
    sub client          { return $session{$_[0]}->client; }
    sub _handle         { return $handle{$_[0]}; }
    sub open_mode       { return $open_mode{$_[0]}; }
    sub open_timestamp  { return $open_timestamp{$_[0]}; }
    sub touch_timestamp { return $touch_timestamp{$_[0]}; }

    sub _open {
        my ($self, $mode) = @_;
        $session{$self}->client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
        if ($mode !~ m[^[rw]$]) {
            $self->client->_do_callback(q[file_error], $self,
                                        sprintf(q[Bad open mode: %s], $mode));
            return;
        }
        my $mode_Fcntl = $mode eq q[r] ? O_RDONLY : O_WRONLY;
        if (    defined $handle{$self}
            and defined $open_mode{$self})
        {   if ($open_mode{$self} == $mode_Fcntl) {
                return 1;
            }
            else {
                $self->_sysclose;
            }
        }
        if ($self->_sysopen(($mode eq q[r]
                             ? (O_RDONLY)
                             : (O_WRONLY)
                            )
            )
            )
        {   $open_mode{$self} = $mode eq q[r] ? O_RDONLY : O_WRONLY;
            $open_timestamp{$self} = time;
            $self->client->_do_callback(q[file_open], $self);
            return 1;
        }
        return;
    }

    sub _seek {
        my ($self, $position) = @_;
        $session{$self}->client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
        if (not defined $handle{$self}) {
            $self->client->_do_callback(q[file_error], q[File not open]);
            return;
        }
        elsif ($position > $size{$self}) {
            $self->client->_do_callback(
                          q[file_error],
                          sprintf(q[Cannot seek beyond end of file (%d > %d)],
                                  $position, $size{$self}
                          )
            );
        }
        return sysseek($handle{$self}, $position, SEEK_SET);
    }

    sub _systell {
        my ($self) = @_;
        $session{$self}->client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
        return sysseek($self->_handle, 0, SEEK_CUR);
    }

    sub _read {
        my ($self, $length) = @_;
        $session{$self}->client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
        my $data = q[];
        if (not $handle{$self}) {
            $self->client->_do_callback(q[file_error], q[File not open]);
        }
        elsif ($open_mode{$self} != O_RDONLY) {
            $self->client->_do_callback(q[file_error],
                                        q[File not open for read]);
        }
        elsif ($self->_systell + $length > $size{$self}) {
            $self->client->_do_callback(q[file_error],
                                        q[Cannot read beyond end of file]);
        }
        else {
            truncate($handle{$self}, $size{$self})
                if -s $handle{$self} != $size{$self};
            my $real_length = sysread($handle{$self}, $data, $length);
            if ($real_length
                or (defined $real_length and $real_length == $length))
            {   $touch_timestamp{$self} = time;
                $self->client->_do_callback(q[file_read], $self,
                                            $real_length);
            }
        }
        return $data;
    }

    sub _write {
        my ($self, $data) = @_;
        $session{$self}->client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
        my $real_length;
        if (not $handle{$self}) {
            $self->client->_do_callback(q[file_error], q[File not open]);
        }
        elsif ($open_mode{$self} != O_WRONLY) {
            $self->client->_do_callback(q[file_error],
                                        q[File not open for write]);
        }
        elsif ($self->_systell + length($data) > $size{$self}) {
            $self->client->_do_callback(q[file_error],
                                        q[Cannot write beyond end of file]);
        }
        else {
            truncate($handle{$self}, $size{$self})
                if -s $handle{$self} != $size{$self};
            $real_length = syswrite($handle{$self}, $data, length($data));
            if (not defined $real_length
                or $real_length != length($data))
            {   $touch_timestamp{$self} = time;
                $self->client->_do_callback(q[file_write], $self,
                                            $real_length);
            }
        }
        return $real_length;
    }

    sub _close {
        my ($self) = @_;
        $session{$self}->client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
        $self->_sysclose($handle{$self});
        delete $handle{$self};
        $open_mode{$self} = undef;
        $self->client->_do_callback(q[file_close], $self);
        return 1;
    }

    sub piece_range {    # cache this only when needed
        my ($self) = @_;
        $session{$self}->client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
        if (not defined $piece_range{$self}) {
            my $offset = 0;
            for my $_index (0 .. $self->index - 1) {
                $offset += $self->session->files->[$_index]->size;
            }
            $piece_range{$self} = [int($offset / $self->session->piece_size),
                                   int(($offset + $size{$self})
                                       / $self->session->piece_size
                                   )
            ];
        }
        return $piece_range{$self};
    }

    sub pieces {
        my ($self) = @_;
        $session{$self}->client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
        if (not defined $piece_range{$self}) {
            $self->piece_range;
        }
        return
            map { $self->session->pieces->[$_] }
            ($piece_range{$self}[0] .. $piece_range{$self}[1]);
    }

    sub priority {
        my ($self, $value) = @_;
        $session{$self}->client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));

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
        my ($self, $advanced) = @_;
        $session{$self}->client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
        my @values = ($index{$self},
                      (q[=] x (25 + length($index{$self}))),
                      $self->path,
                      $size{$self},
                      (((  (scalar grep { $_->check } $self->pieces)
                         / (scalar $self->pieces)
                        )
                       ) * 100
                      ),
                      @{$self->piece_range}
        );
        s/(^[-+]?\d+?(?=(?>(?:\d{3})+)(?!\d))|\G\d{3}(?=\d))/$1,/g
            for @values[3,];    # no 'better way' warning...
        my $dump = sprintf( <<'END', @values);
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
        delete $win32_handle{$self};
        delete $open_timestamp{$self};
        delete $open_mode{$self};
        delete $touch_timestamp{$self};
        return 1;
    }

    sub _sysopen {
        my ($self, $mode) = @_;
        $session{$self}->client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
        $self->_mkpath() if $mode &= O_WRONLY;
        if ((    $^O eq q[MSWin32]
             and utf8::is_utf8($path{$self})
             and require Win32API::File
             and require Encode
            )
            )
        {   Win32API::File->import(qw[:ALL]);
            Encode->import(qw[find_encoding encode]);
            for my $null (qq[\0], q[]) {
                $win32_handle{$self} =
                    CreateFileW(encode('UTF-16LE', $path{$self} . $null),
                                (($mode &= O_WRONLY) ? GENERIC_WRITE()
                                 : GENERIC_READ()
                                ),
                                FILE_SHARE_READ(),
                                [],
                                (($mode &= O_WRONLY) ? OPEN_ALWAYS()
                                 : OPEN_EXISTING()
                                ),
                                FILE_ATTRIBUTE_NORMAL(),
                                0
                    ) and last;
            }
            return if not $win32_handle{$self};
            my $fd = OsFHandleOpenFd($win32_handle{$self}, $mode);
            return if $fd < 0;
            return
                CORE::open($handle{$self},
                           (($mode &= O_WRONLY)
                            ? q[>&]
                            : q[<&]
                           ),
                           $fd
                );
        }
        return
            sysopen($handle{$self},
                    $path{$self},
                    $mode | (($mode &= O_WRONLY)
                             ? O_CREAT
                             : 0
                    )
            );
    }

    sub _sysclose {
        my ($self) = @_;
        $session{$self}->client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
        if (defined $win32_handle{$self} and require Win32API::File) {
            Win32API::File::CloseHandle($win32_handle{$self});
            delete $win32_handle{$self};
        }
        return CORE::close($handle{$self})
            and delete $handle{$self}
            and $open_mode{$self} = undef;
    }

    sub _mkpath {
        my ($self) = @_;
        $session{$self}->client->_do_callback(q[log], TRACE,
                     sprintf(q[Entering %s for %s], [caller 0]->[3], $$self));
        my ($vol, $dir, $file) = File::Spec->splitpath($path{$self});
        if (not -d File::Spec->catpath($vol, $dir, q[])) {
            if (    $^O eq q[MSWin32]
                and utf8::is_utf8($path{$self})
                and require Win32
                and require Win32API::File
                and require Encode)
            {   Win32API::File->import(qw[:ALL]);
                Encode->import(qw[encode]);
                my $path = $vol;
                foreach my $this_dir (
                               File::Spec->splitdir(File::Spec->catdir($dir)))
                {   next unless length($this_dir);
                    $path = File::Spec->catdir($path, $this_dir);
                    utf8::decode($path);
                    next if -d $path;
                    if (Win32::CreateDirectory($path)) {
                        $self->client->_do_callback(q[log], INFO,
                                         sprintf q[mkpath created %s], $path);
                    }
                }
            }
            elsif (require File::Path) {
                grep {
                    $self->client->_do_callback(q[log], INFO,
                                                q[mkpath created $_\n])
                    }
                    File::Path::mkpath(File::Spec->catpath($vol, $dir, q[]),
                                       {verbose => 0});
            }
        }
        return 1;
    }
}
1;
__END__

=pod

=head1 NAME

Net::BitTorrent::Session::File - BitTorrent File I/O Class

=head1 Constructor

=over 4

=item C<new ( { [ARGS] } )>

Creates a C<Net::BitTorrent::Session::File> object.  This constructor
should not be used directly.

=back

=head1 Methods

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

=head1 Author

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
