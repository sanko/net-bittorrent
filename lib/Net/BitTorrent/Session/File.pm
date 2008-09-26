package Net::BitTorrent::Session::File;
{
    use strict;      # core as of perl 5
    use warnings;    # core as of perl 5.006

    #
    use Carp qw[carp];                      # core as of perl 5
    use Scalar::Util qw[blessed weaken];    # core as of perl 5.007003
    use Fcntl qw[/O_/ /SEEK/ :flock];       # core as of perl 5

    # Utility stuff... should be moved to N::B::S::F::Util?
    #use File::Path qw[mkpath]; # core as of perl 5.001
    #use File::Spec::Functions  # core as of perl 5.00504
    #    qw[splitpath catpath];
    #
    use version qw[qv];                     # core as of 5.009
    our $SVN = q[$Id$];
    our $UNSTABLE_RELEASE = 0; our $VERSION = sprintf(($UNSTABLE_RELEASE ? q[%.3f_%03d] : q[%.3f]), (version->new((qw$Rev$)[1])->numify / 1000), $UNSTABLE_RELEASE);

    #
    my (%path,     %session, %size,   %index);          # parameters to new()
    my (%priority, %mode,    %handle, %win32_handle);

    # Constructor
    sub new {

        # Creates a new N::B::Session object
        # Accepts parameters as key/value pairs in a hash reference
        # Required parameters:
        #  - Path    (filename)
        #  - Session (blessed N::B::Session object)
        #  - Size    (length of this file when complete)
        # Returns
        #    - a new blessed object on success
        #    - undef on failure
        # MO
        # - validate incoming parameters
        # -
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

            #carp q[Net::BitTorrent::Session::File->new({}) requires ]
            #    . q[parameters a set of parameters];
            return;
        }
        if (ref($args) ne q[HASH]) {

            #carp q[Net::BitTorrentS::Session::File->new({}) requires ]
            #    . q[parameters to be passed as a hashref];
            return;
        }
        if (not defined $args->{q[Path]}) {

            #carp q[Net::BitTorrent::Session::File->new({}) requires a ]
            #    . q['Path' parameter];
            return;
        }
        if (not defined $args->{q[Session]}) {

            #carp q[Net::BitTorrent::Session::File->new({}) requires a ]
            #    . q['Session' parameter];
            return;
        }
        if (not blessed $args->{q[Session]}) {

            #carp q[Net::BitTorrent::Session::File->new({}) requires a ]
            #    . q[blessed 'Session' object];
            return;
        }
        if (not $args->{q[Session]}->isa(q[Net::BitTorrent::Session])) {

            #carp q[Net::BitTorrent::Session::File->new({}) requires a ]
            #    . q[blessed Net::BitTorrent::Session object in the ]
            #    . q['Session' parameter];
            return;
        }
        if (not defined $args->{q[Size]}) {

            #carp q[Net::BitTorrent::Session::File->new({}) requires a ]
            #    . q['Size' parameter];
            return;
        }
        if ($args->{q[Size]} !~ m[^\d+$]) {

            #carp q[Net::BitTorrent::Session::File->new({}) requires an ]
            #    . q[integer value for 'Size'];
            return;
        }
        if (not defined $args->{q[Index]}) {

            #carp q[Net::BitTorrent::Session::File->new({}) requires an ]
            #    . q['Index' parameter];
            return;
        }
        if ($args->{q[Index]} !~ m[^\d+$]) {

            #carp q[Net::BitTorrent::Session::File->new({}) requires an ]
            #    . q[integer value for 'Index'];
            return;
        }

        #
        $self = bless \$args->{q[Path]}, $class;
        $path{$self}    = $args->{q[Path]};
        $session{$self} = $args->{q[Session]};
        weaken $session{$self};
        $size{$self}     = $args->{q[Size]};
        $priority{$self} = 2;                  # default in 0-3 range where...
                                               #  0 = don't download
                                               #  1 = low priority
                                               #  2 = normal priority
                                               #  3 = high priority
        $index{$self}    = $args->{q[Index]};

        #
        # TODO (for client writers):
        # if -f $args{q[Path]}
        #   we should force a hash check?
        #
        return $self;
    }

    # Accessors | public
    sub priority { return $priority{+shift} }

    sub set_priority {
        my ($self, $newval) = @_;
        if (not defined $newval) {

#carp
#    q[Net::BitTorrent::Session::File->set_priority() requires an priority parameter];
            return;
        }
        if ($newval !~ m[^\d+$]) {

   #carp
   #    q[Net::BitTorrent::Session::File->set_priority() requires an integer];
            return;
        }
        return $priority{$self} = $newval;
    }
    sub mode    { return $mode{+shift} }
    sub size    { return $size{+shift} }
    sub session { return $session{+shift} }
    sub index   { return $index{+shift} }
    sub path    { return $path{+shift} }

    # Accessors | private
    #
    # Methods | public
    #
    # Methods | private
    sub _open {
        my ($self, $mode) = @_;
        if (not defined $mode) {

            #carp q[Net::BitTorrent::Session::File->_open() requires a mode];
            return;
        }
        if ($mode !~ m[^[rw]$]) {

           #carp
           #    q[Malformed mode to Net::BitTorrent::Session::File->_open(): ]
           #    . $mode;
            return;
        }

        #
        if (defined $handle{$self} and defined $mode{$self}) {
            if ($mode{$self} eq $mode) {
                return sysseek($handle{$self}, 0, SEEK_SET);
            }
            if ($mode{$self} eq q[w]) {
                flock($handle{$self}, LOCK_UN) or return;    # unlock
            }
            $self->_close;
        }

        #
        $self->_mkpath;

        #
        my $mode_Fcntl = $mode eq q[r] ? O_RDONLY : O_WRONLY;
        if (not $self->_sysopen(($mode eq q[r] ? (O_RDONLY) : (O_WRONLY)))) {
            $session{$self}->_client->_event(
                 q[file_error],
                 {File    => $self,
                  Message => sprintf(q[Cannot open file for %s: %s],
                                     ($mode eq q[r] ? q[read] : q[write]), $^E
                  )
                 }
            );
            return;
        }

        #
        $mode{$self} = $mode;

        #
        if (not flock($handle{$self},
                      (($mode{$self} eq q[r]) ? LOCK_SH : LOCK_EX)
            )
            )
        {   $session{$self}->_client->_event(
                 q[file_error],
                 {File    => $self,
                  Message => sprintf(q[Cannot lock file for %s: %s],
                                     ($mode eq q[r] ? q[read] : q[write]), $^E
                  )
                 }
            );
            return;
        }

        #
        $session{$self}->_client->_event(q[file_open],
                                       {File => $self, Mode => $mode{$self}});

        #
        return defined $handle{$self};
    }

    sub _write {
        my ($self, $data) = @_;
        if (not defined $data) {return}
        if (not $handle{$self}) {
            $session{$self}->_client->_event(
                           q[file_error],
                           {File    => $self,
                            Message => q[Cannot read from file: File not open]
                           }
            );
            return;
        }
        elsif ($mode{$self} ne q[w]) {
            $session{$self}->_client->_event(
                 q[file_error],
                 {File    => $self,
                  Message => q[Cannot read from file: File not open for write]
                 }
            );
            return;
        }
        elsif (($self->_systell + length($data)) > $size{$self}) {
            $session{$self}->_client->_event(
                q[file_error],
                {File    => $self,
                 Message => sprintf(
                     q[Cannot write beyond end of file (tell: %d | data:%d bytes | size: %d) (%d > %d)],
                     $self->_systell, length($data),
                     $size{$self}, ($self->_systell + length($data)),
                     $size{$self}
                 )
                }
            );
            return;
        }
        truncate($handle{$self}, $size{$self})
            if -s $handle{$self} != $size{$self};
        my $expected_length = length $data;
        my $actual_length = syswrite($handle{$self}, $data, $expected_length);
        if (defined $actual_length) {
            if ($actual_length != $expected_length) {
                $session{$self}->_client->_event(
                    q[file_error],
                    {File    => $self,
                     Message => sprintf(
                         q[Cannot write %d bytes of data to file; Wrote %d bytes instead (%s)],
                         length($data), $actual_length, $^E
                     )
                    }
                );
                return;
            }
        }
        else {
            $session{$self}->_client->_event(
                   q[file_error],
                   {File    => $self,
                    Message => sprintf(
                                q[Cannot write %d bytes of data to file (%s)],
                                length($data), $^E
                    )
                   }
            );
            return;
        }

        #
        $session{$self}->_client->_event(q[file_write],
                                   {File => $self, Length => $actual_length});

        #
        return $actual_length;
    }

    sub _read {
        my ($self, $length) = @_;

        #
        if (not defined $length) {

    #carp
    #    q[Net::BitTorrent::Session::File->_read( LENGTH ) requires a length];
            return;
        }
        if ($length !~ m[^\d+$]) {

#carp
#    q[Net::BitTorrent::Session::File->_read( LENGTH ) requires an integer length];
            return;
        }

        #
        my $data = q[];
        if (not $handle{$self}) {
            $session{$self}->_client->_event(
                           q[file_error],
                           {File    => $self,
                            Message => q[Cannot read from file: File not open]
                           }
            );
            return;
        }
        elsif ($mode{$self} ne q[r]) {
            $session{$self}->_client->_event(
                  q[file_error],
                  {File    => $self,
                   Message => q[Cannot read from file: File not open for read]
                  }
            );
            return;
        }
        elsif ($self->_systell + $length > $size{$self}) {
            $session{$self}->_client->_event(
                                 q[file_error],
                                 {File    => $self,
                                  Message => q[Cannot read beyond end of file]
                                 }
            );
            return;
        }
        else {
            truncate($handle{$self}, $size{$self})
                if -s $handle{$self} != $size{$self};
            my $real_length = sysread($handle{$self}, $data, $length);
            if ($real_length != $length) {
                $session{$self}->_client->_event(
                     q[file_error],
                     {File    => $self,
                      Message => sprintf(q[Failed to read %d bytes from file],
                                         $length)
                     }
                );
                return;
            }
        }
        $session{$self}->_client->_event(q[file_read],
                                         {File   => $self,
                                          Length => length($data)
                                         }
        );
        return $data;
    }

    sub _systell {
        my ($self) = @_;
        if (not $handle{$self}) {
            $session{$self}->_client->_event(
                 q[file_error],
                 {File    => $self,
                  Message => q[Cannot get filehandle position: File not open],
                 }
            );
            return;
        }
        return sysseek($handle{$self}, 0, SEEK_CUR);
    }

    sub _sysseek {
        my ($self, $position, $wence) = @_;
        $wence = defined $wence ? $wence : SEEK_SET;    # default
        if (not defined $handle{$self}) {
            $session{$self}->_client->_event(
                  q[file_error],
                  {File    => $self,
                   Message => q[Cannot set filehandle position: File not open]
                  }
            );
            return;
        }
        elsif (not defined $position) {

            # XXX - is this a param error or a file error?
            $session{$self}->_client->_event(
                                q[file_error],
                                {File    => $self,
                                 Message => q[Cannot seek: Undefined position]
                                }
            );
            return;
        }
        elsif (
             (   (($position < 0) and ($wence == SEEK_SET))
              or
              ((($position + $self->_systell()) < 0) and ($wence == SEEK_CUR))
             )
             or (($position > 0) and ($wence == SEEK_END))
            )
        {   $session{$self}->_client->_event(
                q[file_error],
                {File => $self,
                 Message =>
                     sprintf(q[Cannot seek beyond the start of file (0 > %d)],
                             $position)
                }
            );
            return;
        }
        elsif ((abs($position) > $size{$self})) {
            $session{$self}->_client->_event(
                   q[file_error],
                   {File    => $self,
                    Message => sprintf(
                                   q[Cannot seek beyond %s of file (%d > %d)],
                                   ($position > 0 ? q[start] : q[end]),
                                   $position, $size{$self}
                    )
                   }
            );
            return;
        }
        return sysseek($handle{$self}, $position, $wence);
    }

    sub _sysopen {
        my ($self, $mode) = @_;
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
                    CreateFileW(encode(q[UTF-16LE], $path{$self} . $null),
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
                open($handle{$self},
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

    sub _close {
        my ($self) = @_;
        return if not defined $mode{$self};

        #
        if (defined $win32_handle{$self} and require Win32API::File) {
            Win32API::File::CloseHandle($win32_handle{$self});
            delete $win32_handle{$self};
        }

        #
        my $return = CORE::close($handle{$self});

        #
        if ($return) {

            #
            delete $mode{$self};
            delete $handle{$self};

            #
            $session{$self}->_client->_event(q[file_close], {File => $self});

            #
            return $return;
        }

        #
        $session{$self}->_client->_event(
                            q[file_error],
                            {File    => $self,
                             Message => sprintf(q[Cannot close file: %s], $^E)
                            }
        );

        #
        return;
    }

    sub _mkpath {
        my ($self) = @_;
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
                    Win32::CreateDirectory($path);
                }
            }
            elsif (require File::Path) {
                File::Path::mkpath(File::Spec->catpath($vol, $dir, q[]),
                                   {verbose => 0});
            }
        }
        return 1;
    }

    #
    DESTROY {
        my ($self) = @_;

        #
        delete $path{$self};
        delete $session{$self};
        delete $size{$self};
        delete $priority{$self};
        delete $mode{$self};
        delete $index{$self};
        delete $handle{$self};
        delete $win32_handle{$self};

        #
        return 1;
    }
    1;

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

=item C<index ( )>

Returns the zero based index of this file according to the related
L<Net::BitTorrent::Session|Net::BitTorrent::Session> object's file
list.

=item C<mode ( )>

Returns a value representing if and how the related file handle is open.
Possible values:

    'ro'    - Read only
    'wo'    - Write only
    'rw'    - Read and Write
    undef   - Closed

=item C<path ( )>

Returns the absolute path of the related file.

=item C<priority ( )>

Returns the download priority of this file.

See also: L<set_priority ( )|"/set_priority ( NEWVAL )">

=item C<set_priority ( NEWVAL )>

Sets the download priority of this file.

By default, all files begin with a level two (2) priority with the
intent being on a C<0> (skip), C<1> (low), C<2> (normal), C<3> (high)
priority scale but you may use any scale you want.  For example, you
could set a file's priority to say... C<1,000,000>, leave everything
else at the default C<2> and and be positive we'll work on it first.
To avoid downloading this file, set priority to zero.

See also: L<priority ( )|/"priority ( )">

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

}
