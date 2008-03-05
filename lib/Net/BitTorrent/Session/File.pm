{

    package Net::BitTorrent::Session::File;

    BEGIN {
        use vars qw[$VERSION];
        use version qw[qv];
        our $SVN = q[$Id$];
        our $VERSION = sprintf q[%.3f], version->new(qw$Rev$)->numify / 1000;
    }

    use strict;
    use warnings 'all';
    use Fcntl qw[/O_/ /SEEK/];
    use File::Spec;
    use Carp qw[carp croak];

    my (
        %size,           %path,      %index,
        %session,        %handle,    %unicode_filehandle,
        %open_timestamp, %open_mode, %touch_timestamp
    );
    {    # constructor

        sub new {

            my ($class, $args) = @_;
            my $self = undef;
            if (
                    caller->isa(q[Net::BitTorrent::Session])
                and defined $args->{q[session]}
                and $args->{q[session]}->isa(q[Net::BitTorrent::Session])
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
                $open_mode{$self} = q[c];
            }
            return $self;
        }

        sub size { return $size{+shift} }

        sub path {
            my $self = shift;
            return File::Spec->catfile($session{$self}->base_dir, $path{$self});
        }
        sub index   { $index{+shift} }
        sub session { return $session{+shift} }
        sub client  { return $session{+shift}->client }
        sub handle  { return $handle{+shift} }

        sub use_unicode_handle {
            return $unicode_filehandle{+shift};
        }    # used only on win32
        sub open_timestamp  { return $open_timestamp{+shift} }
        sub open_mode       { return $open_mode{+shift} }
        sub touch_timestamp { return $touch_timestamp{+shift} }

        sub open {
            my ($self, $mode) = @_;
            if ($mode !~ m[^[rw]$]) {
                $self->client->do_callback(q[file_error], $self,
                    sprintf(q[Bad open mode: %s], $mode));
                return;
            }

            if ($handle{$self} and $open_mode{$self}) {
                if ($mode eq $open_mode{$self}) {
                    return 1;
                }
                else {
                    $self->sysclose;
                }
            }

            {    # TODO: Is File::Spec unicode safe?
                my ($vol, $dir, $file) = File::Spec->splitpath($self->path);
                if (not -d File::Spec->catpath($vol, $dir, q[]))
                {    # TODO: -d is certainly not safe :(
                    if ($mode =~ m[w]) {
                        require File::Path;
                        my @created = File::Path::mkpath(
                            File::Spec->catpath($vol, $dir, q[]),
                            {verbose => $Net::BitTorrent::DEBUG}
                        ) or return;
                        grep { print q[mkpath created $_\n] } @created
                          if $Net::BitTorrent::DEBUG;
                    }
                }
            }

            if (
                $self->client->use_unicode
                ? do {
                    $self->sysopen(
                        (
                            $mode eq q[r]
                            ? O_RDONLY
                            : O_WRONLY | O_CREAT
                        ),
                        0777
                    );
                }
                : do {
                    sysopen(
                        $handle{$self},
                        $self->path,
                        (
                            $mode eq q[r]
                            ? O_RDONLY
                            : O_WRONLY | O_CREAT
                        ),
                        0777
                    );
                }
              )
            {
                $open_mode{$self}      = $mode;
                $open_timestamp{$self} = time;
                $self->client->do_callback(q[file_open], $self);
                return 1;
            }
            return;
        }

        sub seek {
            my ($self, $position) = @_;
            if (not $handle{$self}) {
                $self->client->do_callback(q[file_error], q[File not open]);
            }
            elsif ($position > $size{$self}) {
                $self->client->do_callback(
                    q[file_error],
                    sprintf(
                        q[Cannot seek beyond end of file (%d > %d)],
                        $position, $size{$self}
                    )
                );
            }
            else {
                return sysseek($handle{$self}, $position, SEEK_SET);
            }
        }

        sub systell { sysseek($_[0]->handle, 0, SEEK_CUR) }

        sub read {
            my ($self, $length) = @_;
            my $data = q[];
            if (not $handle{$self}) {
                $self->client->do_callback(q[file_error], q[File not open]);
            }
            elsif ($open_mode{$self} ne q[r]) {
                $self->client->do_callback(q[file_error],
                    q[File not open for read]);
            }
            elsif ($self->systell + $length > $size{$self}) {
                $self->client->do_callback(q[file_error],
                    q[Cannot read beyond end of file]);
            }
            else {
                truncate($handle{$self}, $size{$self})
                  if -s $handle{$self} != $size{$self};
                my $real_length = sysread($handle{$self}, $data, $length);
                if ($real_length or ($real_length == $length)) {
                    $touch_timestamp{$self} = time;
                    $self->client->do_callback(q[file_read], $self,
                        $real_length);
                }
            }

            return $data;
        }

        sub write {
            my ($self, $data) = @_;
            my $real_length;
            if (not $handle{$self}) {
                $self->client->do_callback(q[file_error], q[File not open]);
            }
            elsif ($open_mode{$self} ne q[w]) {
                $self->client->do_callback(q[file_error],
                    q[File not open for write]);
            }
            elsif ($self->systell + length($data) > $size{$self}) {
                $self->client->do_callback(q[file_error],
                    q[Cannot write beyond end of file]);
            }
            else {
                truncate($handle{$self}, $size{$self})
                  if -s $handle{$self} != $size{$self};
                $real_length = syswrite($handle{$self}, $data, length($data));
                if (not defined $real_length or $real_length != length($data)) {
                    $touch_timestamp{$self} = time;
                    $self->client->do_callback(q[file_write], $self,
                        $real_length);
                }
            }
            return $real_length;
        }

        sub close {
            my ($self) = @_;
            $self->session->client->use_unicode
              ? do { $self->sysclose($handle{$self}) }
              : do { CORE::close($handle{$self}) };
            delete $handle{$self};
            delete $open_mode{$self};
            $self->client->do_callback(q[file_close], $self);
            return 1;
        }

        my %piece_range;

        sub piece_range {    # cache this only when needed
            my ($self) = @_;
            if (not defined $piece_range{$self}) {
                my $offset = 0;
                for my $_index (0 .. $self->index - 1) {
                    $offset += $self->session->files->[$_index]->size;
                }
                $piece_range{$self} = [
                    int($offset / $self->session->piece_size),
                    int(($offset + $size{$self}) / $self->session->piece_size)
                ];
            }
            return $piece_range{$self};
        }

        sub pieces {
            my ($self) = @_;
            if (not defined $piece_range{$self}) { $self->piece_range }
            return
              map { $self->session->pieces->[$_] }
              ($piece_range{$self}[0] .. $piece_range{$self}[1]);
        }

=pod

=begin future

With this snippet, you can set the priority of any file to anything
you want.  By default, all pieces begin with a level two priority
with the intent being on a 0 (skip), 1 (low), 2 (normal), 3 (high)
priority scale.  You could set a file's priority to say... 1,000,000
and be positive we'll work on it first.

NOTE: Setting the priority to zero will tell C<Net::BitTorrrent> not
to bother requesting these pieces but the file will still be created
on disk if a piece we want overlaps onto this file.  Just give me
some time to work on an intermediate .piece file and this problem
will go away. I may put an improved version of the above code into
L<Net::BitTorrent::Session> when I start working on the rough edges.

=end future

=cut

        sub priority {
            my ($self, $value) = @_;

            # TODO: if $value == 0, we shouldn't write to this file
            # TODO: let's not set the priority for the piece if it overlaps
            return (
                defined $value
                ? do {
                    croak(q[priority is malformed]) and return
                      unless $value =~ m[^\d+$];
                    map { $_->priority($value) } $self->pieces;
                  }
                : map { $_->priority } $self->pieces
            );
        }

        sub as_string {
            my ($self, $advanced) = @_;
            my @values = (
                $index{$self},
                (q[=] x (25 + length($index{$self}))),
                $self->path,
                $size{$self},
                (
                    (
                        (
                            (scalar grep { $_->check } $self->pieces) /
                              (scalar $self->pieces)
                        )
                    ) * 100
                ),
                @{$self->piece_range}
            );

            s/(^[-+]?\d+?(?=(?>(?:\d{3})+)(?!\d))|\G\d{3}(?=\d))/$1,/g
              for @values[3,];    # no 'better way' warning...

            my $dump = sprintf(<<'END', @values);
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

        sub sysopen {
            my ($self, $mode, $perms) = @_;

           # Unicode, and other extended charsets are a pain...
           # [http://groups.google.com/group/perl.unicode/msg/86ab5af239975df7],
           # [id://538097], [id://229642], [id://445883], and others
            if ($self->session->client->use_unicode
                and ($^O eq q[MSWin32] and (utf8::is_utf8($self->path))))
            {
                require Win32API::File;
                Win32API::File->import(qw[:ALL]);
                require Encode;
                Encode->import(qw[find_encoding encode]);
                my $h = CreateFileW(
                    encode(q[UTF-16LE], $self->path . qq[\0]),
                    (
                        ($mode &= O_WRONLY)
                        ? (
                            GENERIC_WRITE(),
                            FILE_SHARE_READ() | FILE_SHARE_WRITE(),
                            [], OPEN_ALWAYS()
                          )
                        : (
                            GENERIC_READ(),
                            FILE_SHARE_READ() | FILE_SHARE_WRITE(),
                            [], OPEN_EXISTING()
                        )
                    ),
                    0,
                    []
                );
                my $fd = OsFHandleOpenFd($h, $mode);
                return if $fd < 0;
                $self->use_unicode_handle($h);
                return CORE::open($handle{$self},
                    (($mode &= O_WRONLY) ? q[>&] : q[<&]), $fd);
            }
            CORE::sysopen($handle{$self}, $self->path, $mode, $perms);
        }

        sub sysclose {
            my ($self) = @_;
            if ($self->session->client->use_unicode
                and defined $self->use_unicode_handle)
            {
                require Win32API::File;
                Win32API::File->import(qw[:ALL]);
                my $OS_FH = CloseHandle($self->use_unicode_handle);
                delete $unicode_filehandle{$self};
            }
            return CORE::close($handle{$self})
              and delete $handle{$self}
              and $open_mode{$self} = q[c];
        }
    }
    1;
}
