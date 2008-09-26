#!/usr/bin/perl -w
use strict;
use warnings;
use Module::Build;
#
use lib q[../../../../../lib];
$|++;

# let's keep track of where we are...
my $test_builder = Test::More->builder;

#
my $simple_dot_torrent = q[./t/900_data/950_torrents/953_miniswarm.torrent];
my $multi_dot_torrent  = q[./t/900_data/950_torrents/952_multi.torrent];
my $single_dot_torrent = q[./t/900_data/950_torrents/951_single.torrent];

# Make sure the path is correct
chdir q[../../../../../] if not -f $simple_dot_torrent;
#

my $build = Module::Build->current;
my $can_talk_to_ourself = $build->notes(q[can_talk_to_ourself]);

#
BEGIN {
    use Test::More;
    plan tests => 140;
    use_ok(q[File::Temp], qw[tempfile tempdir]);
    use_ok(q[File::Spec]);
    use_ok(q[Net::BitTorrent::Session::File]);
    use_ok(q[Net::BitTorrent::Session]);
    use_ok(q[Net::BitTorrent]);
}

#
{
    my $client = Net::BitTorrent->new();
    my $session =
        Net::BitTorrent::Session->new({Client => $client,
                                       Path   => $single_dot_torrent
                                      }
        );

    #
    ok( $client->on_event(
            q[file_error],
            sub {
                my ($self, $args) = @_;
                pass(sprintf q[File error: %s], $args->{q[Message]});
                return 1;
            }
        ),
        q[Installed 'file_error' event handler]
    );
    ok( $client->on_event(
            q[file_close],
            sub {
                my ($self, $args) = @_;
                pass(q[Closed file]);
                return 1;
            }
        ),
        q[Installed 'file_close' event handler]
    );
    ok( $client->on_event(
            q[file_open],
            sub {
                my ($self, $args) = @_;
                pass(
                    sprintf(q[Opened file for %s],
                            (($args->{q[Mode]} eq q[r]) ? q[read] : q[write]))
                );
                return 1;
            }
        ),
        q[Installed 'file_open' event handler]
    );
    ok( $client->on_event(
            q[file_read],
            sub {
                my ($self, $args) = @_;
                pass(sprintf q[Read %d bytes from file], $args->{q[Length]});
                return 1;
            }
        ),
        q[Installed 'file_read' event handler]
    );

    #
    diag(q[Testing Net::BitTorrent::Session::File]);
    my ($tempdir)
        = tempdir(q[~NBSF_test_XXXXXXXX], CLEANUP => 1, TMPDIR => 1);
    my ($filehandle, $filename) = tempfile(DIR => $tempdir);
    diag(sprintf(q[   File::Temp created '%s' for us to play with], $filename)
    );

    #
    diag(q[Net::BitTorrent::Session::File->new() requires parameters...]);
    is(Net::BitTorrent::Session::File->new(),
        undef, q[Net::BitTorrent::Session::File->new( )]);
    is( Net::BitTorrent::Session::File->new(Path => $filename),
        undef,
        sprintf(q[Net::BitTorrent::Session::File->new(Path => q[%s])],
                $filename)
    );
    is(Net::BitTorrent::Session::File->new({}),
        undef, q[Net::BitTorrent::Session::File->new({ })]);
    is(Net::BitTorrent::Session::File->new({Path => $filename}),
        undef, q[Missing Session]);
    is( Net::BitTorrent::Session::File->new({Path    => $filename,
                                             Session => 0
                                            }
        ),
        undef,
        q[Session => 0]
    );
    is( Net::BitTorrent::Session::File->new(
                        {Path    => $filename,
                         Session => bless(\{}, q[Net::BitTorrent::Session])
                        }
        ),
        undef,
        q[Missing Size]
    );
    is( Net::BitTorrent::Session::File->new({Path    => $filename,
                                             Session => $session,
                                             Size    => undef
                                            }
        ),
        undef,
        q[Size => undef]
    );
    is( Net::BitTorrent::Session::File->new({Path    => $filename,
                                             Session => $session,
                                             Size    => q[QQQ]
                                            }
        ),
        undef,
        q[Size => q[QQQ]],
    );
    is( Net::BitTorrent::Session::File->new({Path    => $filename,
                                             Session => $session,
                                             Size    => -1024
                                            }
        ),
        undef,
        q[Size => -1024]
    );
    is( Net::BitTorrent::Session::File->new({Path    => $filename,
                                             Session => $session,
                                             Size    => 1024
                                            }
        ),
        undef,
        q[Missing Index]
    );
    is( Net::BitTorrent::Session::File->new({Path    => $filename,
                                             Session => $session,
                                             Size    => 1024,
                                             Index   => undef,
                                            }
        ),
        undef,
        q[Index => undef]
    );
    is( Net::BitTorrent::Session::File->new(
                       {Path    => $filename,
                        Session => bless(\{}, q[Net::BitTorrent::Session]),
                        Size    => 1024,
                        Index   => -1
                       }
        ),
        undef,
        q[Index => -1]
    );
    is( Net::BitTorrent::Session::File->new({Path    => $filename,
                                             Session => $session,
                                             Size    => 1024,
                                             Index   => q[AAA]
                                            }
        ),
        undef,
        q[Index => 'AAA']
    );
    is( Net::BitTorrent::Session::File->new({Path    => $filename,
                                             Session => $session,
                                             Size    => 1024,
                                             Index   => \0
                                            }
        ),
        undef,
        q[Index => 1]
    );

    #
    my $file =
        Net::BitTorrent::Session::File->new({Path    => $filename,
                                             Session => $session,
                                             Size    => 1024,
                                             Index   => 1
                                            }
        );
    isa_ok($file, q[Net::BitTorrent::Session::File], q[Path => ] . $filename);
    diag(q[Check all sorts of stuff...]);
    is($file->priority, 2, q[   ...priority() defaults to 2]);
    is($file->set_priority(), undef,
        q[   ...set_priority() requires a parameter]);
    ok($file->set_priority(3), q[   ...set_priority(3) works]);
    is($file->set_priority(-3), undef, q[   ...set_priority(-3) does not]);
    is($file->set_priority(q[random]),
        undef, q[   ...set_priority('random') doesn't either]);
    is($file->priority, 3, q[   ...priority() is now 3]);

    #
    is_deeply($file->session(), $session, q[Session is correct]);
    is($file->index(), 1,    q[Index is okay]);
    is($file->size(),  1024, q[Size is okay]);
    ok($file->path() eq $filename, q[Path is correct]);

    #
    is($file->mode(),          undef, q[Closed file has no mode]);
    is($file->_open(),         undef, q[_open requires a mode]);
    is($file->mode(),          undef, q[Closed file has no mode]);
    is($file->_open(q[What?]), undef, q[But not just anything...]);
    is($file->mode(),          undef, q[Closed file has no mode]);

    #
    ok(close($filehandle),  q[...close temp file to test _mkpath]);
    ok(unlink($file->path), q[...unlink temp file to test _mkpath]);
    my ($vol, $dir, undef) = File::Spec->splitpath($file->path);
    ok(rmdir(File::Spec->catdir($vol, $dir)),
        q[...rmdir temp dir to test _mkpath]);
    ok($file->_mkpath(), q[_mkpath is okay]);

    #
    is($file->_open(q[r]), undef, q[A file must be pre-existing to read]);
    is($file->mode(),      undef, q[Mode is still undef]);
    ok($file->_open(q[w]), q['w' opens the file for write]);
    is($file->mode(), q[w], q[Mode is now 'w']);
    ok($file->_open(q[w]), q[Reopening a file with the same mode...]);
    is($file->mode(), q[w], q[...does nothing]);
    ok($file->_open(q[r]), q['r' opens the file for read]);
    is($file->mode(), q[r], q[Mode is now 'r']);
    ok($file->_open(q[r]), q['r' opens the file for read]);
    is($file->mode(), q[r], q[Mode is now 'r']);
    ok($file->_close(), q[Close the file]);
    is($file->mode(), undef, q[Closed file has no mode]);

    #
    is($file->_write(q[Test]), undef, q[Cannot write to a closed file]);
    ok($file->_open(q[r]), q[  ...opening file for read]);
    is($file->_write(q[Test]),
        undef, q[Cannot write to a file opened for read]);
    ok($file->_close(), q[Close the file]);
    is($file->_write(q[Test]), undef,
        q[Cannot write to a file that's closed]);
    ok($file->_open(q[w]), q[  ...opening file for write]);
    is($file->_write(q[Test]),
        4, q[Can only write to a file opened for write]);
    is($file->_systell(), 4, q[we are now on the 4th byte of the file.]);
    ok($file->_sysseek(0), q[ ...seeking to the start]);
    is(int($file->_systell()), 0, q[ ...seeking to the start]);   # 0 but true
    is($file->_read(4), undef, q[Cannot read from a write handle]);
    ok($file->_open(q[r]), q[So we must open the file in read mode...]);
    is($file->_read(4), q[Test], q[...to get what we need.]);
    is($file->_systell(), 4, q[we are now on the 4th byte of the file.]);

    #
    ok($file->_open(q[w]), q[  ...opening file for write]);
    is($file->_write(q[A] x ($file->size + 1)),
        undef, q[Cannot write beyond end of file]);
    is(int($file->_systell()), 0, q[We are still on the first byte.])
        ;                                                         # 0 but true
    is($file->_sysseek($file->size + 1),
        undef, q[Cannot seek beyond end of file]);
    is(int($file->_systell()), 0, q[We are still on the first byte.])
        ;                                                         # 0 but true
    ok($file->_close(), q[Close the file]);
    is($file->_systell(), undef, q[Cannot seek on a closed file]);
    diag(q[TODO: systell wence param]);

    #
    ok($file->_open(q[r]), q['r' opens the file for read]);
    is($file->mode(),      q[r],  q[Mode is now 'r']);
    is($file->_read(),     undef, q[Read requires a length]);
    is($file->_read(q[V]), undef, q[Read requires a numeric length...]);
    ok($file->_open(q[w]), q['w' opens the file for write]);
    is($file->mode(),   q[w],  q[Mode is now 'w']);
    is($file->_write(), undef, q[Write requires data]);
    ok($file->_mkpath, q[mkpath]);

    #
    diag(q[Testing utf8 handling...]);
SKIP: {
        skip(sprintf(q[Requires perl 5.8.1 or better; you have v%vd], $^V),
             10)
            if sprintf(q[%vd], $^V) lt q[5.08.01];
        my $utf8_file =
            Net::BitTorrent::Session::File->new(
                {Path =>
                     File::Spec->catfile(
                        $tempdir, q[three], q[dirs], q[deep], qq[\x{4EAC}.tmp]
                     ),
                 Session => $session,
                 Size    => 1024,
                 Index   => 1
                }
            );
        isa_ok($utf8_file,
               q[Net::BitTorrent::Session::File],
               q[Filename with wide char]);

        #
        like($utf8_file->path, qr[\x{4EAC}\.tmp$], q[Path is correct]);
        ok($utf8_file->_mkpath,     q[mkpath]);
        ok($utf8_file->_open(q[w]), q[  ...opening file for write]);
        is($utf8_file->_write(q[A] x ($file->size + 1)),
            undef, q[Cannot write beyond end of file]);
        is(int($utf8_file->_systell()), 0,
            q[We are still on the first byte.]);    # 0 but true
        is($utf8_file->_sysseek($file->size + 1),
            undef, q[Cannot seek beyond end of file]);
        is(int($utf8_file->_systell()), 0,
            q[We are still on the first byte.]);    # 0 but true
        ok($utf8_file->_open(q[r]),       q[  ...opening file for read]);
        ok(int($utf8_file->_sysseek(30)), q[Move to the 30th byte.]);
        is($utf8_file->_read($file->size),
            undef, q[Cannot read beyond end of file]);
        is(int($utf8_file->_systell()), 30,
            q[We are still on the 30th byte.]);     # 0 but true
        is($utf8_file->_sysseek(), undef, q[Seek needs a new position]);
        ok($utf8_file->_sysseek(0), q[Seek to start of file]);
        ok($utf8_file->_sysseek(1), q[Seek to first byte of file]);
        is($utf8_file->_sysseek($file->size + 1),
            undef, q[Cannot seek beyond end of file]);
        ok($utf8_file->_close(), q[Close the file]);
        is($utf8_file->_systell(), undef,
            q[Cannot get position on a closed file]);
        is($utf8_file->_sysseek(2),
            undef, q[Cannot set position on a closed file]);
        is($utf8_file->_read(15), undef, q[Cannot read from closed file]);

        #
        diag(q[TODO: check if file actually exists]);
    }
}
