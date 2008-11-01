#!C:\perl\bin\perl.exe
package Net::BitTorrent::Torrent::Tracker::HTTP;
{
    use strict;      # core as of perl 5
    use warnings;    # core as of perl 5.006

    #
    use Carp qw[carp];                              # core as of perl 5
    use Scalar::Util qw[blessed weaken refaddr];    # core since perl 5.007003
    use List::Util qw[sum];                         # core since perl 5.007003
    use Socket                                      # core as of perl 5
        qw[PF_INET SOMAXCONN SOCK_STREAM
        inet_aton pack_sockaddr_in
    ];
    use Fcntl qw[F_SETFL O_NONBLOCK];               # core as of perl 5

    #
    use lib q[../../../../../lib];
    use Net::BitTorrent::Util qw[:bencode uncompact];

    #
    use version qw[qv];                             # core as of 5.009
    our $SVN = q[$Id$];
    our $UNSTABLE_RELEASE = 0; our $VERSION = sprintf(($UNSTABLE_RELEASE ? q[%.3f_%03d] : q[%.3f]), (version->new((qw$Rev$)[1])->numify / 1000), $UNSTABLE_RELEASE);

    #
    my (@CONTENTS) = \my (
        %url, %_tier,                               # param to new()
        %resolve, %event, %_socket, %_data_out);
    my %REGISTRY;

    #
    sub new {
        my ($class, $args) = @_;
        my $self;
        if (not defined $args) {
            carp __PACKAGE__ . q[->new() requires params];
            return;
        }
        if (not defined $args->{q[URL]}) {
            carp __PACKAGE__ . q[->new() requires a 'URL' param];
            return;
        }
        if ($args->{q[URL]} !~ m[^http://]i) {
            carp
                sprintf(
                  q[%s->new() doesn't know what to do with malformed url: %s],
                  __PACKAGE__, $args->{q[URL]});
            return;
        }
        if (not defined $args->{q[Tier]}) {
            carp __PACKAGE__ . q[->new() requires a 'Tier' param];
            return;
        }
        if (not $args->{q[Tier]}->isa(q[Net::BitTorrent::Torrent::Tracker])) {
            carp __PACKAGE__ . q[->new() requires a blessed Tracker 'Tier'];
            return;
        }

        #
        $self = bless \$args->{q[URL]}, $class;

        #
        $url{refaddr $self}   = $args->{q[URL]};
        $_tier{refaddr $self} = $args->{q[Tier]};
        weaken $_tier{refaddr $self};

        # threads stuff...
        weaken($REGISTRY{refaddr $self} = $self);

        #share($bitfield{refaddr $self}); # allows non-blocking hashcheck
        #
        return $self;
    }

    # Accesors | Private
    sub _socket { return $_socket{refaddr +shift}; }
    sub _tier   { return $_tier{refaddr +shift}; }     # Make public?

    # Methods | Private
    sub _announce {
        my ($self, $event) = @_;

        #warn sprintf q[%s | %s_announce(%s)], scalar(localtime), $self,
        #    ($event || q[]);
        if (defined $event) {
            if ($event !~ m[^(?:st(?:art|opp)|complet)ed$]) { # don't be silly
                carp sprintf q[Invalid event for announce: %s], $event;
                return;
            }
        }
        $event{refaddr $self} = $event;

        #
        my ($host, $port, $path)
            = $url{refaddr $self} =~ m{^http://([^/:]*)(?::(\d+))?(/.*)$};
        $port = $port ? $port : 80;

        # Resolve hostname
        my $packed_host = undef;
        if ($host
            !~ m[^(?:(?:(?:25[0-5]|2[0-4][0-9]|[0-1]?[0-9]{1,2})[.]?){4})$])
        {    # it's not an IPv4 so it may be a hostname
                # No point resolving an IP address.  Right?
            my ($name, $aliases, $addrtype, $length, @addrs)
                = gethostbyname($host)
                or return;
            $packed_host = $addrs[0];
        }
        else { $packed_host = inet_aton($host) }
        socket($_socket{refaddr $self},
               PF_INET, SOCK_STREAM, getprotobyname(q[tcp]))
            or return;    # TODO: re-Schedule announce

        # Set socket to non-blocking
        if (not($^O eq q[MSWin32]
                ? ioctl($_socket{refaddr $self}, 0x8004667e, pack(q[I], 1))
                : fcntl($_socket{refaddr $self}, F_SETFL, O_NONBLOCK)
            )
            )
        {                 # !!! - impossible to test failure for coverage...
            carp
                q[There was a problem making an outgoing socket non-blocking: ]
                . $^E;
            return;       # TODO: re-Schedule announce
        }

        # Here, connect() is non-blocking so it doesn't return a valid
        # value to test agiainst.  We see how things are progressing by
        # checking $^E further down.
        connect($_socket{refaddr $self},
                pack_sockaddr_in($port, inet_aton($host)));

        #
        $_tier{refaddr $self}->_client->_event(q[tracker_connect],
                                               {Tracker => $self,
                                                (defined $event
                                                 ? (Event => $event)
                                                 : ()
                                                )
                                               }
        );

        #
        my $infohash = $_tier{refaddr $self}->_torrent->infohash;
        $infohash =~ s|(..)|\%$1|g;    # urlencode
        my %query_hash = (
              q[info_hash]  => $infohash,
              q[peer_id]    => $_tier{refaddr $self}->_client->peerid(),
              q[port]       => $_tier{refaddr $self}->_client->_port(),
              q[uploaded]   => $_tier{refaddr $self}->_torrent->_uploaded(),
              q[downloaded] => $_tier{refaddr $self}->_torrent->_downloaded(),
              q[left]       => (
                   $_tier{refaddr $self}->_torrent->_piece_length() * sum(
                       split(q[],
                             unpack(
                                   q[b*],
                                   ($_tier{refaddr $self}->_torrent->_wanted()
                                        || q[]
                                   )
                             )
                       )
                   )
              ),
              q[key]        => $^T,
              q[numwant]    => 200,
              q[compact]    => 1,
              q[no_peer_id] => 1,
              (defined($event{refaddr $self})
               ? (q[event] => $event{refaddr $self})
               : ()
              )
        );
        #use Data::Dump qw[pp];
        #warn pp \%query_hash;
        my $url
            = $path
            . ($path =~ m[\?] ? q[&] : q[?])
            . (join q[&],
               map { sprintf q[%s=%s], $_, $query_hash{$_} }
                   sort keys %query_hash
            );
        $_data_out{refaddr $self} =
            join(qq[\015\012],
                 qq[GET $url HTTP/1.0],
                 q[Connection: close],
                 qq[Host: $host:$port],
                 q[Accept: text/plain],
                 q[Accept-Encoding:],
                 qq[User-Agent: Net::BitTorrent/] . $Net::BitTorrent::VERSION,
                 q[],
                 q[]);

        #
        $_tier{refaddr $self}->_client->_remove_connection($self);
        return $_tier{refaddr $self}->_client->_add_connection($self, q[wo]);
    }

    sub _rw {
        my ($self, $read, $write, $error) = @_;
        my ($actual_read, $actual_write) = (0, 0);
        return if not defined $_tier{refaddr $self}->_client;

        #
        if ($error) {
            carp sprintf q[Error announce: (%d) %s], $^E, $^E;
            $_tier{refaddr $self}->_client->_remove_connection($self);
            close $_socket{refaddr $self};

            # Reschedule announce
            $_tier{refaddr $self}->_client->_schedule(
                {   Time => time + 30,
                    Code => sub {
                        my ($s) = @_;
                        $_tier{$s}->_shuffle;
                        return $_tier{$s}->_announce();
                    },
                    Object => $self
                }
            );
            return;
        }
        if ($write) {
            $actual_write = syswrite($_socket{refaddr $self},
                                     $_data_out{refaddr $self}, $write);
            if (not defined $actual_write) {
                $_tier{refaddr $self}->_client->_event(
                      q[tracker_failure],
                      {Tracker => $self,
                       Message => sprintf(q[Cannot write to tracker: %s], $^E)
                      }
                );
                $_tier{refaddr $self}->_client->_remove_connection($self);
                close $_socket{refaddr $self};

                #return;
                # Reschedule announce
                $_tier{refaddr $self}->_client->_schedule(
                    {   Time => time + 30,
                        Code => sub {
                            my ($s) = @_;
                            $s->_tier->_shuffle;
                            return $s->_tier->_announce();
                        },
                        Object => $self
                    }
                );
                return;
            }
            $_tier{refaddr $self}->_client->_event(q[tracker_data_out],
                                 {Tracker => $self, Length => $actual_write});

            #
            substr($_data_out{refaddr $self}, 0, $actual_write, q[]);
            if (not length $_data_out{refaddr $self}) {
                $_tier{refaddr $self}->_client->_remove_connection($self);
                $_tier{refaddr $self}->_client->_add_connection($self, q[ro]);
            }
        }
        if ($read) {
            $actual_read
                = sysread($_socket{refaddr $self}, my ($data), $read, 0);

            #
            if (not $actual_read) {
                $_tier{refaddr $self}->_client->_event(
                     q[tracker_failure],
                     {Tracker => $self,
                      Message => sprintf(q[Cannot read from tracker: %s], $^E)
                     }
                );
                $_tier{refaddr $self}->_client->_remove_connection($self);
                close $_socket{refaddr $self};

                # Reschedule announce
                $_tier{refaddr $self}->_client->_schedule(
                    {   Time => time + 30,
                        Code => sub {
                            my ($s) = @_;
                            $_tier{refaddr $s}->_shuffle;
                            return $_tier{refaddr $s}->_announce();
                        },
                        Object => $self
                    }
                );
                return;
            }
            else {
                $_tier{refaddr $self}->_client->_event(q[tracker_data_in],
                                  {Tracker => $self, Length => $actual_read});
                $data =~ s[^.+(?:\015?\012){2}][]s;
                $data = bdecode($data);
                if ($data) {
                    if (defined $data->{q[failure reason]}) {
                        $_tier{refaddr $self}->_client->_event(
                                         q[tracker_announce_failure],
                                         {Tracker => $self,
                                          Reason => $data->{q[failure reason]}
                                         }
                        );
                    }
                    else {

                        # XXX - cache resolve
                        $_tier{refaddr $self}
                            ->_torrent->_append_compact_nodes(
                                                           $data->{q[peers]});
                        $_tier{refaddr $self}
                            ->_set_complete($data->{q[complete]});
                        $_tier{refaddr $self}
                            ->_set_incomplete($data->{q[incomplete]});
                        $_tier{refaddr $self}
                            ->_client->_event(q[tracker_announce_okay],
                                        {Tracker => $self, Payload => $data});
                    }
                }

                # Reschedule announce
                $_tier{refaddr $self}->_client->_schedule(
                    {   Time => (time + (defined $data->{q[interval]}
                                         ? $data->{q[interval]}
                                         : 1800
                                 )
                        ),
                        Code =>
                            sub { return $_tier{refaddr +shift}->_announce() }
                        ,
                        Object => $self
                    }
                );
            }
            $_tier{refaddr $self}->_client->_remove_connection($self);
            close $_socket{refaddr $self};
            $_tier{refaddr $self}
                ->_client->_event(q[tracker_disconnect], {Tracker => $self});
        }
        else {
            $_tier{refaddr $self}->_client->_event(
                                     q[tracker_announce_failure],
                                     {Tracker => $self,
                                      Reason => q[Failed to read from tracker]
                                     }
            );

            # Reschedule announce
            $_tier{refaddr $self}->_client->_schedule(
                  { Time => time + 300,
                    Code =>
                        sub { return $_tier{refaddr +shift}->_announce(); },
                    Object => $self
                  }
            );
            return;
        }
        return ($actual_read, $actual_write);
    }

    sub _as_string {
        my ($self, $advanced) = @_;
        my $dump = q[TODO];
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

            # do some silly stuff to avoid user mistakes
            weaken $_tier{$_nID};

            #  update he weak refernce to the new, cloned object
            weaken($REGISTRY{$_nID} = $_obj);
            delete $REGISTRY{$_oID};
        }
        return 1;
    }

    # Destructor
    DESTROY {
        my ($self) = @_;

        #warn q[Goodbye, ] . $$self;
        # Clean all data
        for (@CONTENTS) {
            delete $_->{refaddr $self};
        }
        delete $REGISTRY{refaddr $self};

        #
        return 1;
    }

    #
    1;
}

=pod

=head1 NAME

Net::BitTorrent::Torrent::Tracker::HTTP - Single HTTP BitTorrent Tracker

=head1 Constructor

=over 4

=item C<new ( [ARGS] )>

Creates a C<Net::BitTorrent::Torrent::Tracker::HTTP> object.  This
constructor should not be used directly.

=back

=head1 BUGS/TODO

=over 4

=item *

Does not support HTTPS trackers.

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
