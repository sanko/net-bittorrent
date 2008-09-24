package Net::BitTorrent::Session::Tracker::HTTP;
{
    use strict;      # core as of perl 5
    use warnings;    # core as of perl 5.006

    #
    use Carp qw[carp];                      # core as of perl 5
    use Scalar::Util qw[blessed weaken];    # core since perl 5.007003
    use List::Util qw[sum];                 # core since perl 5.007003
    use Socket                              # core as of perl 5
        qw[PF_INET SOMAXCONN SOCK_STREAM
        inet_aton pack_sockaddr_in
    ];
    use Fcntl qw[F_SETFL O_NONBLOCK];       # core as of perl 5

    #
    use lib q[../../../../../lib];
    use Net::BitTorrent::Util qw[:bencode uncompact];

    #
    my (%url, %tier);                              # param to new()
    my (%resolve, %event, %_socket, %_data_out);

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
        if (not $args->{q[Tier]}->isa(q[Net::BitTorrent::Session::Tracker])) {
            carp __PACKAGE__ . q[->new() requires a blessed Tracker 'Tier'];
            return;
        }

        #
        $self = bless \$args->{q[URL]}, $class;

        #
        $url{$self}  = $args->{q[URL]};
        $tier{$self} = $args->{q[Tier]};
        weaken $tier{$self};

        #
        return $self;
    }

    # Accesors | Private
    sub _socket { return $_socket{+shift}; }
    sub _tier   { return $tier{+shift}; }      # Make public?

    # Methods | Private
    sub _announce {
        my ($self, $event) = @_;
        if (defined $event) {
            if ($event !~ m[^(?:st(?:art|opp)|complet)ed$]) { # being silly...
                carp sprintf q[Invalid event for announce: %s], $event;
                return;
            }
        }
        $event{$self} = $event;

        #
        my ($host, $port, $path)
            = $url{$self} =~ m{^http://([^/:]*)(?::(\d+))?(/.*)$};
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
        socket($_socket{$self}, PF_INET, SOCK_STREAM, getprotobyname(q[tcp]))
            or return;    # TODO: re-Schedule announce

        # Set socket to non-blocking
        if (not($^O eq q[MSWin32]
                ? ioctl($_socket{$self}, 0x8004667e, pack(q[I], 1))
                : fcntl($_socket{$self}, F_SETFL, O_NONBLOCK)
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
        connect($_socket{$self}, pack_sockaddr_in($port, inet_aton($host)));

        #
        $tier{$self}->_client->_event(q[tracker_connect],
                                      {Tracker => $self,
                                       (defined $event
                                        ? (Event => $event)
                                        : ()
                                       )
                                      }
        );

        #
        my $infohash = $tier{$self}->_session->infohash;
        $infohash =~ s|(..)|\%$1|g;    # urlencode
        my %query_hash = (
               q[info_hash]  => $infohash,
               q[peer_id]    => $tier{$self}->_client->peerid,
               q[port]       => $tier{$self}->_client->_port,
               q[uploaded]   => $tier{$self}->_session->_uploaded,
               q[downloaded] => $tier{$self}->_session->_downloaded,
               q[left]       => (
                   $tier{$self}->_session->_piece_length * sum(
                       split(
                           q[], unpack(q[b*], $tier{$self}->_session->_wanted)
                       )
                   )
               ),
               q[key]        => $^T,
               q[numwant]    => 200,
               q[compact]    => 1,
               q[no_peer_id] => 1,
               (defined($event{$self})
                ? (q[event] => $event{$self})
                : ()
               )
        );
        my $url
            = $path
            . ($path =~ m[\?] ? q[&] : q[?])
            . (join q[&],
               map { sprintf q[%s=%s], $_, $query_hash{$_} }
                   sort keys %query_hash
            );
        $_data_out{$self} =
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
        $tier{$self}->_client->_remove_connection($self);
        return $tier{$self}->_client->_add_connection($self, q[wo]);
    }

    sub _rw {
        my ($self, $read, $write, $error) = @_;
        my ($actual_read, $actual_write) = (0, 0);

        #
        if ($error) {
            carp sprintf q[Error announce: (%d) %s], $^E, $^E;
            $tier{$self}->_client->_remove_connection($self);
            close $_socket{$self};

            # Reschedule announce
            $tier{$self}->_client->_schedule(
                                      {Time   => time + 30,
                                       Code   => sub { $self->_announce(); },
                                       Object => $self
                                      }
            );
            return;
        }
        if ($write) {
            $actual_write
                = syswrite($_socket{$self}, $_data_out{$self}, $write);
            if (not defined $actual_write) {
                $tier{$self}->_client->_event(
                      q[tracker_failure],
                      {Tracker => $self,
                       Message => sprintf(q[Cannot write to tracker: %s], $^E)
                      }
                );
                $tier{$self}->_client->_remove_connection($self);
                close $_socket{$self};

                #return;
                # Reschedule announce

                    $tier{$self}->_client->_schedule(
                                      {Time   => time + 30,
                                       Code   => sub { shift->_announce(); },
                                       Object => $self
                                      }
                    );
                    return ;
            }
            $tier{$self}->_client->_event(q[tracker_data_out],
                                 {Tracker => $self, Length => $actual_write});

            #
            substr($_data_out{$self}, 0, $actual_write, q[]);
            if (not length $_data_out{$self}) {
                $tier{$self}->_client->_remove_connection($self);
                $tier{$self}->_client->_add_connection($self, q[ro]);
            }
        }
        if ($read) {
            $actual_read = sysread($_socket{$self}, my ($data), $read, 0);

            #
            if (not $actual_read) {
                $tier{$self}->_client->_event(
                     q[tracker_failure],
                     {Tracker => $self,
                      Message => sprintf(q[Cannot read from tracker: %s], $^E)
                     }
                );
                $tier{$self}->_client->_remove_connection($self);
                close $_socket{$self};

                # Reschedule announce
                $tier{$self}->_client->_schedule(
                                      {Time   => time + 30,
                                       Code   => sub { shift->_announce(); },
                                       Object => $self
                                      }
                );
                return;
            }
            else {
                $tier{$self}->_client->_event(q[tracker_data_in],
                                  {Tracker => $self, Length => $actual_read});
                $data =~ s[^.+(?:\015?\012){2}][]s;
                $data = bdecode($data);
                if ($data) {
                    if (defined $data->{q[failure reason]}) {
                        $tier{$self}->_client->_event(
                                         q[tracker_announce_failure],
                                         {Tracker => $self,
                                          Reason => $data->{q[failure reason]}
                                         }
                        );
                    }
                    else {

                        # XXX - cache resolve
                        $tier{$self}->_session->_append_compact_nodes(
                                                           $data->{q[peers]});
                        $tier{$self}->_set_complete($data->{q[complete]});
                        $tier{$self}->_set_incomplete($data->{q[incomplete]});
                        $tier{$self}
                            ->_client->_event(q[tracker_announce_okay],
                                        {Tracker => $self, Payload => $data});
                    }
                }

                # Reschedule announce
                $tier{$self}->_client->_schedule(
                                      {Time => (
                                              time + (
                                                  defined $data->{q[interval]}
                                                  ? $data->{q[interval]}
                                                  : 1800
                                              )
                                       ),
                                       Code   => sub { shift->_announce(); },
                                       Object => $self
                                      }
                );
            }

            $tier{$self}->_client->_remove_connection($self);
            close $_socket{$self};
            $tier{$self}
                ->_client->_event(q[tracker_disconnect], {Tracker => $self});
        }
        else {
                $tier{$self}->_client->_event(
                                     q[tracker_announce_failure],
                                     {Tracker => $self,
                                      Reason => q[Failed to read from tracker]
                                     }
                );

                # Reschedule announce
                $tier{$self}->_client->_schedule(
                                      {Time   => time + 300,
                                       Code   => sub { shift->_announce(); },
                                       Object => $self
                                      }
                );
                return;
            }
        return ($actual_read, $actual_write);
    }

    #
    DESTROY {
        my ($self) = @_;

        #
        delete $tier{$self};
        delete $url{$self};
        delete $resolve{$self};
        delete $event{$self};
        delete $_socket{$self};

        #
        return 1;
    }

    #
    1;
}


=pod

=head1 NAME

Net::BitTorrent::Session::Tracker::HTTP - Single HTTP BitTorrent Tracker

=head1 Constructor

=over 4

=item C<new ( [ARGS] )>

Creates a C<Net::BitTorrent::Session::Tracker::HTTP> object.  This
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
