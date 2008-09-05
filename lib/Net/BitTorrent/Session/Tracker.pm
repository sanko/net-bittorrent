package Net::BitTorrent::Session::Tracker;
{
    use strict;      # core as of perl 5
    use warnings;    # core as of perl 5.006

    #
    use Carp qw[confess];                      # core as of perl 5
    use Scalar::Util qw[blessed weaken];    # core as of 5.007003
    use List::Util qw[shuffle];             # core as of 5.007003

    #
    use version qw[qv];                     # core as of 5.009
    our $SVN = q[$Id$];
    our $VERSION = sprintf q[%.3f], version->new(qw$Rev: 24 $)->numify / 1000;

    #
    use Net::BitTorrent::Session::Tracker::HTTP;
    use Net::BitTorrent::Session::Tracker::UDP;

    #
    my (%session,  %urls);                  # params to new\
    my (%complete, %incomplete);

    #
    sub new {

        # Creates a new N::B::Session object
        # Accepts parameters as key/value pairs in a hash reference
        # Required parameters:
        #  - Client  (Net::BitTorrent object)
        #  - URLs    (list of urls)
        # Returns
        #    - a new blessed object on success
        #    - undef on failure
        # MO
        # - validate incoming parameters
        # - shuffle list of URLs
        # - bless object
        # - set basic data (urls, client)
        # -
        # -
        # - return $self
        my ($class, $args) = @_;
        my $self;

        # Param validation... Ugh...
        if (not defined $args) {
            confess q[Net::BitTorrent::Session::Tracker->new({}) requires ]
                . q[parameters a set of parameters];
            return;
        }
        if (ref($args) ne q[HASH]) {
            confess q[Net::BitTorrentS::Session::Tracker->new({}) requires ]
                . q[parameters to be passed as a hashref];
            return;
        }
        if (not defined $args->{q[URLs]}) {
            confess q[Net::BitTorrent::Session::Tracker->new({}) requires a ]
                . q['URLs' parameter];
            return;
        }
        if (ref $args->{q[URLs]} ne q[ARRAY]) {
            confess q[Net::BitTorrent::Session::Tracker->new({}) requires a ]
                . q[list of URLs];
            return;
        }
        if (not scalar(@{$args->{q[URLs]}})) {
            confess q[Net::BitTorrent::Session::Tracker->new({}) doesn't (yet) ]
                . q[know what to do with an empty list of URLs];
            return;
        }
        if (not defined $args->{q[Session]}) {
            confess q[Net::BitTorrent::Session::Tracker->new({}) requires a ]
                . q['Session' parameter];
            return;
        }
        if (not blessed $args->{q[Session]}) {
            confess q[Net::BitTorrent::Session::Tracker->new({}) requires a ]
                . q[blessed 'Session' object];
            return;
        }
        if (not $args->{q[Session]}->isa(q[Net::BitTorrent::Session])) {
            confess q[Net::BitTorrent::Session::Tracker->new({}) requires a ]
                . q[blessed Net::BitTorrent::Session object in the 'Session' ]
                . q[parameter];
            return;
        }

        # According to spec, multi-tracker tiers are shuffled initially
        $args->{q[URLs]} = shuffle($args->{q[URLs]});

        #
        $self = bless(\$args->{q[URLs]}->[0], $class);

        #
        $session{$self} = $args->{q[Session]};
        weaken $session{$self};

        #
        $complete{$self}   = 0;
        $incomplete{$self} = 0;

        #
        $urls{$self} = [map ($_ =~ m[^http://]i
                             ? Net::BitTorrent::Session::Tracker::HTTP->new(
                                                    {URL => $_, Tier => $self}
                                 )
                             : Net::BitTorrent::Session::Tracker::UDP->new(
                                                    {URL => $_, Tier => $self}
                             ),
                             @{$args->{q[URLs]}})
        ];

        #
        $session{$self}->_client->schedule(
            {   Time    => time,
                Code => sub {
                    my ($self)=@_;


                    $urls{$self}->[0]->_announce(q[started]);
                    },
                    Object=>$self
            }
        );

        #
        return $self;
    }

    # Accessors | Public
    sub incomplete { return $incomplete{+shift} }
    sub complete   { return $complete{+shift} }

    # Accessors | Private
    sub _client  { return $session{+shift}->_client; }
    sub _session { return $session{+shift}; }

    sub _set_complete {
        my ($self, $value) = @_;
        return $complete{$self} = $value;
    }

    sub _set_incomplete {
        my ($self, $value) = @_;
        return $incomplete{$self} = $value;
    }

    #sub _peer_
    # Methods | private
    #sub _announce { }

    #
    DESTROY {
        my ($self) = @_;

        #
        delete $session{$self};
        delete $urls{$self};

        #
        delete $complete{$self};
        delete $incomplete{$self};

        #
        return 1;
    }
    1;
}

=pod

=head1 NAME

Net::BitTorrent::Session::Tracker - Single BitTorrent Tracker Tier

=head1 Description

This class should not be accessed directly.

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
