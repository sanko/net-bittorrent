#!C:\perl\bin\perl.exe
package Net::BitTorrent::Torrent::Tracker;
{
    use strict;      # core as of perl 5
    use warnings;    # core as of perl 5.006

    #
    use Carp qw[carp];                              # core as of perl 5
    use Scalar::Util qw[blessed weaken refaddr];    # core as of 5.007003
    use List::Util qw[shuffle];                     # core as of 5.007003

    #
    use lib q[./../../../];
    use Net::BitTorrent::Torrent::Tracker::HTTP;
    use Net::BitTorrent::Torrent::Tracker::UDP;

    #
    use version qw[qv];                             # core as of 5.009
    our $SVN = q[$Id$];
    our $UNSTABLE_RELEASE = 0; our $VERSION = sprintf(($UNSTABLE_RELEASE ? q[%.3f_%03d] : q[%.3f]), (version->new((qw$Rev$)[1])->numify / 1000), $UNSTABLE_RELEASE);

    #
    my (@CONTENTS) = \my (
                 %torrent,  %_urls,                          # params to new()
                 %complete, %incomplete);
    my %REGISTRY;

    #
    sub new {

        # Creates a new N::B::Torrent object
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
            carp q[Net::BitTorrent::Torrent::Tracker->new({}) requires ]
                . q[parameters a set of parameters];
            return;
        }
        if (ref($args) ne q[HASH]) {
            carp q[Net::BitTorrentS::Torrent::Tracker->new({}) requires ]
                . q[parameters to be passed as a hashref];
            return;
        }
        if (not defined $args->{q[URLs]}) {
            carp q[Net::BitTorrent::Torrent::Tracker->new({}) requires a ]
                . q['URLs' parameter];
            return;
        }
        if (ref $args->{q[URLs]} ne q[ARRAY]) {
            carp q[Net::BitTorrent::Torrent::Tracker->new({}) requires a ]
                . q[list of URLs];
            return;
        }
        if (not scalar(@{$args->{q[URLs]}})) {
            carp q[Net::BitTorrent::Torrent::Tracker->new({}) doesn't (yet) ]
                . q[know what to do with an empty list of URLs];
            return;
        }
        if (not defined $args->{q[Torrent]}) {
            carp q[Net::BitTorrent::Torrent::Tracker->new({}) requires a ]
                . q['Torrent' parameter];
            return;
        }
        if (not blessed $args->{q[Torrent]}) {
            carp q[Net::BitTorrent::Torrent::Tracker->new({}) requires a ]
                . q[blessed 'Torrent' object];
            return;
        }
        if (not $args->{q[Torrent]}->isa(q[Net::BitTorrent::Torrent])) {
            carp q[Net::BitTorrent::Torrent::Tracker->new({}) requires a ]
                . q[blessed Net::BitTorrent::Torrent object in the 'Torrent' ]
                . q[parameter];
            return;
        }

        #
        $self = bless(\$args->{q[URLs]}->[0], $class);

        #
        $torrent{refaddr $self} = $args->{q[Torrent]};
        weaken $torrent{refaddr $self};

        #
        $complete{refaddr $self}   = 0;
        $incomplete{refaddr $self} = 0;

        #
        $_urls{refaddr $self} = [];
        for my $_url (@{$args->{q[URLs]}}) {
            push @{$_urls{refaddr $self}},
                ($_url =~ m[^http://]i
                 ? q[Net::BitTorrent::Torrent::Tracker::HTTP]
                 : q[Net::BitTorrent::Torrent::Tracker::UDP]
                )->new({URL => $_url, Tier => $self});
        }

        #
        $torrent{refaddr $self}->_client->_schedule({Time   => time,
                                                     Code   => \&_announce,
                                                     Object => $self
                                                    }
        ) if defined $torrent{refaddr $self}->_client;
        weaken($REGISTRY{refaddr $self} = $self);

        # According to spec, multi-tracker tiers are shuffled initially
        @{$_urls{refaddr $self}} = shuffle(@{$_urls{refaddr $self}});

        #
        return $self;
    }

    # Accessors | Public
    sub incomplete { return $incomplete{refaddr +shift} }
    sub complete   { return $complete{refaddr +shift} }

    # Accessors | Private
    sub _client  { return $torrent{refaddr +shift}->_client; }
    sub _torrent { return $torrent{refaddr +shift}; }
    sub _urls    { return $_urls{refaddr +shift}; }

    # Methods | Private
    sub _set_complete {
        my ($self, $value) = @_;
        return $complete{refaddr $self} = $value;
    }

    sub _set_incomplete {
        my ($self, $value) = @_;
        return $incomplete{refaddr $self} = $value;
    }

    sub _shuffle {    # push first (bad) to the end of the list
        my ($self) = @_;
        return (
             push(@{$_urls{refaddr $self}}, shift(@{$_urls{refaddr $self}})));
    }

    sub _announce {
        my ($self) = @_;
        return if not defined $self;
        return if not defined $_urls{refaddr $self};
        return if not scalar @{$_urls{refaddr $self}};
        return $_urls{refaddr $self}->[0]->_announce(q[started]);
    }

    sub _as_string {
        my ($self, $advanced) = @_;
        my $dump = q[TODO];
        return defined wantarray ? $dump : print STDERR qq[$dump\n];
    }

    #
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
            weaken $torrent{$_nID};

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
    1;
}

=pod

=head1 NAME

Net::BitTorrent::Torrent::Tracker - Single BitTorrent Tracker Tier

=head1 Description

Objects of this class should not be created directly.

=head1 Methods

=over

=item C<new()>

Constructor.  Don't use this.

=item C<complete()>

Returns the number of complete seeds the tracker says are present in the
swarm.

=item C<incomplete()>

Returns the number of incomplete peers the tracker says are present in
the swarm.

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
