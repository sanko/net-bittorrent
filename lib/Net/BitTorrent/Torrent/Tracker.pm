#!/usr/bin/perl -w
package Net::BitTorrent::Torrent::Tracker;
{
    use strict;
    use warnings;
    use Carp qw[carp];
    use Scalar::Util qw[blessed weaken refaddr];
    use List::Util qw[shuffle];
    use lib q[./../../../];
    use Net::BitTorrent::Torrent::Tracker::HTTP;
    use Net::BitTorrent::Torrent::Tracker::UDP;
    use version qw[qv];
    our $SVN = q[$Id$];
    our $UNSTABLE_RELEASE = 0; our $VERSION = sprintf(($UNSTABLE_RELEASE ? q[%.3f_%03d] : q[%.3f]), (version->new((qw$Rev$)[1])->numify / 1000), $UNSTABLE_RELEASE);
    my (@CONTENTS) = \my (%torrent, %_urls, %complete, %incomplete);
    my %REGISTRY;

    sub new {
        my ($class, $args) = @_;
        my $self;
        if ((!$args) || (ref($args) ne q[HASH])) {
            carp
                q[Net::BitTorrent::Torrent::Tracker->new({}) requires parameters to be passed as a hashref];
            return;
        }
        if (   (!$args->{q[URLs]})
            || (ref $args->{q[URLs]} ne q[ARRAY])
            || (!scalar(@{$args->{q[URLs]}})))
        {   carp
                q[Net::BitTorrent::Torrent::Tracker->new({}) requires a list of URLs];
            return;
        }
        if (   (!$args->{q[Torrent]})
            || (!blessed $args->{q[Torrent]})
            || (!$args->{q[Torrent]}->isa(q[Net::BitTorrent::Torrent])))
        {   carp
                q[Net::BitTorrent::Torrent::Tracker->new({}) requires a Torrent];
            return;
        }
        $self = bless(\$args->{q[URLs]}->[0], $class);
        $torrent{refaddr $self} = $args->{q[Torrent]};
        weaken $torrent{refaddr $self};
        $complete{refaddr $self}   = 0;
        $incomplete{refaddr $self} = 0;
        $_urls{refaddr $self}      = [];
        for my $_url (@{$args->{q[URLs]}}) {
            push @{$_urls{refaddr $self}},
                ($_url =~ m[^http://]i
                 ? q[Net::BitTorrent::Torrent::Tracker::HTTP]
                 : q[Net::BitTorrent::Torrent::Tracker::UDP]
                )->new({URL => $_url, Tier => $self});
        }
        $torrent{refaddr $self}->_client->_schedule(
                             {Time   => time,
                              Code   => sub { shift->_announce(q[started]) },
                              Object => $self
                             }
        ) if $torrent{refaddr $self}->status & 128;
        weaken($REGISTRY{refaddr $self} = $self);
        @{$_urls{refaddr $self}} = shuffle(@{$_urls{refaddr $self}});
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

    sub _shuffle {
        my ($self) = @_;
        return (
             push(@{$_urls{refaddr $self}}, shift(@{$_urls{refaddr $self}})));
    }

    sub _announce {
        my ($self, $event) = @_;
        return if not defined $self;
        return if not defined $_urls{refaddr $self};
        return if not scalar @{$_urls{refaddr $self}};
        return $_urls{refaddr $self}->[0]->_announce($event ? $event : ());
    }

    sub _as_string {
        my ($self, $advanced) = @_;
        my $dump = !$advanced ? $$self : sprintf <<'END',
Net::BitTorrent::Torrent::Tracker

Complete: %d
Incomplete: %d
Number of URLs: %d
    %s
END
            $complete{refaddr $self},
            $incomplete{refaddr $self},
            scalar(@{$_urls{refaddr $self}}),
            join qq[\r\n    ], map { $_->_url() } @{$_urls{refaddr $self}};
        return defined wantarray ? $dump : print STDERR qq[$dump\n];
    }

    sub CLONE {
        for my $_oID (keys %REGISTRY) {
            my $_obj = $REGISTRY{$_oID};
            my $_nID = refaddr $_obj;
            for (@CONTENTS) {
                $_->{$_nID} = $_->{$_oID};
                delete $_->{$_oID};
            }
            weaken $torrent{$_nID};
            weaken($REGISTRY{$_nID} = $_obj);
            delete $REGISTRY{$_oID};
        }
        return 1;
    }
    DESTROY {
        my ($self) = @_;
        for (@CONTENTS) { delete $_->{refaddr $self}; }
        return delete $REGISTRY{refaddr $self};
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
