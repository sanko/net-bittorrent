package Net::BitTorrent::Protocol::BEP03::Metadata;
{
    use Any::Moose 'Role';
    use Any::Moose '::Util::TypeConstraints';
    our $MAJOR = 0.075; our $MINOR = 0; our $DEV = 1; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use lib '../../../../';
    use Net::BitTorrent::Types;
    use Net::BitTorrent::Protocol::BEP03 qw[:all];
    use Net::BitTorrent::Storage;
    use Fcntl ':flock';

    # Extends Net::BitTorrent::Torrent
    use File::Spec::Functions qw[rel2abs];
    has 'basedir' => (is       => 'ro',
                      isa      => 'Str',
                      required => 1,
                      default  => rel2abs '.',
                      init_arg => 'BaseDir'
    );
    has 'storage' => (
        is  => 'ro',
        required => 1,
        isa => 'Net::BitTorrent::Storage',
        lazy_build => 1,
        builder  => '_build_storage',
        init_arg => 'Storage'
    );
    sub _build_storage {
        Net::BitTorrent::Storage->new( Torrent => $_[0])
    has 'trackers' => (isa => 'ArrayRef[Net::BitTorrent::Tracker]',
                       is  => 'rw',);
    after 'trackers' => sub {    # All trackers have this torrent as parent
        my ($self, $trackers) = @_;
        foreach my $tracker (@{$trackers || []}) {
            $tracker->torrent($self);
        }
    sub _build_tracker {
        Net::BitTorrent::Protocol::BEP12::Tracker->new(Torrent => $_[0]);
    }
    has 'metadata' => (
        isa      => 'HashRef',
        is       => 'rw',
        init_arg => undef,       # cannot set this with new()
        trigger  => sub {
            my ($self, $new_value, $old_value) = @_;
            if (@_ == 2) {
                # parse files and trackers
                if (defined $new_value->{'announce-list'}) {
                }
                else {
                }

                #
                my @files;
                if (defined $new_value->{'info'}{'files'})
                {    # Multi-file .torrent
                    $self->storage->files($new_value->{'info'}{'files'});
                    $self->storage->root($new_value->{'info'}{'name'});
                }
                else {    # single file torrent; use the name
                    $self->storage->files(
                              [{path   => [$new_value->{'info'}{'name'}],
                                length => $new_value->{'info'}{'length'}
                               }
                              ]
                    );
                }
                return 1;
            }
            warn 'Someone changed the metadata!';
        }
    );
    has '_raw' => (
        isa        => 'Str',
        lazy_build => 1,
        is         => 'rw',
        init_arg   => undef,    # cannot set this with new()
        trigger    => sub {
            my ($self, $new_value, $old_value) = @_;
            return $self->metadata(scalar bdecode $new_value) if @_ == 2;

            # XXX - set the current value back to the old value
        }
    );
    has 'path' => (
        is       => 'ro',
        isa      => 'Str',
        required => 1,
        init_arg => 'Path',
        trigger  => sub {
            my ($self, $new_value, $old_value) = @_;
            if (@_ == 2) {

                open(my ($FH), '<', $new_value)
                    || return !($_[0] = undef);    # exterminate! exterminate!
                flock $FH, LOCK_SH;
                sysread($FH, my ($METADATA), -s $FH) == -s $FH
                    || return !($_[0] = undef);    # destroy!
                $self->_raw($METADATA);
                return close $FH;
            }

            # XXX - set the current value back to the old value
        }
    );

    has 'infohash' => (
        is         => 'ro',
        isa        => 'Torrent::Infohash',
        init_arg   => undef,               # cannot set this with new()
        coerce     => 1,                   # Both ways?
        lazy_build => 1,
        builder    => '_build_infohash'               # returns Torrent::Infohash::Packed


    );
    sub _build_infohash {
         require Digest::SHA;
            return Digest::SHA::sha1(bencode $_[0]->metadata->{'info'});

        }

    # Quick accessors
    sub piece_length { return shift->metadata->{'info'}{'piece length'} }
    sub pieces       { return shift->metadata->{'info'}{'pieces'} }
}
1;

=pod

=head1 Author

Sanko Robinson <sanko@cpan.org> - http://sankorobinson.com/

CPAN ID: SANKO

=head1 License and Legal

Copyright (C) 2008-2010 by Sanko Robinson <sanko@cpan.org>

This program is free software; you can redistribute it and/or modify it under
the terms of
L<The Artistic License 2.0|http://www.perlfoundation.org/artistic_license_2_0>.
See the F<LICENSE> file included with this distribution or
L<notes on the Artistic License 2.0|http://www.perlfoundation.org/artistic_2_0_notes>
for clarification.

When separated from the distribution, all original POD documentation is
covered by the
L<Creative Commons Attribution-Share Alike 3.0 License|http://creativecommons.org/licenses/by-sa/3.0/us/legalcode>.
See the
L<clarification of the CCA-SA3.0|http://creativecommons.org/licenses/by-sa/3.0/us/>.

Neither this module nor the L<Author|/Author> is affiliated with BitTorrent,
Inc.

=for rcs $Id$

=cut
