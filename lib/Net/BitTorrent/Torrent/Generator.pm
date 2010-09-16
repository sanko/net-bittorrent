package Net::BitTorrent::Torrent::Generator;
{
    use Moose;
    use Moose::Util::TypeConstraints;
    our $MAJOR = 0.074; our $MINOR = 0; our $DEV = 12; our $VERSION = sprintf('%1.3f%03d' . ($DEV ? (($DEV < 0 ? '' : '_') . '%03d') : ('')), $MAJOR, $MINOR, abs $DEV);
    use lib '../../../';
    use Net::BitTorrent::Protocol::BEP03::Bencode qw[bencode];
    use Net::BitTorrent::Types qw[:all];
    use Digest::SHA;
    use Fcntl qw[SEEK_CUR];
    use File::Spec::Functions
        qw[abs2rel catdir curdir no_upwards rel2abs splitdir splitpath];
    has '_files' => (
        is => 'ro',
        isa =>
            'ArrayRef[NBTypes::File::Path::PreExisting]|NBTypes::File::Path::PreExisting|NBTypes::File::Directory::PreExisting',
        required => 1,
        init_arg => 'files',
        writer   => '_set_files',
        coerce   => 1,
        handles  => {
            files => sub {
                my $s = shift;
                return [$s->_files] if -f $s->_files;
                return $s->_files if grep { !-d } $s->_files;
                my @files;
                require File::Find;
                File::Find::find({wanted => sub { push @files, $_ if -f },
                                  no_chdir => 1
                                 },
                                 $s->_files
                );
                \@files;
            },
            _count_files => sub { scalar @{shift->files} },
            total_size   => sub {
                my $t = 0;
                $t += -s for @{shift->files};
                $t;
                }
        }
    );
    has 'name' => (is        => 'ro',
                   isa       => 'Str',
                   writer    => '_set_name',
                   predicate => '_has_name'
    );
    has 'announce' => (is        => 'ro',
                       isa       => 'Str',
                       writer    => '_set_announce',
                       predicate => '_has_announce'
    );
    has 'announce-list' => (is      => 'ro',
                            isa     => 'ArrayRef[ArrayRef[Str]]',
                            default => sub { [] },
                            traits  => ['Array'],
                            handles => {_add_tier          => 'push',
                                        _get_pier          => 'get',
                                        _has_announce_list => 'count'
                            }
    );
    has 'private' => (is      => 'ro',
                      isa     => 'Bool',
                      traits  => ['Bool'],
                      default => 0,
                      handles => {'_set_private'   => 'set',
                                  '_unset_private' => 'unset'
                      }
    );
    has 'comment' => (is        => 'ro',
                      isa       => 'Str',
                      predicate => '_has_comment',
                      writer    => '_set_comment'
    );
    has 'merge' => (is        => 'ro',
                    isa       => 'NBTypes::Bdecode',
                    coerce    => 1,
                    predicate => '_has_merge',
                    writer    => '_set_merge'
    );
    has 'piece_length' => (
        is      => 'ro',
        isa     => 'Int',
        default => 2**18,
        writer  => '_set_piece_length',
        handles => {
            _piece_count => sub {
                my ($s, $v) = @_;
                $s->_set_piece_length(int($s->total_size / $v) + 1);
                }
        }
    );
    my $bencode_constraint;
    has 'info_hash' => (
        is       => 'ro',
        isa      => 'NBTypes::Torrent::Infohash',
        init_arg => undef,                        # cannot set this with new()
        coerce   => 1,                            # Both ways?
        lazy_build => 1,
        builder    => '_build_info_hash',  # returns Torrent::Infohash::Packed
        clearer    => '_clear_info_hash'
    );

    sub _build_info_hash {
        require Digest::SHA;
        my $s = shift;
        $bencode_constraint //=
            Moose::Util::TypeConstraints::find_type_constraint(
                                                          'NBTypes::Bencode');
        Digest::SHA::sha1(
                         $bencode_constraint->coerce($s->metadata->{'info'}));
    }
    has 'metadata' => (
        is         => 'ro',
        isa        => 'NBTypes::Bdecode',
        init_arg   => undef,
        coerce     => 1,
        lazy_build => 1,
        clearer    => '_clear_metadata',
        handles    => {
            raw_data => sub {
                my $s = shift;
                $bencode_constraint //=
                    Moose::Util::TypeConstraints::find_type_constraint(
                                                          'NBTypes::Bencode');
                $bencode_constraint->coerce($s->metadata);
                }
        }
    );

    sub _build_metadata {
        my ($s, $p) = @_;
        my %data = (
            ($s->_has_merge ? $s->merge : ()),
            info => {
                'piece length' => $s->piece_length,
                pieces         => '',                 # Filled later
                ($s->private ? (private => 1) : ()),
                ($s->_count_files > 1 ? (             # Multiple files
                     sub {
                         my $tree = {};
                         for my $f (@{$s->files}) {
                             my $pos = \$tree;
                             for my $key (splitdir $f) {
                                 $$pos->{$key} ||= ();
                                 $pos = \$$pos->{$key};
                             }
                         }
                         my $base;
                         my $abs = '';
                         while (scalar keys %$tree == 1) {
                             ($base) = keys(%$tree);
                             $abs .= $base . '/';
                             $tree = $tree->{$base};
                         }
                         my $cwd = rel2abs curdir;
                         return (
                             files => [
                                 map {
                                     my $f_abs = rel2abs $_;
                                     my ($c, $d, undef) = splitpath($f_abs);
                                     chdir catdir rel2abs $abs;
                                     my $x = abs2rel $f_abs;
                                     chdir $cwd;
                                     {path => [grep { defined && length }
                                                   splitdir($x)
                                      ],
                                      length => -s $f_abs
                                     };
                                     } @{$s->files}
                             ],
                             name => $s->_has_name ? $s->name : $base
                         );
                         }
                         ->()
                     )
                 : (    # Single file
                     length => $s->total_size,
                     name   => [splitpath($s->files->[0])]->[-1]
                 )
                )
            },
            ($s->_has_announce ? (announce => $s->announce) : ()),
            ($s->_has_announce_list ? ('announce-list' => $s->announce_list)
             : ()
            ),
            'creation date' => time,
            'created by'    => __PACKAGE__ . ' v' . $VERSION,
            ($s->_has_comment ? (comment => $s->comment)
             : ()
            )
        );
        my $data = '';
    FILE: for my $f (@{$s->files}) {
            open(my ($fh), '<', $f)
                || confess sprintf
                'Cannot open "%s" to generate metadata: %s', $f, $!;
        PIECE:
            while (length $data < $s->piece_length) {
                sysread($fh, $data, ($s->piece_length - length($data)),
                        length($data));
                next FILE
                    unless ((-s $fh) - (sysseek($fh, 0, SEEK_CUR)));
                $data{'info'}{'pieces'} .= Digest::SHA::sha1($data);
                $data = '';
            }
        }
        $data{'info'}{'pieces'} .= Digest::SHA::sha1($data);
        return \%data;
    }

    # If anything is changed, update the good stuff
    after qr[^(_(set|unset|add|del|sort)|add)] =>
        sub { my $s = shift; $s->_clear_info_hash; $s->_clear_metadata };
}
1;

=pod

=head1 NAME

Net::BitTorrent::Torrent::Generator - .torrent metadata generator

=head1 Synopsis

    # Generate a single file .torrent to seed file.avi
    my $t1 = Net::BitTorrent::Torrent::Generator->new( files => 'file.avi' );

    # Add everything in the current directory accept .torrent files
    my $t2 = Net::BitTorrent::Torrent::Generator->new(
        files      => '../'
    );

    # Now, let's write the metadata to disk
    print 'Saving ' .  $t2->info_hash->to_Hex . '...';
    open my($torrent), '>', 'test.torrent' || die 'FAIL!';
    syswrite $torrent, $t2->raw_data;
    close $torrent;

=head1 Description

TODO

=head1 Methods

Creating a new .torrent is simple. As is this API.

=head2 my $torrent = Net::BitTorrent::Torrent::Generator->B<new>( files => ..., [ ... ] )

Creates a new generator object.

This constructor requires the following arguments:

=over

=item C<files>

See L<< files|/"$torrent->B<files>( )" >>.

=back

This constructor also supports the following optional arguments:

=over

=item C<announce>

See L<< announce|/"$torrent->B<announce>( )" >>.

=item C<announce_list>

See L<< announce_list|/"$torrent->B<announce_list>( )" >>.

=item C<comment>

See L<< comment|/"$torrent->B<comment>( )" >>.

=item C<merge>

This is an advanced argument containing a hashref which will be merged with
the final L<metadata|/'metadata'> when generated.

Don't use this.

=item C<piece_length>

See L<< piece_length/"$torrent->B<piece_length>( )" >>.

=item C<private>

See L<< private|/"$torrent->B<private>( )" >>.

=back

=head2 $torrent->B<announce>( )

The announce URL of the tracker. With the advent and current popularity of
DHT, PEX, and other alternate means of learning about peers, this value may be
considered optional.

Use C<< $torrent->B<_set_piece_length> >> to set this value.

=head2 $torrent->B<announce_list>( )

Optional list of URLs.

Use C<< $torrent->B<_set_announce_list( [ [url, url], [url] ] )> >> to set
this value. To add a single tier, use
C<< $torrent->B<_add_tier( [ url, url ] )> >.

See also L<BEP12|Net::BitTorrent::Protocol::BEP12>.

=head2 $torrent->B<comment>( )

This is an optional, free-form textual comment.

=head2 $torrent->B<files>( )

Returns the list of files contained in this torrent.

=head2 $torrent->B<info_hash>( )

Returns the C<160bit> hex string used to identify this torrent as a
L<Bit::Vector|Bit::Vector> object.

=head2 $torrent->B<metadata>( )

This method generates and returns the metadata.

For raw data ready to write to disk, see
L<< raw_data|/"$torrent->B<raw_data>( ))" >>.

=head2 $torrent->B<files>( )

This is a string contining either a directory or a single file.

If this is a directory, a multi-file torrent is generated.

Use C<< $torrent->B<_set_files>( $path ) >> to set this value later.

=head2 $torrent->B<piece_length>( )

For the purposes of transfer, files are split into fixed-size pieces which are
all the same length except for possibly the last one which may be truncated.
L<piece_length> is almost always a power of two, most commonly C<2**18>
(256KB). This is also our default value.

Use C<< $torrent->B<_set_piece_length> >> to set this value.

=head2 $torrent->B<private>( )

A boolean value for cheap DRM behaviour.

Use C<< $torrent->B<_set_private> >> to set this on and
C<< $torrent->B<_unset_private> >> to turn it back off.

See also L<BEP27|Net::BitTorrent::Protocol::BEP27>.

=head2 $torrent->B<raw_data>( )

L<< Generates metadata|/"$torrent->B<metadata>( ))" >> and returns it as a
bencoded string. This is what you'd write to disk.

=head2 $torrent->B<total_size>( )

Returns the total size of all the L<< files|/"$torrent->B<files>( )" >>
related to this torrent.

=head1 Notes

This class may be renamed. Suggestions welcome.

=head1 See Also

L<BEP03|Net::BitTorrent::Protocol::BEP03>

L<Net::BitTorrent::File|Net::BitTorrent::File>

=head1 TODO

=over

=item Document optional .torrent C<name>.

=back

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
