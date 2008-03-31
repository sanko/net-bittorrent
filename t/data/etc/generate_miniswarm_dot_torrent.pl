package Net::BitTorrent::GenTorrent;

use strict;
use warnings;

use lib q[../../../lib];
use Net::BitTorrent::Util qw[bencode];
use Digest::SHA;
use Fcntl q[SEEK_CUR];
use File::Spec;

my ( %comment, %info, %trackers ) = @_;

DESTROY {
    my ($self) = @_;
    delete $comment{$self};
    delete $info{$self};
    delete $trackers{$self};
    return 1;
}

sub new {
    my ( $class, $filename ) = @_;
    return undef if not defined $filename;
    my $self = bless \$filename, $class;
    $self->defaults;
    $self->load if -e $self and -s $self;
    return $self;
}

sub load {
    my ( $self, $new_path ) = @_;
    $new_path ||= $$self;
    die $new_path;
}

sub save {
    my ($self) = @_;
    open( my ($out), q[>], $$self );
    syswrite( $out, $self->as_string );
    return close $out;
}

sub as_string {
    my ( $self, $path ) = @_;

    my %data = ( info             => $info{$self},
                 q[creation date] => time,
                 q[created by] => q[Net::BitTorrent::GenTorrent 0.1],
                 ( defined $comment{$self}
                   ? ( comment => $comment{$self} )
                   : ()
                 )
    );
    $data{q[info]}{q[pieces]} = q[];
    my $data = q[];

FILE: for my $index ( 0 .. $#{ $info{$self}{q[files]} } ) {
        my $file = $info{$self}{q[files]}[$index];
        my $path = File::Spec->catfile( @{ $file->{q[path]} } );
        if ( defined $file->{q[fullpath]} ) {
            $path = $file->{q[fullpath]};
            delete $data{q[info]}{q[files]}->[$index]->{q[fullpath]};
        }
        open( my ($fh), q[<], $path )
            or die q[Cannot generate .torrent];
    PIECE:
        while ( length $data < $data{q[info]}{q[piece length]} )
        {
            sysread( $fh, $data,
                     (  $data{q[info]}{q[piece length]}
                            - length($data)
                     ),
                     length($data)
            );
            next FILE
                unless (
                       ( -s $fh ) - ( sysseek( $fh, 0, SEEK_CUR ) ) );
            $data{q[info]}{q[pieces]} .= Digest::SHA::sha1($data);
            $data = q[];
        }
    }
    $data{q[info]}{q[pieces]} .= Digest::SHA::sha1($data);
    return bencode( \%data );
}

sub defaults {
    my ($self) = @_;
    $info{$self}{q[files]} = [];
    $info{$self}{q[name]}  = $$self;
    $info{$self}{q[name]} =~ s|\.torrent$||;
    $info{$self}{q[piece length]} = 131072;
}

sub name {
    my ( $self, $value ) = @_;
    $info{$self}{q[name]} = $value if $value;
    return $info{$self}{q[name]};
}

sub announce {
    my ( $self, $tracker ) = @_;
    if ( defined $tracker ) {

    }
    return $trackers{$self};
}

sub directory {
    my ( $self, @directories_to_search ) = @_;
    require File::Find;
    File::Find::find(
        sub {
            return if not -e or not -f;
            return if $File::Find::name =~ m[\.svn]i;
            return unless m[\.(?:jpg)$];
            warn sprintf q[%s|%s], $_, $File::Find::name;
            push( @{ $info{$self}{q[files]} },
                  {  path     => [ File::Spec->splitdir($_) ],
                     length   => -s,
                     fullpath => $File::Find::name = $File::Find::name
                  }
            );
        },
        @directories_to_search
    );
    return $info{$self}{q[files]};
}

sub files {
    my ( $self, @files ) = @_;
    if (@files) {
        for my $file (@files) {    # sort?
            next if not -e $file;
            push @{ $info{$self}{q[files]} },
                { path   => [ File::Spec->splitdir($file) ],
                  length => -s $file
                };
        }
    }
    return $info{$self}{q[files]};
}

sub piece_length {
    my ( $self, $value ) = @_;
    $info{$self}{q[piece length]} = $value if $value;
    return $info{$self}{q[piece length]};
}

sub piece_count {
    my ( $self, $value ) = @_;
    my $total_size = 0;
    grep { $total_size += $_->{q[length]} }
        @{ $info{$self}{q[files]} };
    return $info{$self}{q[piece length]}
        = int( $total_size / $value ) + 1;
}

sub comment {
    my ( $self, $value ) = @_;
    $comment{$self} = $value if $value;
    return $comment{$self};
}

1;

package main;

use strict;
use warnings;

my $torrent = new Net::BitTorrent::GenTorrent q[../torrents/miniswarm.torrent];

$torrent->directory(q[../miniswarm/seed/]);

#$torrent->files(
#    q[37112393_902495ba23.jpg],     q[488732995_b3cb64291a.jpg],
#    q[1291672777_30adc6a421_o.jpg], q[credit.txt]
#);

#$torrent->piece_length(2**14);
warn $torrent->piece_count(12);

$torrent->comment(q[See credit.txt for attributions.]);
$torrent->name(q[seed]);

$torrent->save;

# $Id: generate_miniswarm_dot_torrent.pl 4 2008-03-20 20:37:16Z sanko@cpan.org $
