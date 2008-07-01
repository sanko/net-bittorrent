package Net::BitTorrent::GenTorrent;
use strict;
use warnings;
use lib q[../../../lib];
use Net::BitTorrent::Util qw[bencode];
use Digest::SHA;
use Fcntl q[SEEK_CUR];
use File::Spec;
my (%comment, %info, %trackers) = @_;
DESTROY {
    my ($self) = @_;
    delete $comment{$self};
    delete $info{$self};
    delete $trackers{$self};
    return 1;
}

sub new {
    my ($class, $filename) = @_;
    return undef if not defined $filename;
    my $self = bless \$filename, $class;
    $self->load_defaults;
    $self->load if -e $self and -s $self;
    return $self;
}

sub load {
    my ($self, $new_path) = @_;
    $new_path ||= $$self;
    die $new_path;
}

sub save {
    my ($self) = @_;
    open(my ($out), q[>], $$self);
    syswrite($out, $self->as_string);
    return close $out;
}

sub as_string {
    my ($self, $path) = @_;
    my %data = (info             => $info{$self},
                q[creation date] => time,
                q[created by]    => q[Net::BitTorrent::GenTorrent 0.1],
                (defined $comment{$self}
                 ? (comment => $comment{$self})
                 : ()
                )
    );
    $data{q[info]}{q[pieces]} = q[];
    my $data       = q[];
    my $total_size = 0;
    grep { $total_size += $_->{q[length]} } @{$info{$self}{q[files]}};
    use Data::Dump qw[pp];
    warn pp \%data;
################################
    # This creates broken .torrent metadata when we have more
    # than a single piece of data, but... eh... who cares.
#########
    my $piece_index = 0;
FILE: for my $index (0 .. $#{$info{$self}{q[files]}}) {
        my $file = $info{$self}{q[files]}[$index];
        my $path = File::Spec->catfile(@{$file->{q[path]}});
        if (defined $file->{q[fullpath]}) {
            $path = $file->{q[fullpath]};
            delete $data{q[info]}{q[files]}->[$index]->{q[fullpath]};
        }
        open(my ($fh), q[<], $path)
            or die q[Cannot generate .torrent];
    PIECE:
        while (length $data <= $data{q[info]}{q[piece length]}) {
            my $piece_length = $data{q[info]}{q[piece length]};
            $piece_length = $total_size % $data{q[info]}{q[piece length]}
                if $piece_index == $data{q[info]}{q[piece count]};
            $piece_index++;
            sysread($fh, $data, ($piece_length - length($data)),
                    length($data));
            next FILE
                unless ((-s $fh) - (sysseek($fh, 0, SEEK_CUR)));
            $data{q[info]}{q[pieces]} .= Digest::SHA::sha1($data);
            warn length $data;
            $data = q[];
        }
    }
    delete $data{q[info]}{q[piece count]};
    $data{q[info]}{q[pieces]} .= Digest::SHA::sha1($data);
    return bencode(\%data);
}

sub load_defaults {
    my ($self) = @_;
    $info{$self}{q[files]} = [];
    my $name = $$self;
    $name =~ s|\.torrent$||;
    $self->set_name($name);
    $self->set_piece_length(131072);
}

sub set_name {
    my ($self, $value) = @_;
    return $info{$self}{q[name]} = $value;
}

sub get_name {
    my ($self, $value) = @_;
    return $info{$self}{q[name]};
}

sub add_tracker {    # XXX - tiers and other stuff...
    my ($self, $tracker) = @_;
    if (defined $tracker) {
    }
    return $trackers{$self};
}

sub get_trackers {
    my ($self) = @_;
    return $trackers{$self};
}

sub add_directory {
    my ($self, @directories_to_search) = @_;
    require File::Find;
    File::Find::find(
        sub {
            return if not -e or not -f;
            return if $File::Find::name =~ m[\.svn]i;
            return unless m[\.(?:jpg)$];
            warn sprintf q[%s|%s], $_, $File::Find::name;
            push(@{$info{$self}{q[files]}},
                 {path     => [File::Spec->splitdir($_)],
                  length   => -s,
                  fullpath => $File::Find::name = $File::Find::name
                 }
            );
        },
        @directories_to_search
    );
    return $info{$self}{q[files]};
}

sub get_files {
    my ($self, @files) = @_;
    if (@files) {
        for my $file (@files) {    # sort?
            next if not -e $file;
            push @{$info{$self}{q[files]}},
                {path   => [File::Spec->splitdir($file)],
                 length => -s $file
                };
        }
    }
    return $info{$self}{q[files]};
}

sub set_piece_length {
    my ($self, $value) = @_;
    my $total_size = 0;
    grep { $total_size += $_->{q[length]} } @{$info{$self}{q[files]}};
    $info{$self}{q[piece count]} = int($total_size / $value);
    return $info{$self}{q[piece length]} = $value;
}

sub get_piece_length {
    my ($self) = @_;
    return $info{$self}{q[piece length]};
}

sub set_piece_count {
    my ($self, $value) = @_;
    my $total_size = 0;
    grep { $total_size += $_->{q[length]} } @{$info{$self}{q[files]}};
    return $self->set_piece_length(int($total_size / $value));
}
sub get_piece_count { return $info{$_[0]}{q[piece count]}; }

sub set_comment {
    my ($self, $value) = @_;
    return $comment{$self} = $value;
}

sub get_comment {
    my ($self) = @_;
    return $comment{$self};
}
1;

package main;
use strict;
use warnings;
my $torrent
    = new Net::BitTorrent::GenTorrent q[../torrents/miniswarm.torrent];
$torrent->add_directory(q[../miniswarm/seed/]); # size: 49,998

#$torrent->files(
#    q[37112393_902495ba23.jpg],     q[488732995_b3cb64291a.jpg],
#    q[1291672777_30adc6a421_o.jpg], q[credit.txt]
#);
#$torrent->piece_length(2**14);
#warn $torrent->set_piece_length(5000); #
#warn $torrent->get_piece_count();
#warn $torrent->set_piece_count(5);
$torrent->set_comment(q[See credit.txt for attributions.]);
$torrent->set_name(q[seed]);
$torrent->save;



# $Id$
