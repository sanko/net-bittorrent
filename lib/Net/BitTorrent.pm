package Net::BitTorrent;
use 5.014;
use Moose;
use strictures 2;
use namespace::clean;
#
use IO::Async::Loop;
use IO::Async::Stream;
use IO::Async::Timer::Periodic;
use IO::Async::Listener;
use base 'IO::Async::Notifier';
#
use Types::Standard qw[ArrayRef CodeRef Enum HashRef Int Ref Str];
use Try::Tiny;
#
use lib 'C:\Users\Sanko\Documents\GitHub\Net-Bittorrent-Protocol\lib';
use Net::BitTorrent::Protocol ':all';
use lib '../../lib';
use Net::BitTorrent::Peer;
use Net::BitTorrent::Torrent;
#
our $VERSION = "0.75";
#
sub BUILD {
    my ($s, $args) = @_;

    #  die "foo and bar cannot be used at the same time"
    #    if exists $args->{foo} && exists $args->{bar};
    #  $s->loop->add($s) unless $s->parent;
    $s->new_peer_timer->start;
    $s->choke_check_timer->start;
    $s->check_interest_timer->start;
    $s->listener;    # Init
}
#
has loop => (is      => 'ro',
             isa     => 'IO::Async::Loop',
             lazy    => 1,
             default => sub { IO::Async::Loop->new() },
             handles => [qw[run]]
);
has port => (is      => 'ro',
             isa     => Int,
             default => sub {0},
             lazy    => 1,
             writer  => '_set_port'
);
has listener => (is  => 'ro',
                 isa => 'IO::Async::Listener', lazy=> 1, builder=> '_build_listener');

sub _build_listener {
    my $s = shift;
    my $listener = IO::Async::Listener->new(
        on_stream => sub {
            my (undef, $stream) = @_;
            $stream->configure(
                on_stream => sub {
                    warn 'NEW PEER?!?!';
                },
                on_read => sub {
                    my ($self, $buffref, $eof) = @_;
                    warn 'STUFF FROM NEW PEER?!?';

                    #           $self->write( $$buffref );
                    #           $$buffref = "";
                    return 0;
                }
            );
            $s->loop->add($stream);
        }
    );
    $s->loop->add($listener);
    $listener->listen(
        service   => $s->port,
        socktype  => 'stream',
        on_listen => sub {
            my ($listener) = @_;
            my $socket     = $listener->read_handle;
            my $port       = $socket->sockport;
            #warn 'Now listening on port ' . $port;
            $s->_set_port($port);
        }
    )->get;
}
has $_
    . '_timer' => (is      => 'ro',
                   isa     => 'IO::Async::Timer::Periodic',
                   builder => '_build_' . $_ . '_timer',
                   lazy    => 1
    ) for qw[new_peer choke_check check_interest];

sub _build_new_peer_timer {
    my $s = shift;
    my $timer = IO::Async::Timer::Periodic->new(
        first_interval => 5,
        interval => 10,
        on_tick  => sub {
            warn 'new peer timer';
            use Data::Printer;
            for my $torrent ($s->torrents) {
                #warn;
                next if !$torrent->is_started;
                #warn;
                next if !$torrent->_left;
                #warn;
                #warn np $torrent->trackers;

                # XXX - Initiate connections when we are in Super seed mode?
                my @cache = map {
                    $_->{peers} ? uncompact_ipv4($_->{peers}) : (),
                        $_->{peers6} ?
                        uncompact_ipv6($_->{peers6})
                        : ()
                } @{$torrent->trackers};
                #warn;
                use Data::Printer;
                #warn np @cache;
                next if !@cache;
                warn;
                for my $i (1 .. @cache) {
                    last if $i > 10;    # XXX - Max half open
                    last
                        if scalar(
                        $s->_peers(
                            sub {
                                $_->torrent->infohash eq $torrent->infohash;
                            }
                        )
                        ) > 100;        # XXX - Max peers
                    my $addr = splice @cache, rand $#cache, 1;
                    #
                    try {
                        $s->_connect(@$addr, $torrent);
                    }
                    catch {
                        #warn 'FAIL! ' . shift;

                    };
                 }
            }
        }
    );
    $s->loop->add($timer);
    $timer;
}

sub _build_choke_check_timer {
    my $s = shift;
    my $timer = IO::Async::Timer::Periodic->new(
        interval => 5,
        on_tick  => sub {

            # warn 'check choke!';
        },
    );
    $s->loop->add($timer);
    $timer;
}

sub _build_check_interest_timer {
    my $s = shift;
    my $timer = IO::Async::Timer::Periodic->new(
        interval => 5,
        on_tick  => sub {

            #warn 'check interest';
            for my $peer ($s->peers) {
                $peer->_consider;
            }
        },
    );
    $s->loop->add($timer);
    $timer;
}
has $_ => (is       => 'bare',
           init_arg => undef,
           isa      => HashRef,
           default  => sub { {} },
           traits   => ['Hash'],
           handles  => {
                       '_add_' . $_         => 'set',
                       '_get_' . $_         => 'get',
                       'has_no_' . $_ . 's' => 'is_empty',
                       'num_' . $_ . 's'    => 'count',
                       'delete_' . $_       => 'delete',
                       $_ . '_pairs'        => 'kv',
                       '_' . $_ . '_exists' => 'exists',
                       $_ . 's'             => 'values'
           }
) for qw[peer torrent];

sub add_torrent {
    my ($s, $t) = @_;
    return if $s->_torrent_exists($t->infohash);
    $s->_add_torrent($t->infohash, $t);
    $t->_set_client($s);
    $t;
}
has peerid => (
    is       => 'ro',
    isa      => Str,    # TODO: len == 40
    required => 1,
    default  => sub {
        pack(
            'a20',
            (sprintf(
                 '-NB%01d%02d%1s-%7s%-5s',
                 ($VERSION =~ m[^(\d+)\.(\d+)]),
                 ($VERSION =~ m[_] ? 'U' : 'S'),
                 (join '',
                  map {
                      ['A' .. 'Z', 'a' .. 'z', 0 .. 9, qw[- . _ ~]]
                      ->[rand(66)]
                  } 1 .. 7
                 ),
                 [qw[KaiLi April Aaron Sanko]]->[rand 4]
             )
            )
        );
    }
);
#
sub _connect {
    my ($s, $host, $port, $torrent) = @_;
    warn 'Connecting to ' . $host . ':' . $port . '...';
    return unless $s->_torrent_exists($torrent->infohash);   # Don't fake seed


    my $connector = $s->loop->connect(
        host      => $host,
        service   => $port,
        socktype  => 'stream',
        handle    => IO::Async::Stream->new(),
        on_fail   => sub { warn ucfirst "$_[0] failed: [$host:$port] $_[-1]\n"; }

);


    my $timer = IO::Async::Timer::Countdown->new(
        delay => 10,
        on_expire => sub { warn 'failed to connect!';$connector->fail('timeout'); $connector = () },
    );

$timer->start;
$s->loop->add($timer);



        my $stream = $connector->get;

return if !$stream;

        $timer->stop;$timer = ();



        $stream->configure(
                read_all => 1,
                on_read  => sub {
                    my ($stream, $buffref, $eof) = @_;
                    my $packet;
                    if (   (length $$buffref >= 68)
                        && ($packet = parse_handshake($$buffref))
                        && ref $packet ne 'HASH'    # error
                        )
                    {
                        # XXX - Disconnect if we aren't serving this torrent
                        return $stream->close
                            unless $s->_torrent_exists($packet->[1]);
                        my $torrent = $s->_get_torrent($packet->[1]);
                        return $stream->close if $torrent->is_stopped;
                        my $peer =
                            Net::BitTorrent::Peer->new(stream  => $stream,
                                                       torrent => $torrent);
                        $s->_add_peer($packet->[2], $peer)
                            unless $s->_peer_exists($packet->[2]);
                    }
                    return 1;
                }
            );
            $stream->write(
                    build_handshake(
                        pack('C*', split('', '00000000')), $torrent->infohash,
                        $s->peerid
                    )
            );
            $s->loop->add($stream);
}

sub _broadcast {    # Send a qualifier to limit who gets the msg
    my ($s, $data, $qualifier) = @_;
    $_->stream->write($data) for $s->_peers($qualifier);
}

sub _peers {
    my ($s, $qualifier) = @_;
    $qualifier //= sub {1};
    grep { $qualifier->($_) } $s->peers;
}
#
1;
__END__

=encoding utf-8

=head1 NAME

Net::BitTorrent -  BitTorrent peer-to-peer protocol class

=head1 SYNOPSIS

    use Net::BitTorrent;

    my $client = Net::BitTorrent->new();
    my $torrent = $client->add_torrent('totally.legal.torrent');
    $torrent->hashcheck;
    $client->loop;

=head1 DESCRIPTION

Net::BitTorrent is being rewritten with IO::Async. Seriously this time.

=head1 Methods


=head2 C<new( ... )>


=head2 C<add_torrent( ... )>


=head1 Todo

Let's see...

=over

=item BEP 03 - BitTorrent Protocol

=item BEP 19 - Webseed



=back

=head1 LICENSE

Copyright (C) Sanko Robinson.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=head1 AUTHOR

Sanko Robinson E<lt>sanko@cpan.orgE<gt>

=cut

