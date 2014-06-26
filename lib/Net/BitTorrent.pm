package Net::BitTorrent;
{ $Net::BitTorrent::VERSION = 'v1.2.3' }
use Net::BitTorrent::Protocol qw[:all $HANDSHAKE];

use Moose;
use Bit::Vector;
use AnyEvent;

use Net::BitTorrent::DHT;
use Types::Standard qw[HashRef Ref Maybe];
extends 'AnyEvent::BitTorrent';
has dht_node => (is => 'ro', isa => 'Net::BitTorrent::DHT', lazy_build => 1);
my $_dht_singleton;
sub _build_dht_node {
$_dht_singleton //= Net::BitTorrent::DHT->new(
 port => [shift->port, 0],
          boot_nodes =>
              [
                ['router.bittorrent.com', 6881],
                ['router.utorrent.com', 6881],
                ['router.bittorrent.com', 8991],
                ['dht.transmissionbt.com', 6881],
                ['dht.aelitis.com', 6881]
                ]


    ); $_dht_singleton }

after BUILD => sub{shift->dht_node};


# Override AnyEvent::BitTorrent's peerid generator
sub _build_peerid {
    pack(
        'a20',
        (sprintf(
             '-NB%01d%01d%01d%1s-%7s%-5s',
             ($Net::BitTorrent::VERSION =~ m[^v(\d+)\.(\d+)\.(\d+)]),
             ($Net::BitTorrent::VERSION =~ m[[^\d\.^v]] ? 'U' : 'S'),
             (join '',
              map {
                  ['A' .. 'Z', 'a' .. 'z', 0 .. 9, qw[- . _ ~]]->[rand(66)]
              } 1 .. 7
             ),
             [qw[KaiLi April Aaron]]->[rand 3]
         )
        )
    );
}


has announce_quest => (is =>'ro', isa => Ref, lazy_build => 1 );
sub _build_announce_quest {
    my $s =  shift;
    return if $s->is_private; # Don't leak

    $s->dht_node->announce_peer( Bit::Vector->new_Hex(160, unpack('H*', $s->infohash)), $s->port, sub {
         warn 'announce complete'});
}

# Start DHT node with announce
has _dht_peers => (
    is => 'ro', isa => HashRef,
);
has get_peers_quest => (is => 'ro', isa => Ref, lazy_build => 1);
sub _build_get_peers_quest {
    my $s = shift;
        return if $s->is_private; # Don't leak
$s->dht_node->get_peers(

    Bit::Vector->new_Hex(160,unpack ('H*', $s->infohash))

    , \&dht_cb);
    }


after announce => sub { # TODO: Should this be in a sub triggered after start()?
    my ($s, $e) = @_;
    return if $s->is_private;
    $s->announce_quest;
    $s->get_peers_quest;
};

sub dht_cb {
    my ($infohash, $node, $peers) = @_;

    AE::log debug => sprintf "We found %d peers for %s from %s:%d via DHT\n\t%s\n",
        scalar(@$peers),
        $infohash->to_Hex, $node->host, $node->port,
        join ', ', map { sprintf '%s:%d', @$_ } @$peers;
}

# c+p from AnyEvent::BitTorrent plus DHT tracker
sub _build_peer_timer {
    my $s = shift;
    AE::timer(
        1, 15,
        sub {
            return if !$s->_left;
            AE::log trace => 'Attempting to connect to new peer...';

            # XXX - Initiate connections when we are in Super seed mode?
            my @cache =
            @{$s->get_peers_quest->[2]}, # Where DHT stores possible peers
            map {
                $_->{peers} ? uncompact_ipv4($_->{peers}) : (),
                    $_->{peers6} ?
                    uncompact_ipv6($_->{peers6})
                    : ()
            } @{$s->trackers};

            return if !@cache;
            for my $i (1 .. @cache) {
                last if $i > 10;    # XXX - Max half open
                last
                    if scalar(keys %{$s->peers}) > 100;    # XXX - Max peers
                my $addr = splice @cache, rand $#cache, 1;
                $s->_new_peer($addr);
            }
        }
    );
}



# TODO: Extend trackers to suport UDP trackers
around _announce_tier => sub {
        my $next = shift;
        my ($s, $e, $tier) = @_;

        my @urls = grep {m[^https?://]} @{$tier->{urls}};
        #return if $tier->{failures} > 5;
        #return if $#{$tier->{urls}} < 0;                 # Empty tier?
        if ($tier->{urls}[0] !~ m[^udp://.+]) {
                warn 'UDP Tracker!';
            return;
        }
        return $next->($s, $e, $tier);
};


# TODO: wrap wire read/write for encryption support


# TODO: Document these new methods
sub is_private { # TODO: Move this to AnyEvent::BitTorrent
    my $s = shift;
    # TODO: See tracker switching changes for private torrents in BEP27
    $s->metadata->{info}{private} // return 0;
}
1;

__END__
package Net::BitTorrent::DHT;
use Moose;
use AnyEvent;
use Types::Standard qw[Int Str];
has id_bits => (is => 'ro', isa => Int, default => 160); # Node ID size
has id_hash => (is => 'ro', isa => Str, default => 'SHA-1'); # Hash function
has alpha => (is => 'ro', isa => Int, default => 3); # Max concurrent msgs in transit
has stale => (is => 'ro', isa => Int, default => 5); # TODO: times a node is marked stale before removal
has timeout_req => (is => 'ro', isa => Int, default => 1.0); # Single req timeout
has rt_k => (is => 'ro', isa => Int, default => 20); # Bucket size

package Net::BitTorrent::DHT::Id;
	use base 'Math::BigInt';
	use Digest;
	use MIME::Base64 qw(encode_base64url decode_base64url);
	use overload '""' => sub { $_[0]->as_b64() };
	sub new { from_b64(@_) }

	sub from_hex {
		my ($class, $hex) = @_;
		my $self = $class->SUPER::from_hex($hex);
		bless ($self, ref($class) || $class);
	}

	sub from_b64 { Kad::Id->from_raw(decode_base64url($_[1])) }
	sub from_raw { Kad::Id->from_hex(join('', unpack('H*', $_[1]))) }
	sub from_hash { Kad::Id->from_hex(Digest->new($Kad::Config{ID_HASH})->add($_[1])->hexdigest()) }
	sub as_raw { pack 'H*', $_[0]->as_hex() }
	sub as_bin { sprintf "%0$Kad::Config{ID_BITS}s", $_[0]->SUPER::as_bin() =~ s/^0b//r }
	sub as_hex { sprintf '%0'.($Kad::Config{ID_BITS}/4).'s', $_[0]->SUPER::as_hex() =~ s/^0x//r }
	sub as_b64 { encode_base64url($_[0]->as_raw()) }

	sub random {
		Kad::Id->from_hex(join('', ('0'..'9', 'a'..'f')[
			map { rand 16 } (1 .. $Kad::Config{ID_BITS}/4)
		]))
	}

	sub prefixlen {
		my ($self) = @_;
		$self->as_bin() =~ /(^0*)/;
		return length($1);
	}

package Net::BitTorrent::DataStore::Hash;

use strict;
	use warnings;
use diagnostics;
	sub new {
		my ($class) = @_;
		my $self = { data => {} };
		bless ($self, ref($class) || $class);
	}

	sub set {
		my ($s, $k, $v, $id) = @_;
		if (defined $v)	{
			if (defined $s->{data}{$k} and $s->{data}{$k}{val} eq $v) { # already exists and same
				$s->{data}{$k}{val} = $v;
				$s->{data}{$k}{time} = time;
			} else {
				$s->{data}{$k} = {
					val => $v,
					time => time,
					pub_time => time,
					pub_id => $id
				};
			}
		} else { delete $s->{data}{$k}; }
	}
	sub get { my ($s, $k) = @_; return $s->{data}{$k}; }
	sub has { my ($s, $k, $v) = @_; return defined $s->{data}{$k}; }
	sub unset { my ($s, $k) = @_;  }
	sub keys { my ($s) = @_; return keys $s->{data} }


package Net::BitTorrent::DHT::Node;

use strict;
	use warnings;
	use feature qw(switch);
	use AnyEvent;
	use JSON::XS;
	use List::Util qw(min);
	use List::MoreUtils qw(firstidx);
	use IO::Socket::INET;
	use Data::Dumper;

	# Node state
	our %STATE => (
		WAIT => 0, JOINING => 1, JOINED => 2, LEFT => 3
	);
	our %RPC => (
		INFO => 'info', FIND => 'find', STORE => 'store',
	);

	sub new {
		my ($class, $name, $bind, $port) = @_;
		my $self = {
			state	=> $STATE{WAIT},
			lid	=> Kad::Id->from_hash($name),
			bind	=> $bind,
			port	=> $port,
			rt	=> [],		# routing table
			ds	=> Kad::DataStore::Hash->new(),
			reqs	=> {},		# current requests (tag => callback)
		};
		bless ($self, ref($class) || $class);

		# create UDP socket
		$self->{sock} = IO::Socket::INET->new(
			LocalAddr => "$bind:$port", Proto => 'udp',
			ReuseAddr => 1, Broadcast => 1, MultiHomed => 1
		) or die "Couldn't create socket: $!\n";

		# set socket read handler
		$self->{_ae_recv} = AnyEvent->io(
			fh => $self->{sock}, poll => 'r',
			cb => sub { $self->udp_recv() }
		) or die "Couldn't set event: $!\n";

		return $self;
	}

	################################
	# Routing table
	################################

	sub rt_update {
		my ($self, $contact) = @_;
		return if $contact->{id} == $self->{lid}; # don't add myself

		if ($STATE{WAIT} == $self->{state}) {
			AnyEvent::postpone {
				$self->join(sub {
				  my ($c) = @_;
				  print "Joined on incoming, to $contact->{addr}\n" if $c;
				}, $contact->{addr});
			};
		}

		my $bn = ($contact->{id} ^ $self->{lid})->prefixlen();
		unless (defined $self->{rt}[$bn]) { $self->{rt}[$bn] = [ $contact ]; return $contact; }

		my $b = $self->{rt}[$bn];
		if ((my $idx = firstidx { $_->{id} == $contact->{id} } @{$b}) >= 0) {	# exists
			unshift $b, splice($b, $idx, 1) if (0 != $idx); # move to top
			return $b->[$idx];
		} elsif (scalar @{$b} > $Kad::Config{RT_K}) {
			# TODO: replacement cache
		} else {
			print "New node: $contact->{id} [$contact->{addr}]\n" if $DBG;
			unshift $b, $contact; # add to top
		}
		return $contact;
	}

	sub rt_find_closest {
		my ($self, $target, $qty, $skip) = @_;
		my $bs = $self->{rt};

		my $bn = ($target ^ $self->{lid})->prefixlen();
		my @res = ();
		push @res, @{$bs->[$bn]} if defined $bs->[$bn];

		for (1 .. $Kad::Config{ID_BITS}) {
			push @res, @{$bs->[$bn-$_]} if $bn-$_ >= 0 and defined $bs->[$bn-$_];
			push @res, @{$bs->[$bn+$_]} if defined $bs->[$bn+$_];
			last if $#res > $qty or ($bn-$_ < 0 and $bn+$_ >= $Kad::Config{ID_BITS});
		}
		@res = sort { ($a->{id} ^ $target) <=> ($b->{id} ^ $target) }
				grep { $_->{id} != $skip } @res;
		@res = @res[0 .. min($qty-1, $#res)];
		return @res;
	}

	################################
	# Helpers
	################################

	sub _endpoint {
		my ($addr, $port) = @_;
		if (defined $port and defined $addr) {
			return sockaddr_in($port, inet_aton($addr));
		} elsif (defined $addr) {
			($port, $addr) = sockaddr_in($addr);
			$addr = inet_ntoa($addr); #gethostbyaddr($host, AF_INET);
			return ($addr, $port);
		}
		return;
	}

	sub on_msg {
		my ($self, $obj, $addr, $port) = @_;
		return if $obj->{type} and $obj->{tag} and $self->{reqs}{$obj->{tag}}; # Request from myself?

		my $cont; $cont = $self->rt_update({	# Update RT on each message
			id => Kad::Id->new($obj->{id}),
			addr => "$addr:$port",
		}) if $obj->{id};

		given ($obj->{type}) {
		when ('stdin') {	# Input from keyboard
			return unless $addr eq "127.0.0.1";	# Small protection ;)
			given ($obj->{text}) {
			when (/^exit$/) { exit; }
			when (/^ping (.*)$/) {
				 print "Ping to $1...\n";
				 $self->ping($1, sub { print $_[0] ? "OK!\n" : "Fail\n"; });
			} when (/^join (.*)$/) {
				 print "Joining to $1...\n";
				 $self->join(sub { print $_[0] ? "OK!\n" : "Fail ;(\n"; }, $1);
			} when (/^find (.*)$/) {
				$self->find(Kad::Id->from_hash($1), sub {
					print $_[0] ? "Addr: $_[0]->{addr}\n" : "Not found\n";
				});
			} when (/^set (\S*) (.*)$/) {
				$self->store(Kad::Id->from_hash($1), $2, sub {
					print $_[0] ? "Stored to $_[0] nodes\n" : "Failed: Stored only locally\n";
				});
			} when (/^get (\S*)$/) {
				$self->retrieve(Kad::Id->from_hash($1), sub {
					print $_[0] ? "Value: $_[0]{val}\nAge: " . (time - $_[0]{pub_time}) . "s\n" : "Not found\n";
				});
			} when (/^hash (\S*)$/) {
				print Kad::Id->from_hash($1);
			} when (/^dump$/) {
				print "State: $self->{state}\n";
				print Data::Dumper->Dump([$self->{rt}, $self->{ds}{data}], [qw(rt ds)]);
			}}
		} when ($RPC{INFO}) {		# Kad PING
			$self->udp_send({
				tag => $obj->{tag}
			}, "$addr:$port");
		} when ($RPC{FIND}) {		# Kad FIND_NODE and FIND_VALUE (tgt or key)
			if (defined $obj->{tgt} or (defined $obj->{key} and not $self->{ds}->has($obj->{key}))) {
				my @res = $self->rt_find_closest(
					Kad::Id->new($obj->{tgt} or $obj->{key}),
					$Kad::Config{RT_K}, Kad::Id->new($obj->{id}));
				my @res2; push @res2, { id => "$_->{id}", addr => $_->{addr} } foreach @res;
				$self->udp_send({ nodes => \@res2, tag => $obj->{tag} }, "$addr:$port");
			} elsif (defined $obj->{key} and $self->{ds}->has($obj->{key})) {
				$self->udp_send({ value => $self->{ds}->get($obj->{key}), tag => $obj->{tag} }, "$addr:$port");
			}
		} when ($RPC{STORE}) {		# Kad STORE
			if (defined $obj->{key} and defined $obj->{val}) {
				print "Set $obj->{key} to $obj->{val}\n" if $DBG;
				$self->{ds}->set($obj->{key}, $obj->{val}, $obj->{id});
				$self->udp_send ({ tag => $obj->{tag} }, "$addr:$port");
			}
		} when (undef) {			# Reply?
			if ($obj->{tag} and my $req = delete $self->{reqs}{$obj->{tag}}) {
				$req->{cbk}->($obj, $addr, $port);
			} else {
				print ("Unhandled rsp: [$addr:$port] tag: $obj->{tag}\n") if $DBG;
			}
		} default {
			print "Unhandled req: [$addr:$port] type: $obj->{type}, tag: $obj->{tag}\n";
		}}
	}

	sub udp_recv {
		my ($self) = @_;
		$self->{sock}->recv(my $msg, 4096);
		return unless $msg;
		my ($addr, $port) = _endpoint($self->{sock}->peername);
		return if $port < $Kad::Config{MIN_PORT};
		my ($len, $obj) = (length $msg, decode_json $msg);
		print "<= udp[$addr:$port] ($len) $msg\n" if $DUMP;
		on_msg($self, $obj, $addr, $port);
	}

	sub udp_send {
		my ($self, $obj, $to, $cbk) = @_;
		die unless $obj and $to;
		my ($addr, $port) = split /:/, $to;
		return if $port < $Kad::Config{MIN_PORT};
		$obj->{id} = "$self->{lid}"; # set id
		# generate tag
		$obj->{tag} = join('', ('0'..'9', 'a'..'z', 'A'..'Z', '-', '_')[
			map { rand 64 } (1 .. 12) # 64-bit number equivalent
		]) unless exists $obj->{tag};
		# register callbacks
		$self->{reqs}{$obj->{tag}} = {
			cbk => $cbk,
			tmr => AnyEvent->timer (after => $Kad::Config{TIMEOUT_REQ}, cb => sub {
				print "Timeout: $obj->{tag}\n" if $DBG;
				delete $self->{reqs}{$obj->{tag}};
				$cbk->();
			})
		} if ($cbk);
		defined $obj->{$_} or delete $obj->{$_} for keys $obj;	# cleanup undefs
		# actually send
		my $msg = encode_json($obj);
		my $len = length($msg);
		print "=> udp[$to] ($len) $msg\n" if $DUMP;
		send ($self->{sock}, $msg, 0, _endpoint($addr, $port));
	}

	sub find_iter {
		my ($self, $tgt, $mode, $cbk) = @_; # mode can be qw(one all val)
		# context of async operation
		my @res = $self->rt_find_closest($tgt, $Kad::Config{RT_K});
		unless (scalar @res) { $cbk->(($mode eq 'val') ? undef : \@res); return }
		if ($mode eq 'one' and $res[0]->{id} == $tgt) { $cbk->(\@res); return } # found in local table -> finish
		my $pend = 0; my %sent; my %seen;
		$seen{$self->{lid}} = 1;	# treat myself as seen
		$seen{$_->{id}} = 1 foreach @res;

		# helpers
		my $ask;
		$ask = sub {
			print "Res: ", scalar @res, ", Pending: $pend, Seen: ", scalar keys %seen, ", Sent: ", scalar keys %sent, "\n" if $DBG;
			foreach my $c (grep { !$sent{$_->{id}} } @res) {
				last if $pend >= $Kad::Config{ALPHA};
				$pend++;
				print "Asking: $c->{id}\n" if $DBG;
				$sent{$c->{id}} = 1;
				$self->udp_send({
					type => $RPC{FIND}, (($mode eq 'val')?'key':'tgt') => "$tgt"
				}, $c->{addr}, sub {
					my ($obj) = @_;
					return unless $tgt; # operation already finished
					$pend--;
					if ($obj and $obj->{nodes}) {
						my $added = 0; # add non-seen items to list
						foreach my $c (grep { !$seen{$_->{id}} } @{$obj->{nodes}}) {
							$c->{id} = Kad::Id->new($c->{id});
							$seen{$c->{id}} = 1; $added++; push @res, $c; $self->rt_update($c);
						}
						# unless found new nodes -> finish
						unless ($added) { $tgt = undef; $cbk->(($mode eq 'val')?undef:\@res); return }
						@res = sort { ($a->{id} ^ $tgt) <=> ($b->{id} ^ $tgt) } @res;
						@res =  @res[0 .. min($Kad::Config{RT_K}-1, $#res)];
						if ($DBG) { print "Res distances:\n"; print '  ', ($_->{id} ^ $tgt)->as_hex(), "\n" foreach @res; }
						if ($mode eq 'one' and $res[0]->{id} == $tgt) {
							$tgt = undef; $cbk->(\@res); return;		# if found required node -> finish
						}
					} elsif ($obj and $obj->{value} and $mode eq 'val') {
						$tgt = undef; $cbk->($obj->{value}); return;	# if found required value -> finish
					}
					$ask->();
					# if no new requests -> finish
					unless ($pend) { $tgt = undef; $cbk->(($mode eq 'val') ? undef : \@res); return }
				});
			}
		};
		$ask->();
	}

	################################
	# API
	################################

	sub ping {
		my ($self, $addr, $cbk) = @_;
		$self->udp_send ({ type => $RPC{INFO} }, $addr, $cbk);
	}

	sub join {
		my ($self, $cbk, @addrs) = @_;
		my $pend = scalar @addrs;
		my $found = 0;
		my $prev = $self->{state}; $self->{state} = $STATE{JOINING};
		# ping all peers
		$self->ping($_, sub {
			my ($obj, $addr, $port) = @_;

			$found++ if $obj;
			return if --$pend;
			if (!$found) { $self->{state} = $prev; $cbk->(); return } # no peers responded ;(
			# find myself
			$self->find_iter($self->{lid}, 'all', sub {
				# TODO: Refresh buckets
				$self->{state} = $STATE{JOINED}; $cbk->(1);
			});
		}) foreach @addrs;
	}

	sub find {
		my ($self, $id, $cbk) = @_;
		unless ($STATE{JOINED} == $self->{state}) { $cbk->(); return }
		$self->find_iter($id, 'one', sub {
			if (my ($ct) = @_) {
				if ($ct->[0]->{id} == $id) {
					$cbk->($ct->[0]); return;
				}
			}
			$cbk->();
		});
	}

	sub store {
		my ($self, $key, $val, $cbk) = @_;
		unless ($STATE{JOINED} == $self->{state}) { $cbk->(0); return }
		$self->find_iter($key, 'all', sub {
			if (my ($ct) = @_) {
				my ($pend, $done) = (scalar @{$ct}, 0);
				# TODO: Store to myself if I'm closer than farthest
				foreach my $c (@{$ct}) {
					$self->udp_send ({
						type => $RPC{STORE}, key => "$key", val => $val
					}, $c->{addr}, sub {
						my ($obj) = @_;
						$done++ if $obj;
						$cbk->($done) unless --$pend;
					});
				}
			} else { $cbk->(); }
		});
	}

	sub retrieve {
		my ($self, $key, $cbk) = @_;
		unless ($STATE{JOINED} == $self->{state}) { $cbk->(); return }
		if ($self->{ds}->has($key)) {
			$cbk->($self->{ds}->get($key));
		} else {
			$self->find_iter($key, 'val', $cbk);
		}
	}






__END__

=encoding utf-8

=head1 NAME

Net::BitTorrent - It's new $module

=head1 SYNOPSIS

    use Net::BitTorrent;

=head1 DESCRIPTION

Net::BitTorrent is ...

=head1 LICENSE

Copyright (C) Sanko Robinson.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=head1 AUTHOR

Sanko Robinson E<lt>sanko@cpan.orgE<gt>

=cut

