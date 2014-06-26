package Net::BitTorrent::Magnet;
use Moose;
use Types::Standard qw[Str];
use URI;
use URI::QueryParam;
use URI::magnet;
extends 'Net::BitTorrent';
#use Net::BitTorrent::DHT;

# Custom types
my $LINK = Type::Tiny->new(name       => 'Link',
                           parent     => Str,
                           constraint => sub { URI->new($_)->scheme() eq 'magnet' },
                           message    => sub {"$_ isn't a magnet link"}
);

has '+path' => (required => 0);
has link => (is => 'ro', isa => $LINK, required => 1);



# Override default from Net::BitTorrent that parses metadata in a file
override _build_metadata => sub {
    my $s = shift;
    return {}
};


sub tinker {
    use Data::Dump;


    my $uri = URI->new(shift->link);


ddx $uri;
warn $uri->scheme;
warn $uri->path;
warn $uri->fragment;



warn "found " . $uri->display_name;
    warn "size: " . $uri->exact_length . " bytes";

    if ( my $source = $uri->acceptable_source ) {
      warn "has direct link: $source";
    }

    warn "the following trackers are available: ". join ', ' => $uri->address_tracker;

  my $topic_urn = $uri->exact_topic;
  warn "hash type: "  . $topic_urn->nid;
  warn "hash value: " . $topic_urn->nss;

    }

sub locate_peers {
    my $s = shift;

    my $DHT = new Net::BitTorrent::DHT();
    $DHT->get_peers();

    my $uri = URI->new($s->link);

  my $topic_urn = $uri->exact_topic;
  warn "hash type: "  . $topic_urn->nid;
  warn "hash value: " . $topic_urn->nss;


    }
