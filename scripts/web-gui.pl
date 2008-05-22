use strict;
use warnings;
use CGI qw[];
use Archive::Zip qw[:ERROR_CODES];
use JSON qw[to_json];
use lib q[../lib];
use Net::BitTorrent;
use HTTP::Daemon;
use HTTP::Status;
use IO::Select;
use MIME::Base64 qw( encode_base64 );
$|++;

# Configuration
my $PORT = 9000;
my $USER = q[username];
my $PASS = q[password];

# Stop editing
my $AUTH = encode_base64($USER . q[:] . $PASS);
chomp $AUTH;
-e q[webui.zip] or die <<'END';
You seem to be missing webui.zip from the µTorrent WebUI project.  Please
download the zip file from http://forum.utorrent.com/viewtopic.php?id=14565

Oh, and don't extract the zip.
END
my $zip    = Archive::Zip->new();
my $status = $zip->read(q[webui.zip]);
die q[Failed to open webui.zip] if $status != AZ_OK;
my $bittorrent = new Net::BitTorrent()
    or die sprintf q[Failed to create Net::BitTorrent object (%s)], $^E;
my $server = HTTP::Daemon->new(LocalPort => $PORT) || die;
print q[[alpha] Net::BitTorrent/µTorrent WebUI: <URL:], $server->url,
    qq[gui/>\n];

# Altnernative Event Processing
while (1) {
    my ($rin, $win, $ein) = (q[], q[], q[]);
    vec($rin, fileno($server), 1) = 1;
    for my $object (values %{$bittorrent->_connections}) {
        vec($rin, $object->_fileno, 1) = 1;
        vec($win, $object->_fileno, 1) = 1
            if $object ne $bittorrent and $object->_queue_outgoing;
    }
    $ein = $rin | $win;
    my ($nfound) = select($rin, $win, $ein, 1);
    $bittorrent->process_timers;    # Don't forget this!
    $bittorrent->process_connections(\$rin, \$win, \$ein)
        if $nfound and $nfound != -1;
    new_http() if vec($rin, fileno($server), 1);
}

# Wow, that was easy...
sub new_http {
    my $client = $server->accept;
    while ($client and my $r = $client->get_request(1)) {

        # warn $r->url;
        if (                        #$r->method eq q[GET]
                                    #and
            $r->url->path =~ m[^/gui(?:/.+)?]
            )
        {
            if (not defined $r->header(q[Authorization])
                or $r->header(q[Authorization]) ne q[Basic ] . $AUTH)
            {   my $res = HTTP::Response->new(401);
                $res->push_header(
                       WWW_Authenticate => qq(Basic Realm="Net::BitTorrent"));
                $client->send_response($res);
            }
            elsif ($r->url->query) {
                my $query = new CGI($r->url->query);
                if (defined $query->param(q[action])) {
                    warn q[Doing action... ] . $query->param(q[action]);
                    syswrite $client,
                        qq[HTTP/1.0 200 OK\nContent-Type: text/html\n\n];
                    syswrite $client, to_json action($client, $r, $query);
                }
                elsif (defined $query->param(q[list])) {
                    warn q[Doing list... ] . $query->param(q[list]);
                    syswrite $client,
                        qq[HTTP/1.0 200 OK\nContent-Type: text/html\n\n];
                    syswrite $client, to_json list($query);
                }
                else {
                    $client->send_error(RC_BAD_REQUEST);
                }
            }
            else {
                my $path = $r->url->path;
                $path =~ s|/gui/||;
                my $content = $zip->contents($path || q[index.html]);
                if ($content) {
                    $client->send_response(
                          HTTP::Response->new(RC_OK, undef, undef, $content));
                }
                else {
                    $client->send_error(RC_NOT_FOUND);
                }
            }
        }
        else {
            $client->send_error(RC_FORBIDDEN);
        }
        $client->close;
        undef($client);
    }
}

sub action {
    my ($client, $request, $query) = @_;
    my %return = (build => 9704);    # lies
    if ($query->param(q[action]) eq q[getsettings]) {
        $return{q[settings]} = [
            [q[bind_port],           0, 9000],
            [q[conns_globally],      0, 200],
            [q[conns_per_torrent],   0, 100],
            [q[enable_scrape],       0, 1],
            [q[max_dl_rate],         0, 0],
            [q[max_ul_rate],         0, 8],
            [q[net.bind_ip],         2, q[]],
            [q[net.max_halfopen],    0, 8],
            [q[reload_freq],         0, 0],
            [q[tracker_ip],          2, q[]],
            [q[webui.cookie],        2, q[{}]],
            [q[webui.enable_guest],  0, 0],
            [q[webui.enable_listen], 0, 1],
            [q[webui.guest],         2, q[guest]],
            [q[webui.password],      2, $PASS],
            [q[webui.port],          0, $PORT],

            #[q[webui.restrict],      2, q[]],
            [q[webui.username], 2, $USER]
        ];
    }
    elsif ($query->param(q[action]) eq q[start]) {

        # http://[IP]:[PORT]/gui/?action=start&hash=[TORRENT HASH]
        #
        # This action tells µTorrent to start the specified torrent job(s).
        # Multiple hashes may be specified to act on multiple torrent jobs.
    }
    elsif ($query->param(q[action]) eq q[stop]) {

        # http://[IP]:[PORT]/gui/?action=stop&hash=[TORRENT HASH]
        #
        # This action tells µTorrent to stop the specified torrent job(s).
        # Multiple hashes may be specified to act on multiple torrent jobs.
    }
    elsif ($query->param(q[action]) eq q[pause]) {

        # http://[IP]:[PORT]/gui/?action=pause&hash=[TORRENT HASH]
        #
        # This action tells µTorrent to pause the specified torrent job(s).
        # Multiple hashes may be specified to act on multiple torrent jobs.
    }
    elsif ($query->param(q[action]) eq q[unpause]) {

        # http://[IP]:[PORT]/gui/?action=unpause&hash=[TORRENT HASH]
        #
        # This action tells µTorrent to unpause the specified torrent job(s).
        # Multiple hashes may be specified to act on multiple torrent jobs.
    }
    elsif ($query->param(q[action]) eq q[forcestart]) {

        # http://[IP]:[PORT]/gui/?action=forcestart&hash=[TORRENT HASH]
        #
        # This action tells µTorrent to force the specified torrent job(s) to
        # start.  Multiple hashes may be specified to act on multiple torrent
        # jobs.
    }
    elsif ($query->param(q[action]) eq q[recheck]) {

        # http://[IP]:[PORT]/gui/?action=recheck&hash=[TORRENT HASH]
        #
        # This action tells µTorrent to recheck the torrent contents for the
        # specified torrent job(s).  Multiple hashes may be specified to act
        # on multiple torrent jobs.
        for my $hash ($query->param(q[hash])) {
            my $session = $bittorrent->_locate_session($hash);
            $session->hash_check if $session;
        }
    }
    elsif ($query->param(q[action]) eq q[remove]) {

        # http://[IP]:[PORT]/gui/?action=remove&hash=[TORRENT HASH]
        #
        # This action removes the specified torrent job(s) from the torrent
        # jobs list.  Multiple hashes may be specified to act on multiple
        # torrent jobs.  This action currently ignores the "Move to trash if
        # possible" option in µTorrent 1.7.x, though that'll change in
        # µTorrent 1.8 (post build 9272).
    }
    elsif ($query->param(q[action]) eq q[removedata]) {

        # http://[IP]:[PORT]/gui/?action=removedata&hash=[TORRENT HASH]
        #
        # This action removes the specified torrent job(s) from the torrent
        # jobs list and removes the corresponding torrent contents (data) from
        # disk.  Multiple hashes may be specified to act on multiple torrent
        # jobs.  This action currently ignores the "Move to trash if possible"
        # option in µTorrent 1.7.x, though that'll change in µTorrent 1.8
        # (post build 9272).
    }
    elsif ($query->param(q[action]) eq q[setprio]) {

# http://[IP]:[PORT]/gui/?action=setprio&hash=[TORRENT HASH]&p=[PRIORITY]&f=[FILE INDEX]
#
# This action sets the priority for the specified file(s) in the
# torrent job.  The possible priority levels are the values returned
# by "getfiles".  A file is specified using the zero-based index of
# the file in the inside the list returned by "getfiles".  Only one
# priority level may be specified on each call to this action, but
# multiple files may be specified.
    }
    elsif ($query->param(q[action]) eq q[add-url]) {

        #http://[IP]:[PORT]/gui/?action=add-url&s=[TORRENT URL]
        #
        # This action adds a torrent job from the given URL.  For servers that
        # require cookies, cookies can be sent with the :COOKIE: method (see
        # here). The string must be URL-encoded.
    }
    elsif ($query->param(q[action]) eq q[add-file]) {

        # http://[IP]:[PORT]/gui/?action=add-file
        #
        # This action is different from the other actions in that it uses HTTP
        # POST instead of HTTP GET to submit data to µTorrent.  The HTTP form
        # must use an enctype of "multipart/form-data" and have an input field
        # of type "file" with name "torrent_file" that stores the local path
        # to the file to upload to µTorrent.
        my $dot_torrent = read_post($client, $request->content_length || 0);
        printf q[Loading '%s'...], $dot_torrent;
        my $session =
            $bittorrent->add_session({path           => $dot_torrent,
                                      skip_hashcheck => 1
                                     }
            )
            or warn sprintf q[Cannot load .torrent (%s): %s],
            $dot_torrent, $^E;
        printf qq[ OK. Infohash = %s\n], $$session if $session;
    }
    elsif ($query->param(q[action]) eq q[getfiles]) {

     # http://[IP]:[PORT]/gui/?action=getfiles&hash=[TORRENT HASH]
     #
     # It is possible to send multiple hashes to µTorrent by stringing one
     # hash GET variable after another
     # (?action=getfiles&hash=[TORRENT HASH1]&hash=[TORRENT HASH 2]&hash=...),
     # but µTorrent will send multiple "files" key/value pairs in its JSON
     # output (each formatted exactly as described above).  I guess this
     # behavior might be useful for people who want to preload file lists.
        my $session = $bittorrent->_locate_session($query->param(q[hash]));
        if ($session) {
            my @files;
            for my $file (@{$session->files}) {
                push @files, [
                    $$file,
                    $file->size,    # size
                    (               # downloaded TODO
                       (scalar grep { $_->check } $file->pieces)
                       * (scalar $session->piece_size)
                    ),
                    2    # priority (0=Skip,1=Low,2=Normal,3=High) TODO
                ];
            }
            $return{q[files]} = [$$session, \@files];
        }
    }
    elsif ($query->param(q[action]) eq q[getprops]) {

        # http://[IP]:[PORT]/gui/?action=getprops&hash=[TORRENT HASH]
    }
    elsif ($query->param(q[action]) eq q[setprops]) {

# http://[IP]:[PORT]/gui/?action=setprops&hash=[TORRENT HASH]&s=[PROPERTY]&v=[VALUE]
#
# This sets the specified property to the specified value for the
# torrent job.  Multiple properties (and in fact, multiple torrent
# jobs) may be set simultaneously by with this action. Each v value is
# used as the value for the s property specified immediately before
# it, and any property/value pair will be set for the last specified
# torrent (hash) in the chain of GET variables.
#
# EXAMPLE:
# http://[IP]:[PORT]/gui/?action=setprops&hash=[TORRENT HASH 1]&s=ulrate&v=10240&s=dlrate&v=20480&hash=[TORRENT HASH 2]&s=ulslots&v=4
# This will set the torrent with [TORRENT HASH 1] to have an upload
# rate limit of 10 KiB/s and a download rate of 20 KiB/s, while
# simultaneously setting the torrent with [TORRENT HASH 2] to have 4
# upload slots.
    }
    return \%return;
}

sub list {
    my @torrents;
    for my $session (@{$bittorrent->sessions}) {
        push @torrents, [
            $$session,
            201,            # STATUS: TODO
                            # (201=Started, 961=Force Download, 1000=finished)
                            # 1 = Started
                            # 2 = Checking
                            # 4 = Start after check
                            # 8 = Checked
                            # 16 = Error
                            # 32 = Paused
                            # 64 = Queued
                            # 128 = Loaded
            $session->name, # NAME
            $session->total_size,    # SIZE
            (    # PERCENT PROGRESS (integer in 1/10 of a percent)
               ((  (scalar grep { $_->check } @{$session->pieces})
                 / (scalar @{$session->pieces})
                )
               ) * 100
            ),
            $session->downloaded,    # DOWNLOADED (integer in bytes)
            $session->uploaded,      # UPLOADED (integer in bytes)
            (                        # RATIO (integer in 1/10 of a percent)
               ($session->downloaded || 1) / ($session->uploaded || 1)
            ),
            0,      # UPLOAD SPEED (integer in bytes per second) TODO
            0,      # DOWNLOAD SPEED (integer in bytes per second) TODO
            -1,     # ETA (integer in seconds) TODO
            q[],    # LABEL (string) TODO
            0,      # PEERS CONNECTED (integer) TODO
            0,      # PEERS IN SWARM (integer) TODO
            0,      # SEEDS CONNECTED (integer) TODO
            0,      # SEEDS IN SWARM (integer) TODO
            0,      # AVAILABILITY (integer in 1/65535ths) TODO
            50,     # TORRENT QUEUE ORDER (integer) TODO
            $session->total_size
                - $session->downloaded,    # REMAINING (integer in bytes) TODO
        ];
    }
    return {
        build => 9704,                     # lies
        label => [

            #[q[Anime],    0], # [label, number of torrents in label]
            #[q[Audio],    1],
            #[q[Books],    0],
            #[q[Films],    0],
            #[q[Software], 0],
            #[q[TV],       4]
        ],
        messages => [q[BLAH!!!!!]]
        ,    # NFI what this should look like as it's always empty
        torrentc => time,        # CacheID - should really be random...
        torrentp => \@torrents
    };
}

sub read_post {                  # bad idea - Let CGI do this...
    my ($client, $length) = @_;
    my $data = q[];
    if ($length) {
        while (length($data) < $length) {
            last unless sysread($client, my ($buf), 512);
            $data .= $buf;
        }
    }
    my ($headers, $body) = split m[\015?\012\015?\012], $data, 2;
    $headers =~ s|^.+\015?\012||;
    $body    =~ s|\015?\012.+$||;
    my %headers = split m[\s?[:;=\s]\s?], $headers;
    grep {s|^"(.+)"$|$1|} values %headers;
    defined $headers{q[name]} or return;
    $headers{q[name]} eq q[torrent_file] or return;
    open(my ($FH), q[>], $headers{q[filename]}) or return;
    syswrite($FH, $body, length $body) or return;
    close $FH or return;
    return $headers{q[filename]};
}
__END__

=pod

=head1 Name

webgui.pl - Very basic webgui leveraging the µTorrent WebUI project

=head1 Description

This began as a short alternative
L<event processing|Net::BitTorrent/"Alternative Event Processing">
example.

Like nearly everything else related to µTorrent, the backend of the WebUI
is undocumented and closed sourced, so there's a great chance that I'll
never complete this.  You're welcome to hack away at it though.

=head1 Installation

=over

=item *

Download the latest (...so far, there's only been one release, but you
never know) webui.zip file through the uTorrent forums:
http://forum.utorrent.com/viewtopic.php?id=14565

=item *

Save the .zip file in the current working directory.  Do not extract the
zip.

=item *

Start the script.

=item *

Load http://localhost/gui/ in your browser.  The login user:pass (static
for now) are C<username:password>. Hey, I did say it was just an example.

=item *

???

=item *

Profit.

=back

=head1 Dependancies

For something so simple, this thing uses a boat load of non-core modules:

=over

=item HTTP::Daemon

=item HTTP::Status

=item JSON

=item Archive::Zip

=item Net::BitTorrent (of course!)

=back

=head1 TODO

=over

=item *

Everything.

Seriously.  I haven't written anything beyond initial load and (partial)
torrent and file lists.  The structure is here if anyone else would like
to do it.  Hint, hint.

=back

=head1 Description

This is a B<very> basic demonstration of a full C<Net::BitTorrent>-based
client.

=head1 See Also

Main WebUI forum: http://forum.utorrent.com/viewforum.php?id=20

WebUI v0.310 Public beta 2:
http://forum.utorrent.com/viewtopic.php?id=14565

Web UI API: http://forum.utorrent.com/viewtopic.php?id=25661

Overview of everything WebUI-related:
http://forum.utorrent.com/viewtopic.php?id=33186

=for svn $Id$

=cut
