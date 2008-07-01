use strict;
use warnings;
use CGI qw[];
use Time::HiRes qw[sleep time];
use Archive::Zip qw[:ERROR_CODES];
use JSON qw[to_json];
use HTTP::Daemon qw[];
use HTTP::Status qw[/RC_/];
use IO::Select qw[];
use MIME::Base64 qw[];
use lib q[../lib];
use Net::BitTorrent qw[];
use Net::BitTorrent::Util qw[:bencode];    # to store/load settings file
$|++;

# Configuration
my %config = (
    q[bind.port]         => 0,
    q[net.bind_ip]       => q[0.0.0.0],
    q[conns_globally]    => 300,
    q[conns_per_torrent] => 100,

    #q[enable_scrape],    0, 1,
     q[net.max_halfopen] => 8,    # max_halfopen

    #q[reload_freq],      0, 1,
    #q[tracker_ip],       2, q[],
    #q[webui.cookie],     2, q[{}],
    q[webui.enable_guest]  => 0,          # bool
    q[webui.enable_listen] => 1,
    q[webui.guest]         => q[guest],
    q[webui.username]      => q[admin],
    q[webui.password]      => q[],
    q[webui.port]          => 0,

    #     ["torrents_start_stopped",      1, "false"],
    #     ["confirm_when_deleting",       1, "true"],
    #     ["confirm_exit",                1, "false"],
    #     ["close_to_tray",               1, "true"],
    #     ["minimize_to_tray",            1, "true"],
    #     ["tray_activate",               1, "true"],
    #     ["tray.show",                   1, "true"],
    #     ["tray.single_click",           1, "true"],
    #     ["activate_on_file",            1, "true"],
    #     ["confirm_remove_tracker",      1, "true"],
    #     ["check_assoc_on_start",        1, "false"],
    #     ["reload_freq",                 0, "0"],
    q[bind_port] => 1337,

  #     ["tracker_ip",                  2, ""],
  #     ["dir_active_download_flag",    1, "true"],
  #     ["dir_torrent_files_flag",      1, "true"],
  #     ["dir_completed_download_flag", 1, "true"],
  #     ["dir_completed_torrents_flag", 1, "true"],
  #     ["dir_active_download",         2, "D:\\Download\\Torrents\\Storage"],
  #     ["dir_torrent_files",           2, "D:\\Download\\Torrents"],
  #     ["dir_completed_download", 2,
  #      "D:\\Download\\Torrents\\Storage\\Completed"
  #     ],
  #     ["dir_completed_torrents", 2, "D:\\Download\\Torrents\\Complete"],
  #     ["dir_add_label",          1, "true"],
    q[max_dl_rate] => 0,
    q[max_ul_rate] => 8,

#     ["max_ul_rate_seed",       0, "0"],
#     ["max_ul_rate_seed_flag",  1, "false"],
#     ["ul_auto_throttle",       1, "false"],
#     ["gui.ulrate_menu", 2, "0,5,10,15,20,30,40,50,100,150,200,300,400,500"],
#     ["gui.dlrate_menu", 2, "0,5,10,15,20,30,40,50,100,150,200,300,400,500"],
#     ["gui.manual_ratemenu",   1, "false"],
#     ["gui.persistent_labels", 2, "TV|JP-TV|Films|Anime|Audio|Software|Books"],
#     ["gui.compat_diropen",    1, "false"],
#     ["gui.alternate_color",   1, "true"],
#     ["sys.prevent_standby",   1, "true"],
#     ["sys.enable_wine_hacks", 1, "false"], # GUI
     q[ul_slots_per_session] => 1, #ul_slots_per_torrent
    q[conns_per_torrent] => 100,
    q[conns_globally]    => 200,

    #     ["max_active_torrent",    0, "2"],
    #     ["max_active_downloads",  0, "1"],
    #     ["seed_prio_limitul",     0, "0"],
    #     ["seed_prio_limitul_flag", 1, "true"],
    #     ["seeds_prioritized",      1, "false"],
    #     ["seed_ratio",             0, "1500"],
    #     ["seed_time",              0, "0"],
    #     ["move_if_defdir",         1, "true"],
    #     ["mainwnd_split",          0, "483"],  # GUI
    #     ["mainwnd_split_x",        0, "150"],  # GUI
    #     ["resolve_peerips",        1, "false"],# GUI
    #     ["check_update",           1, "true"],
    #     ["check_update_beta",      1, "true"],
    #     ["anoninfo",               1, "true"],
    #     ["upnp",                   1, "true"],
    #     ["natpmp",                 1, "true"],
    #     ["lsd",                    1, "true"],
    #     ["disable_fw",             1, "false"],
    #     ["k",                      2, ""],
    #     ["v",                      0, "50342501"],
    #     ["sched_enable",           1, "false"],
    #     ["sched_ul_rate",          0, "0"],
    #     ["sched_dl_rate",          0, "0"],
    #     ["sched_dis_dht",          1, "true"],
    #     ["enable_scrape",          1, "true"],
    #     ["show_toolbar",           1, "true"],
    #     ["show_details",           1, "true"],
    #     ["show_status",            1, "true"],
    #     ["show_category",          1, "true"],
    #     ["show_tabicons",          1, "true"],
    #     ["rand_port_on_start",     1, "false"],
    #     ["prealloc_space",         1, "false"],
    #     ["language",               0, "0"],
    #     ["logger_mask",            0, "0"],
    #     ["autostart",              1, "false"],
    #     ["dht",                    1, "true"],
    #     ["dht_per_torrent",        1, "true"],
    #     ["pex",                    1, "true"],
    #     ["rate_limit_local_peers", 1, "false"],
    #     ["net.bind_ip",            2, ""],
    #     ["net.outgoing_ip",        2, ""],
    #     ["net.outgoing_port",      0, "0"],
    #     ["net.outgoing_max_port",  0, "0"],
    #     ["net.low_cpu",            1, "true"],
    #     ["net.calc_overhead",      1, "false"],
    q[net.max_halfopen] => 8,    # max_halfopen

  #     ["net.wsaevents",          0, "6"],
  #     ["dir_autoload_flag",      1, "false"],
  #     ["dir_autoload_delete",    1, "false"],
  #     ["dir_autoload",           2, ""],
  #     ["notify_complete",        1, "true"],
  #     ["extra_ulslots",          1, "false"],
  #     ["ipfilter.enable",        1, "true"],
  #     ["dht.rate",               0, "-1"],
  #     ["extras",                 0, "2"],
  #     ["score",                  0, "0"],
  #     ["append_incomplete",      1, "false"],
  #     ["show_add_dialog",        1, "true"],
  #     ["always_show_add_dialog", 1, "true"],
  #     ["gui.log_date",           1, "true"],
  #     ["ct_hist_comm",    2, "Test of Unicode/utf8.  Especially on Win32."],
  #     ["ct_hist_flags",   0, "0"],
  #     ["ct_hist_skip",    2, ""],
  #     ["boss_key",        0, "0"],
  #     ["encryption_mode", 0, "1"],
  #     ["encryption_allow_legacy",           1, "true"],
  #     ["rss.update_interval",               0, "15"],
  #     ["rss.smart_repack_filter",           1, "true"],
  #     ["gui.dblclick_seed",                 0, "0"],
  #     ["gui.dblclick_dl",                   0, "3"],
  #     ["gui.update_rate",                   0, "1000"],
  #     ["gui.sg_mode",                       0, "1"],
  #     ["gui.delete_to_trash",               1, "true"],
  #     ["gui.default_del_action",            0, "0"],
  #     ["gui.speed_in_title",                1, "true"],
  #     ["gui.limits_in_statusbar",           1, "true"],
  #     ["gui.graphic_progress",              1, "true"],
  #     ["gui.piecebar_progress",             1, "false"],
  #     ["gui.tall_category_list",            1, "false"],
  #     ["gui.bypass_search_redirect",        1, "true"],
  #     ["gui.last_preference_tab-1.8",       0, "8"],
  #     ["gui.last_overview_tab-1.8",         0, "4"],
  #     ["queue.dont_count_slow_dl",          1, "true"],
  #     ["queue.dont_count_slow_ul",          1, "true"],
  #     ["queue.slow_dl_threshold",           0, "1000"],
  #     ["queue.slow_ul_threshold",           0, "1000"],
  #     ["queue.use_seed_peer_ratio",         1, "true"],
  #     ["queue.prio_no_seeds",               1, "true"],
  #     ["bt.auto_ul_interval",               0, "600"],
  #     ["bt.auto_ul_sample_window",          0, "30"],
  #     ["bt.auto_ul_sample_average",         0, "10"],
  #     ["bt.auto_ul_min",                    0, "8500"],
  #     ["bt.auto_ul_factor",                 0, "80"],
  #     ["bt.transp_disposition",             0, "0"],
  #     ["bt.scrape_stopped",                 1, "true"],
  #     ["bt.compact_allocation",             1, "false"],
  #     ["bt.enable_tracker",                 1, "false"],
  #     ["bt.multiscrape",                    1, "true"],
  #     ["bt.send_have_to_seed",              1, "false"],
  #     ["bt.set_sockbuf",                    1, "true"],
  #     ["bt.connect_speed",                  0, "20"],
  #     ["bt.prio_first_last_piece",          1, "true"],
  #     ["bt.allow_same_ip",                  1, "false"],
  #     ["bt.no_connect_to_services",         1, "true"],
  #     ["bt.no_connect_to_services_list",    2, "25,110,6666,6667"],
  #     ["bt.ban_threshold",                  0, "3"],
  #     ["bt.use_ban_ratio",                  1, "true"],
  #     ["bt.ban_ratio",                      0, "128"],
  #     ["bt.use_rangeblock",                 1, "true"],
  #     ["bt.graceful_shutdown",              1, "false"],
  #     ["peer.lazy_bitfield",                1, "true"],
  #     ["peer.resolve_country",              1, "false"],
  #     ["peer.disconnect_inactive",          1, "true"],
  #     ["peer.disconnect_inactive_interval", 0, "300"],
  #     ["diskio.flush_files",                1, "true"],
  #     ["diskio.sparse_files",               1, "true"],
  #     ["diskio.use_partfile",               1, "true"],
  #     ["diskio.smart_hash",                 1, "true"],
  #     ["diskio.smart_sparse_hash",          1, "true"],
  #     ["diskio.coalesce_writes",            1, "true"],
  #     ["cache.override",                    1, "false"],
  #     ["cache.override_size",               0, "32"],
  #     ["cache.reduce",                      1, "true"],
  #     ["cache.write",                       1, "true"],
  #     ["cache.writeout",                    1, "true"],
  #     ["cache.writeimm",                    1, "true"],
  #     ["cache.read",                        1, "true"],
  #     ["cache.read_turnoff",                1, "true"],
  #     ["cache.read_prune",                  1, "true"],
  #     ["cache.read_thrash",                 1, "false"],
  #     ["cache.disable_win_read",            1, "false"],
  #     ["cache.disable_win_write",           1, "true"],
  #     ["webui.enable",                      0, "1"],
  #     ["webui.enable_guest",                0, "1"],
  #     ["webui.enable_listen",               0, "0"],
  #     ["webui.token_auth",                  1, "false"],
  #     ["webui.username",                    2, "admin"],
  #     ["webui.password",                    2, ""],
  #     ["webui.guest",                       2, "guest"],
  #     ["webui.restrict",                    2, ""],
  #     ["webui.port",                        0, "8080"],
  #     ["webui.cookie",                      2, "{}"],
  #     ["proxy.proxy",                       2, "localhost"],
  #     ["proxy.type",                        0, "0"],
  #     ["proxy.port",                        0, "8118"],
  #     ["proxy.auth",                        1, "false"],
  #     ["proxy.p2p",                         1, "false"],
  #     ["proxy.username",                    2, ""],
  #     ["proxy.password",                    2, ""],
    sessions => [

        #{
        #    path        => q[],
        #    infohash    => q[],
        #    bitfield => q[]
        #}
    ],
);
my $AUTH = setup_auth();
-e q[webui.zip] or die <<'END';
You seem to be missing webui.zip from the µTorrent WebUI project.  Please
download the zip file from http://forum.utorrent.com/viewtopic.php?id=14565

Oh, and don't extract the zip.
END
my $zip    = Archive::Zip->new();
my $status = $zip->read(q[webui.zip]);
die q[Failed to open webui.zip] if $status != AZ_OK;
my $bittorrent = new Net::BitTorrent(
    {LocalPort => $config{q[bind.port]},
     LocalHost => $config{q[net.bind_ip]},

     # advanced settings
     maximum_peers_per_client  => $config{q[conns_globally]},
     maximum_peers_per_session => $config{q[conns_per_torrent]},
     maximum_peers_half_open   => $config{q[net.max_halfopen]},
     kBps_down                 => $config{q[max_dl_rate]},
     kBps_up                   => $config{q[max_ul_rate]},
    }
    )
    or die sprintf q[Failed to create Net::BitTorrent object (%s)],
    $^E;
my $server = HTTP::Daemon->new(LocalPort => $config{q[webui.port]}) || die;
print q[[alpha] Net::BitTorrent/µTorrent WebUI: <URL:], $server->url,
    qq[gui/>\n];
while (1) {    # Altnernative Event Processing
    my ($rin, $win, $ein) = (q[], q[], q[]);
    vec($rin, fileno($server), 1) = 1;
    for my $object (values %{$bittorrent->_get_connections}) {
        vec($rin, $object->_get_fileno, 1) = 1;
        vec($win, $object->_get_fileno, 1) = 1
            if $object ne $bittorrent and $object->_get_queue_outgoing;
    }
    $ein = $rin | $win;
    my ($nfound, $timeleft) = select($rin, $win, $ein, 0.3);
    my $time = time + $timeleft;
    $bittorrent->process_timers();    # Don't forget this!
    $bittorrent->process_connections(\$rin, \$win, \$ein)
        if $nfound and $nfound != -1;
    new_http() if vec($rin, fileno($server), 1);
    sleep($time - time) if $time - time > 0;    # save the CPU
}    # Wow, that was easy...

sub new_http {
    my $client = $server->accept;
    while ($client and my $r = $client->get_request(1)) {
        #warn $r->url;
        if (    #$r->method eq q[GET]
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
                                     #warn $query->param(q[action])
    if ($query->param(q[action]) eq q[getsettings]) {
        $return{q[settings]} = [
            [q[bind_port],      0, $bittorrent->get_sockport()],
            [q[net.bind_ip],    2, $bittorrent->get_sockaddr()],
            [q[conns_globally], 0, $bittorrent->get_conns_per_client()],
            [q[conns_per_torrent], 0,$bittorrent->get_conns_per_session()],
            [q[enable_scrape],    0, 1],
            [q[max_dl_rate],      0, $bittorrent->get_max_dl_rate()],
            [q[max_ul_rate],      0, $bittorrent->get_max_ul_rate()],
            [q[net.max_halfopen], 0, $bittorrent->get_max_halfopen()],
            [q[reload_freq],      0, 1],
            [q[tracker_ip],       2, q[]],
            [q[webui.cookie],     2, q[{}]],
            [q[webui.enable_guest],  0, 0],
            [q[webui.enable_listen], 0, 1],
            [q[webui.guest],         2, q[guest]],
            [q[webui.username],      2, $config{q[webui.username]}],
            [q[webui.password],      2, $config{q[webui.password]}],
            [q[webui.port],          0, $config{q[webui.port]}],

            #[q[webui.restrict],      2, q[]],
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
        my $session = $bittorrent->add_session(
            {path => $dot_torrent,

             #skip_hashcheck => 1
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
            for my $file (@{$session->get_files}) {
                push @files, [
                    $$file,
                    $file->get_size,    # size
                    (               # downloaded TODO
                       (scalar grep { $_->get_cached_integrity } $file->get_pieces)
                       * (scalar $session->get_piece_size)
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
    for my $session (@{$bittorrent->get_sessions}) {
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
            $session->get_name, # NAME
            $session->get_total_size,    # SIZE
            sprintf(
                q[3.2f],    # PERCENT PROGRESS (integer in 1/10 of a percent)
                ((  (scalar grep { $_->get_cached_integrity } @{$session->get_pieces})
                  / (scalar @{$session->get_pieces})
                 )
                    ) * 1000
            ),
            $session->get_downloaded,    # DOWNLOADED (integer in bytes)
            $session->get_uploaded,      # UPLOADED (integer in bytes)
            (                        # RATIO (integer in 1/10 of a percent)
               ($session->get_downloaded || 1) / ($session->get_uploaded || 1)
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
            $session->get_total_size
                - $session->get_downloaded,    # REMAINING (integer in bytes) TODO
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
            last unless sysread($client, my ($buf), 1024 * 16);
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

sub load_settings {
}

sub save_settings {
}

sub setup_auth {
    if (@_) {
        $config{q[webui.username]} = shift;
        $config{q[webui.password]} = shift;
    }
    my $newauth = MIME::Base64::encode_base64(
              $config{q[webui.username]} . q[:] . $config{q[webui.password]});
    chomp $newauth;
    return $newauth;
}
__END__

=pod

=head1 NAME

web-gui.pl - Very Basic WebGUI Leveraging the µTorrent WebUI Project

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

Alternativly, you could try one of the (nifty) unofficial UI (iPhone,
mobile, even a Facebook plugin) while you review the
"Overview of everything WebUI-related" thread:
http://forum.utorrent.com/viewtopic.php?id=33186

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

Seriously.  I haven't written anything beyond initial ui load and
(partial) torrent and file lists.  The structure is here if anyone else
would like to do it.  Hint, hint.

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
