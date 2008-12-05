#!/usr/bin/perl -w
package Net::BitTorrent::Version;
{
    use strict;
    use warnings;
    use version qw[qv];
    our $SVN = q[$Id$];
    our $VERSION_BASE = 42; our $UNSTABLE_RELEASE = 0; our $VERSION = sprintf(($UNSTABLE_RELEASE ? q[%.3f_%03d] : q[%.3f]), (version->new(($VERSION_BASE))->numify / 1000), $UNSTABLE_RELEASE);
    our $PRODUCT_TOKEN = qq[Net::BitTorrent $VERSION];

    sub gen_peerid {
        return pack(
            q[a20],
            (sprintf(
                 q[NB%03d%1s-%8s%5s],
                 $VERSION_BASE,
                 ($UNSTABLE_RELEASE ? q[U] : q[S]),
                 (join q[],
                  map {
                      [q[A] .. q[Z], q[a] .. q[z], 0 .. 9, qw[- . _ ~]]
                      ->[rand(66)]
                      } 1 .. 8
                 ),
                 q[Riana]
             )
            )
        );
    }

    sub gen_node_id {
        return
            pack(q[a20],
                 (join(q[],
                       map { [q[a] .. q[z], q[A] .. q[Z]]->[rand(52)] }
                           1 .. 20)
                 )
            );
    }
    1;
}

=pod

=head1 NAME

Net::BitTorrent::Version - Net::BitTorrent's project-wide version numbers

=head1 DESCRIPTION

Because of the problems coordinating revision numbers in a distributed
version control system and across a directory full of Perl modules, this
module provides a central location for the project's overall release
number, the version string provided in Extended Protocol handshakes,
and the Peer ID generator.

=head1 Methods

=head2 C<gen_node_id ( )>

Returns a random 20-byte string that can be used to identify ourself in a
DHT swarm.

=head2 C<gen_peerid ( )>

Generates a unique Peer ID based on Net::BitTorrent's
L<Specification|/"Peer ID Specification">.

=head1 Peer ID Specification

This section describes and provides examples of the Peer ID format used
by this release of the C<Net::BitTorrent> module.

=head2 Overview

This non-standard format was developed to be URL-safe, unique to the
implementation, and "human parsable."

There are two distinct sections to the Peer IDs generated: the
L<header|/Header> which may be used to identify the software and its
version, and the L<signature|/Signature> which is... well, it's junk.
Consider this example:

 NB004S-rogzGB1v--SVN

Here, C<NB004S> is the header and C<-rogzGB1v--SVN> is the trailing
signature.

=head3 Header

The header consists of two uppercase characters ('C<NB>') followed by
three digits (with leading zeros) representing the SVN revision, and a
single character used to (potentially) indicate stability: C<S>table or
C<U>nstable.  Any deviation from this indicates a bad client.  Recent
stable builds will be found on CPAN.

=head3 Signature

The remainder of the Peer ID is a hyphen followed by 13 random characters
in the following range:

 ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~

That is, all uppercase and lowercase letters, decimal digits, as well as
the hyphen, period, underscore, and tilde (66 total).  These are all
characters allowed in a URL without being encoded (referred to as
"Unreserved Characters" in rfc://3986).

=head2 Version Numbers and Stability

Distribution version numbers will be some value less than one (C<1>) with
the revision number in the three significant decimal places.

Recent stable builds will be found on CPAN.  Unstable builds will most
likely be SVN checkouts and temporary uploads to CPAN where the package
name matches C<m[\d\.\d+_\d]>.  See the PAUSE FAQ section entitled
"Developer Releases" (L<http://www.cpan.org/modules/04pause.html>).

=head2 Examples

=over

=item C<NB393S-04.9EraQ--SVN>

This would be the stable CPAN release C<v0.393>/SVN r393.  The C<--SVN>
signature does not imply an unstable build.

=item C<NB003X-9E-ayR6-IE<lt>3BT>

Improper Peer ID; the sixth char is neither 'C<S>' nor 'C<U>' and E<lt>
is a reserved character according to rfc://3986.

=item C<NB065U--09Egae69sy8W>

Completely legal Peer ID generated by unstable SVN/Dev r65.

=back

=head1 See Also

RFC 3986 (URI: Generic Syntax)
Section 2.3. "Unreserved Characters"
(http://tools.ietf.org/html/rfc3986#section-2.3)

PAUSE FAQ sub-section entitled "Developer Releases"
(http://www.cpan.org/modules/04pause.html)

http://slashdot.org/comments.pl?sid=997033&cid=25390887

=head1 Disclaimer

This document and the specification behind it are subject to change.
All modifications will be documented in the Changes file included with
this distribution.  All versions of this file can be found in the
project's SVN repository.

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
