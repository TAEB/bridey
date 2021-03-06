#!/usr/bin/env perl
use strict;
use warnings;

use IO::Pty::Easy;
use IO::Socket;

my $lis_sock = IO::Socket::INET->new(
    Listen    => 1,
    LocalAddr => 'localhost',
    Proto     => 'tcp',
);

$|++;

open my $port_handle, ">port";
print $port_handle $lis_sock->sockport;
close $port_handle;

my $sock = $lis_sock->accept;

my $pty = IO::Pty::Easy->new;
$pty->spawn("nethack -u bridey -D");

while ($pty->is_active)
{
    my $out = $pty->read(0.08);
    if (defined($out))
    {
        print {$sock} "(", length($out), ")";
        print {$sock} $out;
        print $out;
    }
    else
    {
        print {$sock} "(0)";
    }
    recv($sock, $_, 1, 0);
    $pty->write($_) unless $_ eq chr(0);
}
