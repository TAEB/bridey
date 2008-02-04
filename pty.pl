#!/usr/bin/env perl
use strict;
use warnings;

use IO::Pty::Easy;
use IO::Socket;

my $lis_sock = IO::Socket::INET->new(Listen => 1, LocalAddr => 'localhost',
                                 Proto => 'tcp');
$|++;

open PORT, ">port";
print PORT $lis_sock->sockport;
close PORT;

my $sock = $lis_sock->accept;

my $pty = IO::Pty::Easy->new;
$pty->spawn("nethack -D");

for (;;)
{
    last unless $pty->is_active;
    my $out = $pty->read(0.05);
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
