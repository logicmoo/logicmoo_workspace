#!/usr/bin/perl -w 

# poor man's way of finding a free port for snow
#  this really sucks, should hack snow to report a new port instead

use strict;
use IO::Socket;


socket(SOCK,PF_INET,SOCK_STREAM,(getprotobyname('tcp'))[2]);
bind( SOCK,  sockaddr_in(0, INADDR_ANY));
# pack( 'Sn4x8', AF_INET, 0, "127.0.0.1" ));
my $port = (sockaddr_in(getsockname(SOCK)))[0];
print("$port\n");
close(SOCK);

#--- testing
# socket(SOCK1,PF_INET,SOCK_STREAM,(getprotobyname('tcp'))[2]);
# bind( SOCK1,  sockaddr_in($port, INADDR_ANY));
# # pack( 'Sn4x8', AF_INET, 0, "127.0.0.1" ));
# my $port1 = (sockaddr_in(getsockname(SOCK1)))[0];
# print("Listening to port $port1.\n");
# close(SOCK1);
#my $socket = IO::Socket::INET->new( Proto     => "tcp",PeerAddr  => "localhost"        );
#my $port = $socket->sockport();
#print("Listening to port $port.\n");
