#!/usr/bin/perl

use strict;
use warnings;

use CGI qw(:cgi);
use Socket;
use URI::Escape;

my $port = 2763;
my $logdir = "/var/log/attempto/ws/acerules";

my $input = "";
read(STDIN, $input, $ENV{'CONTENT_LENGTH'});
$input =~ s/\n\s*/\n/g;

# 'input' must be defined
print_error_and_exit("undefined input") if ($input eq "");

my $host = 'localhost';
my $proto = getprotobyname('tcp');

my $iaddr = inet_aton($host);
my $paddr = sockaddr_in($port, $iaddr);

# Create the socket, connect to the port
socket(SOCKET, PF_INET, SOCK_STREAM, $proto) || print_error_and_exit("fail socket init");

# Attempt to connect to the socket
my $result_connect = connect(SOCKET, $paddr);
if(!$result_connect) {
	print_error_and_exit("fail socket connect");
}

# Set $| to non-zero to make selection autoflushed
my $oldfh = select(SOCKET);

$| = 1;

print SOCKET "$input\n\n";

my $results = "";

# BUG: Read the whole socket at once, not line by line.
while(<SOCKET>) {
	last if /^\.$/;
	$results = $results . "$_";
}

print SOCKET "quit.\n\n";
close SOCKET || print_error_and_exit("fail socket close");
select($oldfh);

print STDOUT <<EOF;
Content-type: text/xml

$results
EOF
# content-type should be "application/soap+xml" but IE6 does not parse it

add_log_entry($input, $results);

exit;


sub print_error_and_exit
{
	my $error_message = shift;
	
	print <<EOF;
Content-type: text/xml

<?xml version="1.0" encoding="UTF-8"?>

<env:Envelope xmlns:env="http://schemas.xmlsoap.org/soap/envelope/">
<env:Body>
<env:Fault>
<env:faultcode>env:Sender</env:faultcode>
<env:faultstring>$error_message</env:faultstring>
</env:Fault>
</env:Body>
</env:Envelope>
EOF
# content-type should be "application/soap+xml" but IE6 does not parse it
	
	exit;
}


sub add_log_entry
{
	my $input = shift;
	my $output = shift;
	
	$input =~ s/\n+/\n/g;
	$output =~ s/\n+/\n/g;
	$input =~ s/\n$//;
	$output =~ s/\n$//;

	my $timestamp = localtime;

	# Try to get at least one of those, REMOTE_HOST would be better...
	my $remote_host = "NO_HOST";
	$remote_host = $ENV{'REMOTE_ADDR'} if defined $ENV{'REMOTE_ADDR'};
	$remote_host = $ENV{'REMOTE_HOST'} if defined $ENV{'REMOTE_HOST'};

	my $http_user_agent = "NO_AGENT";
	$http_user_agent = $ENV{'HTTP_USER_AGENT'} if defined $ENV{'HTTP_USER_AGENT'};

	my $log_entry = "[ " .
			$timestamp . " | " .
			$remote_host . " | " .
			$http_user_agent . " ]\n\n" .
			$input . "\n\n" .
			$output . "\n\n";

	open(LOGFILE, ">> $logdir/acerulesws.log");

	print LOGFILE $log_entry;

	close LOGFILE;
}
