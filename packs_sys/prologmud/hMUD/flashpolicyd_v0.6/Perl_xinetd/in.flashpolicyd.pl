#!/usr/bin/perl
#
# in.flashpolicyd.pl
# Simple socket policy file server
#
# Usage: in.flashpolicyd.pl -file=FILE
# Logs to stderr
#

use strict;

my $NULLBYTE = pack( 'c', 0 );

my $filePath;
my $content;

### READ ARGS

while ( my $arg = shift @ARGV )
{
    if ( $arg =~ m/^--file=(.*)/ )
    {
        $filePath = $1;
    }
}

unless ( $filePath )
{
    die "Usage: in.flashpolicyd.pl --file=FILE\n";
}

### READ FILE

-f $filePath or die "No such file: '$filePath'\n";
-s $filePath < 10_000 or die "File probably too large to be a policy file: '$filePath'\n";

local $/ = undef;
open POLICYFILE, "<$filePath" or die "Can't open '$filePath': $!\n";
$content = <POLICYFILE>;
close POLICYFILE;

$content =~ m/cross-domain-policy/ or die "Not a valid policy file: '$filePath'\n";

### HANDLE CONNECTIONS

local $/ = $NULLBYTE;
my $request = <STDIN>;
chomp $request;

if ( $request eq '<policy-file-request/>' )
{
	print STDERR "Valid request received\n";
}
else
{
	print STDERR "Unrecognized request: $request\n\n";
	exit;
}

print STDOUT $content;

print STDERR "Sent policy file\n\n";

# End of file.
