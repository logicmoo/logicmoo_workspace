#!/usr/bin/perl -w

use strict;
use CGI;
use IO::Socket;
use IPC::Open2;
use HTTP::Request::Common;
use LWP::Simple;

my $TemporaryDirectory = "/tmp";

my $query	  = new CGI;
my $input_file	  = $query->param('file');
my $input_tmp     = $query->param('tmp');
my $input_raw     = $query->param('raw');
my $input_html     = $query->param('html');
my $input_refresh  = $query->param('refresh');
my $input_pos  = $query->param('pos');
my $content_type = $query->param('content-type');
my $File0 = "$TemporaryDirectory/matp_" . $input_tmp . "/" . $input_file;

my $print_refresh = 0;

if (defined($input_refresh))
{
    if (!(-e $File0)) { $print_refresh = 1 }
    else
    {
	my $now = time;
	my $mtime = (stat($File0))[9];
	if ($now < $mtime + 6) { $print_refresh = 1 }
    }
}


# if (defined($input_html))
# {
#     use MPTPNames;

#     MPTPNames::HTMLizeLines($MizHtml,$input_article);

#     exit 0;

# }

if ($print_refresh == 1) { print $query->header(-Refresh=>'2'); }
else { print $query->header(); }

print $query->start_html("File Output") unless defined($input_raw);

if (-e $File0) 
{
    open(F,$File0);

# print $File1;
    print "<pre>" unless defined($input_raw);
#    open(F,$File1);
if (defined($input_pos))
{
    my $pos1 = $input_pos - 3;
    my $lnr = 0;
    while($_=<F>) {if($lnr++ > $pos1) {print $_;}}
}


    else { local $/; $_= <F>; print $_; }
    print "<pre/>" unless defined($input_raw);
    close(F);
}

print $query->end_html unless defined($input_raw);
