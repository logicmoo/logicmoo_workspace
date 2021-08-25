#! /usr/bin/perl -w

use strict "vars";
use Getopt::Std;
use HTTP::Request::Common;
use LWP;
use CGI;

#------------------------------------------------------------------------------
my $SystemOnTPTPFormReplyURL = "http://www.cs.miami.edu/~tptp/cgi-bin/SystemOnTPTPFormReply";
my $MMLQMeaningUrl ="http://mmlquery.mizar.org/cgi-bin/mmlquery/meaning?";

    my $URL;
    my %Options;
    my @Content;
    my $Key;
    my $MyAgent;
    my $Request;
    my $Response;
my $TemporaryDirectory = "/tmp";

my $xsltproc =     "/home/mptp/public_html/cgi-bin/bin/xsltproc";
my $Xsl4MizarDir = "/home/mptp/public_html/xsl4mizar";
my $addabsrefs = "$Xsl4MizarDir/addabsrefs.xsl";
my $xtstp2dli = "$Xsl4MizarDir/tstp2dli.xsl";
my $tptp4X =  "/home/mptp/public_html/cgi-bin/bin/tptp4X";



my $query         = new CGI;
my $input_article         = $query->param('article');
my $input_lc      = $query->param('lc');
my $input_tmp     = $query->param('tmp');
my $input_idv     = $query->param('idv');
my ($line, $col) = $input_lc=~m/(.*)_(.*)/;
my $col1 = $col - 4;

my $ext = ".eout"; 

my $File0 = "$TemporaryDirectory/matp_" . $input_tmp . "/problems/" . $input_article . "/" . $input_article . "__" . $line . "_";
my $File1 = $File0 . $col . $ext;
my $File2 = $File0 . $col1 . $ext;
my $File;
if (-e $File1) { $File = $File1; } elsif(-e $File2) { $File = $File2}

my $XmlFile  = $File . ".xml";
my $DliFile  = $File . ".dli";
`$tptp4X -f xml $File > $XmlFile`;
`$xsltproc $xtstp2dli  $XmlFile > $DliFile`;
# $(DLI2HTML)  $*.dli | sed -e 's/meaning[?]/$MMLQMeaningUrl/g' > $*.html


my $ua = LWP::UserAgent->new;
 $ua->timeout(10);
 $ua->env_proxy;

 my $response = $ua->post('http://mmlquery.mizar.org/cgi-bin/mmlquery/dli',
                   Content_Type => 'form-data',
                   Content      => [items   => [$DliFile]]);

print $query->header;
if ($response->is_success) {
     print $response->content;  # or whatever
   }
 else {
     die $response->status_line;
   }




#     $MyAgent = LWP::UserAgent->new;
#     $Request = POST($SystemOnTPTPFormReplyURL,
# Content_Type => 'form-data',Content => \%URLParameters);
# #DEBUG printf("%s\n",$Request->as_string());
#     $Response = $MyAgent->request($Request);
# print $query->header;
#     printf("%s\n",$Response->content);

#------------------------------------------------------------------------------
