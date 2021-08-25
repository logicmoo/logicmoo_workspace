#! /usr/bin/perl -w

use strict "vars";
use Getopt::Std;
use HTTP::Request::Common;
use LWP;
use CGI;

#------------------------------------------------------------------------------
my $SystemOnTPTPFormReplyURL = "http://www.cs.miami.edu/~tptp/cgi-bin/SystemOnTPTPFormReply";

my %URLParameters = (
#    "NoHTML" => 1,
    "QuietFlag" => "-q01",
    "SubmitButton" => "ProcessProblem",
    "ProblemSource" => "UPLOAD",
#    "SystemOnTPTP" => "-P",
#    "SystemOnTSTP" => "-S",
     "System___TPTP4X---0.0" => "TPTP4X---0.0",
#    "IDV" => "-T",
#    "AutoModeTimeLimit" => 100,
#    "AutoMode" => "-cE",
#    "AutoModeSystemsLimit" => 3,
#    "X2TPTP" => "",
    );
#------------------------------------------------------------------------------
    my $URL;
    my %Options;
    my @Content;
    my $Key;
    my $MyAgent;
    my $Request;
    my $Response;
my $TemporaryDirectory = "/tmp";

my $query         = new CGI;
my $input_article         = $query->param('article');
my $input_lc      = $query->param('lc');
my $input_tmp     = $query->param('tmp');
my $input_idv     = $query->param('idv');
my ($line, $col) = $input_lc=~m/(.*)_(.*)/;
my $col1 = $col - 4;

my $ext = "";
if($input_idv == 1) { 
    $ext = ".eout1"; 
    $URLParameters{"IDV"} = "-T";
    $URLParameters{"SystemOnTSTP"} = "-S";
}
elsif($input_idv == 2) { 
    $ext = ".eout1"; 
    $URLParameters{"SystemOnTSTP"} = "-S";
}
else {     $URLParameters{"SystemOnTPTP"} = "-P"; }

my $File0 = "$TemporaryDirectory/matp_" . $input_tmp . "/problems/" . $input_article . "/" . $input_article . "__" . $line . "_";
my $File1 = $File0 . $col . $ext;
my $File2 = $File0 . $col1 . $ext;
my $File;
if (-e $File1) { $File = $File1; } elsif(-e $File2) { $File = $File2}

my $File3 = $File . '2';

if((-e $File) && ($input_idv > 0))
{
    local $/;open(F1,$File);open(F2,">$File3"); $_=<F1>; 
    if(m/% *SZS *output *start *Proof.*((.|[\n])*?)% *SZS *output *end *Proof/) { print F2 $1;}
    close(F1); close(F2);
    $File = $File3;
}



# #----Get format and transform options if specified
#     getopts("hw:rq:t:c:l:s:Sp:a:",\%Options);

# #----Help
#     if (exists($Options{'h'})) {
#         print("Usage: RemoteSOT <options> [<File name>]\n");
#         print("    <options> are ...\n");
#         print("    -h            - print this help\n");
#         print("    -w[<status>]  - list available ATP systems\n");
#         print("    -r            - recommend ATP systems\n");
#         print("    -q<quietness> - control amount of output\n");
#         print("    -t<timelimit> - CPU time limit for system\n");
#         print("    -c<automode>  - one of N, E, S\n");
#         print("    -l<syslimit>  - maximal systems for automode\n");
#         print("    -s<system>    - specified system to use\n");
#         print("    -S            - TSTP format output\n");
#         print("    -p<filename>  - TPTP problem name\n");
#         print("    <File name>   - if not TPTP problem\n");
#         exit(0);
#     }

# #----What systems flag
#     if (exists($Options{'w'})) {
#         $URLParameters{"SubmitButton"} = "ListSystems";
#         if (!defined($Options{'w'}) || $Options{'w'} eq "") {
#             $URLParameters{"ListStatus"} = "READY";
#         } else {
#             $URLParameters{"ListStatus"} = $Options{'w'};
#         }
#         delete($URLParameters{"AutoMode"});
#         delete($URLParameters{"AutoModeTimeLimit"});
#         delete($URLParameters{"AutoModeSystemsLimit"});
#         delete($URLParameters{"ProblemSource"});
#     }
# #----Recommend systems flag
#     if (exists($Options{'r'})) {
#         $URLParameters{"SubmitButton"} = "RecommendSystems";
#         delete($URLParameters{"AutoMode"});
#         delete($URLParameters{"AutoModeTimeLimit"});
#         delete($URLParameters{"AutoModeSystemsLimit"});
#     }
# #----Quiet flag
#     if (exists($Options{'q'})) {
#         $URLParameters{"QuietFlag"} = "-q$Options{'q'}";
#     }
# #----Time limit
#     if (exists($Options{'t'})) {
#         $URLParameters{"AutoModeTimeLimit"} = $Options{'t'};
#     }
# #----Automode
#     if (exists($Options{'c'})) {
#         $URLParameters{"AutoMode"} = "-c$Options{'c'}";
#     }
# #----Systems limit
#     if (exists($Options{'l'})) {
#         $URLParameters{"AutoModeSystemsLimit"} = $Options{'l'};
#     }
# #----Selected system. Do after time limit as it gets moved across
#     if (exists($Options{'s'})) {
#         $URLParameters{"SubmitButton"} = "RunSelectedSystems";
#         $URLParameters{"System___$Options{'s'}"} = $Options{'s'};
#         $URLParameters{"TimeLimit___$Options{'s'}"} = 
# $URLParameters{"AutoModeTimeLimit"};
#         delete($URLParameters{"AutoMode"});
#         delete($URLParameters{"AutoModeTimeLimit"});
#         delete($URLParameters{"AutoModeSystemsLimit"});
#     }
# #----TSTP format output request
#     if (exists($Options{'S'})) {
#         $URLParameters{"X2TPTP"} = "-S";
#     }
# #----TPTP file name
#     if (exists($Options{'p'})) {
#         $URLParameters{"ProblemSource"} = "TPTP";
#         $URLParameters{"TPTPProblem"} = $Options{'p'};
#     }
# #----Password
#     if (exists($Options{'a'})) {
#         $URLParameters{"CPUPassword"} = $Options{'a'};
#     }

#----Get single file name
    if (exists($URLParameters{"ProblemSource"}) &&
$URLParameters{"ProblemSource"} eq "UPLOAD") {
#        if (scalar(@ARGV) >= 1 && $ARGV[0] ne "--") {
            $URLParameters{"UPLOADProblem"} = [$File];
        } 
# else {
#----Read into temporary file from stdin
#die("Need to complete reading from stdin");
#        }
#    }

    $MyAgent = LWP::UserAgent->new;
    $Request = POST($SystemOnTPTPFormReplyURL,
Content_Type => 'form-data',Content => \%URLParameters);
#DEBUG printf("%s\n",$Request->as_string());
    $Response = $MyAgent->request($Request);
print $query->header;
    printf("%s\n",$Response->content);

#------------------------------------------------------------------------------
