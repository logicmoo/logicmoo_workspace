#!/usr/bin/perl -w 

# cgi preparing mizar article for atp methods
# crude initial hack

use strict;
use CGI;
use IO::Socket;
use File::Temp qw/ :mktemp  /;
use IPC::Open2;
use HTTP::Request::Common;
use LWP::Simple;
use POSIX qw( WNOHANG );
use MPTPNames;

my $CgiDir = '/home/mptp/public_html/cgi-bin';
do "$CgiDir/MizARconfig.pl";
my $MyUrl = MyUrl();
my $gloadlimit = 0; # GLoadLimit(); temporary hack!!!!

# possible SZS statuses
sub szs_INIT        ()  { 'Initial' } # system was not run on the problem yet
sub szs_UNKNOWN     ()  { 'Unknown' } # used when system dies
sub szs_THEOREM     ()  { 'Theorem' }
sub szs_UNSAT       ()  { 'Unsatisfiable' }
sub szs_COUNTERSAT  ()  { 'CounterSatisfiable' }
sub szs_RESOUT      ()  { 'ResourceOut' }
sub szs_GAVEUP      ()  { 'GaveUp' }   # system exited before the time limit for unknown reason

my $query	  = new CGI;
my $ProblemSource = $query->param('ProblemSource');
my $VocFile       = $query->param('VocFile');
my $VocURL       = $query->param('VocURL');
my $VocSource       = $query->param('VocSource');
my $VocName       = $query->param('VocName');
my $VocContent       = $query->param('VocContent');
my $input_article	  = $query->param('Formula');
my $input_name	  = $query->param('Name');
my $aname         = lc($input_name); 
# not sure what this is for - now used for admin stuff
my $atp_mode	  = $query->param('ATPMode');
# if defined, snow is started when 1 and not if 0
# if undefined, snow is started according to $generateatp
my $input_snow	  = $query->param('Snow');
# atp calls for hard problems, implies html and mptp
my $linkarproofs  = $query->param('ARProofs');
# if not defined, the htmlized article is not produced, 
# further behavior depends on query_mode:
# if HTML, article with errors is shown in browser, and atp problems/solutions may still be generated
# if TEXT, either only errors are sent back, or also atp solutions/results, or possibly hints, 
# or possibly problem-solving semi-persistent line might be set up
my $generatehtml   = $query->param('HTMLize');
# generate ATP re-proving problems for all article positions; 
# possibly link by-s when $generatehtml, and theorems when $linkarproofs 
my $generateatp   = $query->param('GenATP');
# immediatelly try to prove all Mizar-unsolved (*4) positions, using all of MML (and either SInE or SNoW for selection)
# note that the generated problems differ from the re-proving problems
my $proveunsolved   = $query->param('ProveUnsolved');
my $provepositions  = $query->param('Positions');

# if defined || 1, proofs are hidden by default and shown by ajax calls (undef by default)
my $proofsbyajax  = $query->param('AjaxProofs');

# the mml version used for processing and linking; there should be a suitable default
my $mmlversion    = $query->param('MMLVersion');

# either 'HTML' or 'TEXT'; says if output is in txt or in html
# 'TEXT' switches of $generatehtml
my $query_mode       = $query->param('MODE');

# parallelization options
# If greater than 1, runs problems in parallel, using mizp.pl
# Default is 1 - no parallelization.
my $gparallelize = $query->param('Parallelize');

# Sets the parallelization policy if parallelize > 0.
# 1 is parallelization by using @proof for top proofs in original file 
# This parallelizes analyzer, checker, and htmlizer.
# 2 is parallelization by using ErrorInf in .xml file
# This parallelizes only checker.
# 3 combines 1 and 2 when parallel top proof chunks have not even distribution.
# For example, when there is only one top proof, analyzer and htmlizer are not
# parallelized, and all parallelization is done in checker.
# Default is 1.

my $gppolicy = $query->param('PPolicy');


# the htmlization stylesheet to apply
my $gtransf = $query->param('Transf');

# its parameters
my $gtransfparams = $query->param('TParams');



## (full) htmlization for parallelizer. Only used if $gparallelize>1,
## and other conditions met below.
my $gphtmlize = 2; 

$linkarproofs = 0 unless defined($linkarproofs);
$generateatp = 0 unless defined($generateatp);
$generatehtml = 0 unless defined($generatehtml);
$proveunsolved = "None" unless defined($proveunsolved);
$gparallelize = 1 unless defined($gparallelize);
$gppolicy = 1 unless defined($gppolicy);
$query_mode = 'HTML' unless defined($query_mode);

$mmlversion   = '4.100.1011' unless defined($mmlversion);

$gtransf = 'miz.xsl' if(!defined($gtransf) || $gtransf eq '');

$proofsbyajax = 0; # unless defined($proofsbyajax); comented - not wroking yet, trying to write the relative proof path

# values: 1 for master mode If 1, master_mode (no limits) is used, if
# 2, problems are only created, not solved, if 4, all "by" problems
# are created. Combinations of these can be done by summing their
# codes (i.e., value 7 would tell to use master_mode, create all "by",
# and do not solve).

$atp_mode = 0 unless defined($atp_mode);

my $gmaster_mode = $atp_mode & 1;
my $gonly_create_problems = $atp_mode & 2;
my $gemulate_all_by = $atp_mode & 4;
my $gwatch_cpu_load = $atp_mode & 8;  # start when cpu load falls below $gloadlimit

# timelimit for swi prolog, only unlimited in master mode, otherwise 120s.
my $swiulimit = ($gmaster_mode==1)? 100000 : 120;

my $starttime = time(); # for measuring the query processing time

$ENV{"PATH"} = "/home/mptp/bin:" . $ENV{"PATH"};

if($gwatch_cpu_load == 1)
{
    my $load = `cut -f1 -d \  /proc/loadavg`;
    while($load > $gloadlimit) { sleep 10; $load = `cut -f1 -d \  /proc/loadavg`; }
}


sub my_warning
{
    print $_;
}

my @provepositions = ();

# the problem_list string passed to prolog
my $ATPProblemList = "";

if (($proveunsolved eq "Positions") && (defined($provepositions)))
{
    my @provepositions1 = split(/\, */, $provepositions);
    foreach my $pos (@provepositions1)
    {
	if ($pos =~ m/(\d+):(\d+)/)
	{
	    push(@provepositions, "pos(" . $1 . ',' . $2 . ")" );
	}
	else { my_warning("Position $pos does not have required format line:column, ignored"); }
    }
    $ATPProblemList = ',problem_list([' . join(',', @provepositions) . '])';
}

# my $MyUrl = 'http://octopi.mizar.org/~mptp';
my $PalmTreeUrl = $MyUrl . "/PalmTree.jpg";
my $Xsl4MizarDir = "/home/mptp/public_html/xsl4mizar";
my $Mizfiles = "/home/mptp/public_html/mml$mmlversion";
my $Bindir = "bin$mmlversion";
my $utilspl =  "/home/mptp/public_html/cgi-bin/$Bindir/utils.pl";
my $TemporaryDirectory = "/tmp";
my $TemporaryProblemDirectory = mkdtemp("$TemporaryDirectory/matp_XXXXXX");
$TemporaryProblemDirectory =~ m/^.*\/matp_(.*)/ or die "Bad tmpdir: $TemporaryProblemDirectory";
## This is no longer pidnr - race conditions $$
my $PidNr = $1;
my $MizHtml = $MyUrl . "/mml$mmlversion/html/";
my $mizf =     "$Bindir/mizf";
my $mizp =     "/home/mptp/public_html/cgi-bin/$Bindir/mizp.pl";
my $snow =     "$Bindir/snow";
my $advisor =     "$Bindir/advisor.pl";
my $exporter =     "$Bindir/mizar/exporter";
my $xsltproc =     "xsltproc";
my $dbenv = "$Bindir/dbenv.pl";
my $dbenv2 = "$Bindir/dbenv2.pl";
my $mk_derived_mptp_files = "$Bindir/mk_derived.pl";
my $err2pl = "$Bindir/err2pl.pl";
my $err2xml = "$Bindir/err2xml.pl";
my $mizitemize = "$Bindir/MizItemize.pl";
my $addabsrefs = "$Xsl4MizarDir/addabsrefs.xsl";
my $miz2html = "$Xsl4MizarDir/$gtransf";
my $mizpl = "$Xsl4MizarDir/mizpl.xsl";
my $evl2pl = "$Xsl4MizarDir/evl2pl.xsl";
my $mkxmlhead = "$Xsl4MizarDir/mkxmlhead.pl";
my $mizcomments = "$Xsl4MizarDir/MizComments.pl";
my $doatproof = 0;
my $atproof = '@' . 'proof';
my $idv_img = "<img SRC=\"$PalmTreeUrl\" alt=\"Show IDV proof tree\" title=\"Show IDV proof tree\">";



my $aname_uc      = uc($aname);
my $ProblemFileOrig = $TemporaryProblemDirectory . "/$aname";
my $AjaxProofDir = $TemporaryProblemDirectory . "/proofs"; #/" . $aname;
my $ProblemDir = $TemporaryProblemDirectory . "/problems/" . $aname;
my $ProblemFile = $ProblemFileOrig . ".miz";
my $ProblemFileXml = $ProblemFileOrig . ".xml";
my $ProblemFileXml2 = $ProblemFileOrig . ".xml2";
my $ProblemFileFlaNames = $ProblemFileOrig . ".propnames";
my $ProblemFileHtml = $ProblemFileOrig . ".html";
my $ProblemFileErr = $ProblemFileOrig . ".err";
my $ProblemFileErr1 = $ProblemFileOrig . ".err1";
my $ProblemFileErr2 = $ProblemFileOrig . ".err2";
my $ProblemFileErrX = $ProblemFileOrig . ".errx";
my $ProblemCgiParams = $ProblemFileOrig . ".cgiparams";

my $ProblemFileComments = $ProblemFileOrig . ".cmt";

my $MizOutput = $ProblemFileOrig . ".mizoutput";
my $MizOutputEmacs = $ProblemFileOrig . ".mizoutputemacs";
my $ProblemFileBex = $ProblemFileOrig . ".bex";
my $lbytmpdir = $PidNr;
my $lbycgiparams = '\&ATP=refs\&HTML=1\&MMLVersion=' . $mmlversion;
my $ltptproot= $MyUrl . '/';
my $lbytptpcgi= $MyUrl . "/cgi-bin/$Bindir/showby.cgi";

my $SnowDataDir =     $Mizfiles . "/mptp/snowdata";
my $SnowFileStem =    $SnowDataDir . "/thms3";
my $SnowMMLNet =      $SnowFileStem . ".net";
my $SnowMMLArch =     $SnowFileStem . ".arch";
my $AdvisorOutput  =  $ProblemFileOrig . ".adv_output";
my $SnowOutput  =     $ProblemFileOrig . ".snow_output";
my $SnowSymOffset =   500000;

my (%gsyms,$grefs,$ref);
my $ghost	  = "localhost";
my $snowport	  = -1;
my $start_snow    = (defined $input_snow)? $input_snow : $generateatp;
my $advisorport	  = -1;
my %gconstrs      =
    (
     'func'   , 'k',
     'pred'   , 'r',
     'attr'   , 'v',
     'mode'   , 'm',
     'aggr'   , 'g',
     'sel'    , 'u',
     'struct' , 'l'
    );


sub min { my ($x,$y) = @_; ($x <= $y)? $x : $y }

#----Check for proper article name
sub CheckArticleName
{
    my ($aname1) = @_;
    unless((length($aname1) < 9) && ($aname1=~m/^[a-z][a-z0-9_]*$/))
    {
	print "<pre>";
	print ("Error: article name must not exceed 8 characters, has to start with a-z, and only contain [a-z0-9_]: ", $aname1);
	print "<pre/>";
	print $query->end_html;
	die "article name";
    }
}

sub CreateTmpDir
{
    my ($tmpdir) = @_;

    if (!mkdir($tmpdir,0777)) {
        print("ERROR: Cannot make temp dir $tmpdir\n");
        die("\n");
    }

    system("chmod 0777 $tmpdir");
}
# Make temporary directories for files;
# creates the ( 'proofs', 'problems', 'dict') subdirs
# Change permissions so MizAR can cleanup later if needed
sub CreateTmpDirs
{
    my ($tmproot) = @_;

#    CreateTmpDir($tmproot);

    system("chmod 0777 $tmproot");

    foreach my $subdir ( 'proofs', 'problems', 'dict')
    {
	CreateTmpDir($tmproot . "/". $subdir)
    }

## nasty hack to get around the dict/text issues 
##    system("ln -s $TemporaryProblemDirectory $TemporaryProblemDirectory/text");

#DEBUG print("----$TemporaryProblemDirectory----$!---\n");

}

sub LogCGIParams
{
    open (CGIPARAMS,'>',$ProblemCgiParams) || die;
    $query->save(\*CGIPARAMS);
    close CGIPARAMS;
}

# Print input article into $ProblemFile;
# handle vocabulary, convert to unix, handle atproof.
# Uses lots of global vars now.
sub SetupArticleFiles
{


#    my $ProblemFileTxt = "${TemporaryProblemDirectory}/text/$aname";
    open(PFH, ">$ProblemFileOrig") or die "$ProblemFileOrig not writable";
#    my ($ProblemFileHandle,$ProblemFileOrig) = mkstemp("${TemporaryProblemDirectory}/$aname");
    if ($ProblemSource eq "UPLOAD")
    {
	my $UploadFileHandle = $query->param('UPLOADProblem');
	if (!defined($UploadFileHandle) || $UploadFileHandle eq "")
	{
	    print("ERROR: Empty uploaded problem file\n");
	    die("ERROR: Empty uploaded problem file\n");
	}
	my $UploadLine;
#DEBUG print("UPLOAD file: $UploadFileHandle \n");
	while (defined($UploadLine = <$UploadFileHandle>)) { print PFH $UploadLine; };
	close($UploadFileHandle);
    }
    elsif ($ProblemSource eq "URL") 
    {
	my $FormulaURL = $query->param('FormulaURL');
	if (!defined($FormulaURL)) 
	{
	    die("ERROR: No URL supplied\n");
	}
	getstore($FormulaURL,$ProblemFileOrig) or
	    die("ERROR: Could not fetch from $FormulaURL");
    }
    elsif (!($input_article eq ""))
    {
#----Convert \r (DOS EOLN) to \n (UNIX EOLN)
	if($doatproof > 0) 
	{
	    $input_article =~ s/\bproof\b/$atproof/g;
	}
#    $input_article =~ s/\r/\n/g;
#----Somehow WWW services are duplicating the \n ... kill them (aargh)
#            $Formulae =~ s/\n\n/\n/g;
	printf(PFH "%s",$input_article);
    }
    else
    {
	print("ERROR: No article provided\n"); 
	die("ERROR: No article provided\n"); 
    }

    close(PFH);

    if (defined($VocSource) && ($VocSource eq 'UPLOAD')
	&& defined($VocFile) && !($VocFile eq ""))
    {
	my $VOCFileOrig1 = "${TemporaryProblemDirectory}/dict/$VocFile";
	open(VOC, ">$VOCFileOrig1") or die "$VOCFileOrig1 not writable";
	my $UploadLine;
#DEBUG print("UPLOAD file: $VocFile \n");
	while (defined($UploadLine = <$VocFile>)) { print VOC $UploadLine; };
	close($VocFile);
	close(VOC)
    }
    elsif (defined($VocSource) && ($VocSource eq 'URL') 
	   && defined($VocURL) && !($VocURL eq ""))
    {
	my @VocURLparts = split(/\//, $VocURL);
	my $VocFile1 = $VocURLparts[$#VocURLparts];
	my $VOCFileOrig1 = "${TemporaryProblemDirectory}/dict/$VocFile1";
	getstore($VocURL,$VOCFileOrig1) or
	    die("ERROR: Could not fetch from $VocURL");
    }


    # we now allow vocnames concatenated by "::" and contents by "::::::"
    elsif (defined($VocSource) && ($VocSource eq 'CONTENT')
	   && defined($VocName) &&  (length($VocName) < 1000) && (lc($VocName)=~m/^[a-z][a-z0-9_.:]*$/)
	   && !($VocName eq "") && defined($VocContent))
    {
	my @VocNames = split(/::/, $VocName);
	my @VocContents = split(/::::::/, $VocContent);
	foreach my $vi (0 .. $#VocNames)
	{
	    my $vn = lc($VocNames[$vi]);
	    my $vc = $VocContents[$vi];
	    my $VOCFileOrig1 = "${TemporaryProblemDirectory}/dict/$vn";
	    open(VOC, ">$VOCFileOrig1") or die "$VOCFileOrig1 not writable";
	    printf(VOC "%s",$vc);
	    close(VOC);
	    system("dos2unix $VOCFileOrig1");
	}
    }



    system("dos2unix $ProblemFileOrig");
    `mv $ProblemFileOrig $ProblemFile`;
    system("chmod 0666 $ProblemFile");
}

#  sort the .bex file
sub SortByExplanations
{
    my ($Bex) = @_;

    if(open(BEX,$Bex))
    {
	local $/;
	my $bex=<BEX>;
	close(BEX);
	if($bex=~m/((.|[\n])*?)<PolyEval/)
	{
	    open(BEX,">$Bex");
	    print BEX $1;
	    my %h=();
	    while($bex=~m/(<PolyEval((.|[\n])*?)<\/PolyEval>)/g)
	    {
		if(!(exists $h{$1})) { print BEX $1; $h{$1} = (); }
	    }
	    print BEX "</ByExplanations>\n";
	    close(BEX);
	}
    }
}

sub StartSNoW
{
#--- get unused port for SNoW
    socket(SOCK,PF_INET,SOCK_STREAM,(getprotobyname('tcp'))[2]);
    bind( SOCK,  sockaddr_in(0, INADDR_ANY));
    my $sport = (sockaddr_in(getsockname(SOCK)))[0];
#    print("snowport $sport\n");
    close(SOCK);

#--- start snow instance:
# ###TODO: wrap this in a script remembering a start time, and self-destructing
#          in one day
    system("nohup $snow -server $sport -o allboth -F $SnowMMLNet -A $SnowMMLArch > $SnowOutput 2>&1 &");

# nohup /home/mml/public_html/cgi-bin/bin/snow -server 60003 -o allboth -F /home/urban/public_html/holdata/holdata.net -A /home/urban/public_html/holdata/holdata.arch > /dev/null 2>&1 &

# cd /home/urban/public_html/holdata && nohup /home/urban/gr/MPTP/SCRIPT/advisor/holadvisor.pl -p60003 -w45001 holdata > /dev/null 2>&1 &

#--- get unused port for advisor
    socket(SOCK1,PF_INET,SOCK_STREAM,(getprotobyname('tcp'))[2]);
    bind( SOCK1,  sockaddr_in(0, INADDR_ANY));
    my $aport = (sockaddr_in(getsockname(SOCK1)))[0];
#    print("advisorport $aport\n");
    close(SOCK1);

    system("nohup $advisor -p $sport -a $aport -o $SnowSymOffset $SnowFileStem > $AdvisorOutput 2>&1 &");

    $lbycgiparams = $lbycgiparams . '\&ap=' . $aport;
    return ($aport, $sport);
}


print $query->header;
print $query->start_html($aname_uc) unless($query_mode eq 'TEXT');



CheckArticleName($aname);

CreateTmpDirs($TemporaryProblemDirectory);
CreateTmpDir("$AjaxProofDir/$aname");
CreateTmpDir($ProblemDir);

LogCGIParams();
SetupArticleFiles();

# temporary hack to prevent snow starting afresh
$start_snow=0;

    # this has to precede creation of html, so that $aport
    # was set in the by-calls
if($start_snow > 0) { ($advisorport, $snowport) = StartSNoW(); }
else {$advisorport = 50001;}


$ENV{"MIZFILES"}= $Mizfiles;
if($gparallelize == 1)
{
    system("$mizf $ProblemFile 2>&1 > $MizOutput");
}
## TODO: this should be a library call rather than execution
else
{
    (-x $mizp) or die "Parallelizer not executable: $mizp";
    if(($generateatp > 0) || ($proveunsolved eq 'All') || 
       (($proveunsolved eq 'Positions') && (scalar(@provepositions) > 0)))
    {
	$gppolicy = 2; ## cannot use analyzer parallelization yet fro MPTP
	$gphtmlize = 0; ## no sense to htmlize if not analyzer parallelization
    }
    elsif(($query_mode eq 'TEXT') || ($generatehtml == 0))
    {
	$gphtmlize = 0;
    }

    system("$mizp -q -l -j $gparallelize -P $gppolicy -M $gphtmlize -H $MizHtml -x $Xsl4MizarDir $ProblemFile 2>&1 > $MizOutput");
}
system("grep -A100 Verifier $MizOutput 2>&1 > $MizOutputEmacs");

system("cp $ProblemFileErr $ProblemFileErr1");
system("$err2pl $ProblemFileErr > $ProblemFileErr2");
system("$err2xml $ProblemFileErr > $ProblemFileErrX");
system("$mizitemize $ProblemFileOrig");
system("$mizcomments $ProblemFile > $ProblemFileComments");

my $InferenceNr = `egrep -c '<(Proof|By|From|Now)' $ProblemFileXml`;
my $errorsnr = `wc -l <$ProblemFileErr`;

if($proveunsolved eq 'All')
{
    my %done = ();
    open(ERR,$ProblemFileErr);
    while (<ERR>)
    {
	if(m/^(\d+) +(\d+) +[14] *$/) 
	{ 
	    my $pos = "pos(" . $1 . ',' . $2 . ")";
	    if( ! exists( $done{$pos}) )
	    {
		push(@provepositions, $pos ); 
		$done{$pos}++;
	    }
	}
    }
    close(ERR);
    $ATPProblemList = ',problem_list([' . join(',', @provepositions) . '])';
}

## ignore the unproved positions if we want all
if ($gemulate_all_by==4)
{
    $ATPProblemList = "";
    @provepositions = ();
}

my $problemstosolvenr = scalar @provepositions;

my $absolutize = (($generatehtml > 0) || ($generateatp > 0) || ($problemstosolvenr >0) || ($gemulate_all_by==4))? 1 : 0;



# if query_mode is HTML, produce either html or text output for the browser
unless($query_mode eq 'TEXT')
{
    print "<a href=\"$MyUrl/cgi-bin/showtmpfile.cgi?file=$aname.mizoutput&tmp=$PidNr\" target=\"MizarOutput$PidNr\">Show Mizar Output</a>\n";  
#    print $AjaxProofDir;

    print "<a href=\"$MyUrl/cgi-bin/showtmpfile.cgi?file=$aname.err1&tmp=$PidNr\" target=\"MizarOutput$PidNr\">($errorsnr Errors)</a>\n";

    print "<a href=\"$MyUrl/cgi-bin/showtmpfile.cgi?file=$aname.ploutput&tmp=$PidNr&refresh=1\" target=\"MPTPOutput$PidNr\">Generating $InferenceNr TPTP re-proving problems (click to see progress)</a><br>\n" if($generateatp > 0);

    print "<a href=\"$MyUrl/cgi-bin/showtmpfile.cgi?file=$aname.ploutput&tmp=$PidNr&refresh=1\" target=\"MPTPOutput$PidNr\">Preparing $problemstosolvenr Mizar-unsolved problems for ATPs (click to see progress)</a><br>\n" if($problemstosolvenr > 0);

    print "<a href=\"$MyUrl/cgi-bin/showtmpfile.cgi?file=$aname.atpoutput&tmp=$PidNr&refresh=1\" target=\"MPTPOutput$PidNr\">ATP-solving $problemstosolvenr (10 at most) Mizar-unsolved problems (click to see progress)</a><br>\n" if($problemstosolvenr > 0);
    
#    print "<a href=\"$MyUrl/cgi-bin/showtmpfile.cgi?file=$aname.xml.abs&tmp=$PidNr&content-type=text%2Fplain\" target=\"XMLOutput$PidNr\">Show XML Output</a>\n";


    my $lnr = 0;
    open(PFH, "$ProblemFile");

    print ('<pre>', "\n");
    while( my $aline = <PFH>)
    {
	if($generatehtml==1)
	{
	    # print ('<div id="', ++$lnr, '" style="display:none">', $aline, '</div>');
	}
	else { print $aline; }
    }
    close(PFH);
    print ('</pre>', "\n");
}

SortByExplanations($ProblemFileBex);


#    system("time $xsltproc --param explainbyfrom 1 $addabsrefs $ProblemFileXml > $ProblemFileXml.abs 2>$ProblemFileXml.errabs");

# ###TODO: note that const_links=2 does not work correctly yet    

system("time $xsltproc --param explainbyfrom 1 $addabsrefs $ProblemFileXml 2>$ProblemFileXml.errabs > $ProblemFileXml.abs") if(($absolutize==1) && (($gparallelize==1) || ($gppolicy==2)));

my $outseparator= "\n==========\n";

if($query_mode eq 'TEXT')
{
    print $outseparator;
    open(F,$MizOutputEmacs);
    local $/; $_= <F>; print $_;
    close(F);
    print $outseparator;

    open(F,$ProblemFileErr);
    local $/; $_= <F>; print $_;
    close(F);
    print $outseparator;
}
elsif($generatehtml==1)
{
    if(($gparallelize>1) && ($gphtmlize>0))
    {
	open(F,$ProblemFileHtml) or print "HTMLizer failed, please complain\n";
	local $/; $_= <F>; print $_;
	close(F);
    }
    else
    {
	my $genatpparams = ($generateatp==1)? " --param by_titles 1 --param linkarproofs $linkarproofs --param ajax_by 1 --param linkbytoself 1 --param linkby 3 --param thms_tptp_links 1 --param ltptproot \\\'$ltptproot\\\' --param lbytptpcgi \\\'$lbytptpcgi\\\' --param lbytmpdir \\\'$lbytmpdir\\\' --param lbycgiparams \\\'$lbycgiparams\\\' " : "";

	my $ajaxproofparams = ($proofsbyajax==1)? " -param lbytmpdir \\\'$lbytmpdir\\\'  --param ajax_proofs 3 " : " ";

	system("$mkxmlhead -s $ProblemFile > $ProblemFileOrig.hdr");

	system("time $xsltproc  $genatpparams $ajaxproofparams --param mk_comments 1 --param mk_header 1 --param const_links 1  --param default_target \\\'_self\\\'  --param linking \\\'l\\\' --param mizhtml \\\'$MizHtml\\\' --param selfext \\\'html\\\'  --param titles 1 --param colored 1 --param proof_links 1 $miz2html $ProblemFileXml.abs |tee $ProblemFileHtml 2>$ProblemFileXml.errhtml"); 
    }
}

## now just 'system'
sub ExecSlow { my $command = shift; system($command); }

## the forking to print some bogus while we wait in master mode
## this does not work as expected
sub ExecSlowOld
{
    my $command = shift;

    if($gmaster_mode == 1)
    {

	my $pid = fork();
	if ($pid) # parent
	{ }
	elsif ($pid == 0) 
	{
	    # child
	    system($command);
	    exit(0);
	} 
	else { die "couldn’t fork: $!\n"; }
	
	my $cnt=0;
	while (waitpid($pid, WNOHANG) == 0) 
	{
	    sleep 1;
	    if(++$cnt == 300) { print '.'; $cnt = 0; }
	}
    }
    else
    {	
	system($command);
    }
}

if(($generateatp > 0) || ($problemstosolvenr > 0) || ($gemulate_all_by == 4)) 
{
    ### NOTE: this can be more targeted and parallelized as the html parallelization

    ExecSlow("cd $TemporaryProblemDirectory; time $xsltproc --param dump_prop_labels 1 $mizpl $ProblemFileXml.abs  > $ProblemFileXml2 2>$ProblemFileXml.errpl");

# ajax proofs are probably not wanted for the first stab
#    system("time $xsltproc --param default_target \\\'_self\\\' --param ajax_proof_dir \\\'$AjaxProofDir\\\' --param linking \\\'l\\\' --param mizhtml \\\'$MizHtml\\\' --param selfext \\\'html\\\' --param ajax_proofs 1 --param titles 1 --param colored 1 --param proof_links 1 $miz2html $ProblemFileXml.abs > $ProblemFileHtml 2>$ProblemFileXml.errhtml"); 

    system("$mk_derived_mptp_files $ProblemFileOrig 2> $ProblemFileOrig.derived_err");

    if($mmlversion eq '4.100.1011')
    {
	system("$dbenv $ProblemFileOrig > $ProblemFileOrig.evl2");
    }
    else
    {
	system("$xsltproc $evl2pl $ProblemFileOrig.evl   > $ProblemFileOrig.evl1");    
	system("$dbenv2 $ProblemFileOrig > $ProblemFileOrig.evl2");
    }
    
    my @env_keywords = ("constructors","theorems","vocabularies","notations",
			"registrations","schemes","requirements");
    my %henv=();  # create hash of environment articles
    if(open(EVL,"$ProblemFileOrig.evl2"))
    {
	my $evl=<EVL>; close(EVL);
	
	## Remove the syntactic-only directives (all before notations), in particular the
	## vocabulary directive for the current article which might
	## have the same name.
	$evl =~ s/^.*notations.*?\]\),//;
	my @env= $evl =~ m/[a-z0-9_]+/g; 
	@henv{@env}=(); 
	foreach my $k (@env_keywords) { delete $henv{$k}; }
    }

    my $Tmp1 = $TemporaryProblemDirectory . '/';
# swipl -G50M -s utils.pl -g "mptp2tptp('$1',[opt_NO_FRAENKEL_CONST_GEN],user),halt." |& grep "^fof"
    
    # no need to run twice, here, just do not filter; the Prolog code
    # ensures creation of all problems then
    if($generateatp > 0) { $ATPProblemList = ""; }

    my $CreateProblemsCommand = "cd $TemporaryProblemDirectory; ulimit -t $swiulimit; swipl -G50M -s $utilspl -g \"(A=$aname,D=\'$Tmp1\',declare_mptp_predicates,time(load_mml_for_article(A, D, [A])),time(install_index),time(mk_article_problems(A,[[mizar_by,mizar_from,mizar_proof],[theorem, top_level_lemma, sublemma] $ATPProblemList],[opt_LOAD_MIZ_ERRORS,opt_ARTICLE_AS_TPTP_AXS,opt_REM_SCH_CONSTS,opt_TPTP_SHORT,opt_ADDED_NON_MML([A]),opt_NON_MML_DIR(D),opt_LINE_COL_NMS,opt_PRINT_PROB_PROGRESS,opt_ALLOWED_REF_INFO,opt_PROVED_BY_INFO])),halt).\" > $aname.ploutput 2>&1";
    open(F,">$ProblemFileOrig.plparams");
    print F $CreateProblemsCommand;
    close(F);
    ExecSlow($CreateProblemsCommand);

    if(($problemstosolvenr > 0) || ($gemulate_all_by == 4))
    {

	my $atpout = ($query_mode eq 'HTML')? "$ProblemFileOrig.atpoutput" : '-';
	open(my $fhout,">$atpout");
	open(my $fatplog,">$ProblemFileOrig.atplog");
	print $fatplog ($outseparator, "Starting atp ", time() - $starttime, "s\n");
	my $runwtlimit = "$Bindir/runwtlimit";
	my $parwtlimit = "$Bindir/parwtlimit";
	my $parwtlimit1 = "$Bindir/parwtlimit1";
	my $vampire =     "$Bindir/vampire_rel2";
	my $fofshared2 =  "$Bindir/fofshared.trms2";
	my $cpulimit = 15; 
	my $cpulim1 = $cpulimit - 5; 
	my $vampire_params = " -proof tptp -ss included -sd 1 -output_axiom_names on --mode casc -t $cpulimit -m 1234  -input_file ";
	
	my $vampire_params2 = " -proof tptp -output_axiom_names on --mode casc -t $cpulimit -m 1234  -input_file ";
	my $vampire_params3 = " -proof tptp -output_axiom_names on --mode casc -t $cpulim1 -m 1234  -input_file ";

	my $LocalAxs = $ProblemDir . "/" . $aname . ".ax" ;
	my $MMLAxs = $Mizfiles . "/mptp/00allmmlax" ;

	my $MMLSym0 = $Mizfiles . "/mptp/00sym0eq" ;
	my $MMLSymd = $Mizfiles . "/mptp/00symdeq" ;

	my $MMLAtpdeps15 = $Mizfiles . "/mptp/00atp15" ;
	my $MMLMizdeps15 = $Mizfiles . "/mptp/00miz15" ;
	my $MMLSeq = $Mizfiles . "/mptp/00seq" ;

	my $EnvIncludeFile = $ProblemDir . "/" . $aname . ".envinclude";

	open(ENVIN,">$EnvIncludeFile");
	foreach my $envin (keys %henv) 
	{ 
	    print ENVIN "include('$Mizfiles/mptp/Axioms/$envin.ax').\n"
		if(-e "$Mizfiles/mptp/Axioms/$envin.ax");
	}
	close(ENVIN);

	my %fla2pos = ();
	my %fla2name = ();
	my %grefdeps = ();
	open(XML2, $ProblemFileXml2);
	while (<XML2>)
	{
	    if(m/^fof\(((.)[^,]+),.*position\((\d+),(\d+)\)/) 
	    {
		$fla2pos{$1}= "$3:$4";

		# so far we learn only from the toplevel thms
		if(($2 eq 'l') || ($2 eq 't'))
		{
		    my $nm = $1;
		    if(m/inference\(mizar_(proof|by|from),.*?\],\[([^\]]*)\]/)
		    {
			$grefdeps{$nm} = $2;
		    }
		}
	    }
	    if(($gemulate_all_by == 4) && m/mizar_by..position\((\d+),(\d+)\)/)
	    {
		push(@provepositions, "pos(" . $1 . ',' . $2 . ")");
		$problemstosolvenr = scalar @provepositions;
	    }
	}
	close(XML2);

	if(open(PROPNAMES, $ProblemFileFlaNames))
	{
	    while (<PROPNAMES>)
	    {
		if(m/^propname\(([^,]+),[']([^']+)[']\)/)
		{
		    $fla2name{$1}= $2;
		}
	    }
	    close(PROPNAMES);
	}

	if(($problemstosolvenr > 10) && ($gmaster_mode != 1))
	{
	    @provepositions = @provepositions[0..9]
	}
	print $fatplog ($outseparator, "Starting atp positions ", time() - $starttime, "s\n");
      POS: foreach my $pos (@provepositions)
	{

	    my @refs=();
	    my @htmlrefs = ();
	    my @mizrefs = ();
	    my @simprefs = ();
	    my $status = szs_UNKNOWN;
	    
	    ###TODO: this is mostly stolen from showby.cgi, refactor!
	    my ($line, $col) = $pos =~ m/.*pos\((\d+),(\d+)\).*/;
	    my $col1 = $col - 4;

	    my $File0 = $ProblemDir . "/" . $aname . "__" . $line . "_";
	    my $File1 = $File0 . $col;
	    my $File2 = $File0 . $col1;
	    my $File= "";

	    if (-e $File1) { $File = $File1; } elsif(-e $File2) { $File = $File2}
	    if(    ( -e $File) && open(F1, $File . '.allowed_local'))
	    {
		my $allowed_line = <F1>;
		close(F1);

		$allowed_line =~ m/.*\[(.*)\].*/ or die "Bad allowed_local";
		my @allowed = split(/\, */, $1);

		my $regexp = '"^fof( *\(' . join('\|',@allowed) . '\) *,"';
		`grep $regexp $LocalAxs >> $File.big0`;
		`cat $File >> $File.big0`;
		`cat $EnvIncludeFile > $File.big1`;
		`cat $File.big0 >> $File.big1`;
		`echo "include('$MMLAxs')." > $File.big`;
		`cat $File.big0 >> $File.big`;

		print $fatplog ($outseparator, "Big files written for $File ", time() - $starttime, "s\n");
		if( -e $fofshared2)
		{
		    my @newaxs = ();
		    my $newconj = "";
		    foreach my $role ("axiom", "conjecture")
		    {
			open(SYM0, ">$File.sym0.$role") or die;
			open(SYMD, ">$File.symd.$role") or die;
			my $regexp1 = '"_' . $aname . ', *' . $role . ' *,"';
			my $fofsh_pid = open(FSH,"grep  $regexp1 $File.big0 | sort -u | $fofshared2 |");
#open(FSH,"grep $regexp $LocalAxs | $fofshared2 ");
			while($_=<FSH>) 
			{
			    m/^terms\( *([a-z0-9A-Z_]+) *, *\[(.*)\] *\)\./ or die; 
			    my ($ref, $trms) = ($1, $2); 
			    my $trms1 = $trms; my $trms2=$trms;
			    $trms=~s/X\d+/X1/g; 
			    $trms2=~s/\bc\d+_[_a-z0-9]+/X1/g; 
			    my %trms=(); my %trms1 = (); my %trms2 = ();
			    @trms{split(/, /, $trms . ', ' . $trms2)}=(); 
			    @trms1{split(/, /, $trms1 . ', ' . $trms2)}=(); 
			    if(scalar(keys %trms)==0) {$trms{'"\$true"'}=()} 
			    if(scalar(keys %trms1)==0) {$trms1{'"\$true"'}=()} 
			    print SYM0 "$ref:", join(", ", sort keys %trms), "\n";
			    print SYMD "$ref:", join(", ", sort keys %trms1), "\n";
			    if($role eq 'axiom') { push(@newaxs,$ref) } else { $newconj = $ref }
			}
			close SYM0;
			close SYMD;
			close FSH;
		    }

# TODO: delete after processing

		    `cat $MMLSym0 $File.sym0.axiom > $File.0eq`;
		    `cat $MMLSymd $File.symd.axiom > $File.deq`;
		    `cp $MMLAtpdeps15 $File.atp15`;
		    `cp $MMLMizdeps15 $File.miz15`;
		    `cp $MMLSeq $File.seq`;
		    `grep  "conjecture," $File > $File.conjecture`; 
		    
		    open(FS,">>$File.seq"); print FS join("\n",@newaxs),"\n"; close(FS);
		    open(FS2,">>$File.atp15"); 
		    open(FS3,">>$File.miz15"); 
		    open(FSDBG2,">$File.atpdbg15"); 
		    foreach my $nax (@newaxs)
		    {
			my $lrfs = '';
			if(exists($grefdeps{$nax}))
			{
			    my @mrfs = grep { m/^[ltds]/ } split(/, */, $grefdeps{$nax});
			    $lrfs = join(' ', @mrfs);
			}
			print FS2 $nax,':',$lrfs,"\n";
			print FS3 $nax,':',$lrfs,"\n";
			print FSDBG2 $nax,':',$lrfs,"\n";
		    }
		    close(FS2);
		    close(FS3);
		    close(FSDBG2);
		}

		print $fatplog ($outseparator, "Learning files written for $File ", time() - $starttime, "s\n");
		next POS  if ($gonly_create_problems == 2);

		##DEBUG print `pos`;
		##DEBUG print `pwd`;
		##DEBUG print "$runwtlimit $cpulimit $vampire -proof tptp -ss included -sd 1 -output_axiom_names on --mode casc -t 10 -m 1234  -input_file $File | tee $File.eout1 | grep '\bfile('|";
		# my $eproof_pid = open(EP,"$runwtlimit $cpulimit $vampire\ $vampire_params_sd 1 $vampire_params2 $File.big | tee $File.eout1 | grep '\\bfile('|") or die("bad vampire input file $File.big"); 


		if( -e $fofshared2)
		{

		    system("$parwtlimit1 $cpulimit $aname $File $mmlversion $Bindir $MMLAxs  > $File.eout");

		    print $fatplog ($outseparator, "parwtlimit1 done for $File ", time() - $starttime, "s\n");
		    my $exit_code = system('grep', '-q', '\\bfile(',  "$File.eout");
		    if(0 == $exit_code)
		    {
			open(EP,"cat $File.eout|");
		    }
		    else
		    {
			#my $eproof_pid = open(EP,"$parwtlimit $cpulim1 $vampire $File $mmlversion $vampire_params3  | tee $File.eout |") or die("bad vampire run on input filestem $File");
		    }
#		    my $eproof_pid = open(EP,"$parwtlimit1 $cpulimit $aname $File $mmlversion $Bindir $MMLAxs  | tee $File.eout |") or die("bad vampire run on input filestem $File");
		}
		else
		{
		    my $eproof_pid = open(EP,"$parwtlimit $cpulimit $vampire $File $mmlversion $vampire_params2  | tee $File.eout |") or die("bad vampire run on input filestem $File");
		}
##--- read the needed axioms for proof
 		while ($_=<EP>)
 		{
		    m/.*\bfile\([^\),]+, *([a-z0-9A-Z_]+) *\)/ or die "bad proof line: $File.eout: $_";
		    my $ref = $1;
		    my $ref1 = ($ref=~ m/(.*)__.*/)? $1 : $ref;
		    my $pos = $fla2pos{$ref1};
		    push( @refs, "$ref\[$pos\]");
		    my ($fullmiz, $simplemiz) = MPTPNames::MizarizeRef($ref, $aname_uc, \%fla2name, $line, $pos);
		    push( @mizrefs, $fullmiz . ($pos ? '[' . $pos .']' : ''));
		    push( @simprefs, $simplemiz) if(length($simplemiz) > 0);
		}
		close(EP);

		if(-e $fofshared2)
		{
		    unlink ("$File.0eq", "$File.deq", "$File.atp15", "$File.miz15", "$File.seq");
		}

		print $fatplog ($outseparator, "proving done for $File ", time() - $starttime, "s\n");

		##DEBUG print ("refs: ", join(",",@refs));

		# Vampire can print multiple SZS lines (for each strategy); get the last one
 		my $status_line = `grep 'SZS status' $File.status |tail -n1`;

		if ($status_line=~m/.*SZS status[ :]*([a-zA-Z0-9_-]+)/)
		{
		    $status = $1;
		}
		else
		{
		    #print "Bad vampire status line: $status_line, please complain";
		}
 		if (!($status eq szs_THEOREM) && !($status eq szs_UNSAT)) 
		{ @refs = (); @mizrefs = (); @simprefs = (); 
		  print $fhout ($line, "_", $col, ":::", "Unsolved\n"); 
		}
		else 
		{ 
		    print $fhout ($line, "_", $col, ":", join(',',@refs), "\n"); 
		    print $fhout ($line, "_", $col, "::", join(',',@mizrefs), "\n");
		    print $fhout ($line, "_", $col, ":::", join(',',@simprefs), "\n");
		}
	    }

	}
	print $fhout ($outseparator, "Request took ", time() - $starttime, "s\n");
	print $fatplog ($outseparator, "Request took  ", time() - $starttime, "s\n");

    }
}

# A=m_drxj,D='/tmp/matp_704/',load_mml_for_article(A, D, [A]),install_index,mk_article_problems(A,[[mizar_by],[theorem, top_level_lemma, sublemma]],[opt_REM_SCH_CONSTS,opt_TPTP_SHORT,opt_ADDED_NON_MML([A]),opt_NON_MML_DIR(D),opt_LINE_COL_NMS]).

#    print "<pre>";
#    open(F,$ProblemFile);
#    open(F,$ProblemFileHtml);
#    { local $/; $_= <F>; print $_; }
#    print "<pre/>";
#    print $query->end_html;
#    close(F);


# if((length($input_article) < 1)
#    or ($input_limit < 1)
#    or (0 == GetQuerySymbols($input_article, \%gsyms)))
# {
#     print "Insufficient input\n";
#     $query->end_html unless($text_mode);
#     exit;
# }

# $grefs = GetRefs(\%gsyms, $input_limit);
# if($#{ @$grefs} < 1)
# {
#     print "Input contained no known constructors, no advice given\n";
#     $query->end_html unless($text_mode);
#     exit;
# }


# my $i = -1;
# my $outnr = min($input_limit, 1+$#{ @$grefs});

# unless($text_mode)
# {
#     print "<pre>";
#     print $query->h2("References sorted by expected importance");
# }

# my $megrezurl = "http://megrez.mizar.org/cgi-bin/meaning.cgi";
# while(++$i < $outnr)
# {
#     my $ref = $grefs->[$i];
#     my ($kind, $nr, $an);
# #    MPTP-like constructors commented, we now expect Query-like format
# #    $ref=~/^([td])([0-9]+)_(.*)/ or die "Bad reference $ref\n";
# #    ($kind, $nr, $an) = ($1, $2, $3);
# #    $kind = ($kind eq "t")? "th" : "def";
#     $ref=~/^([A-Z0-9_]+):(th|def|sch) (\d+)/ or die "Bad reference $ref\n";
#     ($an, $kind, $nr) = ($1, $2, $3);

#     if($text_mode)
#     {
# #	my $nkind = ($kind eq "def")?"def ":"";
# 	my $nkind = ($kind eq "th")? "": $kind . " ";
# 	print uc($an) . ":" . $nkind . $nr . "\n";
#     }
#     else
#     {
# 	print "<a href=\"".$megrezurl."?article=".$an."&kind=".$kind
# 	    ."&number=".$nr."\" target=entry>".uc($an).":".$kind." "
# 		.$nr."</a>\n";
#     }
# }

# unless($text_mode)
# {
#     print "<pre/>";
#     print $query->end_html;
# }


#----Old advisor stuff (unused now)

# returns nr. of syms with repetitions
sub GetQuerySymbols
{
    my ($fla, $syms) = @_;
    my $res = 0;

    while($fla =~ /\b([0-9A-Z_]+):(func|pred|attr|mode|aggr|sel|struct)[ .]([0-9]+)/g)
    {
#    MPTP-like constructors commented, we now expect Query-like format
#	my $aname	= lc($1);
#	my $sname	= $gconstrs{$2}."$3"."_".$aname;

        my $sname	= $1.":".$2." ".$3;
	$syms->{$sname}	= ();	# counts can be here later
	$res++;
    }
    return $res;
}


# limit not used here yet
sub GetRefs
{
    my ($syms, $limit) = @_;
    my ($msgin, @res);
    my $EOL = "\015\012";
    my $BLANK = $EOL x 2;
    my $remote = IO::Socket::INET->new( Proto     => "tcp",
					PeerAddr  => $ghost,
					PeerPort  => $advisorport,
				      );
    unless ($remote)
    {
	print "The server is down, sorry\n";
	$query->end_html unless($query_mode eq 'TEXT');
	exit;
    }
    $remote->autoflush(1);
    print $remote join(",",(keys %$syms)) . "\n";
    $msgin = <$remote>;
    @res  = split(/\,/, $msgin);
    close $remote;
    return \@res;
}
