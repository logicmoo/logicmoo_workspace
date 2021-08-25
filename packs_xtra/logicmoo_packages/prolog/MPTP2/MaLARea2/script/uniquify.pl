#!/usr/bin/perl -w

## $Revision: 1.5 $

=head1 NAME

uniquify.pl (prepare LTB problems for malarea)

=head1 SYNOPSIS

script/uniquify.pl [options] ltbfileinput malareaprobdir problemrenamingfile formularenamingfile

script/uniquify.pl /home/urban/ltbtest1 /tmp/mal1/probdir1 pr1 fr1

 Options:
   --forgetlimit=<arg>,     -f<arg>
   --duplprint=<arg>,       -d<arg>
   --merge=<arg>,           -m<arg>
   --unique=<arg>,          -u<arg>
   --help,                  -h
   --man

=head1 OPTIONS

=over 8

=item B<<< --forgetlimit=<arg>, -f<arg> >>>

Limits the problems to those that have less formulas than this limit.
This is useful if some parts of malarea cannot cope (e.g. tptp_to_ladr
takes ages on the CSR002+4.ax, which has 540294 formulas and 372664 symbols).
Default is 0 - no limiting.

=item B<<< --duplprint=<arg>, -d<arg> >>>

If > 0, prints all the duplications to stdout. Useful e.g. for
finding duplications in the whole MPTP, when run just on
the thms.allasax file. Use the htmlize.pl script afterwards
to link the results. Default is 0.

=item B<<< --merge=<arg>, -m<arg> >>>

Do merging of names of the same formulas.
Default is 1 - do the merging.

=item B<<< --unique=<arg>, -u<arg> >>>

Do uniquifying of names shared by different formulas.
Default is 1 - do the uniquifying.

=item B<<< --help, -h >>>

Print a brief help message and exit.

=item B<<< --man >>>

Print the manual page and exit.

=back


=head1 DESCRIPTION

Gets the LTB input file with problems' absolute paths and solutions absolute paths.
Normalizes the problems so that the conjectures were named after the problems,
formula names were unique, and same formulas had the same name (if possible
by the unique conjecture naming constraint).
The second mandatory input is the directory where the preprocessed problems will 
be placed for malarea, it should be a subdir of where malarea is installed.
The third and fouth optional args are the namse of files created by uniquify.pl,
where the new->old problem mapping will be stored, and where the
newflaname,, newproblemname -> oldflaname mapping will be stored.

Note that some original problems can be forgotten (e.g., if they are too big).

Algo:

=cut

use strict;
use Pod::Usage;
use Getopt::Long;
use IPC::Open2;
use File::Spec;


my ($gforgetlimit,  $gmerge,    $gunique,
    $gduplprint, $gconjnaming);


my ($help, $man);

Getopt::Long::Configure ("bundling");

GetOptions('forgetlimit|f=i'    => \$gforgetlimit,
	   'duplprint|d=i'    => \$gduplprint,
	   'merge|m=i'    => \$gmerge,
	   'unique|u=i'    => \$gunique,
	   'conjnaming|c=i'    => \$gconjnaming,
	   'help|h'          => \$help,
	   'man'             => \$man)
    or pod2usage(2);

pod2usage(1) if($help);
pod2usage(-exitstatus => 0, -verbose => 2) if($man);

pod2usage(2) if ($#ARGV != 3);

my $gltbfile   = shift(@ARGV);
my $gprobdir   = shift(@ARGV);
my $gprobrenm   = shift(@ARGV);
my $gflarenm   = shift(@ARGV);


$gforgetlimit = 100000000 unless(defined($gforgetlimit));
$gduplprint = 0 unless(defined($gduplprint));
$gmerge = 1 unless(defined($gmerge));
$gunique = 1 unless(defined($gunique));
$gconjnaming = 0 unless(defined($gconjnaming));


my %gltbprobnames;
my %gfla2ref = ();
my %gfla2ref1 = ();
my %gref2fla = ();
my %gref2fla1 = ();
my %gfile2ref = ();
my %gfile2conj = ();
my %gfla2orig = ();

## read the ltb file into the %gltbprobnames hash, indexed by corresponding
## tptp name, with the orig problem and solution names as values
sub ReadLTB
{
    my ($ltbfile) = @_;

    open(LTB, $ltbfile) or die "$ltbfile not readable";

    my $readprobs = 0;

    %gltbprobnames = ();
    while($_=<LTB>)
    {
	if(m/^\s*%\s*SZS\s+start\s+BatchProblems/)
	{
	    $readprobs = 1;
	}
	if(m/^\s*%\s*SZS\s+end\s+BatchProblems/)
	{
	    $readprobs = 0;
	}
	if(($readprobs == 1) && m/^\s*(\S+)\s+(\S+)\s*$/)
	{
	    my ($pname, $sname) = ($1,$2);
	    die "$pname not readable!" unless(-e $pname);
	    my ($volume,$directories,$file) = File::Spec->splitpath( $pname );
	    my $tptpname = MkTptpName($file);
	    if($gconjnaming == 1)
	    {
		$tptpname = `grep 'fof(.*, *conjecture[ ,]' $pname | sed -e 's/^.*fof( *\\([^ ,]*\\) *, *conjecture.*/\\1/'`;
		chomp($tptpname);
	    }
	    die "Non-unique TPTP-fied basenames not allowed: $file, $tptpname, $pname, $gltbprobnames{$file}->[0]"
		if(exists $gltbprobnames{$tptpname});
	    $gltbprobnames{$tptpname} = [$pname, $sname];
	}
    }
    close(LTB);
}

## downcase, translate nonalphanum chars to '_', add 'p' in the beginning
## if starting with '_' after that
sub MkTptpName
{
    my ($basename) = @_;
    $basename =~ tr/A-Z/a-z/; $basename =~ tr/a-zA-Z0-9_/_/c;
    if($basename =~ m/^_.*/) { $basename = 'p' . $basename; }
    return $basename;
}


# if multiple entries in ${$gref2fla{$nm}}{$rest}, they get their
# $file appended, and the entries in $gfla2ref{$rest}{$nm},
# and $gfile2ref{$file}}{$nm} are changed
# after this $gref2fla has just one entry for each $nm, so it becomes
# an array $gref2fla{$nm}->[$rest,@filenames]
sub Rename
{
    foreach my $nm (keys %gref2fla)
    {
	my @restarr = keys %{$gref2fla{$nm}};
	if($#restarr > 0)
	{
	    foreach my $rest (@restarr)
	    {
		my @files = @{${$gref2fla{$nm}}{$rest}};
		my $newname = $nm . '__' . $files[0];
		$gref2fla1{$newname} = [$rest, @files];
		${$gfla2ref{$rest}}{$newname} = ${$gfla2ref{$rest}}{$nm};
		delete ${$gfla2ref{$rest}}{$nm};
		foreach my $file (@files)
		{
		    ${$gfile2ref{$file}}{$newname} = ${$gfile2ref{$file}}{$nm};
		    delete ${$gfile2ref{$file}}{$nm};
		    ${$gfla2orig{$file}}{$newname} = $nm;
		}
	    }
	}
	else
	{
	    $gref2fla1{$nm} = [ $restarr[0], @{${$gref2fla{$nm}}{$restarr[0]}} ];
	}
    }
}



# then if multiple entries in $gfla2ref{$rest}{$nm},
# all nms are renamed to the first nm, and again renamed in gfile2ref
# and possibly also gfla2ref (not much needed)

sub Merge
{
    foreach my $rest (keys %gfla2ref)
    {
	my @nmarr = keys %{$gfla2ref{$rest}};
	if($#nmarr > 0)
	{
	    my $newname = $nmarr[0];
	    print (join(',',@nmarr),"\n") if ($gduplprint > 0);
	    $gfla2ref1{$rest} = [$newname];
	    foreach my $nm (@nmarr)
	    {
		my @files = @{${$gfla2ref{$rest}}{$nm}};
		push ( @{$gfla2ref1{$rest}}, @files);
		if(!($nm eq $newname))
		{
		    push( @{$gref2fla1{$newname}}, @{$gref2fla1{$nm}});
		    delete $gref2fla1{$nm};
		    foreach my $file (@files)
		    {
			${$gfile2ref{$file}}{$newname} = ${$gfile2ref{$file}}{$nm};
			delete ${$gfile2ref{$file}}{$nm};
			${$gfla2orig{$file}}{$newname} = $nm;
		    }
		}
	    }
	}
	else
	{
	    $gfla2ref1{$rest} = [ $nmarr[0], @{${$gfla2ref{$rest}}{$nmarr[0]}} ];
	}
    }
}


## for each file its axioms, conjcture,
sub ParseFileFlas
{
    my ($file) = @_;
    my $origfile = $gltbprobnames{$file}->[0];
    my @lines = `bin/tptp4X -x -f tptp:short  -u machine -c $origfile`;
    return 0 if($#lines > $gforgetlimit);  ## forget about this one
    foreach $_ (@lines)
    {
	$_ = FixQuoted($_) if(m/^ *fof\(.*['"].*/);
	if(m/^ *fof\( *([^, ]+) *, *([^, ]+) *,(.*)/)
	{

	    my ($nm,$status,$rest)=($1,$2,$3);
	    if ($status=~m/^conjecture/)
	    {
		$gfile2conj{$file} = $rest;
		${$gfla2orig{$file}}{$file} = $nm;
	    }
	    else
	    {
		push( @{${$gfla2ref{$rest}}{$nm}}, $file);
		push( @{${$gref2fla{$nm}}{$rest}}, $file);
		${$gfile2ref{$file}}{$nm} = $rest;
	    }
	}
    }
    return 1;
}

## replace single/double quoted strings with something sane for malarea
sub FixQuoted
{
    my ($fof) = @_;
    $fof =~ s/[\\][']/qqq01/g;
    $fof =~ s/[\\]["]/qqq02/g;
    if($fof=~ m/^[^"]*['].*/)   ## the first quoting is single, kill it first
    {
	my @parts = split(/\'/,$fof);
	my $i;
	for ($i = 1; $i < $#parts; $i=$i+2)
	{
	    $parts[$i] =~ s/[^_a-zA-Z0-9]/'_qqq' . ord($&)/ge;
	}
	$fof = join('qqq01', @parts);
    }
    if($fof=~ m/^[^']*["].*/)   ## the first/remaining quoting is double, kill it now
    {
	my @parts = split(/\"/,$fof);
	my $i;
	for ($i = 1; $i < $#parts; $i=$i+2)
	{
	    $parts[$i] =~ s/[^_a-zA-Z0-9]/'_qqq' . ord($&)/ge;
	}
	$fof = join('qqq02', @parts);
    }
    if($fof=~ m/^[^"]*['].*/)   ## finally kill the possible nonfirst single quoting
    {
	my @parts = split(/\'/,$fof);
	my $i;
	for ($i = 1; $i < $#parts; $i=$i+2)
	{
	    $parts[$i] =~ s/[^_a-zA-Z0-9]/'_qqq' . ord($&)/ge;
	}
	$fof = join('qqq01', @parts);
    }
    return $fof;
}

sub Main
{
    ReadLTB($gltbfile);
    my @tptpnames = keys %gltbprobnames;
    foreach my $file (@tptpnames)
    {
	my $res = ParseFileFlas($file);
	delete $gltbprobnames{$file} if ($res == 0);
    }

    Rename() if($gunique > 0);
    Merge() if($gmerge > 0);

    open(PR,">$gprobrenm") or die "$gprobrenm not writable";
    foreach my $file (sort keys %gltbprobnames)
    {
	print PR ($file, ' ', $gltbprobnames{$file}->[0], ' ', $gltbprobnames{$file}->[1], "\n");
    }
    close(PR);

    open(FR,">$gflarenm") or die "$gflarenm not writable";
    foreach my $file (sort keys %gfla2orig)
    {
	foreach my $name (sort keys %{$gfla2orig{$file}})
	{
	    print FR ($file, ' ', $name, ' ', ${$gfla2orig{$file}}{$name}, "\n");
	}
    }

    foreach my $file (sort keys %gltbprobnames)
    {
	open(NEW, ">$gprobdir/$file") or die "$gprobdir/$file not writable";
	foreach my $name (sort keys %{$gfile2ref{$file}})
	{
	    print NEW ('fof(',$name,',axiom,',${$gfile2ref{$file}}{$name},"\n");
	}
	print NEW ('fof(',$file,',conjecture,',$gfile2conj{$file},"\n");
	close(NEW);
    }
}

Main();

