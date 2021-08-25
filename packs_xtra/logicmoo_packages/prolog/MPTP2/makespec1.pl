#!/usr/bin/perl -w
# make the snow_spec(Theorem, Refs) input
# from the snow input, now hardwired the name of reference file
# - fix it.

use strict;
use Getopt::Long;

my $glimit = 30;
my @gnrref;     # Nr2Ref array for references
my @refarr  = ();
my $namein="";
my $predpos=0;
my $ignor=0;
my $gignore_defs=1;

Getopt::Long::Configure ("bundling","no_ignore_case");

my ($help, $man);
GetOptions('limit|l=i'         => \$glimit,
	   'help|h'            => \$help,
	   'man'               => \$man)
    or pod2usage(2);

my @gnrtoref = ();
open(IN, 'Snow_MML.refnr');
while ($_=<IN>)
{
    if (/snow_refnr[(]([^,]+),([^)]+)[)]/)
    {
	$gnrtoref[$2] = $1;
    }	
}
close(IN);

while ($_=<>) 
    {
	if (/Example/)        # Start entry for a new example
	{
	    /Example.*:(.*)/ or die "Bad Example $_";
	    $predpos = 0;
	    if ((length($namein) > 0) && (! $ignor))
	    {
	    print ("snow_spec(",$namein, ",[", join(",", @refarr), "]).\n");
	    }
	    @refarr  = ();
	    my @wanted = split /\, /, $1;
	    $namein = $gnrtoref[$wanted[0]];	    
	    $ignor = (($gignore_defs == 1) && ($#wanted == 0));
	}
# Push wanted targets 
	else
	{
	    if (($predpos++ < $glimit) && 
	    (/^(\d+):[ \t]*[^ \t]+[ \t]+([^ \t]+).*/))
	    {
		push(@refarr, $gnrtoref[$1]) unless (0 == $2);
	    }
	}
    }
print ("snow_spec(",$namein, ",[", join(",", @refarr), "]).\n");
