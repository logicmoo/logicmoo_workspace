#!/usr/bin/perl -w

=head1 NAME


SnowRes2SpecFile.pl ( Snow predictions to mkproblem's input )

=head1 SYNOPSIS

  # Train snow with Naive Bayes targets 0-41079 on $NAME.train,
  # test then on $NAME.test, limiting prediction output to 100 best hits,
  # create problem specifications taking the best 30 predictions as axioms,
  # create the problems according to the specifications

 snow -train -I $NAME.train -F $NAME.net -B :0-41079
 snow -test -o allboth -I $NAME.test -F $NAME.net -B :0-41079 | LimitSnow.pl 100 > $NAME.res
 SnowRes2SpecFile.pl -l30 -r$NAME.refnr <$NAME.res > $NAME.spec
 mkproblem.pl -F $NAME.spec

 Options:
   --reftable=<arg>,        -r<arg>
   --limit=<arg>,           -l<arg>
   --help,                  -h
   --man

=head1 OPTIONS

=over 8

=item B<<< --reftable=<arg>, -r<arg> >>>

Mandatory argument giving the translation table for references 
(.refnr), this file is produced by the MPTPMakeSnowDB.pl 
data generating script, when preparing learning.

=item B<<< --limit=<arg>, -l<arg> >>>

Limit the number of Snow predictions used as axioms to
this value.

=item B<<< --help, -h >>>

Print a brief help message and exit.

=item B<<< --man >>>

Print the manual page and exit.

=back

=head1 DESCRIPTION

Output on stdout B<mkproblem.pl>'s problem specification file
created from the Snow predictions (result file).
We assume that the first reference in the 'Example' header is
the target, which is correct for current version of Snow (3.0.3).

=cut

use strict;
use Pod::Usage;
use Getopt::Long;

my $glimit;     # How many references we want
my $reftable;   # Translation table for references (*.refnr),
                # one refname in line, 0-based

my $gonly_smaller_refs = 1;   # Only smaller refs are allowed
my $gone_setof = 1;           # Allow at most one reference containing "setof"

my @gnrref;     # Nr2Ref array for references

my ($help, $man);
Getopt::Long::Configure ("bundling");

GetOptions('reftable|r=s'    => \$reftable,
	   'limit|l=i'       => \$glimit,
	   'help|h'          => \$help,
	   'man'             => \$man)
    or pod2usage(2);

pod2usage(1) if($help);
pod2usage(-exitstatus => 0, -verbose => 2) if($man);

pod2usage(2) unless (defined $reftable);

$glimit    = 30 unless(defined($glimit));


# Load refnr
open(REFNR, "$reftable") or die "Cannot read refnr file";
while($_=<REFNR>) { chop; push(@gnrref, $_); };
close REFNR;

# Skip to the first example
do { $_=<> } while($_ && !($_=~/Example/));

die "STDIN contains no example predictions!"
    unless ($_=~/Example/);


my @predictions = ();
my $target = 0;
my $gexample_nr = 0;

while ($_)
{
    if (/Example/)        # Start a new example
    {
	if($gexample_nr++ > 0)   # cleanup the previous if any
	{
	    print (join(" ", @predictions), "\n");
	}

	@predictions = ();

	/Example *(\d+) *.*:(.*)/ or die "Bad Example $_";
	my @wanted = split /\, /, $2;
	$target = $1 - 1;

	die "Target $target has no name in $reftable!"
	    unless (exists $gnrref[$target]);

	  
	print ($gnrref[$target], ":");
    }
    elsif (/^([0-9]+):.*(\d+)[*]?$/)
    {
	my $axiom = $1;
	die "Reference $axiom has no name in $reftable!"
	    unless (defined $gnrref[$axiom]);

	if(($2>0) && (1+$#predictions) < $glimit)
	{
	    push(@predictions, $gnrref[$axiom]);
	}
    }
    $_=<>;
}

if($gexample_nr > 0)   # cleanup the last one
{
    print (join(" ", @predictions), "\n");
}
