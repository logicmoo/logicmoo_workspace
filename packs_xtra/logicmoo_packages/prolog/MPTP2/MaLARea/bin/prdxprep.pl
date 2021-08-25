#!/usr/bin/perl -w

## $Revision: 1.5 $

## prdxprep.pl

## Postprocess Paradox models for prdx2p9.pl
##
## Makes a Prolog program, that can be loaded into prolog,
## so that a Mace4 model could be created.
## Creating the Mace4 model directly here could be hard,
## because Paradox uses general clauses to make a more compact
## output, which is not allowed in the Mace4 format.
## These general clauses are easily instantiated in prolog.

## All numerical values are decreased by 1 here, so that they
## were zero-based.

use strict;
my $state = 0;
my %pred = ();
my %func = ();

while(<>)
{
    if(m/SZS output start FiniteModel/) { $state = 1; $_=<>; }
    if(m/SZS output end FiniteModel/) { $state = 0; }
    if($state == 1)
    {
	if(m/fi_domain/) { $_=<>; $_=<>; $_=<>;}
	s/\$dp[!][0-9]+/dppppp/g;
	s/<=>/:-/g;
	s/[!] *\[[A-Z][^\]]*\] *[:.]//g;
	s/\"//g;
	s/\$false/fail/g;
	s/\$true/true/g;
	if(m/^fof. *([^,]+), *fi_predicates/) { $pred{$1} = -1 unless('dppppp' eq $1); }
	elsif(m/^fof. *([^,]+), *fi_functors/) { $func{$1} = -1 unless('dppppp' eq $1); }
	elsif(m/^% +domain size is +([0-9]+)/) { print "domain_size($1).\n"; }
	elsif(m/([a-z][a-zA-Z0-9_]*)\(?([^ \)]*)\)? *(=|:-) *([0-9]+|true|fail|[a-zA-Z0-9_(),]*)/)
	{
	    my ($pf, $args, $which, $val) = ($1, $2, $3, $4);
	    if (!($pf eq 'dppppp'))
	    {
		my $h = ($which eq '=') ? \%func : \%pred;

		die "error on line: $_" unless exists $h->{$pf};

		my @argsa = split(/\,/, $args);
		if ( $h->{$pf} == -1 ) { $h->{$pf} = scalar( @argsa ); }
		elsif( $h->{$pf} != scalar( @argsa ))
		{
		    die "arity $h->{$pf} expected on line $_";
		}

		foreach my $i (0 .. $#argsa) { if($argsa[$i] =~ m/^\d+/) { $argsa[$i]--; }}
		$args = join(',',@argsa);

		## the recursive case
		if(!($val eq 'true') && !($val eq 'fail') && !($val =~ m/^\d+/))
		{
		    $val =~ m/([a-z][a-zA-Z0-9_]*)\(?([^ \)]*)\)?/ or die "error on line: $_";
		    my ($pf1, $args1)  = ($1, $2);
		    ($pf eq $pf1) or die "error on line: $_";
		    my @argsa1 = split(/\,/, $args1);
		    foreach my $i (0 .. $#argsa1) { if($argsa1[$i] =~ m/^\d+/) { $argsa1[$i]--; }}
		    $args1 = join(',',@argsa1);

		    if($which eq '=') { print ("$pf([$args],N):- !,$pf([$args1],N).\n"); }
		    else { print "$pf([$args]):- !,$pf([$args1]).\n"; }
		}
		## the nonrecursive case
		elsif($which eq '=') { print ("$pf([$args],", $val - 1, "):- !.\n"); }
		else { print "$pf([$args]):- !,$val.\n"; }
	    }
	}
#	print $_;
    }
}

my @fa = ();
foreach my $f (sort keys %func) { push(@fa, $f . '/' . $func{$f}); }
my $fas = join(',', @fa);

my @pa = ();
foreach my $p (sort keys %pred) { push(@pa, $p . '/' . $pred{$p}); }
my $pas = join(',', @pa);


print "functors([$fas]).\n";
print "predicates([$pas]).\n";

