#!/usr/bin/perl -w

# SYNOPSIS:

# Run E on provable problems and save proof premises into *.proof:

# for i in `cat 00proved`;do echo $i;
# eprover -l4 --pcl-compact --tstp-in -xAuto -tAuto $i | \
# epclextract  --tstp-out | grep file > $i.proof; done

# Then extract the references into 00proofs:

# for i in `ls *.proof`; do extract_e_refs.pl $i; done > 00proofs

# extract the references from E proof in tstp syntax;
# put them into 
# snow_proof( conjecture_name, th_or_def_names, bg_names)

use strict;

my @td_names = ();
my @bg_names = ();
my $conjecture = "";
while (<>)
{
    (/^fof[^,]+,(axiom|conjecture).*\bfile\b[^,]+,([^\)]+)[\)].*/)
	or die "Bad fof $_";

    if ("conjecture" eq $1) { $conjecture = $2; }
    else
    {
	my $ref = $2;
	if ($ref =~ m/^ *[dt]\d+_/) { push(@td_names, $ref); }
	else { push(@bg_names, $ref); }
    }
}

die "No conjecture" if ($conjecture eq "");

print "snow_proof($conjecture, [";
print (join(",", @td_names), "],[", join(",", @bg_names));
print "]).\n";
