#!/usr/bin/perl -w

# make the dcl2,dco2,lem2,sch2 and the2 files from xml2

# SYNOPSIS: 
# for i in tarski `cat mml.lar`; do echo $i; 
# perl -F mk_derived.pl $i;
# done

my $f=$ARGV[0]; 
# @path = split(/\//,$f);

my ($Dcl, $The, $Sch, $Lem, $Dco, $Xml) 
    = ($f . '.dcl2', $f . '.the2', 
       $f . '.sch2', $f . '.lem2', 
       $f . '.dco2', $f . '.xml2'); 


## create the derived files
open(DCL, ">$Dcl"); open(THE, ">$The"); open(SCH, ">$Sch"); 
open(LEM, ">$Lem"); open(DCO, ">$Dco"); open(XML, $Xml);
while($_=<XML>)
{
    if(m/^fof.[dt][0-9]/) { print THE $_; }
    elsif(m/^fof.dt_[fklmgu]/) { print DCO $_; }
    elsif(m/^fof.(([fcr]c)|(ie)|(rd))[0-9]/) { print DCL $_; }
    elsif(m/^fof.[s][0-9]/) { print SCH $_; }
    elsif(m/^fof.[l][0-9]/) { print LEM $_; }
    elsif(m/^fof.(abstractness|antisymmetry|asymmetry|commutativity|connectedness|existence|free|idempotence|involutiveness|irreflexivity|projectivity|redefinition|reflexivity|symmetry)_[gklmruv]/) { print DCO $_; }
}

close(DCL); close(THE); close(SCH); 
close(LEM); close(DCO); close(XML);
