#!/usr/bin/perl

# In a directory with E protokolls,
# create their aaa* versions with solved problems only (a bit redundant),
# list all solved problems into 00SOLVED,
# then write to stdout the minimal set cover problem
# for getting the minimal set of protokolls which cover all solved problems.
# Note that protokolls are translitarated to be acceptable by minisat+, while
# problem names are not, which can potentially cause trouble.

# SYNOPSIS:
# cd protokoll_directory
# ./find_problem_cover.pl > InputFile
# minisat+_64-bit_static InputFile > OutputFile

foreach $prot ( `ls protoko*` ) { chop($prot); `cat $prot | grep success | cut -f1 -d \\  > aaa$prot`; }
`cat aaaprotoko* | sort -u > 00SOLVED`;
`ls aaaprotoko* > 00PROTS`;
open(PROT,"00PROTS");
$i=0; 
@prot = ();
while(<PROT>) { chop; push(@prot,$_); $hprot{$_}=1+$#prot; }
close(PROT);
print "min: ";
foreach $prot (@prot) { $prot=~s/aaaprotokoll_//; $prot=~tr/\-/_/; print " +1*",$prot; }
print ";\n";
open(PROB,"00SOLVED");
while(<PROB>) 
{
    chop;
    @matched=`grep -l $_ aaaprotoko*`;
    foreach $prot (@matched) { chop($prot); $prot=~s/aaaprotokoll_//; $prot=~tr/\-/_/; print " +1*",$prot; }
    print ">= 1;\n";
} 



