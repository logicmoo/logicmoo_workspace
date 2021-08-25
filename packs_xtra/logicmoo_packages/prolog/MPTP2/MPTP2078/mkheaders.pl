#!/usr/bin/perl
#
# make TPTp headers for the MPTP2078 files
#
# ./mkheaders.pl 00chordth

open(F,"00challdesc");
while(<F>) 
{
    chop; ($id,$t,$a)=split("#",$_); $title{$id}=$t; $autor{$id}=$a; 
} 
close(F); 
open(F,"00domains");
while(<F>) 
{
    chop; m/#(.*):[ \t]*([^ \t]*)/ or die; print $2; $dom{$2}=$1;  
} 
while(<>) 
{
    chop; 
    open(G,">headers/$_") or die;
    m/(.*)__(.*)/ or die; 
    ($a,$t)=($1,$2);
    $t=~m/t(\d+).*/ or die; $t1=$1;
    $au=uc($a);
    print G "% Domain   : ", $dom{$a}, 
    "\n% Problem  :", $title{$a},", theorem $t1",
    "\n% Refs     : [$au] ",$autor{$a}, ", ", $title{$a}, 
    "\n%          : ", "[Urb12] Urban 2012, MPTP2078 problems, Email to G. Sutcliffe",
    "\n% Source   : [Urb12]",
    "\n% Names    : $a","__",$t,"\n";
    close(G);
}
