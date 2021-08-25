#!/usr/bin/perl

# expects sorted formulas in file 01: cat *| sort -u >01
# then it renames  duplicate names  to ju_hack_1, _2, etc.
%h=();%d=();%r=(); $ju=0; 

open(I,"01");
while(<I>) { 
  m/([^,]+),[^,]+,(.*)/ or die "bad"; 
  if ((exists $h{$1}) && (!($h{$1} eq $2)))
  {
    if(!(exists $r{$2})) { $r{$2} = ++$ju; 
    print $1, $h{$1},"\n";
    print $1,$2,"\n"; }
  } 
  else {$h{$1}=$2}
};
close(I); 

@args = `ls [a-z]*`;
while($f=shift @args) 
{
    chop $f;
    open(F,$f); open(O,">../renamed/$f"); 
    while(<F>) { 
	m/([^,]+),([^,]+),(.*)/ or die "bad";
	if (exists $r{$3}) {print O ("fof(ju_hack_", $r{$3})} else {print O "$1";}; print O ",$2,$3\n"; } close(F); close(O);} 
