#!/usr/bin/perl

# SYNOPSIS
# ./mkloggraph.pl unsolved.log >unsolved.stats
# creates summary of results of all passes in unsolved.log


my @cumul = (0);
$total = 0;
$state = 0;
$giter = 0;
$theoremnbr = 0;
$timelimit[0] = 1;
while (<>) 
{
  if(m/SOLVED: *1[+]([0-9]+)/) 
    { 
      $solved[$giter] = 1+$1; 
      die "Theorems not in sync: $1: $theoremnbr" if(($giter> 3) && (1+$1 != $theoremnbr));
    }
  if(m/SYMBOL ONLY/) { $solved[$giter] = $theoremnbr; $state = 1; } 
  if(m/THRESHOLD: *([0-9]+)/) 
    { 
       $giter++; $threshold[$giter] = $1; $theoremnbr = 0;
       $timelimit[$giter] = $timelimit[$giter - 1];
      $solved[$giter] = 0;
     }
  if(m/TIMELIMIT: *([0-9]+)/) { $timelimit[$giter] = $1; }
  if(m/LEARNING: *([0-9]+)/) 
    { 
      $learning[$giter] = $1; 
      if($state == 1) 
	{
	  $solved[$giter] = $theoremnbr;
	  $giter++; $threshold[$giter] = $threshold[$giter - 1]; $theoremnbr = 0;
	  $timelimit[$giter] = $timelimit[$giter - 1];
	  $solved[$giter] = 0;
	  $state = 2;
	}
    }
  if((m/Theorem/) && !(m/% SZS/)){ $theoremnbr++; }
  if(($state == 0) && (m/.*:.*:/)) { $total++; }
}
#print "$giter\n";

foreach $i (1 .. $giter) { $cumul[$i] = $cumul[$i-1] + $solved[$i]; }
my $rest = $cumul[$giter] - $solved[1];

print "total: $total, solved: $cumul[$giter], pass 1: $solved[1], pass 2- : $rest\n";
print "iternr\ttimelim\taxlimit\tsolved\tcumul\n";
foreach $i (1 .. $giter)
  {
    print "$i:\t$timelimit[$i]\t$threshold[$i]\t$solved[$i]\t$cumul[$i]\n";
  }


#   THRESHOLD: 4
#   TIMELIMIT: 1
#   LEARNING:3
