#!/usr/bin/perl
# translate first $lime accessible numeric predictions (from MOR, etc) to formulas names
# run like: 
# ./predictions2specs.pl 40 MORPredictions > zzz.advice40-mor
#
# TODO: this is still with hardwired paths in it

$i=0; $j=0; %g=(); $lim=shift;

sub min { my ($x,$y) = @_; ($x <= $y)? $x : $y }
sub max { my ($x,$y) = @_; ($x <= $y)? $y : $x }


open(F,"probs6.refnr");
while(<F>) {chop; $a[$i]=$_; $h{$_}=$i; $i++;}
open(F,"00chordth1");
while(<F>) {chop; $c[$j]=$_; $ch{$_}=$j; $j++;}
open(G,"probs1.specs");

while(<G>)
{
  m/^spec\( *([a-z0-9A-Z_]+) *, *\[(.*)\] *\)\./ or die;
  ($ref, $refs) = ($1, $2); my @refs = split(/\,/, $refs);
  foreach $rr (@refs) { $g{$ref}{$rr}=(); }
}

$thnr=0;
while(<>)
{
    chop;
    exists $c[$thnr] or die;
    @k=($c[$thnr]);
    foreach $l (split(",",$_))
    { 
	exists $a[$l] or die "$l:$a[$l]";
	push(@k,$a[$l]) if (exists $g{$c[$thnr]}{$a[$l]}) ;
    }
    my $l1 = min($#k,$lim);
    print join(",",@k[0 .. $l1]),"\n";
    $thnr++;
}
