#!/usr/bin/perl
# like predictions2specs but without translation
# run like: 
# ./namepredictions2specs.pl 40 MORPredictions > zzz.advice40-mor
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
    s/\r?\n$//; # get rid of newlines also in dos files
    exists $c[$thnr] or die;
    @k=($c[$thnr]);
    foreach $l (split(",",$_))
    { 
	exists $h{$l} or die "bad: $l, thnr: $thnr, $th: $c[$thnr], sofar: @k";
	push(@k,$l) if (exists $g{$c[$thnr]}{$l}) ;
    }
    my $l1 = min($#k,$lim);
    print join(",",@k[0 .. $l1]),"\n";
    $thnr++;
}
