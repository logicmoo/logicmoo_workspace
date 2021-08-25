#!/usr/bin/perl
#
# Combine two rankings with linear weights. Print the 10,20,..,200 prediction files.
#
# ./mk_comb2.pl zzz.advice200-cg zzz.advice200-sine1
#
# TODO: remove the hardwired weight and result file names

$adv=$ARGV[0]; # zzz.advice200-snow
$adv1=$ARGV[1]; # zzz.advice200-sine

sub min { my ($x,$y) = @_; ($x <= $y)? $x : $y }

die unless ((defined $adv) && open(A,$adv));
die unless ((defined $adv1) && open(B,$adv1));

# The weight for the first component - now hardwired here.
$wr = 0.5;
$wl = 1 - $wr;

while(<A>) { chop; @r=split /,/; $i=0; $f=$r[0]; push(@all,$f); foreach $k (@r) {$rh{$f}{$k} = $i * $wr; $i++} $maxr{$f}= 200 * $wr;}
while(<B>) { chop; @l=split /,/; $j=0; $f=$l[0]; foreach $k (@l) {$lh{$f}{$k} = $j * $wl; $j++} $maxl{$f}=200 * $wl;}

foreach $f (@all)
{
    %h= %{$rh{$f}};
    foreach $k (keys %h) { if(!(exists $lh{$f}{$k})) { $h{$k} += $maxl{$f} }  }
    foreach $k (keys %{$lh{$f}}) { if(exists $h{$k}) { $h{$k} += $lh{$f}{$k} } else  { $h{$k} = $maxr{$f}+$lh{$f}{$k} }  }
    @res = sort {$h{$a}<=>$h{$b}} keys %h;
    print join( ",",@res), "\n"; print %h,"\n";
    $res{$f} = [@res];
}

foreach $i (1 .. 20)
{
    $j = 10*$i;
    open(F,">zzz.advice$j-zcombt");
    foreach $f (@all) { @res = @{$res{$f}}; print F join(",",@res[0 .. min($j,$#res)]), "\n"; }
    close(F);
}
