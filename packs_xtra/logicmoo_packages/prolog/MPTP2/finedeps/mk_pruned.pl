#!/usr/bin/perl
#
# Create small problems from big ones by grepping for the formulas that are in
# prediction files. Run like:
#
# for i in `seq 10 10 200`; do echo $i; time ./predictions2specs.pl $i MORPredictions > zzz.advice$i-mor; done
# time for i in `seq 10 10 200`; do time ./mk_pruned.pl $i zzz.advice"$i"-mor; done
#
# TODO: this still relies on the big problems living in "probspr40-cg/*__$f.big"

$lim=$ARGV[0]; # 40
$adv=$ARGV[1]; # zzz.advice40-mor

die unless ((defined $adv) && open(A,$adv));

while(<A>)
{
   chop;
   @r=split /,/;
   $f=$r[0];
   @l = glob("probspr40-cg/*__$f.big");
   die @l unless($#l == 0);
   open(F,$l[0]) or die;
   open(F1,">$l[0].pr$lim") or die;
   $s= "^fof.(" . join("|",@r) . "),";
   while(<F>)
   {
       if(m/$s/) { print F1 $_ }
   }
   close(F1);
   close(F);
}

