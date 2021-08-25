#!/usr/bin/perl
# generate a reasonable MML-compatible ordering of all premises & conjectures
# assumes all problems together with corresponding problem.allowed_local files
# some hard-wired paths below: /dev/shm/probs, each article in its directory, prob2, etc.
# should be parameterized

# algorithm:
# 0. all problems for a given article are topologically sorted using their axioms, their .allowed_local, 
#    and the order from .xml2 file
# 1. only article-proper formulas are kept in the result of topological sorting
# 2. the article-level chunks are concatenated using mml.lar

# creating the .xml2 ordering:
# cd /home/mptp/mizwrk/7.11.06_4.150.1103/MPTP2/pl
# for i in `ls *.xml2| sed -e 's/.xml2//'`; do cut -f1 -d\, $i.xml2 |grep -v '([ie][0-9]' | grep -v  '(d[te]_c'| perl -e 'while(<>) {chop; s/^fof.//; if(defined $prev) { print "$_ $prev\n"} $prev=$_;}' > /dev/shm/probs/$i/01xmldeps; done

# then creating the 00srt data in /dev/shm/probs:

# old version allowing for allowed_local, which turned out to be buggy, see e.g. dt_k3_subset_1 in t10_subset_1
# for z in `cat mml.lar`; do cd $z; for j in `ls *$z`; do perl -e '$i=shift; $f=shift; open(F,$f); while(<F>) { if(m/^fof.([^,]+), conjecture,/) {$c=$1} elsif(m/^fof.([^,]+_)$i, axiom,/) {$h{$1.$i}=() }  } open(G,"$f.allowed_local"); $_=<G>; m/^allowed_local.$c, \[(.*)\]/ or die $c; @k=split(/ *, */, $1); foreach $l (@k) {$h{$l} = ();} foreach $l (sort keys %h) {print "$c $l\n"}' $z $j; done > 00uns; cat 01xmldeps 00uns | tsort | tac > 00srt; cd /dev/shm/probs; done

# new version, using only the problem and 01xmldeps (even then there are several bugs caused by redefs):
# for z in `cat mml.lar`; do cd $z; for j in `ls *$z`; do perl -e '$i=shift; $f=shift; open(F,$f); while(<F>) { if(m/^fof.([^,]+), conjecture,/) {$c=$1} elsif(m/^fof.([^,]+_)$i, axiom,/) {$h{$1.$i}=() }  } foreach $l (sort keys %h) {print "$c $l\n"}' $z $j; done > 00uns; cat 01xmldeps 00uns | tsort | tac > 00srt; cd /dev/shm/probs; done


# time sed -e 's/([^)]*)//g' probs3.train_0 >probs3.train_0.noweights
# then run like: ./orderforlearning.pl > 00res1
# the main result is probs2.train_0.noweights.out
# further checking:
#
# grep -v warning 00res1 >00res2
# sort 00res2 >00res2.sorted 
# diff 00res2.sorted probs3.refnr |less

# running snow incrementally (takes a lot of time with term features, so better parallelize):
#
# head -n1 probs2.train_0.noweights.out >  probs2.train_0.noweights0
# time ./snow -train -I probs2.train_0.noweights0 -F probs2.net_01  -B :0-112421
# time ./snow -test -i+ -I probs2.train_0.noweights.out -F probs2.net_01  -L 200 -o allboth  -B :0-112421> zzz1

# paralellized version:
# head -n1 probs2.train_0.noweights.out >probs2.train_0.noweights.out1
# head -n 31000 probs2.train_0.noweights.out >probs2.train_0.noweights0
# tail -n +31001 probs2.train_0.noweights.out >probs2.train_0.noweights80k
# time ./snow -train -I probs2.train_0.noweights.out1 -F probs2.net_0  -B :0-112421
# screen -r snow1
# time ./snow -test -i+ -I probs2.train_0.noweights0 -F probs2.net_0  -L 200 -o allboth  -B :0-112421> /dev/shm/uuu31k

# this splits the training/testing into 24 batches
# perl -e '$j=shift; $l=shift; $pref=shift; $basis=shift; open(G,$basis) or die; @basis=<G>; @lines=<>; $avrg=int($l/$j); $min=int($avrg/2); $step=2*$min/($j-1); foreach $s (0..$j-1) {$lngth[$s]=int($avrg+$min-$s*$step); if($s==$j-1) {$lngth[$s]=$#lines-$done} open(F,">$pref.test_$s"); print F @lines[$done..$done+$lngth[$s]]; close(F); open(F,">$pref.train_$s"); print F @basis; print F @lines[0..$done-1]; close(F); $z=$lngth[$s]; $done+=$lngth[$s]+1;}'  24 `wc -l probs2.train_0.noweights80k | cut -f1 -d\ ` probs2_batch probs2.train_0.noweights0 probs2.train_0.noweights80k

# now prepare for each comp:
# mkdir cn45 cn44 mizar
# perl -e '@a=@ARGV; foreach $d ("cn44","cn45","mizar") { foreach $s (@a[8*$z..8*($z+1)-1]) {`mv probs2_batch.train_$s $d`; `mv probs2_batch.test_$s $d`;} $z++; }' `seq 0 23 |sort -R`

# run inside screen on cn44, cn45, mizar:
# ls  probs2_batch.train_* | sed -e 's/probs2_batch.train_//' | ./parallel -j8 "time ./snow -train -I probs2_batch.train_{} -F probs2_batch.net_{}  -B :0-112421; time ./snow -test -i+ -I probs2_batch.test_{} -F probs2_batch.net_{}  -L 200 -o allboth  -B :0-112421 > /dev/shm/probs2_batch.res_{}"


# creating problems from predictions (typically use 40 and 200)

=begin code1

time perl -e '
$lim=shift;
open(F,"probs3.refnr"); while(<F>) {chop; $a[$i]=$_; $h{$_}=$i++;} 
open(G,"probs3.allasax"); while(<G>) { m/^fof.([^,]+),axiom/ or die $_; $p[$j]=$_; $a[$j]=$1 or die "$a[$j]:$_"; $j++;}
open(H,"probs3.allconjs"); while(<H>) { m/^fof.([^,]+),conjecture/ or die $_; exists $h{$1} or die "$1:$_"; $c[$h{$1}] = $_; }
$th = 0;
$axs=0;
open(P,"00dummy");
while(<>) {if(/^Example.*: *([0-9]+) */) { $axs=0; close(P); if(exists $c[$1]) { open(P,">advised/$a[$1]") or die; print P $c[$1]; $th=1;} else {$th=0}}
elsif (/^([0-9]+):/) { exists $a[$1] or die; if(($th==1) && ($axs++ < $lim)) { print P $p[$1]; }}}' 40 zzz

=end code1

=cut

# proof minimization:
# for i in `cat 00`; do grep '^ *file(' $i.vout | perl -e '$i=shift; while (<>) {m/.*,([a-z0-9_]*)/ or die; push(@a,$1);} $b= "\\b\\(" . join("\\|",@a) . "\\)\\b"; `grep \"$b\" minim/$i > $i.min`' $i; done

# for i in `ls t*.vout | sed -e 's/\.vout//'`; do grep ' file(' $i.vout | perl -e '$c=shift; while(<>) { m/ +file\([^,]*,([^)]*)\)/ or die; print "$1\n" unless($1 eq $c)}' $i > $i.needed_vampire_orig; done

# for i in `ls t*.needed_vampire_orig | sed -e 's/\.needed_vampire_orig//'`; do perl -e 'while(<>) { if(m/^dt_/) {} elsif(m/^(t|l|d|ie|[rcf]c)\d+/) {print $_; } elsif(m/(reflexivity|commutativity|symmetry|connectedness|irreflexivity|projectivity|idempotence|antisymmetry|involutiveness)_/) {print $_; } elsif(m/^(s\d+_.*)__.*__.*/) {print "$1\n"; } }' $i.needed_vampire_orig > $i.needed_vampire_filtered; done

use strict;

my %ma = ();
my %mh = ();
my @mml = ();

open(M,"mml.lar") or die;
while(<M>) {chop; push(@mml,$_);}

foreach my $a (@mml) 
{
    $ma{$a} = [];
    $mh{$a} = {};
    if(open(A,"$a/00srt"))
    {
	my $tmpa = $ma{$a};
	my $tmph = $mh{$a};
	while(<A>)
	{
	    chomp;
	    push(@$tmpa, $_);
	    $tmph->{$_} = scalar(@$tmpa)-1;
	}
    }
    else { print "warning1: $a/00srt not found\n"; }
}

my @refs = ();
my %refsh = ();

open(P,"probs2.refnr") or die;
while(<P>) {chomp; push(@refs, $_); $refsh{$_}=$#refs; }

# classify by article
# find refs that are not in 00srt and add them
foreach my $ref (@refs)
{
    my $a;
    if($ref =~ m/^rq/)
    {
	$ref =~ m/.*__[a-z]\d+_(.*)__.*/ or die $_;
	exists $mh{$1} or die $$_;
	$a = $1;
    }
    else
    {
	my @tmp1 = split(/_/, $ref);
	my $end = $#tmp1;
	my $chunk = $tmp1[$end];
	while(! (exists $mh{$chunk})) { if($end > 0) { $end--;} else {die $ref;} $chunk = $tmp1[$end] . '_' . $chunk; }
	$a = $chunk;
	my $bigger = $tmp1[$end-1] . '_' . $chunk; # check for one bigger - should be enough
	$a = $bigger if(exists $mh{$bigger});
    }

    if(! exists $mh{$a}->{$ref})
    {
	my $tmpa = $ma{$a};
	my $tmph = $mh{$a};
	print "warning2: adding $ref\n";
	push(@$tmpa, $ref);
	$tmph->{$ref} = scalar(@$tmpa)-1;
    }
}

my @train = ();
open(T,"probs2.train_0.noweights") or die;
while(<T>) {push(@train, $_);}
close(T);
open(T1,">probs2.train_0.noweights.out") or die;

foreach my $a (@mml)
{
    my $tmpa = $ma{$a};
    my $tmph = $mh{$a};
    foreach my $ref (@$tmpa)
    {
	if(exists $refsh{$ref})
	{
	    print ($ref, "\n");
	    print T1 $train[$refsh{$ref}];
	}
	else { print "warning3: $ref not in file refs: $a\n"; }

    }
}
close(T1);
