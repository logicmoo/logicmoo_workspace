#!/usr/bin/perl
#
# Produce the svm format from ours
#
#
# ./svm.pl data > svmdata
#

@s=(0);
while(<>)
{

    m/(^\d+);(.*);(.*)/ or die;
    @f0=split(/,/,$2);
    @f1=();
    foreach $ff (@f0) 
    { 
	if(!exists $h{$ff}) { push(@s,$ff); $h{$ff}=$#s; }
	push(@f1,$h{$ff});
    }
    @f = sort {$a <=> $b} @f1;
    @r=split(/,/,$3);
    push(@r,$1);
    $f1=join(":1 ",@f);
    foreach $r (@r) {print $r," ",$f1,":1\n" }
}

# select conjectures
# perl -e 'open(F,"00chordth1") or die; @t=<F>; @ht{@t}=(); open(G,"probs6.refnr") or die; @r=<G>; foreach $rr (@r) {$hr{$rr}=$i; $i++;  } while(<>) {m/^(\d+) .*/ or die; if(exists $ht{$r[$1]}) { print $_  }   }'  svmdata.tst2220 > svmdata.tst2078

# predict
# time ./svm_multiclass_classify exmptp/svmdata.tst2078 exmptp/model1all exmptp/predt2078 | tee 00allpred2078log 

# create predictions
# time perl -e 'while(<>) { chop; m/^\d+ (.*)/; @r=split(/ +/,$1); %h=(); foreach $i (0..$#r) {$h{$i+1}=$r[$i];  } @s=sort {$h{$b} <=> $h{$a}} (keys %h); print join(",",@s[0..500]), "\n"  }' predt2220 >predt2220.srt2
