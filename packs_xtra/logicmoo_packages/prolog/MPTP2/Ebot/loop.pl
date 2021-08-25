#!/usr/bin/perl

# This is the bare-bones looper finding contradictions and removing
# the problematic formulas involved in them. Many things are hardwired.

open(F,"statements") or die;
%s=(); while(<F>) {m/^fof.([^,]+),/ or die; $s{$1}=$_ } close F;
open(F,"00good123") or die;
%h=(); while(<F>) {m/^fof.([^,]+),/ or die; $h{$1}=$_ } close F;

my $i=3;
while($i<30)
{
    chdir "/dev/shm/res3fix$i";
    open(I,"grep -l CNFRef * | xargs grep -h true|");
    while(<I>)
    {
	m/^fof.([^,]*),.*/; $h{$1}=$s{$1};
    }
    close(I);
    my $j=$i+1;
    chdir "/home/mptp/big/ec/eprover-git/PROVER/miz3";
    open(F,">00gooda$j") or die; print F join("\n",sort keys %h),"\n"; close F;
    `mkdir /home/mptp/big/ec/eprover-git/PROVER/miz3fix$j`;
    for my $k (glob("*.p"))
    {
	open(G,$k) or die;
	open(H,">../miz3fix$j/$k") or die;
	while(<G>) {if((m/^fof.([^,]+),/) && exists($h{$1})) {print H $h{$1}} else {print H $_}}
	close(G); close(H);
    }
    `cd /home/mptp/big/ec/eprover-git/PROVER/miz3fix$j/; mkdir /dev/shm/res3fix$j;  ls *.p | time parallel -j50 "../eprover -p -R -s --delete-bad-limit=1024000000 --free-numbers --satauto --tstp-format --cpu-limit=30 {} > /dev/shm/res3fix$j/{}.out"`;
    $i=$j;
}

