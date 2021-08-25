#!/usr/bin/perl
# prune problem specs using an initial dependency table starting with conjecture
# run like: 
# time  ~/gr/MPTP/SCRIPT/advisor/deps2mpadata.pl item-dependency-table > item_mptp_deps
# time ./pruneproblems.pl item_mptp_deps `ls`
use strict;

my %h = ();

my $deps = shift;

open(F,$deps) or die $deps;
while(<F>)
{
    chop;
    my @a = split(/ +/);
    $h{$a[0]} = ();
    foreach my $i (1 .. $#a) { $h{$a[0]}->{$a[$i]} = (); }
}

close(F);

my $f;

while($f=shift)
{
    $f=~m/.*__(.*)/ or die $f; 
    my $t = $1; 
    if(exists $h{$t}) 
    {
	open(G,$f); 
	open(G1,">$f.out"); 
	while(<G>) 
	{
	    if(m/^fof.((d|ie|[rcf]c)[0-9]+_[^,]+),/)
	    {
		print G1 $_ if(exists $h{$t}->{$1}); 
	    }
	    else
	    {
		print G1 $_;
	    }
	}
    }
}
