#!/usr/bin/perl
# prune problem specs using an initial dependency table starting with conjecture
# run like: 
# cat /local/data/alama/mizar-items/data/4.150.1103/requirements/*|grep ':theorem'>/home/mptp/ph/00rq1
# time ./prunereqs.pl /home/mptp/ph/00rq1 `ls`
#
# real: rqLess, arithm: rqReal, boole: t\d+_boole$, subset: t\d+_subset
use strict;

my %h = ();

my $deps = shift;

open(F,$deps) or die $deps;
while(<F>)
{
    chop;
    my @a = split(/[ ,]+/);
    $a[0] =~ m/^([a-z0-9_]+):theorem:(\d+)$/ or die "$_";
    $a[0] = 't' . $2 . '_' . $1;
    $h{$a[0]} = ();
    foreach my $i (1 .. $#a) { $h{$a[0]}->{$a[$i]} = (); }
}

close(F);

my $f;
my $pruned = 0;

while($f=shift)
{
    $f=~m/.*__(.*)/ or die $f; 
    my $t = $1; 
# don't test for existence - non-existent don't need any
#    if(exists $h{$t}) 
#    {
	open(G,$f); 
	open(G1,">$f.out"); 
	while(<G>) 
	{
	    if(m/^fof.t[0-9]+_(boole|subset|numerals),/)
	    {
		if(exists $h{$t}->{uc($1)}) { print G1 $_;} else { $pruned++; } 
	    }
	    elsif(m/^fof.rqReal/)
	    {
		if(exists $h{$t}->{'ARITHM'}) { print G1 $_;} else { $pruned++; }
	    }
	    elsif(m/^fof.rqLess/)
	    {
		if(exists $h{$t}->{'REAL'}) { print G1 $_; } else { $pruned++; }
	    }
	    else
	    {
		print G1 $_;
	    }
	}
#    }
}

print "$pruned\n";
