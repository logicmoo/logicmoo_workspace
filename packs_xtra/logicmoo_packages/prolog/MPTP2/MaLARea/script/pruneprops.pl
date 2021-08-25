#!/usr/bin/perl
# prune problem specs using an initial dependency table starting with conjecture
# run like: 
# cat /local/data/alama/mizar-items/data/4.150.1103/properties/one-step/* | grep ':theorem'>/home/mptp/ph/00prop1
# time ./pruneprops.pl /home/mptp/ph/00props1 `ls`
#
# Reflexivity Commutativity Symmetry Connectedness Irreflexivity Abstractness Projectivity Idempotence Antisymmetry Involutiveness
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
    # $h{$a[0]} = (); this was stupid - only leavs last one
    $a[1] =~ m/^(Reflexivity|Commutativity|Symmetry|Connectedness|Irreflexivity|Abstractness|Projectivity|Idempotence|Antisymmetry|Involutiveness)$/ or die "$a[1]:$_";
    $a[2] =~ m/^([a-z0-9_]+):([gklmruv])constructor:(\d+)$/ or die "$_";

    my $mn = lc($a[1]) . '_' . $2 . $3 . '_' . $1;
    $h{$a[0]}->{$mn} = ();
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
	    # we always include abstractness now - don't trust jesse :-)
	    if(m/^fof.((reflexivity|commutativity|symmetry|connectedness|irreflexivity|projectivity|idempotence|antisymmetry|involutiveness)_[^,]+),/)
	    {
		if(exists $h{$t}->{$1}) { print G1 $_;} else { $pruned++; }
	    }
	    else
	    {
		print G1 $_;
	    }
	}
#    }
}

print "$pruned\n";
