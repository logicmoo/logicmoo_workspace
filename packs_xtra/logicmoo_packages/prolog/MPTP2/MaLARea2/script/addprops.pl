#!/usr/bin/perl
# add propoerties to specs from initial dependency table
# run like: 
# cat /local/data/alama/mizar-items/data/4.150.1103/properties/one-step/* | grep ':theorem'>/home/mptp/ph/00prop1
# or cat /local/data/alama/mizar-items/data/4.150.1103/properties/one-step/* >/home/mptp/ph/00propall1
# time ./addprops.pl /home/mptp/ph/00props1 item_mptp_deps
#
# Reflexivity Commutativity Symmetry Connectedness Irreflexivity Abstractness Projectivity Idempotence Antisymmetry Involutiveness
use strict;

my %h = ();

my $props = shift;
my $deps = shift;

open(F,$props) or die $props;
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
    $h{$a[0]}->{$mn} = () unless (($mn eq 'reflexivity_r1_hidden') or ($mn eq 'symmetry_r1_hidden'));
}

close(F);

open(F,$deps) or die $deps;
while(<F>)
{
    m/^(t[^ ]*) / or die $_; 
    chop;
    my $t = $1; 
    if(exists $h{$t}) 
    { 
	print $_ . ' ' . join(' ', sort keys %{$h{$t}}) . "\n";
    }
    else { print ($_, "\n"); }
}
