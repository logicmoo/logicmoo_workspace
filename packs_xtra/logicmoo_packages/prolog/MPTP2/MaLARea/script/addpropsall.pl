#!/usr/bin/perl
# add propoerties to specs from initial dependency table
# run like: 
# cat /local/data/alama/mizar-items/data/4.150.1103/properties/one-step/* | grep ':theorem'>/home/mptp/ph/00prop1
# or cat /local/data/alama/mizar-items/data/4.150.1103/properties/one-step/* >/home/mptp/ph/00propall1
# time ./addpropsall.pl /home/mptp/ph/00propsall1 item_mptp_deps
#
# Reflexivity Commutativity Symmetry Connectedness Irreflexivity Abstractness Projectivity Idempotence Antisymmetry Involutiveness
use strict;

my %h = ();

# memorized trnslation table for speed
my %j2mt = ();

sub j2m
{
    my ($d) = @_;
    return $j2mt{$d} if(exists $j2mt{$d});

    $d =~ m/^([a-z0-9_]+):([a-z0-9]+):(\d+)$/ or die "$d";
    my ($article, $k, $nr) = ($1, $2, $3);
    my $k1;

    if($k =~ m/([rcf])cluster/) { $k1 = $1 . 'c';  }
    elsif($k =~ m/([gklmruv])constructor/) { $k1 = 'dt_' . $1; }
    elsif($k =~ m/([gklmruv])identification/) { $k1 = 'ie' ; } #just 'k'
    elsif($k =~ m/([gklmruvj])pattern/) { $k1 = 'pattern'; }
    elsif($k =~ m/deftheorem|definiens/) { $k1 = 'd'; }
    elsif($k =~ m/theorem/) { $k1 = 't' ; }
    elsif($k =~ m/scheme/) { $k1 = 's' ; }
    elsif($k =~ m/lemma/) { $k1 = 'lemma'; }
    else { die "$d:::  $k"; }
    my $mn = $k1 . $nr . '_' . $article;
    $j2mt{$d} = [$k1, $mn];
    return $j2mt{$d};
}


my $props = shift;
my $deps = shift;

open(F,$props) or die $props;
while(<F>)
{
    chop;
    my @a = split(/[ ]+/);
    # $a[0] =~ m/^([a-z0-9_]+):theorem:(\d+)$/ or die "$_";
    my ($k, $frst) = @{j2m($a[0])};
    if($k =~ m/^(t|s|d|ie|[rcf]c)$/)
    {
	$a[0] = $frst;
	# $a[0] = 't' . $2 . '_' . $1;
    # $h{$a[0]} = (); this was stupid - only leavs last one
	$a[1] =~ m/^(Reflexivity|Commutativity|Symmetry|Connectedness|Irreflexivity|Abstractness|Projectivity|Idempotence|Antisymmetry|Involutiveness)$/ or die "$a[1]:$_";
	$a[2] =~ m/^([a-z0-9_]+):([gklmruv])constructor:(\d+)$/ or die "$_";
	
    
	my $mn = lc($a[1]) . '_' . $2 . $3 . '_' . $1;
	$h{$a[0]}->{$mn} = () unless (($mn eq 'reflexivity_r1_hidden') or ($mn eq 'symmetry_r1_hidden'));
    }
}

close(F);

open(F,$deps) or die $deps;
while(<F>)
{
    m/^((t|s|d|ie|[rcf]c)[^ ]*) / or die $_; 
    # m/^(t[^ ]*) / or die $_; 
    chop;
    my $t = $1; 
    if(exists $h{$t}) 
    { 
	print $_ . ' ' . join(' ', sort keys %{$h{$t}}) . "\n";
    }
    else { print ($_, "\n"); }
}
