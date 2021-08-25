#!/usr/bin/perl
# produce mor input from snow input
# run like:
#
# perl -F snow2mor.pl probs6.train_01 > probs6.mortrain
#
# or with a testfile (which will silently create file tst1.mor below):
#
# ./snow2mor.pl probs6.train_01 tst1 > probs6.softrain
#
#
# It puts all examples for one conjecture on a single line
# (compressing model and feature info consistently for the train and
# test files)
# 

use strict;

my $gsymoffset    = 2000000; # offset at which symbol numbering starts
my %h=();    # hash keeping contiguous numbering for features
my $goutsymoffset=1; # offset for mor feature numbering - none as default
my $z = $goutsymoffset;

my $tst = 0;
if(defined $ARGV[1]) { $tst = 1; open(T0,"$ARGV[1]") or die; open(T,">$ARGV[1].mor") or die; }

open(F,$ARGV[0]) or die;

my %gfeatures = ();  # collect feauters for each formula
my %glabels   = ();  # collect labels for each formula


while (<F>)
{
    my @l= m/(\d+)/g;
    my $q= $l[0];
    $q < $gsymoffset or die $q;
    my @t= grep {$_ < $gsymoffset} @l;

    if(! exists($glabels{$q})) { $glabels{$q} = {}; }

    my $labels = $glabels{$q};
    @$labels{@t} = ();

    my @f= grep {$_ >= $gsymoffset} @l;
    my @f1= ();

    if(! exists($gfeatures{$q})) { $gfeatures{$q} = {}; }
    my $features = $gfeatures{$q};

    foreach my $j (@f)
    {

	if (!exists($h{$j})) { $h{$j}=$z++; }
	push(@f1,$h{$j})
    }

    @$features{@f1} = ();

}

foreach my $i ( sort {$a <=> $b} keys %glabels)
{
    my $labels = $glabels{$i};
    my $features = $gfeatures{$i};
    print $i, ';', join(',', map { $_ . ':1' } (sort {$a <=> $b} keys %$features)), ';'; 
    print join(',', map { $_ . ':1' } (sort {$a <=> $b} keys %$labels)), "\n"; 
}
 

if($tst == 1)
{
    while (<T0>)
    {
	my @l= m/(\d+)/g;
	
	my @f= grep {$_ >= $gsymoffset} @l;
	my @f1 = ();

	foreach my $j (@f)
	{

	    if (!exists($h{$j})) { $h{$j}=$z++; }
	    push(@f1,$h{$j});
	}

	print T ('0;', join(',', map { $_ . ':1' } (sort {$a <=> $b} @f1)), ";\n"); 
    }
    close(T);
}

