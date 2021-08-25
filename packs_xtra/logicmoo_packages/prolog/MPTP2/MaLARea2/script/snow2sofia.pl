#!/usr/bin/perl
# produce sofia input from snow input
# run like:
#
# perl -F snow2sofia.pl probs6.train_01 > probs6.softrain
#
# or to have a testfile with 10%:
#
# ./snow2sofia.pl probs6.train_01 tst1 > probs6.softrain

use strict;

my %h=();    # hash keeping contiguous numbering for pars of features
my $z=10000; # max bound for premise numbering - we start the pair numbering here

my @prev = (); # premises already seen
my $neg = 30; # number of negative examples added

my $tst = 0;
if(defined $ARGV[1]) { $tst = 1; open(T,">$ARGV[1]") or die; }

open(F,$ARGV[0]);

while (<F>)
{
    my @l= m/(\d+)/g;
    my $q= $l[0];
    my @t= grep {$_ < 2000000} @l;
    my @f= grep {$_ >= 2000000} @l;
    my @p= ();

    if($#prev > 5*$neg) # if enough previous, put random negative example
    {
	for (my $i = $neg; --$i; ) { push(@p, $prev[int rand ($#prev)]) }
    }

    my $r= 1;  # bigger weight for the name of the formula than for the premises
    my $cnt = 0;
    if(($tst == 1) && ($#t > 0) && (int rand 9 == 1)) { select T } else { select STDOUT }
    foreach my $j (@t,@p)
    {
	print "$r qid:$q ";
	$cnt++;
	if($cnt > $#t) { $r= -1;}
	my @f1=();
	foreach my $i (@f)
	{
	    if (!exists($h{"$j:$i"})) { $h{"$j:$i"}=$z++; }
	    push(@f1,$h{"$j:$i"})
	}
	foreach my $k (sort {$a<=>$b} @f1) { print " $k:1"; }
	print "\n";
    }
    push(@prev, $q);
}
close(T) if($tst == 1);
