#!/usr/bin/perl

# In a directory with E protokolls,
# load protokolls' solved problems into hashes with the cputime information,
# and then write to stdout the problem of selecting the optimal set
# of strategies which solve maximal number of problems in 60s.
# Note that protokolls are transliterated to be acceptable by minisat+, while
# problem names are not, which can potentially cause trouble.

# SYNOPSIS:
# cd protokoll_directory
# ./max_solved_in_time.pl > InputFile
# minisat+_64-bit_static InputFile > OutputFile

my %prots = ();
my %times = ();
my @prot_arr =  `ls protoko*`;

foreach $prot ( @prot_arr)
{ 
  chop($prot); 
  open(PROT1,$prot);
  $prot=~s/protokoll_//; 
  $prot=~tr/\-/_/;
  my @time_arr = ();
  my %probh = ();
  my %timeh = ();
  my ($prob,$time);

  while(<PROT1>)
  {
    if(m/^([^ ]+) +[T] +([0-9]+)[.0-9]+ +.*/)
    { 
	($prob,$time) = ($1,$2);
	$time++;
	$probh{$prob} = $time;
	$timeh{$time} = ();
    }
  }
  close(PROT1);
  $prots{$prot} = \%probh;
  $times{$prot} = \%timeh;
}

# maximize number of problems solved
my %allprobs = ();
foreach $prot (keys %prots)
{
    my $prh1 = $prots{$prot};
    @allprobs{(keys %$prh1)} = ();
}

print "min: ";
foreach $prb (keys %allprobs) { print " -1*$prb"; }
print ";\n";

# sum of times smaller than 60
foreach $prot (keys %prots)
{
    my $prh1 = $prots{$prot};
    my $th1 = $times{$prot};
    foreach $time (sort {$a <=> $b} keys %$th1) { print " +$time*$prot","__",$time; }
}
print " <= 60;\n";

# problem negations
foreach $prb (keys %allprobs) { print " +1*$prb +1*NN$prb = 1;\n"; }

# protokol negations
foreach $prot (keys %prots)
{
    my $prh1 = $prots{$prot};
    my $th1 = $times{$prot};
    foreach $time (sort {$a <=> $b} keys %$th1) { print " +1*$prot","__",$time," +1*NN$prot","__",$time," = 1;\n" ; }
}

# not needed, but could help the solver: only one timelimit allowed for each protokol
foreach $prot (keys %prots)
{
    my $prh1 = $prots{$prot};
    my $th1 = $times{$prot};
    foreach $time (sort {$a <=> $b} keys %$th1) { print " +1*$prot","__",$time; }
    print " < 2;\n";
}


# problem is true only if some protokol solved it
foreach $prb (keys %allprobs) 
{ 
    print " +1*NN$prb";
    foreach $prot (keys %prots)
    {    
	my $prh1 = $prots{$prot};
	my %prhh = %$prh1;
	my $th1 = $times{$prot};
	if (exists $prhh{$prb})
	{
	    my $t1 = $prhh{$prb};
	    foreach $time (sort {$a <=> $b} keys %$th1) 
	    {
		if($t1 <= $time) { print " +1*$prot","__",$time; }
	    }
	}
    }
    print " > 0;\n";
}


# protokol is true implies that all its problems are true
foreach $prot (keys %prots)
{
    my $prh1 = $prots{$prot};
    my %prhh = %$prh1;
    my $th1 = $times{$prot};
    foreach $time (sort {$a <=> $b} keys %$th1) 
    { 
	my $c = 0;
	foreach $prb (sort keys %$prh1) 
	{ 
	    if ($prhh{$prb} <= $time)
	    {
		print " +1*$prb";
		$c++;
	    }
	}
	print " +$c*NN$prot","__",$time," > ",$c-1,";\n";
    }
}



#    print $prot;
#     foreach $val (sort values %$prh1) { print $val," "; } 
#     print "\n";




# `  cat $prot | grep success | cut -f1 -d \\  > aaa$prot`; 
# `cat aaaprotoko* | sort -u > 00SOLVED`;
# `ls aaaprotoko* > 00PROTS`;
# open(PROT,"00PROTS");
# $i=0; 
# @prot = ();
# %times=();
# while(<PROT>) { chop; push(@prot,$_); $hprot{$_}=1+$#prot; }
# close(PROT);
# print "min: ";
# foreach $prot (@prot) { print " +1*v",$hprot{$prot}; }
# print ";\n";
# open(PROB,"00SOLVED");
# while(<PROB>) 
# {
#     chop;
#     @matched=`grep -l $_ aaaprotoko*`;
#     foreach $prot (@matched) { chop($prot); print " +1*v",$hprot{$prot}; }
#     print ">= 1;\n";
# } 



