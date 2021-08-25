#!/usr/bin/perl


# create Excell input data from Geoff's TSTP results

%StatusName	= (
    'Unsatisfiable'	, 'THM', 
    'Timeout'           , 'TMO',
    'GaveUp'		, 'GUP',
    'Unknown'           , 'UNK'
);

my %ProblemNumber = ();
my @Problem = ();
my $counter = 0;
my $scounter = 0;
my %SystemNumber = ();


while(<>)
{
    die "Unexpected input:\n $_" unless(
	m/(\S+)\s+-\s+(\S+)\s+says\s+(\S+)\s+-\s+CPU\s+=\s+([0-9.]+)\s+$/ );

    ($name, $system, $status, $time) = ($1, $2, $3, $4);

#DEBUG    print  $name, $system, $status, $time, "\n";

    die "Unexpected status: $status" unless( exists $StatusName{$status});

    if (exists $ProblemNumber{$name})
    {
	$pn = $ProblemNumber{$name}
    }
    else
    {
	$pn = $counter++;
	$ProblemNumber{$name} = $pn;
	$problemname[$pn] = $name;
    }

    if (exists $SystemNumber{$system})
    {
	$sn = $SystemNumber{$system}
    }
    else
    {
	$sn = $scounter++;
	$SystemNumber{$system} = $sn;
    }



    $problem[$pn][2*$sn] = $StatusName{$status};
    $problem[$pn][2*$sn + 1] = $time;
}

print join (", ", sort {$SystemNumber{$a} <=> $SystemNumber{$b}} keys %SystemNumber), "\n";

for($i = 0; $i <= $#problem; $i++)
{
    print "$problemname[$i],";
    for($j = 0; $j < $sn; $j++)
    {
	print $problem[$i][2*$j], ",";
	print $problem[$i][2*$j + 1], ",";
    }
    print $problem[$i][2*$j], ",";
    print $problem[$i][2*$j + 1], "\n";
}
