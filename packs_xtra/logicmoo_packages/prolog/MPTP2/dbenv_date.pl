#!/usr/bin/perl -w

# Environment massaging script for MPTP
# version which also extract date and author

# SYNOPSIS: 
# for i in tarski `cat mml.lar`; do echo $i;
# perl -F dbenv.pl tmp/$i > pl/$i.evl2;
# done


use Date::Manip;

$dirnbr = 8; # number of directives in .evl
@dirs   = ([],[],[],[],[],[],[],[]);
@dirname = ('vocabularies','notations',
      'definitions','theorems','schemes','registrations',
      'constructors','requirements');
$codirnr  = 6; # order of the constr. directive 
$f=$ARGV[0]; 
@path = split(/\//,$f);
$f1 = $path[$#path];

# get the date and author from .miz first:
$miz = $f.".miz";
$date1 = `head -n 10 $miz|grep Received`;
($date1 =~ m/.*Received *(.*)/) or die "Date not found: $f: $date1";
$date2 = $1;
$date_sec = &UnixDate($date2,"%o");
# now $date_sec contains seconds since Jan 1, 1970  in current timezone
$date_days = $date_sec/(3600*24);
# now $date_days contains days since Jan 1, 1970  in current timezone
$author1 = `head -n 10 $miz|grep ":: *by" `;
($author1 =~ m/:: +by +(.*)/) or die "Author not found: $f: $author1";
$author2 = $1;

$sg = $f.".sgl";
open(SGL, $sg) or die "File not found $sg";
$c=<SGL>; 
while($c-- > 0) 
{
    $a=<SGL>; 
    chop($a);
    push @co, lc($a);
#    print "$a\n";
}
# print @co;

$ev = $f.".evl";
open(EVL, $ev) or die "File not found $ev";
for($i=0; $i<$dirnbr; $i++)
{
    $c=<EVL>; 
    while($c-- > 0) 
    {
	$a=<EVL>; 
	chop($a);
#	$dirs[$i][1+$#{$dirs[$i]}] = lc($a);
	push @{$dirs[$i]}, lc($a);
#	print "$a\n";
	$a=<EVL>;
    }
}

@{$dirs[$codirnr]} = @co;

print "theory($f1,[";
for($i=0; $i<$dirnbr; $i++)
{
    if($i>0) 
    {
	print ",";
    }
    print $dirname[$i];
    print "([";
    for($j=0; $j<=$#{$dirs[$i]}; $j++)
    {
	if($j>0) 
	{
	    print ",";
	}
	print $dirs[$i][$j];
    }
    print "])";
}
# add the date:
print ",date([$date_days,\"$date2\"]),authors([\"$author2\"])";
print "]).\n";
