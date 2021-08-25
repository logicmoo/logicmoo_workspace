#!/usr/bin/perl -w

# Environment massaging script for MPTP, for xml-based evl files

# SYNOPSIS: 
# for i in tarski `cat mml.lar`; do echo $i; xsltproc evl2pl.xsl $i.evl >$evl1;
# perl -F dbenv2.pl $i > $i.evl2;
# done

$f=$ARGV[0]; 
# @path = split(/\//,$f);
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
my $sgl_co = join(',',@co);

# print @co;

my $ev = $f.".evl1";
open(EVL, $ev) or die "File not found $ev";
my $evl1 = <EVL>;
close(EVL);
$evl1 =~ s/constructors\(\[([^\]]*)\]\)/constructors([$sgl_co])/;
print $evl1;
