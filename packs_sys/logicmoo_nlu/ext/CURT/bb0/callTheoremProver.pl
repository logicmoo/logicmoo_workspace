#! /usr/local/bin/perl

#         name: callTheoremProver.pl (Chapter 5)
#      version: Jan 17, 2001
#  description: Perl Interface to various Theorem Provers
#      authors: Patrick Blackburn & Johan Bos
#        otter: http://www-unix.mcs.anl.gov/AR/otter/
#      bliksem: http://www.mpi-sb.mpg.de/~bliksem/

my $otter="./otter";
my $bliksem="./bliksem";

my $engine = $ARGV[0];
my $inputfile = $ARGV[1];
my $outputfile = $ARGV[2];
my $result = 0;

if ($engine =~ /otter/)
{
 open(OUTPUT, "$otter < $inputfile |");
 while (<OUTPUT>)
 {
  if ($_ =~ /proof of the theorem/)
     {
      $result = 1;
     }
 }
 close OUTPUT;			 
}
elsif ($engine =~ /bliksem/)
{
 open(OUTPUT, "$bliksem < $inputfile |");
 while (<OUTPUT>)
 {
  if ($_ =~ /found a proof/)
     {
      $result = 1;
     }
 }
 close OUTPUT; 
}
else
{
 print "\nERROR:Unknown Inference Engine: $engine";
}

open(OUTPUT,">$outputfile");
if ($result == 1)
{
    print OUTPUT "proof.\n";
}
else
{
    print OUTPUT "unknown.\n";
}
close(OUTPUT);

exit $result;
