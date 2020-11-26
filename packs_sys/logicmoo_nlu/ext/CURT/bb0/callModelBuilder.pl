#! /usr/local/bin/perl

#         name: callModelBuilder.pl (Chapter 5)
#      version: Jan 17, 2001
#  description: Perl Interface to Model Builders (Mace)
#      authors: Patrick Blackburn & Johan Bos
#         mace: http://www-unix.mcs.anl.gov/AR/mace/

my $mace="mace";

my $engine = $ARGV[0];
my $inputfile = $ARGV[1];
my $outputfile = $ARGV[2];
my $domainsize = $ARGV[3];
my $result = 0;
my $model = "";
my $readmodel = 0;

if ($engine =~ /mace/)
{
 open(OUTPUT, "$mace -N $domainsize -P < $inputfile |");
 while (<OUTPUT>)
 {
  if ($_ =~ /end_of_model/)
     {
      $readmodel = 0;
     }
  if ($readmodel == 1)
     {
      $model = "$model$_";
      $model =~ s/\$(.*?)\,/$1\,/;
     }
  if ($_ =~ /======================= Model/)
     {
      $readmodel = 1;
     }
  if ($_ =~ /The set is satisfiable/)
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
    print OUTPUT "$model\n";
}
else
{
    print OUTPUT "unknown.\n";
}
close(OUTPUT);

exit $result;
