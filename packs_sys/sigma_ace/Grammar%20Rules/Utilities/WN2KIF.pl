
#!/usr/bin/perl
##############################################################################

# Program to convert a marked up WordNet file (like VERBS.DAT) to a set of KIF statements
# the .dat file must have markups corresponding to SUMO terms using a simple syntax
# created by Ian Niles.  The source file for the marked up .dat file for WordNet nouns can be found at 
# http://reliant.teknowledge.com/cgi-bin/cvsweb.cgi/SUO/WordNetMappings-Top.txt?rev=1.13&content-type=text/x-cvsweb-markup
# Set the $partOfSpeech variable for the part of speech of the terms being processed from WordNet
#
# Author: Adam Pease apease@ks.teknowledge.com
# Usage: 
#   perl WN2KIF.pl WordNetMappings-Top.txt > out.kif

&convert;

sub convert {

   $partOfSpeech = "Verb";

   open(FILE,$ARGV[0]);
   @LINES = <FILE>;
   close(FILE);

   print "\n";
   for ($linenum=0; $linenum<$#LINES; $linenum=$linenum+1) {
      $LINES[$linenum] =~ /^\d+ /;
      $SynID = $&;
      $LINES[$linenum] =~ /^\d+ \d+ \w \d+ (\w+)/;
      $firstword = $1;
      while ($LINES[$linenum] =~ /\&\%(\w+[\+\=\@])[^\&]*$/) {
         $LINES[$linenum] =~ s/\&\%(\w+[\+\=\@])[^\&]*$//;
         $SUMOterm = $1;
         if ($SUMOterm =~ /\+/) {
            $relation = 'subsumingExternalConcept';
         }
         else {
            if ($SUMOterm =~ /\=/) {
               $relation = 'synonymousExternalConcept';
            }
            else {
               if ($SUMOterm =~ /\@/) {
                  $relation = 'instance';
               }
            }
         }
         $SUMOterm =~ /(\w*)./;
         $term = $1;
         print "($relation $firstword-WN$SynID";
         print "$term WordNet-16)\n";
      }
      print "(instance $firstword-WN$SynID";
      print $partOfSpeech . ")\n";
   }
print "\n";
}
