# Program to convert a KIF file of WordNet verb to SUMO mappings into prolog
# clauses that can be included in the CELT lexicon.
# The program discards multiple meanings for the same word.
# The program converts compound terms into prolog lists.
# The program assumes that the verbs are transitive, singular, simple events.
# Author: Adam Pease apease@ks.teknowledge.com
# Usage:
#  perl WN2CELT-verbs.pl mappings.kif > output.pl

open(FILE,$ARGV[0]);
@LINES = <FILE>;
close(FILE);
$lastLex = "xxxx";

for ($linenum=0; $linenum<$#LINES; $linenum++) {
       $line = $LINES[$linenum];
       $firstSpace = index($line, " ");
       $firstDash = index($line, "-WN");
       $secondSpace = index($line, " ", $firstSpace+1);
       $thirdSpace = index($line, " ", $secondSpace+1);
       $lexicalItem = substr($line,$firstSpace+1,$firstDash - $firstSpace-1);
       $term = substr($line,$secondSpace+1,$thirdSpace - $secondSpace-1);
       if ($lastLex ne $lexicalItem) {
          $lastLex = $lexicalItem;
          if ($lexicalItem =~ /\_/) {
             $lexicalItem = "[" . $lexicalItem . "]";
             $lexicalItem =~ s/\_/\,/g;
          }
          $lexicalItem = lc($lexicalItem);
          print "verb_in_lexicon(" . $lexicalItem . "," . $lexicalItem . ",";
          print "transitive,singular,simple,event,'";
          print $term . "').\n";
       }
}


