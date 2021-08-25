# Program to convert a KIF file of WordNet noun to SUMO mappings into prolog
# clauses that can be included in the CELT lexicon.
# The program discards multiple meanings for the same word.
# The program converts compound terms into prolog lists.
# The program assumes that the nouns are singular, countable objects.
# Author: Adam Pease apease@ks.teknowledge.com
# Usage:
#  perl WN2CELT-nouns.pl mappings.kif > output.pl

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
          if ($lexicalItem =~ /\_/) {                     # convert compound term to a prolog list
             $lexicalItem = "[" . $lexicalItem . "]";
             $lexicalItem =~ s/\_/\,/g;
          }
          $lexicalItem = lc($lexicalItem);
          print "noun_in_lexicon(" . $lexicalItem . ",object,_,count,singular,'";
          print $term . "').\n";
       }
}


