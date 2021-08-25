
#!/usr/bin/perl
##############################################################################

# Program to convert a marked up WordNet file (like VERBS.DAT) to a set of prolog statements.
# The input file must have markups corresponding to SUMO terms using a simple syntax
# created by Ian Niles.  The source file for the marked up .dat file for WordNet nouns can be found at 
# http://reliant.teknowledge.com/cgi-bin/cvsweb.cgi/SUO/WordNetMappings-Top.txt?rev=1.13&content-type=text/x-cvsweb-markup
# Set the $partOfSpeech variable for the part of speech of the terms being processed from WordNet
#
# Author:                     Adam Pease     apease@ks.teknowledge.com
# Comments and Modifications: William Murray wmurray@teknowledge.com
# 1. Added gloss and synset ID info to output of SUMO clauses.
# 2. Fixed handling of WordNet words like winner's_circle
# 3. Fixed handling of fields like 0a, 0b, etc. and other hex values, rather than numbers, in 
#    entries like ' 00044168 04 n 0a blunder 0 blooper 0 bungle 0 foul-up 0 fuckup 
#    0 flub 0 botch 0 boner 0 boo-boo 0 misdoing 0 009 @ 00042411 n 0000 ~ 00044471 n 
#    0000 ~ 00044604 n 0000 ~ 00044878 n 0000 ~ 00045000 n 0000 ~ 00045103 n 0000 ~ 00045209 n 
#    0000 ~ 00045336 n 0000 ~ 00045479 n 0000 | an embarrassing mistake &%SubjectiveAssessmentProperty+ '
# Examples of Usage: 
#   perl WN2assoc3.perl WordNetMappings-Top.txt noun > mapped_nouns.pl
#
#   and...
#
#   perl WN2assoc3.perl WordNetMappings-verbs.txt verb > mapped_verbs.pl
#
# Make sure that the part of speech passed on the command line is lower case!

&convert;

sub convert {

   $partOfSpeech = $ARGV[1]; #second argument to command is the part of speech (e.g., verb)

   open(FILE,$ARGV[0]);      #first argument to command is the file name (e.g., WordNetMappings-verbs.txt)
   @LINES = <FILE>;          #now create an array LINES where each array element is one line in the file
   close(FILE);

   print "\n";
   for ($linenum=0; $linenum<$#LINES; $linenum=$linenum+1) {   # for each line...
      $LINES[$linenum] =~ /^\d+ /;                             # find the synset ID as a string of digits that start at beginning of line
      $SynID = $&;                                             # bind SynID to the string matched in the last pattern match 
      $LINES[$linenum] =~ /^\d+ \d+ \w [0-9a-f]+ (\S+)/;       # now match to number, number, letter, number, word chars (non-whitespace)
      $firstword = $1;                                         # save the first word just matched as $firstword (e.g., 'entity')
      $firstword =~ s/_/ /g;                                   # replace underscore by space in words like Baltimore_oriole
      $firstword =~ s/\'/\\\'/g;                               # precede single quotes with backslash in words like winner's_circle
      while ($LINES[$linenum] =~ /\&\%(\w+[\+\=\@])[^\&]*$/) { # find SUMO concept, identified by characters &%. Concept follows,
                                                               # then one of the characters '+', '=', or '@', then swallow up any
                                                               # characters but & up to the end of line.
         $LINES[$linenum] =~ /\| (.*) \&\%/;                   # now find the gloss, the definition after the vertical bar | but before
                                                               # the &% characters which precede the SUMO concept.
         $gloss = $1;                                          # Save the gloss as $gloss.
         $gloss =~ s/\"/\'/g;                                  # Replace double-quotes by single-quotes throughout the gloss ($gloss).

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
         $numPrefix = "";
         if ($partOfSpeech =~ /noun/) { $numPrefix = "1"; }
         if ($partOfSpeech =~ /verb/) { $numPrefix = "2"; }
         if ($partOfSpeech =~ /adjective/) { $numPrefix = "3"; }
         if ($partOfSpeech =~ /adverb/) { $numPrefix = "4"; }
         print "sumo('$term',$relation,'$firstword',$numPrefix$SynID,$partOfSpeech,\n     \"$gloss\").\n";
      }
   }
print "\n";
}
