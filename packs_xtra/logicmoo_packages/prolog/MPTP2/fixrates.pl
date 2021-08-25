#!/usr/bin/perl -w

# this scripts finds interestingness ratings in one-line-fof tptp output
# of AGInt rater (first argument), and adds them as the "interesting" 
# attribute to the XML file (second argument)
# this is then used by the mhtml xslt processing
$rating = shift;
open(RATED,$rating);
my %h=();
while(<RATED>) 
{ 
  if((m/^fof\(t([0-9]+)_matrix11,.*interesting\(([0-9.]+)\).*/) && ($2 > 0))
    {
      $h{$1} = $2;
    }
}
close(RATED);
while(<>) 
{
  if ((m/(^.*JustifiedTheorem.*\bnr=\"([0-9]+)\".*)>/) && (exists $h{$2}))
    {
      print "$1 interesting=\"$h{$2}\">";
    }
  else { print $_ }
}
