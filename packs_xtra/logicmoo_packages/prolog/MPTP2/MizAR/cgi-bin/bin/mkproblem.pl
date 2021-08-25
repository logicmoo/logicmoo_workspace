#!/usr/bin/perl -w

# create ATP problem

use strict;

my $aname = shift @ARGV;
my $filestem = shift @ARGV;
my $MMLAxs = shift @ARGV;

$_=<>;
chomp;
my @refs=split(/ +/);

my @lrefs=();
my @grefs=();

foreach my $ref (@refs)
  {
    if($ref=~m/_$aname$/)
    {
      push(@lrefs,$ref)
    }
    else { push(@grefs,$ref) }
  }

# print  "include('$MMLAxs',[", join(',',@grefs), "]).\n";
# my $regexp = '"^fof( *\\(' . join('\\|',@lrefs) . '\\) *,"';
# # print $regexp;
# exec("grep $regexp $filestem.big0");

my $regexp = '"^fof( *\\(' . join('\\|',@refs) . '\\) *,"';
exec("grep -h $regexp $MMLAxs $filestem.big0| sort -u");
exit 0;
