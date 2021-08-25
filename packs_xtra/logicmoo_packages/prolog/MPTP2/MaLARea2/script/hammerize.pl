#!/usr/bin/perl

# SYNOPSIS
# ./hammerize.pl HahnBanach.thy > HahnBanach.thy.new

# script inserting the "sledgehammer;" directive
# into "interesting places" of an Isabelle theory file

my $st = 0;
my $problemnr = 0;
my $subp = 0;
my $filename = $ARGV[0];
# print "ML{* ResAtp.destdir := \"/home/urban/isatp\"; *}\n";
while(<>)
  {
#     if(m/^(theorem|lemma|corrollary|instance)\b/)
#     { 
    # $st = 1; 
    # print $_;
#     }
    if(m/^(proof)\b/)
      {
	$problemnr++; $subp=0;
	print "ML{*ResAtp.problem_name := \"$filename\___$problemnr\"*}\n";
	print $_; print "sledgehammer;\n";
      }
    elsif(m/^[ \t]*(show|have)[ \t]+\"\b/)
      {
	$subp++;
	print "ML{*ResAtp.problem_name := \"$filename\___$problemnr\__$subp\"*}\n";
	print $_; print "sledgehammer;\n";
      }
    else { print $_; }
  }
