#!/usr/bin/perl -w

# Format Mizar errors in XML

print '<?xml version="1.0"?>',"\n",'<Errors>',"\n";

while (<>)
{
  if(m/^(\d+) +(\d+) +(\d+) *$/) { print "<Error line=\"$1\" col=\"$2\" nr=\"$3\"/>\n"; }
}

print '</Errors>',"\n";
