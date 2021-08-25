#!/usr/bin/perl -w

# Format Mizar errors for Prolog

while (<>)
{
  if(m/^(\d+) +(\d+) +(\d+) *$/) { print "miz_errors($1,$2,$3).\n"; }
}
