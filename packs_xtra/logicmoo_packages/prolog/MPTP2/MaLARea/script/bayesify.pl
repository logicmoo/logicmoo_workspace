#!/usr/bin/perl
while(<>) { %h=(); m/\d+;((\d+:\d+,)*)(.*)/ or die "Bad"; $pairs=$1; $refs=$3; while ($pairs=~m/(\d+):(\d+),/g) {$h{$1} = (); $h{$2} = ();}  print (join(",", keys %h), ",", $refs, "\n"); }
