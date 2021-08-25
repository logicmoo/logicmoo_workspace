#!/usr/bin/perl
$s=0; $t=0; while(<>) { m/eqmods.([0-9]+),/ or die "bad $_"; $t++; $s+=$1; } $r=$s/$t; print "total: $t, redund: $s, ratio: $r\n";
