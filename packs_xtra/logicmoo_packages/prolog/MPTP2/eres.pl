#!/usr/bin/perl
while(<>) {
  if (m/^([a-z].*)$/) { $n = $1; $nmread = 1; push @names, $n; }
  if (m/^[#] Proof found.*$/) {
    die "no preceding name" unless ($nmread == 1);
    push @proved, $n;
    $nmread = 0;
  }
}
open(OUT, '>00proved');
foreach $k (@proved) { print OUT "$k\n"; 
  foreach $i (0.. $#names) { if ($k eq $names[$i]) {delete $names[$i]}}}

open(OUT1, '>00unproved');
foreach $k (@names) { if (length($k) > 0) {print OUT1 "$k\n";}}
