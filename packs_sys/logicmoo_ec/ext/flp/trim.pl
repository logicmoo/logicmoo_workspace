#!/usr/bin/perl -w

use File::Slurp qw(read_file);

die unless $ARGV[0];
my $f = $ARGV[0];
if (-f $f) {
  my $c = read_file($f);
  foreach my $line (split /\n/, $c) {
    if ($line =~ /^\s*$/) {
      print $line."\n";
    } elsif ($line =~ /^\s*;/) {
      print $line."\n";
    } elsif ($line =~ /^([\d\.]+):\s+\(([^)]+)\)\s+\[([\d\.]+)\]$/) {
      print "$1:   ($2) $3\n";
    } else {
      print "<$line>\n";
    }
  }
}
