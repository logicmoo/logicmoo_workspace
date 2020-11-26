#!/usr/bin/perl -w

use File::Slurp qw(read_file);

die unless $ARGV[0];
my $f = $ARGV[0];
my $state = 0;
if (-f $f) {
  my $c = read_file($f);
  foreach my $line (split /\n/, $c) {
    if ($state) {
      if ($line =~ /^([\d\.]+):\s*\(([^\)]+)\)\s*\[([\d\.]+)\]$/) {
	# 0:   (ACTION1 OBJ1 OBJ2) [1]
	# 0.101: (pay-on-time jessbalint andrewdougherty towardsbillcomcastfromjessbalint201708)  [0.150]
	print "$1:   (".uc($2).") $3\n";
      }
    }
    if ($line =~ /^; Time .*$/) {
      $state = 1;
    }
  }
}
