:
eval 'exec perl -w -S $0 ${1+"$@"}'
 if 0;
#
#    Copyright (C) 2004 Patrick Blackburn & Johan Bos
#
#    This file is part of BB1, version 1.2 (August 2005).
#
#    BB1 is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    BB1 is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with BB1; if not, write to the Free Software Foundation, Inc., 
#    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
my $otter_selected = ($ARGV[0] =~ /otter/);
my $bliksem_selected = ($ARGV[0] =~ /bliksem/);

my $otter_result = 0;
my $readotter = 0;

my $bliksem_result = 0;
my $readbliksem = 0;


if ($otter_selected) {
   open(OTTER_OUTPUT, "otter < otter.in 2>/dev/null |");
   $readotter=1;
}

if ($bliksem_selected) {
   open(BLIKSEM_OUTPUT, "bliksem < bliksem.in 2>/dev/null |");
   $readbliksem=1;
}

while($readotter || $readbliksem) { 
  if ($readotter && defined($_ = <OTTER_OUTPUT>)) {
     if ($_ =~ /proof of the theorem/) {
        $otter_result = 1;
        $readotter = 0;
        $readbliksem = 0;
     }
  }
  else {
     $readotter=0;
  }
  if ($readbliksem && defined($_ = <BLIKSEM_OUTPUT>)) {
     if ($_ =~ /found a proof/) {
        $bliksem_result = 1;
        $readotter = 0;
        $readbliksem = 0;
     }
  }
  else {
     $readbliksem=0;
  }
}

if ($otter_selected) {
   close OTTER_OUTPUT;			 
}

if ($bliksem_selected) {
   close BLIKSEM_OUTPUT;			 
}

open(OUTPUT,">tp.out");
if ($otter_result == 1) {
   print OUTPUT "proof.\n";
   print OUTPUT "engine(otter).\n";
}
elsif ($bliksem_result == 1) {
   print OUTPUT "proof.\n";
   print OUTPUT "engine(bliksem).\n";
}
else {
   print OUTPUT "unknown.\n";
}
close(OUTPUT);

exit 1;

