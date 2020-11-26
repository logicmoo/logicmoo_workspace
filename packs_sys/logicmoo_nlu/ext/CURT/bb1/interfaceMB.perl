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
my $domainsize = $ARGV[0];
my $mace_selected = ($ARGV[1] =~ /mace/);
my $paradox_selected = ($ARGV[1] =~ /paradox/);
my $mace_result = 0;
my $paradox_result = 0;
my $readmacemodel = 0;
my $readparadoxmodel = 0;
my $readmace = 0;
my $readparadox = 0;
my $model = "";

if ($mace_selected) {
   open(MACE_OUTPUT, "mace -t 30 -n1 -N$domainsize -P < mace.in 2>/dev/null |");
   $readmace=1;
} 

if ($paradox_selected) {
   open(PARADOX_OUTPUT, "paradox paradox.in --sizes 1..$domainsize --print Model 2>/dev/null |");
   $readparadox=1;
} 

while($readmace || $readparadox) { 
  if ($readmace && defined($_ = <MACE_OUTPUT>)) {
     if ($_ =~ /end_of_model/) {
        $readmacemodel = 0;
        $readmace = 0;
        $mace_result = 1;
     }
     elsif ($readmacemodel == 1) {
        $model = "$model$_";
        $model =~ s/\$(.*?)\,/$1\,/;
     }
     elsif ($_ =~ /======================= Model/) {
        $readmacemodel = 1;
        $readparadox = 0;
        $readotter = 0;
        $readbliksem = 0;
     }
  }
  else {
     $readmace=0;
  }
  if ($readparadox && defined($_ = <PARADOX_OUTPUT>)) {
     if ($_ =~ /== Result ======/ && $readparadoxmodel) {
        $model = "$model dummy\n]).\n";
        $readparadoxmodel = 0;
        $readparadox = 0;
        $paradox_result = 1;
     }
     elsif ($readparadoxmodel == 1) {
        $newpart = $_;
        $newpart =~ s/\n/,\n/;
        $newpart =~ s/TRUE/1/;
        $newpart =~ s/FALSE/0/;
        if ($newpart =~ s/\'/d/g) {
            $model = "$model $newpart";
        }
     }
     elsif ($_ =~ /== Model =======/) {
        $model = "paradox([\n";
        $readparadoxmodel = 1;
        $readmace = 0;
        $readotter = 0;
        $readbliksem = 0;
     }
  }
  else {
     $readparadox=0;
  }
}

if ($mace_selected) {
   close MACE_OUTPUT;			 
} 

if ($paradox_selected) {
   close PARADOX_OUTPUT;			 
} 

open(OUTPUT,">mb.out");
if ($mace_result == 1) {
   print OUTPUT "$model";
   print OUTPUT "engine(mace).\n";
}
elsif ($paradox_result == 1) {
   print OUTPUT "$model";
   print OUTPUT "engine(paradox).\n";
}
else {
   print OUTPUT "unknown.\n";
}
close(OUTPUT);

exit 1;

