:
eval 'exec perl -w -S $0 ${1+"$@"}'
 if 0;
#
#    Copyright (C) 2004 Patrick Blackburn & Johan Bos
#
#    This file is part of BB2, version 1.0 (June 2004).
#
#    BB2 is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    BB2 is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with BB2; if not, write to the Free Software Foundation, Inc., 
#    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
my $domainsize = $ARGV[0];
my $mace_selected = ($ARGV[1] =~ /mace/);
my $otter_selected = ($ARGV[1] =~ /otter/);
my $bliksem_selected = ($ARGV[1] =~ /bliksem/);
my $paradox_selected = ($ARGV[1] =~ /paradox/);
my $mace_result = 0;
my $otter_result = 0;
my $bliksem_result = 0;
my $paradox_result = 0;
my $readmacemodel = 0;
my $readparadoxmodel = 0;
my $readmace = 0;
my $readparadox = 0;
my $readotter = 0;
my $readbliksem = 0;
my $model = "";

if ($mace_selected) {
   open(MACE_OUTPUT, "mace -t 30 -n1 -N$domainsize -P < mace.in 2>/dev/null |");
   $readmace=1;
} 

if ($paradox_selected) {
   open(PARADOX_OUTPUT, "paradox paradox.in --sizes 1..$domainsize --print Model 2>/dev/null |");
   $readparadox=1;
} 

if ($otter_selected) {
   open(OTTER_OUTPUT, "otter < otter.in 2>/dev/null |");
   $readotter=1;
}

if ($bliksem_selected) {
   open(BLIKSEM_OUTPUT, "bliksem < bliksem.in 2>/dev/null |");
   $readbliksem=1;
}

while($readmace || $readparadox || $readotter || $readbliksem) { 
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
     if ($_ =~ /== Result ======/) {
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
  if ($readotter && defined($_ = <OTTER_OUTPUT>)) {
     if ($_ =~ /proof of the theorem/) {
        $otter_result = 1;
        $readotter = 0,
        $readbliksem = 0,
        $readmace = 0;
        $readparadox = 0;
     }
  }
  else {
     $readotter=0;
  }
  if ($readbliksem && defined($_ = <BLIKSEM_OUTPUT>)) {
     if ($_ =~ /found a proof/) {
        $bliksem_result = 1;
        $readotter = 0,
        $readmace = 0;
        $readparadox = 0;
        $readbliksem = 0;
     }
  }
  else {
     $readbliksem=0;
  }
}

if ($mace_selected) {
   close MACE_OUTPUT;			 
} 

if ($paradox_selected) {
   close PARADOX_OUTPUT;			 
} 

if ($otter_selected) {
   close OTTER_OUTPUT;			 
}

if ($bliksem_selected) {
   close BLIKSEM_OUTPUT;			 
}

open(OUTPUT,">tpmb.out");
if ($otter_result == 1) {
   print OUTPUT "proof.\n";
   print OUTPUT "engine(otter).\n";
}
elsif ($bliksem_result == 1) {
   print OUTPUT "proof.\n";
   print OUTPUT "engine(bliksem).\n";
}
elsif ($mace_result == 1) {
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

