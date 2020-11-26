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

# parameterisation of the programs
my $domainsize = $ARGV[0];
my $pleaseload = $ARGV[1];

# for keeping track of running programs 
my %pids = (); 

# where to store results
my $model = "";
my $winner = "";

# how to run programs
my %command = ( 
 otter   => "otter < otter.in > otter.out 2>/dev/null; echo otter > otter.ready",
 bliksem => "bliksem < bliksem.in > bliksem.out 2>/dev/null; echo bliksem > bliksem.ready",
 mace    => "mace -t20 -n1 -N$domainsize -P < mace.in > mace.out 2>/dev/null; echo mace > mace.ready",
 paradox => "paradox paradox.in --sizes 1..$domainsize --print Model > paradox.out 2>/dev/null; echo paradox > paradox.ready"
);

# delete any cruft from previous instances of this script 
system "rm -f *.ready";

# run any requested processes
foreach my $p (("otter", "bliksem", "mace", "paradox")) {
   if ($pleaseload =~ /$p/) {
      my $forkedpid = fork;
      unless ($forkedpid) {
          # the child process execs the program
          exec($command{$p}); 
      }
      # the parent process keeps track of the child process
      $pids{$p} = $forkedpid;
   }
}
 

# continue looping while there are still processing running
# and none of them has found a result
while( (keys %pids) > 0 && $winner eq "") {

  # give some time to the child processes
  sleep 0.5;

  if (-e "mace.ready" && $winner eq "") {
      my $readmacemodel = 0;
      open(OUTPUT,"mace.out");
      while (<OUTPUT>) {
             if (/end_of_model/) {
                $winner = "mace";
                $readmacemodel = 0;
             }
             elsif ($readmacemodel) {
                $model = "$model$_";
                $model =~ s/\$(.*?)\,/$1\,/;
             }
             elsif (/======================= Model/) {
                $readmacemodel = 1;
	    }
      }
      close(OUTPUT);
      delete $pids{mace};
  }

  if (-e "paradox.ready" && $winner eq "") {
      my $readparadoxmodel = 0;
      open(OUTPUT,"paradox.out");
      while (<OUTPUT>) {
            if (/== Result ======/) {
               $model = "$model dummy\n]).\n";
               $winner = "paradox";
               $readparadoxmodel = 0;
            }
            elsif ($readparadoxmodel) {
               s/\n/,\n/;
               s/TRUE/1/;
               s/FALSE/0/;
               if (s/\'/d/g) {
                   $model = "$model $_";
               }
            }
            elsif (/== Model =======/) {
               $model = "paradox([\n";
               $readparadoxmodel = 1;
	    }
            elsif ($_ =~ /CONTRADICTION/) {
               $model = "paradox([]).\n";
               $readparadoxmodel = 0;
               $winner = "paradox";
	    }
	 }
      close(OUTPUT);
      delete $pids{paradox};
  }

  if (-e "otter.ready" && $winner eq "") {
      open(OUTPUT,"otter.out");
      while (<OUTPUT>) {
             if (/proof of the theorem/) {
                $winner = "otter";
            }
     }
     close(OUTPUT);
     delete $pids{otter};
  }

  if (-e "bliksem.ready" && $winner eq "") {
      open(OUTPUT,"bliksem.out");
      while (<OUTPUT>) {
             if (/found a proof/) {
                $winner = "bliksem";
            }
       }
      close(OUTPUT);
      delete $pids{bliksem};
  }
}


# kill any remaining child processes (for example, any theorem
# provers or model builders which are still working)
foreach (values %pids) { kill $_; }


# write the results out to a file which will be read by Curt
open(OUTPUT,">tpmb.out");
if ($winner ne "") {
   my $details = "proof.\n";
   if ($model ne "") {
      $details = $model;
   }
   print OUTPUT $details;
   print OUTPUT "engine($winner).\n";
}
else {
   print OUTPUT "unknown.\n";
}
close(OUTPUT);

exit 0;

