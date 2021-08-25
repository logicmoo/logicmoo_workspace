# expects sorted formulas in file 00: cat *| sort -u >00
# then it renames  duplicates to the first name
# does this also for 00
perl -e '%h=();%d=();open(I,"00");while(<I>) { m/([^,]+),[^,]+,(.*)/ or die "bad"; if (exists $h{$2}) {$d{$1}=$h{$2}} else {$h{$2}=$1}};close(I); while($f=shift) {open(F,$f); open(O,">../renamed0/$f"); while(<F>) { m/([^,]+),(.*)/ or die "bad"; if (exists $d{$1}) {print O $d{$1}} else {print O "$1";}; print O ",$2\n"; } close(F); close(O);}' `ls`