my @miz1=('hidden', 'tarski');
my @miz=();
open(F,'mml.lar');
while(<F>) {chomp; push(@miz, $_); }
close(F);
foreach my $f ( 0 .. $#miz)
{
#  chdir $miz[$f];
  open(G,">00$miz[$f]");
  print G "include(\'Axioms/hidden.ax\').\n";
  print G "include(\'Axioms/tarski.ax\').\n";
  foreach my $j ( 0 .. $f - 1)
  {
    print G "include(\'Axioms/$miz[$j].ax\').\n";
  }
  close(G);
}
