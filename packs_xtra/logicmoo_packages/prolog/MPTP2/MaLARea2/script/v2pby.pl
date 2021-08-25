#!/usr/bin/perl -w

# ./v2pby conjname foo.vout > foo.pby
# for i in `ls | sed -e 's/.p.vout$//'`; do /home/mptp/gr/MPTP2/MaLARea/script/v2pby.pl $i $i.p.vout; done > 00proved_by
# 
# the same thing for z3 is simpler:
# cat *|grep '^core'  | sed -e 's/^core(\([^,]*\),\[/proved_by(\1,[\1,/' > 00zproved_by

my $vampire_regexp = '.*\bfile\([^\),]+, *([a-z0-9A-Z_]+) *\)';

my $conj = shift;

while ($_=<>)
{
    if(m/$vampire_regexp/) { push(@proved_by, $1); }
}

my $conj_refs = join(",", @proved_by);
print "proved_by($conj,[$conj_refs]).\n";
