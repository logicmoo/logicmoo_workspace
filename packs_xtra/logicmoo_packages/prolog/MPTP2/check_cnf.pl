#!/usr/bin/perl -w

# print files in which the cnf was bad

@files = `cat $ARGV[0]`;

while ($file = shift @files)
{
    chop($file);
    $cnf_file = $file . ".cnf";
    $cnf_size = -s $cnf_file;
    $fof_size = -s $file;

    if (($cnf_size > 60000) && (($cnf_size/$fof_size) > 3))
    {
#	print "$cnf_size: $fof_size: $file\n";
	print "$file\n";
    }
}
