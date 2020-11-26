#!/usr/bin/perl
#
# Extract the YQT members from the HTML description of the Sisyphus-I problem
# and generate CGIF to standard output. Assumptions about the file format are 
# made, as can be seen from the regular expressions used.
# 
# David Benn, October 2000

if (@ARGV == 1) {
  # Read all lines from HTML file.
  open(YQT, $ARGV[0]) or die "can't open $ARGV[0] for reading.\n";
  @lines = <YQT>;
  close(YQT);

  $i = 0;
  while ($line = $lines[$i++]) {
    # Find next person.
    if ($line =~ /<TD WIDTH=211>(.+)<BR>/i) {
      $name = $1;
      $name =~ s/\&uuml\;/u/; # replace HTML u-umlaut with u

      # Extract key-value pairs for this person.
      while ($line = $lines[$i++]) {
	if ($line =~ /\s*<\/TD>/) {
	  # Start of next person, so break out of loop.
	  $i--;
	  last;
	}

	# Key-value pair.
	if ($line =~ /\s*(.+)\s*=\s*(.+)\s*/) {
	  $key = ucfirst($1);
	  $value = ucfirst($2);
	  $key =~ s/ //; # why necessary, but not for value?
	  $value =~ s/<BR>//; # lines other than "Works-with" end with "<BR>" 
	  $value =~ s/True/Yes/; # all Yes or No

	  # Skip over blank line to second colleagues line?
	  if ($key =~ /Works\-with/ and $value =~ /\,\s*$/) {
	    $i++;
	    $line = $lines[$i++];
	    if ($line =~ /\s*(.+)/) { # "." won't include linefeed
		$value .= $1;
	    }
	  }

	  $value =~ s/\&uuml\;/u/; # replace HTML u-umlaut with u (Works-with)

	  # Generate CGIF for this YQT member.
	  if ($key ne 'Works-with') {
	    if ($key eq 'Project') {
	      $relation = 'Member';
	    } else {
	      $relation = "Chrc";
	    }
	    print "[Person:*a'$name'][$key:*b'$value']($relation?a?b)\n";
	  } else {
	    @coworkers = split(/\s*,\s*/, $value);
	    foreach $coworker (@coworkers) {
	      if ($coworker !~ /^\s+$/) {
		print "[Person:*a'$name'][Person:*b'$coworker']";
		print "(Coworker?a?b)\n";
	      }
	    }
	  }
	}
      }
      print "\n";
    }
  }
} else {
  die "Sisyphus-I HTML description page required.\n";
}
