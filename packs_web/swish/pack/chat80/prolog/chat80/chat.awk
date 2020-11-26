# @(#)chat.awk	24.2 4/14/88

# chat.awk : Produce a table showing all the ratios from two sets of Chat
#	     performance data. Assumes that the input is the concatenation
#	     of two lots of output from Chat with each line being of the
#	     form: "1 |  3 4 5 6". There should be no other formatting lines
#	     in the input, and the inputs should NOT include any TOTAL line
#	     either (this will be calculated).
#
BEGIN	{ x = 0; count1 = 0; count2 = 0; }
x == 0  { x = 1; marker = $1; markcount = 1; }
x == 1	{
	  if ($1 == marker && markcount++ > 1) x = 2;
	   else {
		    a[$1] = $3;
		    b[$1] = $4;
		    c[$1] = $5;
		    d[$1] = $6;
		    e[$1] = $7;
		    count1++;
	  }
	}
x == 2	{
	  if ($3 == 0) a[$1] = 0;  else a[$1] = a[$1] / $3;
	  if ($4 == 0) b[$1] = 0;  else b[$1] = b[$1] / $4;
	  if ($5 == 0) c[$1] = 0;  else c[$1] = c[$1] / $5;
	  if ($6 == 0) d[$1] = 0;  else d[$1] = d[$1] / $6;
	  if ($7 == 0) e[$1] = 0;  else e[$1] = e[$1] / $7;
	  count2++;
	}
END	{

    if (count1 != count2)
	printf("WARNING: Two input sets are not the same size (%d != %d)\n", \
	    count1, count2);

    printf("RATIO TABLE\n\n")
    printf("%10s |%10s%10s%10s%10s%10s\n", \
	"Test", "Parse", "Semantics", "Planning", "Reply", "TOTAL");
    for (i = 1; i <= 23; i++) {
	printf("%10d |%10.2f%10.2f%10.2f%10.2f%10.2f\n", \
	    i, a[i], b[i], c[i], d[i], e[i]);
	atotal += a[i];
	btotal += b[i];
	ctotal += c[i];
	dtotal += d[i];
	etotal += e[i];
    }
    atotal /= count1;
    btotal /= count1;
    ctotal /= count1;
    dtotal /= count1;
    etotal /= count1;
    printf("\n%10s |%10.2f%10.2f%10.2f%10.2f%10.2f\n", \
	"TOTAL", atotal, btotal, ctotal, dtotal, etotal);

	}
