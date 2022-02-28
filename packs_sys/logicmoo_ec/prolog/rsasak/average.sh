#!/bin/sh

# Create a new file with average values for multiple running.
for file in output/*.out ; do
	awk '{ cnt[$1]++; tot[$1]+=$2;rest[$1]=substr($0,length($1 $2) + 3) } \
	END{ for (i in tot) { print length(i), i, tot[i]/cnt[i], rest[i]}} ' $file | sort | \
	cut -d\  -f2-  | \
	awk '{ print substr($1,0, 10), substr($0,length($1)+2)}' > $file.avg
	
done