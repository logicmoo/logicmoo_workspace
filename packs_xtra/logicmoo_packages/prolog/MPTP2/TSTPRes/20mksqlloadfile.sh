# create the load file by running ./20mksqlloadfile.sh $1
j=`echo -n $1 | sed -e 's/[-.]/_/g'`
sed -e 's/\([^ ]\+\) \+\([^-]*\)-\([^ ]*\) \+\([^ ]\+\).*/\1\t\2\t\3\t\4/' geoff_res/$1  |sed -e 's/(no.*/700/' > loadsql/"$j"_r1
