#!/bin/sh

export nr
nr=0

for tdir in *_tests
do
	(	cd $tdir
		for file in *_temp.* temp_new temp_old core*
		do
			if [ -f $file ]
			then 
				rm -f $file
				nr=$[nr+1]
			fi
		done
		exit $nr
	)
	nr=$?
done

if [ $nr -eq 0 ]
then
	echo "Nothing to do!"
fi
