
XEMU=$1

$XEMU -l $options <<EOF 

[test_forest_view].

test.
 
EOF

$XEMU -l $options <<EOF 

[write_canonical_fv].

rewrite_for_diff.
 
EOF

mv temp test_forest_view_new

status=0
diff -w test_forest_view_new test_forest_view_old || status=1
if test "$status" = 0 ; then 
	echo "table_tests/test_forest_view tested"
	rm -f test_forest_view_new 
else
	echo "table_tests/test_forest_view differ!!!"
	diff -w test_forest_view_new test_forest_view_old
fi

