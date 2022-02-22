cd ..
find -name "*.xml"
find -name "*.xml" -exec tidy -xml -iq '{}' ;\
find -name "*.xml" -exec tidy -xml -iq '{}' \;
find -name "*.xml" -exec tidy -xml -iq -m '{}' ;\
find -name "*.xml" -exec tidy -xml -iq -m '{}' \;
find -name "*.rels" -exec tidy -xml -iq -m '{}' \;
grep interested_in . -r
grep social_object . -r
find -name load_special_csv_row -r
grep load_special_csv_row . -r 
cat ./Utilities/startup.prolog
lmoo bash
lmoo ansi
cd ..
