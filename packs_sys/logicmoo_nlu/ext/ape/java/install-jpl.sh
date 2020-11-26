echo OLD WAS mvn install:install-file \
-Dfile=/usr/local/lib/swipl-6.4.1/lib/jpl.jar \
-DgroupId=jpl \
-DartifactId=jpl \
-Dversion=6.4.1 \
-Dpackaging=jar \
-DgeneratePom=true

mvn install:install-file \
-Dfile=/usr/lib/swi-prolog/lib/jpl.jar \
-DgroupId=jpl \
-DartifactId=jpl \
-Dversion=7.6.4 \
-Dpackaging=jar \
-DgeneratePom=true
