VERSION=2.0.11

JARFILE = PPCGApplet.jar
JARFILES = -C classes PrologPlusCG/ README.txt COPYING.txt

# Target ZIP file
ZIPFILE = /tmp/PPCG-$(VERSION).zip

# What goes in the ZIP file?
ZIPFILES = $(JARFILE) start.bat ChangeLog.txt NEWS.txt \
           Makefile AUTHORS.txt COPYING.txt README.txt \
           ppcg.nsi ppcg.jsmooth
ZIPDIRS = classes/ manual/ Samples/ src/
ZIPPREFIX = PPCG-$(VERSION)
ZIPTARGETDIR = /tmp/$(ZIPPREFIX)


# Cleanup
CLEANFILES = *~ core $(JARFILE) ppcg.nsi tests/*~
CLEANDIRS = classes/


# Programs
ZIP = zip
#JAR = /usr/java/jdk1.5.0_03/bin/jar
#JAVAC = /usr/java/jdk1.5.0_03/bin/javac
#JAVA = /usr/java/jdk1.5.0_03/jre/bin/java
JAR=/usr/bin/jar
JAVA=/usr/bin/java
JAVAC=/usr/bin/javac
INSTALL = /usr/bin/install


all: $(JARFILE) manual/index.html ppcg.nsi

zip: distclean all
	rm -f $(ZIPFILE)
	rm -rf $(ZIPTARGETDIR)
	mkdir -p $(ZIPTARGETDIR)
	for d in `find $(ZIPDIRS) -type d -a -not -name CVS`; do mkdir $(ZIPTARGETDIR)/$$d; for f in `find $$d -maxdepth 1 -type f`; do cp $$f $(ZIPTARGETDIR)/$$f; done; done
	cp $(ZIPFILES) $(ZIPTARGETDIR)
	PWD=`pwd`; cd $(ZIPTARGETDIR)/.. ; zip -r $(ZIPFILE) $(ZIPPREFIX)/ ; cd $$PWD 



$(JARFILE): classes/PrologPlusCG/PrologPlusCG.class
	$(JAR) cf $(JARFILE) $(JARFILES)


classes/PrologPlusCG/PrologPlusCG.class:
	rm -rf classes/
	mkdir classes
	$(JAVAC) -source 1.4 -target 1.4 -d classes -classpath classes -sourcepath src src/PrologPlusCG/PrologPlusCG.java src/PrologPlusCG/PrologPlusCGCLI.java
	cp src/PrologPlusCG/gui/ImgPrlg.png classes/PrologPlusCG/gui/ImgPrlg.png

manual/index.html: manual/index.tex
	cd manual ; make all ; cd ..


ppcg.nsi: ppcg.nsi.in
	sed -e 's/@VERSION@/$(VERSION)/g' ppcg.nsi.in > ppcg.nsi


clean:
	rm -rf $(CLEANFILES) $(CLEANDIRS)
	cd manual ; make clean ; cd ..


distclean: clean
	cd manual ; make distclean ; cd ..


run:
	$(JAVA) -classpath classes PrologPlusCG/PrologPlusCG


test: classes/PrologPlusCG/PrologPlusCG.class
	./testall.sh
