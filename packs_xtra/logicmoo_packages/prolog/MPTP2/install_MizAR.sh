#!/bin/bash
# $1= 7.8.10 $2=4.99.1005 $3=http://mws.cs.ru.nl/~mptp
# script installing MizAR for particular version of library and sources
# assumes we are in some mizwrk directory
# old copied info:
# script creating the mizar html from a mizar distro
# current dir should contain the Makefile for doing this,
# and all prerequisities required by the Makefile have to be present 
ver="$1_$2"
myurl=$3
cvsver=`echo -n $1| sed -e 's/\./_/g'`
root=`pwd`
ph=/home/mptp/public_html
cgi=$ph/cgi-bin
bindir=bin$2
mycgi=$cgi/bin$2
mymml=$ph/mml$2
jobs=64
mmlver=`echo -n \[$2\]| sed -e 's/\./,/g'`


wget ftp://mizar.uwb.edu.pl/pub/system/i386-linux/mizar-$ver-i386-linux.tar
if [ "$?" != "0" ]; then exit 1; fi
mkdir $ver

##WS: unzip mizar-$1-i386-win32.exe -d $ver
tar xf mizar-$ver-i386-linux.tar -C$ver 
cd $ver
cvs -d:pserver:softdev@mizar.uwb.edu.pl:2401/srv/cvsroot co -rver_$cvsver kernel libtools base
cd kernel
fpc -Sd -dCH_REPORT -dSCH_REPORT -Fu../base verifier.dpr 
cd ../libtools
fpc -Sd -Fu../base -Fu../kernel envget.dpr
cd ..
### TODO: fix mizf!!

### TODO: make this conditional instead of commented, compile all binaries in that case
# doing it with the windows version (not working yet):

##WS: unzip -a -o mmlfull -d mml
##WS: unzip -a -o mizdoc -d doc
##WS: unzip -a -o mizbib -d doc
##WS: unzip -a -o mizxml -d doc/xml
##WS: unzip -a -o prel -d prel
##WS: unzip -a -o abstr -d abstr
##WS: unzip -a -o mizdb1

tar xzf mizshare.tar.gz 
tar xzf mizdoc.tar.gz 
mkdir bin
tar xzf mizbin.tar.gz -Cbin
cp  kernel/verifier bin/verifier.bfex
cp  libtools/envget bin/envget
mv bin/verifier bin/verifier.std
cd bin && ln -s verifier.bfex verifier && cd ..
mkdir $mycgi
ln -s $root/$ver/bin $mycgi/mizar 
# tar xzf mizbin.tar.gz -C$mycgi

pushd /home/mptp/gitrepo/MPTP2/
git pull; if [ $? != 0 ]; then echo "fix MPTP2 repo first, exiting"; exit 1; fi
cd ../xsl4mizar
git pull; if [ $? != 0 ]; then echo "fix xsl4mizar repo first, exiting"; exit 1; fi
popd

mkdir html 
sed -e 's/urban/mptp/' /home/mptp/gitrepo/MPTP2/mizsys/Makefile.4.145 > Makefile
cp -a mml miztmp
cp Makefile  miztmp
export MIZFILES=`pwd`
cd miztmp
make -j $jobs allacc | tee 00acc.log
make -j $jobs allhdr | tee 00hdr.log
make -j $jobs allcmt | tee 00cmt.log
make -j $jobs allxml | tee 00xml.log
if [ "$?" != "0" ]; then exit 1; fi
make -j $jobs allxml1 | tee 00xml1.log
make -j $jobs allhtmla1 | tee 00htmla1.log
if [ "$?" != "0" ]; then exit 1; fi
make -j $jobs allxml2 | tee 00xml2.log
make -j $jobs allevl1 | tee 00evl1.log
make -j $jobs allevl2 | tee 00evl2.log
if [ "$?" != "0" ]; then exit 1; fi

make hidden.acc
make hidden.hdr
make hidden.cmt
make hidden.xml
make hidden.xml1
make hidden.htmla1

make tarski_0.acc
make tarski_0.hdr
make tarski_0.cmt
make tarski_0.xml
make tarski_0.xml1
make tarski_0.htmla1
make tarski_0.xml2

make tarski_a.acc
make tarski_a.hdr
make tarski_a.cmt
make tarski_a.xml
make tarski_a.xml1
make tarski_a.htmla1
make tarski_a.xml2

make tarski.acc
make tarski.hdr
make tarski.cmt
make tarski.xml
make tarski.xml1
make tarski.htmla1
make tarski.xml2

make -j $jobs alldco2
make tarski_0.dco2
make tarski_a.dco2
make tarski.dco2

for j in `ls *.htmla1| sed -e 's/.htmla1//'`; do mv $j.htmla1 ../html/$j.html; done
cd ..
mv miztmp/refs html
tar czf html_abstr.$ver.noproofs.tar.gz html
mv miztmp/proofs html
tar czf html_abstr.$ver.tar.gz html

git clone https://github.com/JUrban/MPTP2.git
if [ $? != 0 ]; then echo "cannot clone MPTP2 repo, exiting"; exit 1; fi
#git@github.com:JUrban/MPTP2.git
# git branch --track MizAR1096 origin/MizAR1096
# git checkout MizAR1096

ln -s MPTP2 mptp
cd mptp
mkdir pl
mkdir Axioms
mkdir problems
for i in dcl2 dco2 evl2 lem2 sch2 the2 xml2; do mv ../miztmp/*.$i pl; done

cp hidden.dco2 pl/hidden.dco2
ln -s ../mml.lar mml.lar

ln -s $root/$ver $ph/$ver
ln -s $root/$ver $mymml
ln -s /home/mptp/gitrepo/xsl4mizar $ph/xsl4mizar

cp /home/mptp/gitrepo/MPTP2/MizAR/cgi-bin/bin/* $mycgi 

ln -s /home/mptp/gitrepo/MPTP2/MizAR/cgi-bin/showby.cgi $mycgi/showby.cgi
ln -s /home/mptp/gitrepo/MPTP2/MizAR/cgi-bin/showtmpfile.cgi $mycgi/showtmpfile.cgi
ln -s /home/mptp/bin/vampire_1.8 $mycgi/vampire_rel2

ln -s $mymml/mptp/utils.pl $mycgi/utils.pl


sed -ie "s/^mml_dir(.*/mml_dir(\"\/home\/mptp\/mizwrk\/$ver\/MPTP2\/pl\/\")./; s/^mml_version(.*/mml_version($mmlver)./" utils.pl
sed -ie "s/^bindir=.*/bindir=$bindir/" $mycgi/mizf
sed -ie "s/^\(.*MyUrl.*\)http:..mws.cs.ru.nl.~mptp.*/\1$myurl\2/" $mycgi/MizARconfig.pl

## this is needed in the showby.cgi, however showby now seems symlinked to the main dir??
# sed -ie "s/^\(my .MyUrl = \).http:..mws.cs.ru.nl.~mptp.;/\1'$myurl';/" $mycgi/showby.cgi


## this will fail in suse, forcing us to create a symlink to "pl" from ~mptp/bin/swipl
which swipl > /dev/null ; if [ $? != 0 ]; then echo "swipl not executable, exiting"; exit 1; fi

swipl -nodebug -A0 -L0 -G0 -T0 -q -t "[utils], mml2tptp_includes('Axioms/'), halt."
cat Axioms/*.ax > 00allmmlax

# make problems, this could use a makefile instead of parallel
cat ../mml.lar | sort -R | time nice parallel -j $jobs  time "swipl -nodebug -A0 -L0 -G0 -T0  -t \"[utils], declare_mptp_predicates,load_mml_for_article({}, 'pl', []),install_index,time(mk_article_problems({},[[mizar_by,mizar_from,mizar_proof],[theorem, top_level_lemma] ],[opt_REM_SCH_CONSTS,opt_TPTP_SHORT])),halt.\"" 

# sublemmas with proof, now commented, about 20 minutes and 156k probs
# cat ../mml.lar | sort -R | time nice parallel -j $jobs  time "swipl -nodebug -A0 -L0 -G0 -T0  -t \"[utils], declare_mptp_predicates,load_mml_for_article({}, 'pl', []),install_index,time(mk_article_problems({},[[mizar_proof],[sublemma] ],[opt_REM_SCH_CONSTS,opt_TPTP_SHORT])),halt.\""

mv problems problems_small
mkdir problems
tar czf mptp_problems_small.$ver.tar.gz problems_small

# make snow data and train incrementally (so we can compare the performance with other versions)
swipl -nodebug -A0 -L0 -G0 -T0 -q -t "[utils], mk_snow_input_for_learning(snow3,[opt_LEARN_STDFILES]), halt."
# need to count the targets
refnr0=`cat snow3.refnr | wc -l`
# we start from 0
refnr=$((refnr0-1))
head -n1 snow3.train > snow3.train1
~/gitrepo/MPTP2/MaLARea/bin/snow -train -I snow3.train1 -F snow3.net  -B :0-$refnr
time ~/gitrepo/MPTP2/MaLARea/bin/snow -test -o allboth -i+ -I snow3.train -F snow3.net  -L 100 -B :0-$refnr > snow3.eval

# no need for this - no background flas at this moment
# time /home/urban/gitrepo/MPTP2/finedeps/SnowRes2Specs.pl -l2048 -r refnr$ext < snow3.eval$ext  > snow3.spec



# fix bindir in mizf - done

# upon first install, files in cgi-bin need to be symlinked to MizAR/cgi-bin files
# the same for the main html file 
# xsl4mizar expected in public_html - done
# ERROR: script_file `/home/mptp/public_html/cgi-bin/bin4.160.1126/utils.pl' does not exist - DONE
# Not Found: The requested URL /~mptp/cgi-bin/bin4.160.1126/showby.cgi was not found on this server. - DONE

# The requested URL /~mptp/cgi-bin/tptp/RemoteSOT1.cgi was not found on this server.
# - one-time symlink from ~/gr/MPTP2/MizAR/cgi-bin/tptp needed in ~ph/cgi-bin/tptp
# - one-time: icons in ~/ph
