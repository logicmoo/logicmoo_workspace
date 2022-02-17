#!/bin/bash


if [ -f "/is_evil_making" ]; then
   echo "is_evil_making"
   sleep 1
   return 0 2>/dev/null ; exit 0
fi

if [ -f "/is_evil_made" ]; then
   echo "is_evil_made"
   sleep 1
   return 0 2>/dev/null ; exit 0
fi


echo "Evil is not born, it is made"
touch '/is_evil_making'
sleep 1
return 0 2>/dev/null ; exit 0

( cd malmo/Minecraft ; cmake . ; make ; (echo -n "malmomod.version=" && cat ../VERSION) > ./src/main/resources/version.properties )


apt install -y guile-3.0-dev
(
sudo ln -s /usr/lib/x86_64-linux-gnu /usr/lib64

(
cd /usr/local/share
# mv guile/ dead.guile
# ln -s /usr/share/guile
# \cp -a dead.guile/?* guile/
)

(
cd /usr/lib/x86_64-linux-gnu
mv guile/ dead.guile
ln -s /usr/local/lib/guile
\cp -a dead.guile/?* guile/
)

cd /opt/logicmoo_workspace/packs_sys/logicmoo_cogserver
ln -s /usr/lib/x86_64-linux-gnu/libguile-3.0.so /usr/local/lib/libguile-3.0.so
find -maxdepth 2 -name .git -print -execdir git pull \;
find -maxdepth 2 -name CMakeLists.txt -print -execdir bash -c "rm -rf build; mkdir build ; cd build ; cmake .. ; /bin/true " \; -print
find -maxdepth 2 -name CMakeLists.txt -print -execdir bash -c "cd build ; make -j 12 ; make install; /bin/true " \; -print
find -maxdepth 2 -name CMakeLists.txt -print -execdir bash -c "cd build ; cmake .. ; /bin/true " \; -print
find -maxdepth 2 -name CMakeLists.txt -print -execdir bash -c "cd build ; make -j 12 ; make install; /bin/true " \; -print
find -maxdepth 2 -name configure -print -execdir bash -c "./configure; /bin/true " \; -print
find -maxdepth 2 -name Makefile -print -execdir bash -c "make -j 12 ; make install; /bin/true " \; -print
#find -maxdepth 2  -name requirements-dev.txt -print -execdir bash -c "pip install -r requirements-dev.txt --prefer-binary; /bin/true " \; -print
#find -maxdepth 2  -name requirements.txt -print -execdir bash -c "pip install -r requirements.txt --prefer-binary; /bin/true " \; -print
find -maxdepth 3  -name setup.py -print -execdir bash -c "pip install -e .; /bin/true " \; -print
)

find -maxdepth 2  -name guile.am -print -execdir bash -c "autoreconf -i; /bin/true " \; -print

# Btw in order to run the `./launchClient.sh ...` without it crashing on startup I first had to 


#apt install ocaml ocaml-findlib cython libboost-dev cxxtest postgresql postgresql-client libpq-dev

touch '/is_evil_made'
rm -f '/is_evil_making'

echo "Evil was not born, it was made"
