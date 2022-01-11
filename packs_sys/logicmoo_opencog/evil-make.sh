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

cd /opt/logicmoo_workspace/packs_sys/logicmoo_opencog
ln -s /usr/lib/x86_64-linux-gnu/libguile-3.0.so /usr/local/lib/libguile-3.0.so
#find -maxdepth 2 -name .git -execdir git pull \;
find -maxdepth 2  -name CMakeLists.txt -execdir bash -c "rm -rf build; mkdir build ; cd build ; cmake .. " \;
find -maxdepth 2  -name CMakeLists.txt -execdir bash -c "cd build ; make -j 12 ; make install" \;
find -maxdepth 2  -name CMakeLists.txt -execdir bash -c "cd build ; cmake .. " \;
find -maxdepth 2  -name CMakeLists.txt -execdir bash -c "cd build ; make -j 12 ; make install" \;
find -maxdepth 2  -name guile.am -execdir bash -c "autoreconf -i" \;
find -maxdepth 2  -name configure -execdir bash -c "./configure" \;
find -maxdepth 2  -name Makefile -execdir bash -c "make -j 12 ; make install" \;
find -maxdepth 2  -name requirements-dev.txt -execdir bash -c "pip install -r requirements-dev.txt" \;
#find -maxdepth 2  -name requirements.txt -execdir bash -c "pip install -r requirements.txt" \;
find -maxdepth 3  -name setup.py -execdir bash -c "pip install -e ." \;
)

touch '/is_evil_made'
rm -f '/is_evil_making'

echo "Evil was not born, it was made"
