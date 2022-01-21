

## To be storing state in Atomspace rep
```bash
git clone https://github.com/logicmoo/decreasoner
```

## OpenCog deps
```bash
git clone https://github.com/fastai/nbdev 
git clone https://github.com/microsoft/malmo 
git clone https://github.com/ngeiswei/asmoses 
git clone https://github.com/ngeiswei/pln 
git clone https://github.com/ngeiswei/rocca 
git clone https://github.com/opencog/atomspace 
git clone https://github.com/opencog/cogprotolab 
git clone https://github.com/opencog/cogutil 
git clone https://github.com/opencog/miner 
git clone https://github.com/opencog/spacetime 
git clone https://github.com/opencog/ure
git clone https://github.com/opencog/moses
git clone https://github.com/opencog/cogserver
git clone https://github.com/opencog/atomspace-cog
git clone https://github.com/opencog/atomspace-rocks
git clone https://github.com/opencog/agi-bio
```

ln -s /usr/lib/x86_64-linux-gnu/libguile-3.0.so /usr/local/lib/libguile-3.0.so

```
find -maxdepth 2  -name .git -print -execdir bash -c " git pull" \;

find -maxdepth 2  -name CMakeLists.txt -print -execdir bash -c "rm -rf build; mkdir build ; cd build ; cmake .. " \;
find -maxdepth 2  -name CMakeLists.txt -print -execdir bash -c "cd build ; make -j 12 ; make install" \;
find -maxdepth 2  -name guile.am -execdir -print bash -c "autoreconf -i" \;
find -maxdepth 2  -name configure -execdir -print bash -c "./configure" \;
find -maxdepth 2  -name Makefile -execdir -print bash -c "make -j 12 ; make install" \;
find -maxdepth 2  -name requirements-dev.txt -print -execdir bash -c "pip install -r requirements-dev.txt" \;
find -maxdepth 2  -name requirements.txt -print -execdir bash -c "pip install -r requirements.txt" \;
find -maxdepth 3  -name setup.py -print -execdir bash -c "pip install -e ." \;
```


## Guile/GuileLog
```bash
#git clone https://git.sv.gnu.org/git/guile.git 
git clone https://github.com/wingo/fibers
#git clone https://gitlab.com/bdw-gc-logical-mod/bdw-gc-logical-mod 
git clone https://gitlab.com/tampe/guile-persist 
git clone https://gitlab.com/guile-syntax-parse
git clone https://gitlab.com/gule-log/guile-log 
git clone https://github.com/logicmoo/QuProlog

export GUILE_LOAD_COMPILED_PATH=/usr/local/lib/guile/3.0/ccache
export GUILE_EXTENSIONS_PATH=/usr/local/lib/guile/3.0/extensions

unalias cp

ln -s /usr/local/lib/guile/ /usr/lib/guile

cp -a /usr/share/guile/* /usr/local/share/guile/
mv /usr/share/guile /usr/share/guile-OLD
ln -s /usr/local/share/guile/ /usr/share/guile

cp -a /usr/local/lib/guile/* /usr/lib/x86_64-linux-gnu/guile/
mv /usr/local/lib/guile /usr/local/lib/guile-OLD
ln -s /usr/lib/x86_64-linux-gnu/guile /usr/local/lib/guile

if [ -d "/guile" ]; then
   cp -a /guile/* /usr/local/lib/guile/
   cp -a /guile/* /usr/lib/x86_64-linux-gnu/guile/
   mv /guile /guile-OLD
fi 
if [ ! -f "/guile" ]; then
   ln -s /usr/lib/x86_64-linux-gnu/guile /guile
fi

cd /opt/logicmoo_workspace/packs_sys/logicmoo_opencog

apt install -y guile-3.0-dev

(
cd /opt/logicmoo_workspace/packs_sys/logicmoo_opencog/guile-persist
autoreconf -i
./configure
make ; make install
)
(
cd /opt/logicmoo_workspace/packs_sys/logicmoo_opencog/guile-syntax-parse
autoreconf -i
./configure
make ; make install
)

(
cd /opt/logicmoo_workspace/packs_sys/logicmoo_opencog/fibers
./autogen.sh
autoreconf -i
./configure
make ; make install
)


```


cp /dockerstartup/generate_container_user /generate_container_user

export DEBIAN_FRONTEND=noninteractive

if [ "GOOD"=="INGORED" ]; then
   
      if [ ! -d "/usr/local/lib/python3.10-orig" ]; then
         \cp -a /usr/local/lib/python3.10/?* /usr/local/lib/python3.8/
         mv /usr/local/lib/python3.10 /usr/local/lib/python3.10-orig
         mv /usr/local/lib/python3.8 /usr/local/lib/python3.10
         \cp -a /home/opencog/.local/lib/python3.10/?* /usr/local/lib/python3.10
      fi
      
      if [ ! -d "/home/opencog/.local/lib/python3.10-orig" ]; then
         mv /home/opencog/.local/lib/python3.10 /home/opencog/.local/lib/python3.10-orig
         ln -s /usr/local/lib/python3.10 /home/opencog/.local/lib/python3.10
      fi
      
   
   curl -sS https://bootstrap.pypa.io/get-pip.py | /usr/bin/python3.10
   python -m pip install pip -U
   pip uninstall -y html5lib pyzmq zmq gevent greenlet spacy nltk nbconvert jupyter jupyterlab requests six gevent.websocket
   pip install html5lib pyzmq zmq gevent greenlet spacy nltk nbconvert jupyter jupyterlab requests six gevent.websocket
   
   (
   pip install jswipl 
   cd /usr/share/jupyter/kernels
   mkdir -p jswipl && cd jswipl
   wget https://raw.githubusercontent.com/targodan/jupyter-swi-prolog/master/kernel.json
   cat kernel.json
   )
   
   
   pip install nbnovnc
   jupyter serverextension enable  --py --sys-prefix nbnovnc
   jupyter nbextension     install --py --sys-prefix nbnovnc
   jupyter nbextension     enable  --py --sys-prefix nbnovnc
   
   (
   sudo ln -s /usr/lib/x86_64-linux-gnu /usr/lib64
   ln -s /usr/lib/x86_64-linux-gnu/libguile-3.0.so /usr/local/lib/libguile-3.0.so
   )
fi
