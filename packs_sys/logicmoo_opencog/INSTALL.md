

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
find -maxdepth 2  -name CMakeLists.txt -execdir bash -c "rm -rf build; mkdir build ; cd build ; cmake .. " \;
find -maxdepth 2  -name CMakeLists.txt -execdir bash -c "cd build ; make -j 12 ; make install" \;
find -maxdepth 2  -name guile.am -execdir bash -c "autoreconf -i" \;
find -maxdepth 2  -name configure -execdir bash -c "./configure" \;
find -maxdepth 2  -name Makefile -execdir bash -c "make -j 12 ; make install" \;
find -maxdepth 2  -name requirements-dev.txt -execdir bash -c "pip install -r requirements-dev.txt" \;
find -maxdepth 2  -name requirements.txt -execdir bash -c "pip install -r requirements.txt" \;
find -maxdepth 3  -name setup.py -execdir bash -c "pip install -e ." \;
```


## Guile/GuileLog
```bash
#git clone https://git.sv.gnu.org/git/guile.git 
git clone https://github.com/wingo/fibers
#git clone https://gitlab.com/bdw-gc-logical-mod/bdw-gc-logical-mod 
git clone https://gitlab.com/tampe/guile-persist 
git clone https://gitlab.com/guile-syntax-parse/guile-syntax-parse.git 
git clone https://gitlab.com/gule-log/guile-log 
git clone https://github.com/logicmoo/QuProlog
```


