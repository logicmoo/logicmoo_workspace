#!/bin/bash

#-Wl,-R,$(YAPLIBDIR) -Wl,-R,$(LIBDIR)
#  $(CC) -export-dynamic swicli4.o  $(LDFLAGS) -o $(SWICLI_SO) ;\
# swicli64.o : src/swicli64/swicli64.c $(CC) -c $(CFLAGSSWICLI64) src/swicli64/swicli64.c -o swicli64.o

target="$1"

export WITH_IKVM="-define:USE_IKVM -r:jpl7,IKVM.OpenJDK.Core,IKVM.OpenJDK.Util,IKVM.Runtime,System.Windows.Forms"

if [ -z "$ODIR" ]; then export ODIR="./lib"; fi
if [ -z "$DMCS_OPTS" ]; then DMCS_OPTS=" -lib:${ODIR}/ -define:PROLOG_SWI -unsafe -warn:0 -r:System.Drawing ${WITH_IKVM}"; fi
if [ -z "$EXTRA_C_FLAGS" ]; then export EXTRA_C_FLAGS="-Wno-unused-result `pkg-config --cflags --libs monosgen-2`"; fi


if [ "$target" == "prepare" ]; then
mkdir -p ${ODIR}
fi

if [ "$target" == "clean" ]; then
echo removing previous build
find ${ODIR} -ipath "*swicli*"  -not -ipath "*win*" -not -ipath "*Symbols*" -exec rm -f '{}' \;
rm -rf ./src/?*/lib/ ./src/?*/obj/ ./src/?*/bin/ ./src/?*/Debug/ ./src/?*/Release/ ./obj ./src/obj ./src/lib ./src/Debug
fi

if [ "$target" == "prepare" ]; then
export LIBARCH=./lib/x86_64-linux
mkdir -p ${LIBARCH}/
fi

if [ "$target" == "compile" ]; then
export LIBARCH=./lib/i386-linux
cp src/Swicli.Library/app.config ${LIBARCH}/swicli.dll.config
swipl-ld -m64 src/swicli/swicli.c $EXTRA_C_FLAGS -shared -o ${LIBARCH}/swicli.so
fi

if [ "$target" == "prepare" ]; then
export LIBARCH=./lib/i386-linux
mkdir -p ${LIBARCH}/
fi

if [ "$target" == "compile" ]; then
export LIBARCH=./lib/i386-linux
cp src/Swicli.Library/app.config ${LIBARCH}/swicli.dll.config
swipl-ld -m32 src/swicli/swicli.c $EXTRA_C_FLAGS -shared -o ${LIBARCH}/swicli.so
echo local C build complete!
fi


if [ "$target" == "install" ]; then
mkdir -p lib/amd64/
export LIBARCH=./lib/x86_64-linux
cp -a ${LIBARCH}/?* lib/amd64/
fi


if [ "$target" == "prepare" ] || [ "$target" == "csharp" ]; then
mkdir -p ${ODIR}
fi

if [ "$target" == "prepare" ] || [ "$target" == "csharp" ]; then
cp src/Swicli.Library/app.config ${ODIR}/Swicli.Library.dll.config
fi

if [ "$target" == "compile" ] || [ "$target" == "csharp" ]; then
echo doing local C# build
mcs ${DMCS_OPTS} src/Swicli.Library/?*.cs -out:${ODIR}/PInvokeTest
mcs ${DMCS_OPTS} src/Swicli.Library/?*.cs -out:${ODIR}/Swicli.Library.dll
mcs ${DMCS_OPTS}  src/SWICLITestDLL/?*.cs -r:Swicli.Library -out:${ODIR}/SWICLITestDLL.dll
mcs ${DMCS_OPTS}   src/SWICFFITests/?*.cs -r:Swicli.Library -out:${ODIR}/SWICFFITests.exe
mcs -lib:/usr/lib/mono/2.0 -pkg:dotnet src/Example4SWICLI/?*.cs -out:${ODIR}/Example4SWICLI.dll
rm -rf ./src/?*/lib/ ./src/?*/obj/ ./src/?*/Debug/ ./src/?*/Release/ ./obj ./src/obj ./src/lib ./src/Debug
echo local C# build complete!
fi

if [ "$target" == "install" ]; then
echo ODIR=$ODIR
find $ODIR -xdev -iname "*.so" -or -iname "*.dll" -or -iname "*.pdb" -or -iname "*.lib" -or -iname "*.dll.config" -or -iname "*.pl" -or -iname "*.cffi"  -or -iname "*.exe"
echo Done
fi


