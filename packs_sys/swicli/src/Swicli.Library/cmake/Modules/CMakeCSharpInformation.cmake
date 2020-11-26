
# mcs [options] source-files
#    --about              About the Mono C# compiler
#    -addmodule:M1[,Mn]   Adds the module to the generated assembly
#    -checked[+|-]        Sets default aritmetic overflow context
#    -clscheck[+|-]       Disables CLS Compliance verifications
#    -codepage:ID         Sets code page to the one in ID (number, utf8, reset)
#    -define:S1[;S2]      Defines one or more conditional symbols (short: -d)
#    -debug[+|-], -g      Generate debugging information
#    -delaysign[+|-]      Only insert the public key into the assembly (no signing)
#    -doc:FILE            Process documentation comments to XML file
#    -help                Lists all compiler options (short: -?)
#    -keycontainer:NAME   The key pair container used to sign the output assembly
#    -keyfile:FILE        The key file used to strongname the ouput assembly
#    -langversion:TEXT    Specifies language version: ISO-1, ISO-2, 3, Default, or Future
#    -lib:PATH1[,PATHn]   Specifies the location of referenced assemblies
#    -main:CLASS          Specifies the class with the Main method (short: -m)
#    -noconfig            Disables implicitly referenced assemblies
#    -nostdlib[+|-]       Does not reference mscorlib.dll library
#    -nowarn:W1[,Wn]      Suppress one or more compiler warnings
#    -optimize[+|-]       Enables advanced compiler optimizations (short: -o)
#    -out:FILE            Specifies output assembly name
#    -pkg:P1[,Pn]         References packages P1..Pn
#    -platform:ARCH       Specifies the target platform of the output assembly
#                         ARCH can be one of: anycpu, x86, x64 or itanium
#    -recurse:SPEC        Recursively compiles files according to SPEC pattern
#    -reference:A1[,An]   Imports metadata from the specified assembly (short: -r)
#    -reference:ALIAS=A   Imports metadata using specified extern alias (short: -r)
#    -sdk:VERSION         Specifies SDK version of referenced assemlies
#                         VERSION can be one of: 2 (default), 4
#    -target:KIND         Specifies the format of the output assembly (short: -t)
#                         KIND can be one of: exe, winexe, library, module
#    -unsafe[+|-]         Allows to compile code which uses unsafe keyword
#    -warnaserror[+|-]    Treats all warnings as errors
#    -warnaserror[+|-]:W1[,Wn] Treats one or more compiler warnings as errors
#    -warn:0-4            Sets warning level, the default is 4 (short -w:)
#    -helpinternal        Shows internal and advanced compiler options
# 
# Resources:
#    -linkresource:FILE[,ID] Links FILE as a resource (short: -linkres)
#    -resource:FILE[,ID]     Embed FILE as a resource (short: -res)
#    -win32res:FILE          Specifies Win32 resource file (.res)
#    -win32icon:FILE         Use this icon for the output
#    @file                   Read response file for more options
# 
# Options can be of the form -option or /option

# <TARGET> <TARGET_BASE> <OBJECT> <OBJECTS> <LINK_LIBRARIES> <FLAGS> <LINK_FLAGS> <SOURCE> <SOURCES>

set(CMAKE_CSharp_ECHO_ALL "echo \"TARGET: <TARGET> TARGET_BASE: <TARGET_BASE> ")
set(CMAKE_CSharp_ECHO_ALL "${CMAKE_CSharp_ECHO_ALL} OBJECT: <OBJECT> OBJECTS: <OBJECTS> OBJECT_DIR: <OBJECT_DIR> SOURCE: <SOURCE> SOURCES: <SOURCES> ")
set(CMAKE_CSharp_ECHO_ALL "${CMAKE_CSharp_ECHO_ALL} LINK_LIBRARIES: <LINK_LIBRARIES> FLAGS: <FLAGS> LINK_FLAGS: <LINK_FLAGS> \"")

if(NOT CMAKE_CSharp_CREATE_SHARED_LIBRARY)
	set(CMAKE_CSharp_CREATE_SHARED_LIBRARY
		"echo \"CMAKE_CSharp_CREATE_SHARED_LIBRARY\""
		"${CMAKE_CSharp_ECHO_ALL}"
		"${CMAKE_CSharp_COMPILER} /target:library <OBJECTS> /out:<TARGET>")
endif()

if(NOT CMAKE_CSharp_CREATE_SHARED_MODULE)
	set(CMAKE_CSharp_CREATE_SHARED_MODULE
		"echo \"CMAKE_CSharp_CREATE_SHARED_MODULE\""
		"${CMAKE_CSharp_ECHO_ALL}"
		"${CMAKE_CSharp_COMPILER} /target:module <OBJECTS> /out:<TARGET>")
endif()

if(NOT CMAKE_CSharp_CREATE_STATIC_LIBRARY)
	set(CMAKE_CSharp_CREATE_STATIC_LIBRARY
		"echo \"CMAKE_CSharp_CREATE_STATIC_LIBRARY\""
		"${CMAKE_CSharp_ECHO_ALL}"
		"${CMAKE_CSharp_COMPILER} /target:library <OBJECTS> /out:<TARGET>")
endif()

if(NOT CMAKE_CSharp_COMPILE_OBJECT)
	set(CMAKE_CSharp_COMPILE_OBJECT
		"echo \"CMAKE_CSharp_COMPILE_OBJECT\""
		"${CMAKE_CSharp_ECHO_ALL}"
		"cp <SOURCE> <OBJECT>")
endif()

if(NOT CMAKE_CSharp_LINK_EXECUTABLE)
	set(CMAKE_CSharp_LINK_EXECUTABLE
		"echo \"CMAKE_CSharp_LINK_EXECUTABLE\""
		"${CMAKE_CSharp_ECHO_ALL}"
		"${CMAKE_CSharp_COMPILER} /target:exe <OBJECTS> /reference:${LIBRARY_OUTPUT_PATH}/HelloLibrary /out:<TARGET>.exe")
endif()

mark_as_advanced(
	CMAKE_CSharp_FLAGS
	CMAKE_CSharp_FLAGS_DEBUG
	CMAKE_CSharp_FLAGS_MINSIZEREL
	CMAKE_CSharp_FLAGS_RELEASE
	CMAKE_CSharp_FLAGS_RELWITHDEBINFO)

set(CMAKE_CSharp_INFORMATION_LOADED 1)

