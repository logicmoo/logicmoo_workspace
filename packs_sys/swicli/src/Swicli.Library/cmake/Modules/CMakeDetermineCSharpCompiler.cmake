
if(NOT CMAKE_CSharp_COMPILER)
	find_package(Mono)
	if(MONO_FOUND)
		set(CMAKE_CSharp_COMPILER "${MCS_EXECUTABLE}")
		set(CMAKE_CSharp_COMPILER_ID "Mono")
		set(CMAKE_CSharp_COMPILER_VERSION "${MONO_VERSION}")
		set(CMAKE_CSharp_PLATFORM_ID "Mono")
	endif()
endif()

message(STATUS "Mono GAC Prefix: ${MONO_GAC_PREFIX}")
message(STATUS "Mono GAC Directory: ${MONO_GAC_DIR}")
message(STATUS "Mono Compiler Version: ${MONO_COMPILER_VERSION}")

mark_as_advanced(CMAKE_CSharp_COMPILER)

if(CMAKE_CSharp_COMPILER)
	set(CMAKE_CSharp_COMPILER_LOADED 1)
endif(CMAKE_CSharp_COMPILER)

configure_file(${CMAKE_LOCAL_ROOT}/Modules/CMakeCSharpCompiler.cmake.in
	${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeCSharpCompiler.cmake IMMEDIATE @ONLY)

set(CMAKE_CSharp_COMPILER_ENV_VAR "CSC")

