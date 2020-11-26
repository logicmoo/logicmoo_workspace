
find_program(MONO_EXECUTABLE mono)
find_program(MCS_EXECUTABLE mcs)
find_program(GACUTIL_EXECUTABLE gacutil)

set(MONO_FOUND FALSE CACHE INTERNAL "")

if(MONO_EXECUTABLE AND MCS_EXECUTABLE AND GACUTIL_EXECUTABLE)
	set(MONO_FOUND TRUE CACHE INTERNAL "")

	# default GAC is located in <prefix>/lib/mono/gac
	find_path(MONO_GAC_PREFIX lib/mono/gac
		PATHS "/usr;/usr/local")

	set(MONO_GAC_PREFIX "${MONO_GAC_PREFIX}" CACHE PATH "Mono GAC prefix")
	set(MONO_GAC_DIR "${MONO_GAC_PREFIX}/lib/mono" CACHE PATH "Mono GAC directory")

	execute_process(COMMAND ${MCS_EXECUTABLE} --version OUTPUT_VARIABLE MONO_VERSION OUTPUT_STRIP_TRAILING_WHITESPACE)
	string(REGEX REPLACE ".*version ([^ ]+)" "\\1" MONO_VERSION "${MONO_VERSION}")
endif()

if(NOT MONO_FOUND)
	message(FATAL_ERROR "Could not find Mono!")
endif()

mark_as_advanced(MONO_EXECUTABLE MCS_EXECUTABLE GACUTIL_EXECUTABLE)
mark_as_advanced(MONO_GAC_PREFIX MONO_GAC_DIR MONO_VERSION)

