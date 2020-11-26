# - Try to find SWI-Prolog
# Once done this will define
#
#  LIBSWIPL_FOUND - system has SWI-Prolog
#  LIBSWIPL_INCLUDE_DIRS - the SWI-Prolog include directory
#  LIBSWIPL_LIBRARIES - Link these to use SWI-Prolog
#  LIBSWIPL_DEFINITIONS - Compiler switches required for using SWI-Prolog
#  SWIPL_ARCH - Architecture identifier used by SWI-Prolog
#
#  Copyright (c) 2019 Jan Wielemaker (jan@swi-prolog.org)
#
#  Redistribution and use is allowed according to the terms of the BSD-2
#  license.

if(NOT (LIBSWIPL_INCLUDE_DIR AND LIBSWIPL_LIBRARY))
  find_package(PkgConfig)
  pkg_check_modules(PC_LIBSWIPL QUIET swipl)

  find_program(
      SWIPL
      NAMES swipl swi-prolog)

  execute_process(
      COMMAND ${SWIPL} --dump-runtime-variables
      OUTPUT_VARIABLE swipl_output)

  string(REGEX MATCHALL [^\n]+\n lines ${swipl_output})
  foreach(line IN LISTS lines)
    string(REGEX REPLACE "^PL([A-Z]+)=.*" \\1 name ${line})
    string(REGEX REPLACE "^PL[A-Z]+=\"(.*)\".*" \\1 value ${line})
    set(SWIPL_${name} ${value})
    # message("SWIPL_${name} <- ${value}")
  endforeach()

  if(SWIPL_ARCH)
    set(SWIPL_ARCH ${SWIPL_ARCH} CACHE STRING
	"Architecture subdirectory for finding foreign libraries")
  endif()

  find_path(LIBSWIPL_INCLUDE_DIR
    NAMES
      SWI-Prolog.h
    PATHS
      ${SWIPL_BASE}/include
      /usr/include
      ${CMAKE_INCLUDE_PATH}
      ${CMAKE_INSTALL_PREFIX}/include
  )

  # SWIPL_BINARY_DIR deals with SWI-Prolog running from the build directory
  get_filename_component(SWIPL_BINARY_DIR ${SWIPL_BASE} DIRECTORY)
  find_library(LIBSWIPL_LIBRARY
    NAMES
      swipl
      libswipl
    PATHS
      ${SWIPL_BASE}/lib/${SWIPL_ARCH}
      ${SWIPL_BINARY_DIR}/src
      /usr/lib
      ${CMAKE_LIBRARY_PATH}
      ${CMAKE_INSTALL_PREFIX}/lib
  )
  mark_as_advanced(LIBSWIPL_INCLUDE_DIR LIBSWIPL_LIBRARY SWIPL_ARCH)

endif(NOT (LIBSWIPL_INCLUDE_DIR AND LIBSWIPL_LIBRARY))

# Finish up and create the final target

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(LIBSWIPL
    FOUND_VAR LIBSWIPL_FOUND
    REQUIRED_VARS LIBSWIPL_LIBRARY LIBSWIPL_INCLUDE_DIR)

if(LIBSWIPL_FOUND)
  set(LIBSWIPL_LIBRARIES ${LIBSWIPL_LIBRARY})
  set(LIBSWIPL_INCLUDE_DIRS ${LIBSWIPL_INCLUDE_DIR})
  set(LIBSWIPL_DEFINITIONS ${PC_LIBSWIPL_CFLAGS_OTHER})
endif()

if(LIBSWIPL_FOUND AND NOT TARGET LIBSWIPL::LIBSWIPL)
  add_library(LIBSWIPL::LIBSWIPL UNKNOWN IMPORTED)
  set_target_properties(
      LIBSWIPL::LIBSWIPL PROPERTIES
      IMPORTED_LOCATION "${LIBSWIPL_LIBRARY}"
      INTERFACE_COMPILE_OPTIONS "${PC_LIBSWIPL_CFLAGS_OTHER}"
      INTERFACE_INCLUDE_DIRECTORIES "${LIBSWIPL_INCLUDE_DIR}")
endif()
