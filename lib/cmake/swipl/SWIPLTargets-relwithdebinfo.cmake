#----------------------------------------------------------------
# Generated CMake target import file for configuration "RelWithDebInfo".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "swipl::swipl" for configuration "RelWithDebInfo"
set_property(TARGET swipl::swipl APPEND PROPERTY IMPORTED_CONFIGURATIONS RELWITHDEBINFO)
set_target_properties(swipl::swipl PROPERTIES
  IMPORTED_LOCATION_RELWITHDEBINFO "${_IMPORT_PREFIX}/lib/swipl/bin/x86_64-linux/swipl"
  )

list(APPEND _IMPORT_CHECK_TARGETS swipl::swipl )
list(APPEND _IMPORT_CHECK_FILES_FOR_swipl::swipl "${_IMPORT_PREFIX}/lib/swipl/bin/x86_64-linux/swipl" )

# Import target "swipl::libswipl" for configuration "RelWithDebInfo"
set_property(TARGET swipl::libswipl APPEND PROPERTY IMPORTED_CONFIGURATIONS RELWITHDEBINFO)
set_target_properties(swipl::libswipl PROPERTIES
  IMPORTED_LOCATION_RELWITHDEBINFO "${_IMPORT_PREFIX}/lib/swipl/lib/x86_64-linux/libswipl.so.8.3.11"
  IMPORTED_SONAME_RELWITHDEBINFO "libswipl.so.8"
  )

list(APPEND _IMPORT_CHECK_TARGETS swipl::libswipl )
list(APPEND _IMPORT_CHECK_FILES_FOR_swipl::libswipl "${_IMPORT_PREFIX}/lib/swipl/lib/x86_64-linux/libswipl.so.8.3.11" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
