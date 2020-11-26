CMAKE_MINIMUM_REQUIRED ( VERSION 2.8 )

add_subdirectory (src/Swicli.Library)
add_subdirectory (src/PlConsole)

PROJECT ( YAP_SWICLI C )


SET ( YAP_SWICLI_VERSION 1.1.1)

SET(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake ${CMAKE_MODULE_PATH})

SET( CMAKE_FIND_FRAMEWORK LAST)
SET( CMAKE_FIND_APPBUNDLE LAST)

if (NOT YAP_FOUND)
  FIND_PACKAGE(YAP REQUIRED)
ENDIF (NOT YAP_FOUND)

IF (NOT YAP_FOUND)
  MESSAGE (SEND_ERROR "YAP was not found!")
ENDIF (NOT YAP_FOUND)


find_package(Mono REQUIRED)

IF (MONO_FOUND)
  #  MONO_FOUND       - system has MONO
  #  MONO_LIBRARIES   - Link these to use MONO
  #  MONO_INCLUDE_DIR - Include directory for using MONO

add_subdirectory (src/swicli32)
add_subdirectory (src/swicli64)
add_subdirectory (src/swicliYap64)

if (YAP_ROOT)
	set(YAP_SYSTEM_OPTIONS "MONO " ${YAP_SYSTEM_OPTIONS} PARENT_SCOPE)
	set(YAP_SYSTEM_OPTIONS "SWICLI " ${YAP_SYSTEM_OPTIONS} PARENT_SCOPE)
endif()

INSTALL(FILES prolog/swicli.pl DESTINATION ${libpl})
INSTALL(FILES prolog/swicffi.pl DESTINATION ${libpl})

ENDIF (MONO_FOUND)



