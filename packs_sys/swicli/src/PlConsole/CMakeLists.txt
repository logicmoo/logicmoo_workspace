cmake_minimum_required(VERSION 2.8 FATAL_ERROR)

PROJECT( PlConsole.exe )


set(${PROJECT_NAME}_REFS

	Swicli.Library.dll

) 

set(${PROJECT_NAME}_SRC

	Properties/AssemblyInfo.cs
	Program.cs

)

FOREACH( file1  ${${PROJECT_NAME}_REFS} )
    SET( mono_options ${mono_options} -r:${file1} )
ENDFOREACH( file1 )

FOREACH( file1  ${${PROJECT_NAME}_SRC} )
    SET( arguments ${arguments} ${file1} )
ENDFOREACH( file1 )

set(MAKE_CMD dmcs -warn:0 -unsafe -lib:${dlls} ${mono_options} -define:PROLOG_YAP -out:${dlls}/${PROJECT_NAME} )

add_custom_target(${PROJECT_NAME} ALL ${MAKE_CMD} ${arguments}
	WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
        COMMENT ${dlls}/${PROJECT_NAME} SOURCES ${arguments}
#   https://cmake.org/pipermail/cmake/2007-March/013419.html says: next line prevents remaking the target each time (but we want to for a while until bugs are fixed)
#	OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}
)


# next line requires making of the dll
# add_custom_target(${PROJECT_NAME} ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME})
# else do 
# add_custom_target(${PROJECT_NAME} ALL )


# for testing locally
if(NOT dlls) 
  set(dlls ${CMAKE_CURRENT_BINARY_DIR}/output )
endif()

install (FILES ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME} DESTINATION ${dlls} )

