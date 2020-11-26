CMAKE_MINIMUM_REQUIRED ( VERSION 2.8 )

if (YAP_ROOT)
# SET(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/../../cmake ${CMAKE_MODULE_PATH})
endif()

#set(CMAKE_C_COMPILER_ARG1 "-m64")
#find_package(Mono REQUIRED)
#FIND_PACKAGE(YAP REQUIRED)

add_library (swicli64 SHARED swicli64.c)
  
include_directories (${MONO_INCLUDE_DIRS} )
# target_link_libraries(swicli64 libYap ${MONO_LIBRARIES} ${IKVM_LIBRARIES})
  
  target_link_libraries(swicli64  libYap ${MONO_LIBRARIES})

set_target_properties(swicli64 PROPERTIES OUTPUT_NAME swicli64)
set_target_properties(swicli64 PROPERTIES INSTALL_RPATH_USE_LINK_PATH TRUE )
  
if (YAP_ROOT)
  # set(YAP_SYSTEM_OPTIONS "swicli64 " ${YAP_SYSTEM_OPTIONS} PARENT_SCOPE)
endif()

# for testing locally
if(NOT dlls) 
  set(dlls ${CMAKE_CURRENT_BINARY_DIR}/output )
endif()

install(TARGETS  swicli64 LIBRARY DESTINATION ${dlls}  )


