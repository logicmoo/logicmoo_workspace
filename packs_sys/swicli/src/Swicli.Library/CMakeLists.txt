cmake_minimum_required(VERSION 2.8 FATAL_ERROR)

PROJECT( Swicli.Library.dll )

set(${PROJECT_NAME}_SRC

	Properties/AssemblyInfo.cs
	Arrays.cs
	AssemblyLoader.cs
	ClassBuilder.cs
	Coercion.cs
	CoercionLayout.cs
	Collections.cs
	DelegateObjectInProlog.cs
	DesignByContract.cs
	EventHandlerInProlog.cs
	Exceptions.cs
	ForeignMethod.cs
	GenericDelegate.cs
	GetSet.cs
	IKVMClassLoader.cs
	Init.cs
	Instance.cs
	Invoke.cs
	LibPl.cs
	LockInfo.cs 
	NativeMethods.cs
	NondetContextHandle.cs
	PInvokeByEmit.cs
	PInvokeDLL.cs
	PInvokeMetaObject.cs
	PrologBackedDictionary.cs
	PrologClient.cs
	PrologCLR.cs
	PrologCLRExt.cs
	RuntimeTypeInfo.cs
	SWI-cs.cs
	SWI-cs-PlQuery.cs
	Threads.cs
	Tracker.cs
	TypeSystem.cs
	WaitUntilEvent.cs
)



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

