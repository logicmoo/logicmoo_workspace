include(LibFindMacros)

set(MONO_ROOT $ENV{MONO_ROOT})

find_path(MONO_INCLUDE_DIR
  NAMES mono/jit/jit.h
  HINTS
  ${MONO_ROOT}/include/mono-2.0
  PATHS
  /usr/include/mono-2.0
  )

find_library(MONO_LIBRARY_DEBUG
  NAMES mono-2.0.lib libmono-2.0.so
  PATHS ${MONO_ROOT}/lib
  /usr/lib
)

find_library(MONO_LIBRARY_RELEASE
  NAMES mono-2.0.lib libmono-2.0.so
  PATHS ${MONO_ROOT}/lib
  /usr/lib
)

SET(MONO_LIBRARY optimized ${MONO_LIBRARY_RELEASE} debug ${MONO_LIBRARY_DEBUG})

set(MONO_PROCESS_INCLUDES MONO_INCLUDE_DIR)
set(MONO_PROCESS_LIBS MONO_LIBRARY)
libfind_process(MONO)