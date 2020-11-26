cmake_minimum_required(VERSION 3.5)
project(swipl-pack-ssh-server)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

include(CheckIncludeFiles)
include(CheckSymbolExists)

find_package(SWI-Prolog REQUIRED)
find_package(LibSSH REQUIRED)

set(CMAKE_REQUIRED_DEFINITIONS "-D_GNU_SOURCE")
check_include_files(pty.h HAVE_PTY_H)
check_include_files(util.h HAVE_UTIL_H)
find_library(LIBUTIL NAMES util libutil)
check_symbol_exists(gettid "unistd.h" HAVE_GETTID)

include_directories(BEFORE ${CMAKE_CURRENT_BINARY_DIR})
configure_file(c/config.h.cmake config.h)

add_library(sshd4pl MODULE c/sshd4pl.c)
target_link_libraries(sshd4pl LIBSSH::LIBSSH LIBSWIPL::LIBSWIPL ${LIBUTIL})
set_target_properties(sshd4pl PROPERTIES PREFIX "")

install(TARGETS sshd4pl
	DESTINATION ${CMAKE_CURRENT_SOURCE_DIR}/lib/${SWIPL_ARCH})
