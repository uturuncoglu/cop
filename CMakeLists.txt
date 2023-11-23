cmake_minimum_required(VERSION 3.19)

project(cop
        VERSION 1.0
        LANGUAGES C CXX Fortran)

###############################################################################
### Add module directory 
###############################################################################

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)

###############################################################################
### Set required packages
###############################################################################

find_package(ESMF REQUIRED)
find_package(catalyst 2.0 REQUIRED)

###############################################################################
### File lists and macros
###############################################################################

list(APPEND cop_src_files
  cop_comp_nuopc.F90)

###############################################################################
### Set variables
###############################################################################

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

###############################################################################
### Application
###############################################################################

add_library(cop STATIC  ${cop_src_files})
target_link_libraries(cop PRIVATE esmf catalyst::catalyst catalyst::catalyst_fortran)
target_include_directories(cop PRIVATE ${ESMF_F90COMPILEPATHS}) 

###############################################################################
### Install
###############################################################################

install(
  TARGETS cop
  EXPORT  cop-config
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib
  COMPONENT Library)

install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod DESTINATION ${CMAKE_INSTALL_PREFIX})

install(EXPORT      cop-config
        DESTINATION lib/cmake )
