cmake_minimum_required(VERSION 3.19)

project(cop
        VERSION 1.0
        LANGUAGES C CXX Fortran)

###############################################################################
### Add module directory 
###############################################################################

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)

###############################################################################
### Build options
###############################################################################

option(COP_USE_CATALYST "Support to Use Catalyst for Insitu Visualization" OFF)
option(COP_USE_PYTHON "Support to Interact with Python" OFF)

###############################################################################
### Set required packages
###############################################################################

if(NOT ESMF_FOUND)
  find_package(ESMF REQUIRED)
endif()

if(COP_USE_CATALYST)
  find_package(catalyst 2.0 REQUIRED)
endif()

if(COP_USE_PYTHON)
  find_package(Conduit REQUIRED)
endif()

###############################################################################
### File lists and macros
###############################################################################

list(APPEND cop_src_files
  cop_comp_internalstate.F90
  cop_comp_nuopc.F90
  cop_comp_shr.F90)

###############################################################################
### Set variables
###############################################################################

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

###############################################################################
### Application
###############################################################################

add_library(cop STATIC  ${cop_src_files})
target_include_directories(cop PRIVATE ${ESMF_F90COMPILEPATHS}) 

# Support for I/O, which is provided by ESMF/NUOPC
add_subdirectory(plugins/io)
add_dependencies(cop cop_io)
target_link_libraries(cop PRIVATE cop_io)

# Support for ParaView Catalyst
add_subdirectory(plugins/catalyst)
add_dependencies(cop cop_catalyst)
target_link_libraries(cop PRIVATE cop_catalyst)
if(COP_USE_CATALYST)
  add_definitions(-DENABLE_CATALYST)
  target_link_libraries(cop PRIVATE catalyst::catalyst catalyst::catalyst_fortran)
endif()

# Support for Python
add_subdirectory(plugins/python)
add_dependencies(cop cop_python)
target_link_libraries(cop PRIVATE cop_python)
if(COP_USE_PYTHON)
  add_definitions(-DENABLE_PYTHON)
  target_link_libraries(cop PRIVATE conduit::conduit conduit::conduit_python conduit::conduit_mpi)
endif()

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
