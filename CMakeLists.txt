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

# Find ESMF
if(NOT ESMF_FOUND)
  find_package(ESMF 8.8.0 REQUIRED)
endif()

# Find Catalyst
if(COP_USE_CATALYST)
  find_package(catalyst 2.0 REQUIRED)
endif()

# Find Conduit
if(COP_USE_PYTHON)
  find_package(Conduit REQUIRED)
endif()

if(CONDUIT_PYTHON_ENABLED OR CATALYST_USE_PYTHON)
  # Find Python
  set(Python_FIND_STRATEGY "LOCATION")
  find_package(Python COMPONENTS Interpreter Development)
  set(PYTHON_EXECUTABLE "${Python_EXECUTABLE}")
  # Setup Python
  include(cmake/SetupPython.cmake)
  # Conduit requires c++14 support
  set(CMAKE_CXX_STANDARD 14)
  set(CMAKE_CXX_STANDARD_REQUIRED ON)

  message("Python_INCLUDE_DIRS  = ${Python_INCLUDE_DIRS}")
  message("Python_EXECUTABLE    = ${Python_EXECUTABLE}")
  message("Python_EXECUTABLE    = ${PYTHON_EXECUTABLE}")
  message("Python_LIBRARY_DIRS  = ${Python_LIBRARY_DIRS}")
  message("PYTHON_CONFIG_LIBDIR = ${PYTHON_CONFIG_LIBDIR}")
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
  find_package(catalyst 2.0 REQUIRED)
  target_include_directories(cop PUBLIC ${CATALYST_PYTHONPATH})
  target_link_libraries(cop PRIVATE catalyst::catalyst catalyst::catalyst_fortran)
endif()

# Support for Python
add_subdirectory(plugins/python)
add_dependencies(cop cop_python)
target_link_libraries(cop PRIVATE cop_python)
if(COP_USE_PYTHON)
  target_include_directories(cop PUBLIC ${PYTHON_INCLUDE_DIR})
  target_link_libraries(cop PRIVATE conduit::conduit conduit::conduit_python conduit::conduit_mpi)
  target_link_libraries(cop PRIVATE ${PYTHON_LIBRARY})
endif()

get_cmake_property(_variableNames VARIABLES)
list (SORT _variableNames)
foreach (_variableName ${_variableNames})
    if (_variableName MATCHES catalyst OR _variableName MATCHES Catalyst OR _variableName MATCHES CATALYST)
        message(STATUS "${_variableName}=${${_variableName}}")
    endif()
endforeach()

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
