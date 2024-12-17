project(cop_python Fortran C)
if(COP_USE_PYTHON)
  set(SRCFILES cop_phases_python.F90
               cop_interface.cpp
               python_interpreter.cpp
	       ../../cop_comp_internalstate.F90
               ../../cop_comp_shr.F90)
else()
  set(SRCFILES cop_phases_python_void.F90)
endif()

get_cmake_property(_variableNames VARIABLES)
list (SORT _variableNames)
foreach (_variableName ${_variableNames})
    message(STATUS "${_variableName}=${${_variableName}}")
endforeach()

add_library(cop_python ${SRCFILES})
target_include_directories(cop_python PRIVATE ${CONDUIT_INCLUDE_DIRS})
target_include_directories(cop_python PRIVATE ${Python_INCLUDE_DIRS})
target_include_directories(cop_python PRIVATE ${CONDUIT_PYTHON_MODULE_DIR}/conduit)

install(TARGETS cop_python
        EXPORT  cop_python
        ARCHIVE DESTINATION lib
        LIBRARY DESTINATION lib
        COMPONENT Library)
install(EXPORT  cop_python
        DESTINATION lib/cmake)
