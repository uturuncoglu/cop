project(cop_python Fortran)
if(COP_USE_PYTHON)
  set(SRCFILES cop_phases_python.F90
	       ../../cop_comp_internalstate.F90
               ../../cop_comp_shr.F90)
else()
  set(SRCFILES cop_phases_python_void.F90)
endif()
add_library(cop_python ${SRCFILES})
target_include_directories(cop_python PRIVATE ${CONDUIT_INCLUDE_DIRS})
install(TARGETS cop_python
        EXPORT  cop_python
        ARCHIVE DESTINATION lib
        LIBRARY DESTINATION lib
        COMPONENT Library)
install(EXPORT  cop_python
        DESTINATION lib/cmake)
