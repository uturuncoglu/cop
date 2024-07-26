project(cop_catalyst Fortran)
set(SRCFILES cop_phases_catalyst.F90)
if(COP_USE_CATALYST)
  add_definitions(-DUSE_CATALYST)
endif()
add_library(cop_catalyst ${SRCFILES})
install(TARGETS cop_catalyst
        EXPORT  cop_catalyst
        ARCHIVE DESTINATION lib
        LIBRARY DESTINATION lib
        COMPONENT Library)
install(EXPORT  cop_catalyst 
        DESTINATION lib/cmake)
