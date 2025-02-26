project(cop_catalyst Fortran)
if(COP_USE_CATALYST)
  set(SRCFILES cop_phases_catalyst.F90)
else()
  set(SRCFILES cop_phases_catalyst_void.F90)
endif()

add_library(cop_catalyst ${SRCFILES})

message(INFO "catalyst_DIR = ${catalyst_DIR}")
if(catalyst_FOUND)
  include_directories(${catalyst_DIR}/../../../include/catalyst-${catalyst_VERSION})
  if(EXISTS ${catalyst_DIR}/../../../include/catalyst-${catalyst_VERSION}/conduit)
    include_directories(${catalyst_DIR}/../../../include/catalyst-${catalyst_VERSION}/conduit)
  endif()
endif()

install(TARGETS cop_catalyst
        EXPORT  cop_catalyst
        ARCHIVE DESTINATION lib
        LIBRARY DESTINATION lib
        COMPONENT Library)
install(EXPORT  cop_catalyst 
        DESTINATION lib/cmake)
