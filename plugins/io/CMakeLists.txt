project(cop_io Fortran)
set(SRCFILES cop_phases_io.F90)
add_library(cop_io ${SRCFILES})
install(TARGETS cop_io
        EXPORT  cop_io
        ARCHIVE DESTINATION lib
        LIBRARY DESTINATION lib
        COMPONENT Library)
install(EXPORT  cop_io 
        DESTINATION lib/cmake)
