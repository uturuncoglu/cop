project(geogate_io Fortran)
set(SRCFILES geogate_phases_io.F90
             ../../geogate_internalstate.F90
             ../../geogate_share.F90)
add_library(geogate_io ${SRCFILES})
install(TARGETS geogate_io
        EXPORT  geogate_io
        ARCHIVE DESTINATION lib
        LIBRARY DESTINATION lib
        COMPONENT Library)
install(EXPORT  geogate_io 
        DESTINATION geogate/cmake)
