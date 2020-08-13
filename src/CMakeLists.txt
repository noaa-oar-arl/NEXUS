add_executable(nexus
	nexus_driver.F90
)

add_library(NEXUS_Shared STATIC EXCLUDE_FROM_ALL
	nexus_error_mod.F90
	nexus_methods_mod.F90
)

target_include_directories(NEXUS_Shared
	INTERFACE ${HEMCO_BINARY_DIR}/mod
)

# find external libraries
find_package(MPI REQUIRED)
find_package(ESMF REQUIRED)
find_package(NetCDF REQUIRED COMPONENTS Fortran)

target_link_libraries(NEXUS_Shared
        PUBLIC HCOI_Shared
        PUBLIC esmf
)

target_link_libraries(nexus
        PUBLIC NEXUS_Shared
        PUBLIC esmf
        PUBLIC NetCDF::NetCDF_Fortran
)

set_target_properties(nexus PROPERTIES
	RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

install(TARGETS nexus
	RUNTIME DESTINATION ${RUNDIR}
)
