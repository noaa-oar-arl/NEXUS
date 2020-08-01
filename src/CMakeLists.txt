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
target_link_libraries(NEXUS_Shared
        PUBLIC HCOI_Shared
        PUBLIC HCOI_Standalone
        PUBLIC ${ESMF}
)

include_directories(
  /scratch1/NCEPDEV/nems/emc.nemspara/soft/esmf/8.0.0-intel18.0.5.274-impi2018.0.4-netcdf4.7.4_parallel/mod
  /scratch1/NCEPDEV/nems/emc.nemspara/soft/netcdf_parallel/include
)
target_link_libraries(nexus
        PUBLIC NEXUS_Shared
)
set_target_properties(nexus PROPERTIES
	RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
	LINK_FLAGS "-Wl,-rpath,/scratch1/NCEPDEV/nems/emc.nemspara/soft/esmf/8.0.0-intel18.0.5.274-impi2018.0.4-netcdf4.7.4_parallel/lib  -Wl,-rpath,/scratch1/NCEPDEV/nems/emc.nemspara/soft/netcdf_parallel/lib -L/scratch1/NCEPDEV/nems/emc.nemspara/soft/esmf/8.0.0-intel18.0.5.274-impi2018.0.4-netcdf4.7.4_parallel/lib -L/scratch1/NCEPDEV/nems/emc.nemspara/soft/netcdf_parallel/lib -lesmf  -cxxlib -lrt -ldl -lnetcdff -lnetcdf")

install(TARGETS nexus
	RUNTIME DESTINATION ${RUNDIR}
)
