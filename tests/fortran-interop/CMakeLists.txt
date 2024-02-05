
if(USE_OPENMP)
	add_executable(interop-test f-openmp.f90)
else()
	add_executable(interop-test f.f90)
endif()
find_package(OpenMP COMPONENTS Fortran)
target_compile_options(interop-test PRIVATE -fopenmp)
target_link_options(interop-test PRIVATE -fopenmp)
target_link_libraries(
  interop-test
  mt
  mt-fortran
  OpenMP::OpenMP_Fortran
)

