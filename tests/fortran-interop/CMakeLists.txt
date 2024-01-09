add_executable(interop-test c.cpp f.f90)
find_package(OpenMP COMPONENTS Fortran)
target_compile_options(interop-test PRIVATE -fopenmp)
target_link_libraries(
  interop-test
  mt
  OpenMP::OpenMP_Fortran
)

