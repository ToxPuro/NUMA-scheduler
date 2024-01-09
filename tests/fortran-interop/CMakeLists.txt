add_executable(interop-test c.cpp f.f90)
target_link_libraries(
  interop-test
  mt
)

