add_executable(interop-test c.c f.f90)
target_link_libraries(
  interop-test
  mt
)

