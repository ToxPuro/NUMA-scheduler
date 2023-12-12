add_executable(reduction-test main.cpp)
target_include_directories(reduction-test PRIVATE include)
target_link_libraries(
  reduction-test
  mt
)

