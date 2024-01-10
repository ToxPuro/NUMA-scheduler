add_library(mt STATIC multithreading.cpp)
target_compile_options(mt PRIVATE -fPIC)
target_link_libraries(mt PRIVATE Threads::Threads ${MPI_CXX_LIBRARIES})
