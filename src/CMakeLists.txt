add_library(mt multithreading.cpp)
target_link_libraries(mt PRIVATE Threads::Threads ${MPI_CXX_LIBRARIES})
