cmake_minimum_required(VERSION 3.18)
project(numa-scheduler LANGUAGES C CXX Fortran)

include(FortranCInterface)
# if you are working with C and Fortran
FortranCInterface_VERIFY()

# if you are working with C++ and Fortran
FortranCInterface_VERIFY(CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
include_directories(${CMAKE_SOURCE_DIR})
set(THREADS_PREFER_PTHREAD_FLAG ON)

find_package(Threads REQUIRED)
find_package(MPI REQUIRED COMPONENTS CXX) 
include_directories(${MPI_CXX_INCLUDE_DIRS})
include_directories(include)

add_subdirectory(src)
add_subdirectory(tests)
