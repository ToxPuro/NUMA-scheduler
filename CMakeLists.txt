cmake_minimum_required(VERSION 3.18)
project(numa-scheduler LANGUAGES C CXX Fortran)

option(USE_OPENMP "Use OpenMP for multithreading" OFF)

include(FortranCInterface)
# if you are working with C and Fortran
FortranCInterface_VERIFY()

# if you are working with C++ and Fortran
FortranCInterface_VERIFY(CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
include_directories(${CMAKE_SOURCE_DIR})
if(USE_OPENMP)
    find_package(OpenMP)
    if (OPENMP_FOUND)
        set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
        set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
        set (CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${OpenMP_EXE_LINKER_FLAGS}")
    endif()
    add_definitions(-DUSE_OPENMP)
endif()

set(THREADS_PREFER_PTHREAD_FLAG ON)

find_package(Threads REQUIRED)
find_package(MPI REQUIRED COMPONENTS CXX) 
include_directories(${MPI_CXX_INCLUDE_DIRS})
include_directories(include)

add_subdirectory(src)
add_subdirectory(tests)
