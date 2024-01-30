
function(add_fortran_library LIB)
    add_library(${LIB} ${ARGN})

    # set module path to LIB_DIR/mod
    get_target_property(LIB_DIR ${LIB} BINARY_DIR)
    set_target_properties(${LIB} PROPERTIES Fortran_MODULE_DIRECTORY ${LIB_DIR}/mod)

    # making LIB_DIR/mod available for libraries linking LIB 
    target_include_directories(${LIB} INTERFACE ${LIB_DIR}/mod)
endfunction(add_fortran_library)
add_library(mt STATIC multithreading.cpp)
add_fortran_library(mt-fortran STATIC fortran-interop.f90 fortran-interop.cpp)
target_compile_options(mt PRIVATE -fPIC)
target_compile_options(mt-fortran PRIVATE -fPIC -ffree-line-length-none)
target_link_libraries(mt PRIVATE Threads::Threads ${MPI_CXX_LIBRARIES})
target_link_libraries(mt-fortran mt)
add_custom_command(TARGET mt-fortran POST_BUILD
                   COMMAND ${CMAKE_COMMAND} -E copy_directory
                        $<TARGET_FILE_DIR:mt-fortran>/mod ${CMAKE_SOURCE_DIR}/include/)
