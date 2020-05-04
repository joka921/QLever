#find_package(HPX REQUIRED)
project(QLeverBenchmark)
add_executable(Benchmark-Simple benchmark-simple.cpp)
target_link_libraries(Benchmark-Simple benchmark ${CMAKE_THREAD_LIBS_INIT})

#add_hpx_executable(hpx-sort SOURCES hpx-sort.cpp COMPONENT_DEPENDENCIES iostreams)
