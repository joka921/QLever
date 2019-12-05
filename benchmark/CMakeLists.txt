add_executable(Benchmark-Simple benchmark-simple.cpp)
target_link_libraries(Benchmark-Simple benchmark index ${CMAKE_THREAD_LIBS_INIT})

