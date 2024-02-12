cmake_minimum_required(VERSION 3.15...3.28)
project(kater VERSION 0.2 LANGUAGES CXX)

# clang-tidy integration
option(TIDY "Run clang-tidy")
if(TIDY)
  find_program(CLANG_TIDY clang-tidy REQUIRED)
  set(CMAKE_CXX_CLANG_TIDY
    clang-tidy;
    -header-filter=.*;
    -config-file=${CMAKE_SOURCE_DIR}/.clang-tidy
  )
endif()

set (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})
add_subdirectory(src)
