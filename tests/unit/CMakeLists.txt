# Prefer a packaged googletest (no network in CI); fall back to fetching it
find_package(GTest QUIET)
if(NOT GTest_FOUND)
  include(FetchContent)
  FetchContent_Declare(
    googletest
    URL https://github.com/google/googletest/archive/refs/tags/v1.14.0.zip
    URL_HASH SHA256=1f357c27ca988c3f7c6b4bf68a9395005ac6761f034046e9dde0896e3aba00e4
  )
  # Keep googletest out of `cmake --install`
  set(INSTALL_GTEST OFF CACHE BOOL "" FORCE)
  FetchContent_MakeAvailable(googletest)
endif()

add_executable(unit_tests
  test_findSimilarStates.cpp
  test_NFA.cpp
  test_NFAUtils.cpp
  test_RegExp.cpp
)
target_link_libraries(unit_tests PRIVATE kater_lib GTest::gtest_main)

# register with ctest
include(GoogleTest)
gtest_discover_tests(unit_tests PROPERTIES LABELS "unit")

# Timings only; kept out of the `unit` label so normal runs stay fast
add_executable(kater_bench bench_similarity.cpp)
target_link_libraries(kater_bench PRIVATE kater_lib)

add_test(NAME bench-similarity COMMAND kater_bench)
set_tests_properties(bench-similarity PROPERTIES LABELS "benchmark" TIMEOUT 600)
