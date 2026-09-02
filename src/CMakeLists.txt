set(SOURCES
  Builtins.cpp
  Config.cpp
  Constraint.cpp
  Counterexample.cpp
  DbgInfo.cpp
  Error.cpp
  GenMCPrinter.cpp
  Kater.cpp
  KatModule.cpp
  Logger.cpp
  NFA.cpp
  NFAUtils.cpp
  ParsingDriver.cpp
  Predicate.cpp
  Printer.cpp
  Relation.cpp
  RegExp.cpp
  Saturation.cpp
  Theory.cpp
  TransLabel.cpp
  Utils.cpp
)

# need bison and flex
find_package(FLEX 2.6 REQUIRED)
find_package(BISON 3.2 REQUIRED)

# generate flex/bison sources out of source (clean git; helpful for linting)
flex_target(LEXER Lexer.lpp ${CMAKE_CURRENT_BINARY_DIR}/Lexer.cpp)
bison_target(PARSER Parser.ypp ${CMAKE_CURRENT_BINARY_DIR}/Parser.cpp
  DEFINES_FILE ${CMAKE_CURRENT_BINARY_DIR}/Parser.hpp)
add_flex_bison_dependency(LEXER PARSER)

# link with TBB if present
find_package(TBB)

# the logger serializes concurrent diagnostics with a mutex
find_package(Threads REQUIRED)

add_library(kater_lib ${SOURCES} ${BISON_PARSER_OUTPUTS} ${FLEX_LEXER_OUTPUTS})
target_include_directories(kater_lib
  PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
  PRIVATE ${CMAKE_CURRENT_BINARY_DIR}) # generated files include handwritten
target_compile_features(kater_lib PUBLIC cxx_std_23)
target_link_libraries(kater_lib PRIVATE ${FLEX_LIBRARIES} Threads::Threads)

if(TBB_FOUND)
  target_link_libraries(kater_lib PRIVATE TBB::tbb)
else()
  message(TBB not found. Assertion parallelization disabled.)
endif()

option(KATER_DEBUG "Enable kater debugging infrastructure (ON for Debug)" OFF)
target_compile_definitions(kater_lib
  PUBLIC
   $<$<OR:$<CONFIG:Debug>,$<BOOL:${KATER_DEBUG}>>:ENABLE_KATER_DEBUG>
)
add_executable(${PROJECT_NAME} main.cpp)
target_include_directories(${PROJECT_NAME} PRIVATE ${CMAKE_CURRENT_BINARY_DIR})
target_link_libraries(${PROJECT_NAME} PRIVATE kater_lib)

install(TARGETS ${PROJECT_NAME} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
