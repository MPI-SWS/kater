set(SOURCES
  main.cpp
  Builtins.cpp
  Config.cpp
  Constraint.cpp
  Counterexample.cpp
  Error.cpp
  Kater.cpp
  KatModule.cpp
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

# generate targets in source to include freely...
flex_target(LEXER Lexer.lpp ${CMAKE_CURRENT_SOURCE_DIR}/Lexer.cpp)
bison_target(PARSER Parser.ypp ${CMAKE_CURRENT_SOURCE_DIR}/Parser.cpp)
add_flex_bison_dependency(LEXER PARSER)

# link with TBB if present
find_package(TBB)

add_executable(${PROJECT_NAME} ${SOURCES} ${BISON_PARSER_OUTPUTS} ${FLEX_LEXER_OUTPUTS})
target_include_directories(${PROJECT_NAME} PRIVATE ${CMAKE_CURRENT_BINARY_DIR})
target_compile_features(${PROJECT_NAME} PRIVATE cxx_std_20)
target_link_libraries(${PROJECT_NAME} PRIVATE ${FLEX_LIBRARIES})
if(TBB_FOUND)
  target_link_libraries(${PROJECT_NAME} PRIVATE TBB::tbb)
else()
  message(TBB not found. Assertion parallelization disabled.)
endif()

option(KATER_DEBUG "Enable kater debugging infrastructure (ON for Debug)" OFF)
target_compile_definitions(${PROJECT_NAME}
  PRIVATE
   $<$<OR:$<CONFIG:Debug>,$<BOOL:${KATER_DEBUG}>>:ENABLE_KATER_DEBUG>
)
