set(SCRIPT_DIR ${CMAKE_SOURCE_DIR}/scripts)

add_test(
    NAME checkall
    COMMAND bash -c "KATER=$<TARGET_FILE:${PROJECT_NAME}> ${SCRIPT_DIR}/checkall.sh"
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)
set_tests_properties(checkall PROPERTIES LABELS "integration" TIMEOUT 600)

# Unit tests
add_subdirectory(unit)
