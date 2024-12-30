# CMakeLists.txt for C99 Compiler Project

cmake_minimum_required(VERSION 3.14)
project(C99Compiler VERSION 1.0 LANGUAGES CXX)

# ===========================
# 1. Compiler Configuration
# ===========================

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Default build type if not specified
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Debug CACHE STRING "Build type" FORCE)
endif()

# ===========================
# 2. Source Files
# ===========================

# Collect all source files
file(GLOB_RECURSE SRCS "src/*.cpp")

# Define the main executable
add_executable(${PROJECT_NAME} ${SRCS})

# Include directories for main executable
target_include_directories(${PROJECT_NAME} PRIVATE ${PROJECT_SOURCE_DIR}/include)

# ===========================
# 3. Google Test Integration
# ===========================

# Enable testing
enable_testing()

# Include FetchContent module
include(FetchContent)

# Fetch Google Test
FetchContent_Declare(
  googletest
  GIT_REPOSITORY https://github.com/google/googletest.git
  GIT_TAG        release-1.12.1  # Updated to a valid tag
)

# For Windows: Prevent overriding the parent project's compiler/linker settings
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)

# Fetch and add Google Test
FetchContent_MakeAvailable(googletest)

# ===========================
# 4. Unit Tests
# ===========================

# Find all unit test source files
file(GLOB_RECURSE UNIT_TEST_SRCS "tests/unit/*_test.cpp")

# Add unit test executable
add_executable(run_unit_tests ${UNIT_TEST_SRCS} ${SRCS})

# Include directories for unit tests
target_include_directories(run_unit_tests PRIVATE ${PROJECT_SOURCE_DIR}/include)

# Link Google Test libraries
target_link_libraries(run_unit_tests PRIVATE gtest_main)

# Add unit tests to CTest
include(GoogleTest)
gtest_discover_tests(run_unit_tests)

# ===========================
# 5. End-to-End (e2e) Tests
# ===========================

# Custom target for e2e tests
add_custom_target(e2e_test
    COMMAND ${CMAKE_COMMAND} -E echo "Running end-to-end tests..."
    COMMAND bash ${PROJECT_SOURCE_DIR}/tests/scripts/run_e2e_tests.sh
    DEPENDS ${PROJECT_NAME}
    WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}/tests/scripts
    COMMENT "Executing e2e_test target..."
)

# ===========================
# 6. Installation (Optional)
# ===========================

# Install the main executable
install(TARGETS ${PROJECT_NAME} DESTINATION bin)

# Install headers
install(DIRECTORY include/ DESTINATION include)
