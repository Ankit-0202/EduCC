cmake_minimum_required(VERSION 3.10)
project(C99Compiler)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# Include directories
include_directories(include)

# Source files
file(GLOB_RECURSE SRC_FILES src/*.cpp)

add_executable(c99compiler ${SRC_FILES})

# Link libraries if necessary
# target_link_libraries(c99compiler <libraries>)
