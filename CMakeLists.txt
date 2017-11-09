SET(MAKE_VERBOSE_MAKEFILE ON)
SET(CMAKE_BUILD_TYPE Debug CACHE STRING "default to debug" FORCE)
set(CXX_EXTENSIONS OFF)

# Basic project setup.
cmake_minimum_required(VERSION 3.5) # Xenial's CMake version.
project(ods)
set (${PROJECT_NAME}_VERSION_MAJOR 0)
set (${PROJECT_NAME}_VERSION_MINOR 1)

# Require clang.
set(CC clang)
set(CXX clang++)

# Set C++ compile flags for all builds.
add_compile_options(-Wall -Wextra -pedantic -Wshadow -Wpointer-arith)
add_compile_options(-Wcast-align -Wwrite-strings -Wmissing-declarations)
add_compile_options(-Werror -Wunused-variable -fno-elide-constructors)

# These might be contentious.
add_compile_options(-Weffc++)
# add_compile_options(-std=c++14)

if(CMAKE_BUILD_TYPE MATCHES DEBUG)
add_compile_options(-O0)
endif()

if (CMAKE_BUILD_TYPE MATCHES RELEASE)
add_compile_options(-O2)
endif()

if(CMAKE_GENERATOR MATCHES "Unix Makefiles")
SET(CMAKE_COLOR_MAKEFILE ON)
SET(CMAKE_VERBOSE_MAKEFILE ON)
endif()

file(GLOB_RECURSE SOURCES src/*.cc src/*.cpp)
file(GLOB_RECURSE HEADERS include/*.h)
file(GLOB HEADIRS include/*) # ha ha ha

include_directories(include test/include)

# Build shared library that can be reloaded while testing.
add_library(${PROJECT_NAME} SHARED ${SOURCES})

# Add static library for inclusion in final builds.
add_library(${PROJECT_NAME}-static STATIC ${SOURCES})

# Require C++14.
set_property(TARGET ${PROJECT_NAME} PROPERTY CXX_STANDARD 14)
set_property(TARGET ${PROJECT_NAME}-static PROPERTY CXX_STANDARD 14)

set_property(TARGET ${PROJECT_NAME} PROPERTY CXX_STANDARD_REQUIRED ON)
set_property(TARGET ${PROJECT_NAME}-static PROPERTY CXX_STANDARD_REQUIRED ON)

# Set up the library installation target.
install(TARGETS ${PROJECT_NAME} DESTINATION lib/${PROJECT_NAME})
install(TARGETS ${PROJECT_NAME}-static DESTINATION lib/${PROJECT_NAME})

# Install the headers.
install(DIRECTORY ${HEADIRS} DESTINATION include/${PROJECT_NAME})

# include(CMakeTests.txt)
# include(CMakePack.txt)