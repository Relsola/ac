# 设置CMake最低版本要求
cmake_minimum_required(VERSION 3.16)

# 设置项目名称和版本
project(ac VERSION 0.0.1 LANGUAGES CXX)

# 设置C++标准
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# 设置编译选项
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -w")
set(CMAKE_CXX_FLAGS_DEBUG "-g -O0")
set(CMAKE_CXX_FLAGS_RELEASE "-O3 -DNDEBUG")

# 编译模式 Release Debug
SET(CMAKE_BUILD_TYPE "Debug")

# 开启调试 出现问题时开启
# set(CMAKE_VERBOSE_MAKEFILE ON)

# 包含源文件目录
include_directories(src)
include_directories(include)

# 设置输出目录（在创建目标之前）
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/debug/bin)
    message(STATUS "Debug build - Output to build/debug/bin")
else()
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/release/bin)
    message(STATUS "Release build - Output to build/release/bin")
endif()

# 收集所有源文件
file(GLOB_RECURSE SOURCES "src/*.cpp" "include/*.h")

# 创建可执行文件
add_executable(${PROJECT_NAME} ${SOURCES})

add_subdirectory(test)
