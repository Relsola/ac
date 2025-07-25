# 设置CMake最低版本要求
cmake_minimum_required(VERSION 3.16)

# 设置项目名称和版本
project(AliceCompiler VERSION 0.0.1 LANGUAGES CXX)

# 设置C++标准
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# 设置编译选项
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra")
set(CMAKE_CXX_FLAGS_DEBUG "-g -O0")
set(CMAKE_CXX_FLAGS_RELEASE "-O3 -DNDEBUG")

# 包含源文件目录
include_directories(src)

# 设置输出目录（在创建目标之前）
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# 收集所有源文件
file(GLOB_RECURSE SOURCES "src/*.cpp" "src/*.h" "src/*.hpp")

# 创建可执行文件
add_executable(${PROJECT_NAME} ${SOURCES})
