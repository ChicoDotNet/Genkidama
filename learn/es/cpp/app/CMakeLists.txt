cmake_minimum_required(VERSION 3.28)
project(ThreadSeek VERSION 0.4.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

find_package(Threads REQUIRED)

add_library(threadseek_core src/indexer.cpp)
target_include_directories(threadseek_core PUBLIC include)
target_link_libraries(threadseek_core PUBLIC Threads::Threads)
target_compile_options(threadseek_core PRIVATE
    $<$<CXX_COMPILER_ID:GNU,Clang>:-Wall;-Wextra;-Wpedantic;-Werror>
    $<$<CXX_COMPILER_ID:MSVC>:/W4;/WX>
)

add_executable(threadseek src/main.cpp)
target_link_libraries(threadseek PRIVATE threadseek_core)
target_compile_options(threadseek PRIVATE
    $<$<CXX_COMPILER_ID:GNU,Clang>:-Wall;-Wextra;-Wpedantic;-Werror>
    $<$<CXX_COMPILER_ID:MSVC>:/W4;/WX>
)

enable_testing()
add_executable(threadseek_tests tests/indexer_tests.cpp)
target_link_libraries(threadseek_tests PRIVATE threadseek_core)
target_compile_options(threadseek_tests PRIVATE
    $<$<CXX_COMPILER_ID:GNU,Clang>:-Wall;-Wextra;-Wpedantic;-Werror>
    $<$<CXX_COMPILER_ID:MSVC>:/W4;/WX>
)
add_test(NAME threadseek_tests COMMAND threadseek_tests)
