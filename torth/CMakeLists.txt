cmake_minimum_required(VERSION 3.25)
project(torth)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(BINARY_NAME torth)

set(SOURCE_FILES
    src/main.cpp
    src/Interpreter.cpp
    src/Lexer.cpp
    src/Token.cpp
    src/Parser.cpp
    src/Util.cpp
    src/Statement.cpp
)

find_package(Boost REQUIRED COMPONENTS program_options) 

add_executable(${BINARY_NAME} ${SOURCE_FILES})
target_include_directories(${BINARY_NAME} PRIVATE include)
# target_include_directories(${BINARY_NAME} PRIVATE ${Boost_INCLUDE_DIRS})
target_link_libraries(${BINARY_NAME} PRIVATE ${Boost_LIBRARIES})

install(TARGETS ${BINARY_NAME} RUNTIME DESTINATION bin)
