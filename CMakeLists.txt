cmake_minimum_required(VERSION 3.0.0)

if(NOT DEFINED CMAKE_BUILD_TYPE)
	set(CMAKE_BUILD_TYPE Release)
endif()

project(brainf VERSION 1.0.0)
add_executable(brainf brainf.c)
install(TARGETS brainf DESTINATION bin)
