project(linear_regression)
cmake_minimum_required(VERSION 3.0.2)
aux_source_directory(. SRC_LIST)
add_executable(${PROJECT_NAME} ${SRC_LIST})
