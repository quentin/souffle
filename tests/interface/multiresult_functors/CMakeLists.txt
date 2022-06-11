# Souffle - A Datalog Compiler
# Copyright (c) 2021 The Souffle Developers. All rights reserved
# Licensed under the Universal Permissive License v 1.0 as shown at:
# - https://opensource.org/licenses/UPL
# - <souffle root>/licenses/SOUFFLE-UPL.txt

add_library(multiresult_functors SHARED multiresult_functors.cpp)
target_include_directories(multiresult_functors PRIVATE "${CMAKE_SOURCE_DIR}/src/include")

target_compile_features(multiresult_functors
                        PUBLIC cxx_std_17)

set_target_properties(multiresult_functors PROPERTIES
  CXX_EXTENSIONS OFF
  WINDOWS_EXPORT_ALL_SYMBOLS ON)
set_target_properties(multiresult_functors PROPERTIES OUTPUT_NAME "functors")

if (Threads_FOUND)
  target_link_libraries(multiresult_functors PUBLIC Threads::Threads)
endif ()

if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  if (CMAKE_CXX_COMPILER_VERSION VERSION_LESS 9)
    target_link_libraries(multiresult_functors PUBLIC stdc++fs)
  endif ()
endif ()

if (NOT MSVC)
  target_compile_options(multiresult_functors
    PUBLIC "-Wall;-Wextra;-Werror;-fwrapv")
else ()
  target_compile_options(multiresult_functors PUBLIC /W3 /WX /EHsc)
endif()

if (WIN32)
  # Prefix all shared libraries with 'lib'.
  set(CMAKE_SHARED_LIBRARY_PREFIX "lib")

  # Prefix all static libraries with 'lib'.
  set(CMAKE_STATIC_LIBRARY_PREFIX "lib")
endif ()

if (SOUFFLE_DOMAIN_64BIT)
    target_compile_definitions(multiresult_functors
                               PUBLIC RAM_DOMAIN_SIZE=64)
endif()
