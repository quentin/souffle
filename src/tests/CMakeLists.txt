# Souffle - A Datalog Compiler
# Copyright (c) 2021 The Souffle Developers. All rights reserved
# Licensed under the Universal Permissive License v 1.0 as shown at:
# - https://opensource.org/licenses/UPL
# - <souffle root>/licenses/SOUFFLE-UPL.txt

include(SouffleTests)

souffle_add_binary_test(binary_relation_test src SOUFFLE_HEADERS_ONLY)
souffle_add_binary_test(brie_test src SOUFFLE_HEADERS_ONLY)
souffle_add_binary_test(btree_multiset_test src SOUFFLE_HEADERS_ONLY)
souffle_add_binary_test(btree_set_test src SOUFFLE_HEADERS_ONLY)
souffle_add_binary_test(dynamic_btree_test src SOUFFLE_HEADERS_ONLY)

# Build explicitly with `cmake --build <build-dir> --target dynamic_btree_benchmark`.
add_executable(dynamic_btree_benchmark EXCLUDE_FROM_ALL dynamic_btree_benchmark.cpp)
target_compile_features(dynamic_btree_benchmark PRIVATE cxx_std_17)
target_include_directories(dynamic_btree_benchmark PRIVATE ${PROJECT_SOURCE_DIR}/src/include ${PROJECT_SOURCE_DIR}/src)
if (OPENMP_FOUND)
    target_link_libraries(dynamic_btree_benchmark PRIVATE OpenMP::OpenMP_CXX)
endif ()

souffle_add_binary_test(compiled_tuple_test src SOUFFLE_HEADERS_ONLY)
souffle_add_binary_test(disjoint_set_property_test src SOUFFLE_HEADERS_ONLY)
souffle_add_binary_test(eqrel_datastructure_test src SOUFFLE_HEADERS_ONLY)
souffle_add_binary_test(flyweight_test src SOUFFLE_HEADERS_ONLY)
souffle_add_binary_test(graph_utils_test src SOUFFLE_HEADERS_ONLY)
souffle_add_binary_test(parallel_utils_test src SOUFFLE_HEADERS_ONLY)
souffle_add_binary_test(profile_util_test src SOUFFLE_HEADERS_ONLY)
souffle_add_binary_test(record_table_test src SOUFFLE_HEADERS_ONLY)
souffle_add_binary_test(symbol_table_test src SOUFFLE_HEADERS_ONLY)
souffle_add_binary_test(table_test src SOUFFLE_HEADERS_ONLY)
souffle_add_binary_test(util_test src SOUFFLE_HEADERS_ONLY)
souffle_add_binary_test(visitor_test src SOUFFLE_HEADERS_ONLY)
souffle_add_binary_test(getopt_long_test src SOUFFLE_HEADERS_ONLY)
