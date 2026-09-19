/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2026, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

#include "souffle/datastructure/DynamicBTree.h"
#include "tests/test.h"
#include <algorithm>
#include <stdexcept>
#include <vector>

namespace souffle {

TEST(DynamicBTreeSet, NaturalOrderAndDuplicates) {
    DynamicBTreeSet set(3);
    EXPECT_TRUE(set.insert({2, 0, 1}));
    EXPECT_FALSE(set.insert({2, 0, 1}));
    EXPECT_TRUE(set.insert({1, 9, 2}));
    EXPECT_TRUE(set.contains({2, 0, 1}));
    EXPECT_FALSE(set.contains({0, 0, 0}));

    std::vector<DynamicTuple> rows(set.begin(), set.end());
    EXPECT_EQ((std::vector<DynamicTuple>{{1, 9, 2}, {2, 0, 1}}), rows);
}

TEST(DynamicBTreeSet, RuntimeIndexOrder) {
    DynamicBTreeSet set(3, {2, 0, 1});
    set.insert({0, 0, 2});
    set.insert({9, 0, 1});
    set.insert({1, 7, 1});

    std::vector<DynamicTuple> rows(set.begin(), set.end());
    EXPECT_EQ((std::vector<DynamicTuple>{{1, 7, 1}, {9, 0, 1}, {0, 0, 2}}), rows);
    EXPECT_EQ(DynamicTuple({1, 7, 1}), *set.lower_bound({0, 0, 1}));
}

TEST(DynamicBTreeSet, NullaryAndLargeArity) {
    DynamicBTreeSet nullary(0);
    EXPECT_TRUE(nullary.insert({}));
    EXPECT_FALSE(nullary.insert({}));
    EXPECT_TRUE(nullary.contains({}));

    DynamicBTreeSet large(32);
    DynamicTuple tuple(32);
    for (std::size_t i = 0; i < tuple.size(); ++i) {
        tuple[i] = static_cast<RamDomain>(tuple.size() - i);
    }
    EXPECT_TRUE(large.insert(tuple));
    EXPECT_TRUE(large.contains(tuple));
}

TEST(DynamicBTreeSet, ArityAndOrderValidation) {
    DynamicBTreeSet set(2);
    auto throwsInvalidArgument = [](auto&& operation) {
        try {
            operation();
        } catch (const std::invalid_argument&) {
            return true;
        }
        return false;
    };
    EXPECT_TRUE(throwsInvalidArgument([&]() { set.insert({1}); }));
    EXPECT_TRUE(throwsInvalidArgument([&]() { set.contains({1, 2, 3}); }));
    EXPECT_TRUE(throwsInvalidArgument([&]() { DynamicBTreeSet invalid(2, {0}); }));
    EXPECT_TRUE(throwsInvalidArgument([&]() { DynamicBTreeSet invalid(2, {0, 0}); }));
    EXPECT_TRUE(throwsInvalidArgument([&]() { DynamicBTreeSet invalid(2, {0, 2}); }));
}

TEST(DynamicBTreeSet, CopyMoveClearAndRange) {
    DynamicBTreeSet set(2);
    for (RamDomain i = 0; i < 100; ++i) {
        set.insert({i, i % 7});
    }
    DynamicBTreeSet copy(set);
    EXPECT_EQ(set.size(), copy.size());
    DynamicBTreeSet moved(std::move(copy));
    EXPECT_EQ(set.size(), moved.size());
    EXPECT_EQ(DynamicTuple({25, 4}), *moved.lower_bound({25, 4}));
    EXPECT_EQ(DynamicTuple({26, 5}), *moved.upper_bound({25, 4}));
    moved.clear();
    EXPECT_TRUE(moved.empty());
}

}  // namespace souffle
