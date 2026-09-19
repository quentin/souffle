/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2026, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

#pragma once

#include "souffle/RamTypes.h"
#include "souffle/datastructure/BTree.h"
#include <algorithm>
#include <cstddef>
#include <memory>
#include <numeric>
#include <stdexcept>
#include <utility>
#include <vector>

namespace souffle {

/** A tuple whose arity is determined when the value is created. */
using DynamicTuple = std::vector<RamDomain>;

/** Lexicographic tuple comparator with an order configured at runtime. */
class DynamicTupleComparator {
    std::size_t arity = 0;
    std::vector<std::size_t> order;

public:
    DynamicTupleComparator() = default;

    explicit DynamicTupleComparator(std::size_t arity) : arity(arity), order(arity) {
        std::iota(order.begin(), order.end(), std::size_t{0});
    }

    DynamicTupleComparator(std::size_t arity, std::vector<std::size_t> order)
            : arity(arity), order(std::move(order)) {
        if (this->order.size() != arity) {
            throw std::invalid_argument("B-tree order must contain one column per tuple attribute");
        }
        auto sorted = this->order;
        std::sort(sorted.begin(), sorted.end());
        for (std::size_t i = 0; i < arity; ++i) {
            if (sorted[i] != i) {
                throw std::invalid_argument("B-tree order must be a permutation of tuple attributes");
            }
        }
    }

    int operator()(const DynamicTuple& lhs, const DynamicTuple& rhs) const {
        if (lhs.size() != arity || rhs.size() != arity) {
            throw std::invalid_argument("tuple arity does not match B-tree comparator arity");
        }
        for (const auto column : order) {
            if (lhs[column] != rhs[column]) {
                return lhs[column] < rhs[column] ? -1 : 1;
            }
        }
        return 0;
    }

    bool less(const DynamicTuple& lhs, const DynamicTuple& rhs) const {
        return (*this)(lhs, rhs) < 0;
    }

    bool equal(const DynamicTuple& lhs, const DynamicTuple& rhs) const {
        return (*this)(lhs, rhs) == 0;
    }

    std::size_t getArity() const {
        return arity;
    }

    const std::vector<std::size_t>& getOrder() const {
        return order;
    }
};

/** A B-tree set of dynamically-sized tuples. */
class DynamicBTreeSet
        : public btree_set<DynamicTuple, DynamicTupleComparator, std::allocator<DynamicTuple>, 256,
                  typename detail::default_strategy<DynamicTuple>::type, DynamicTupleComparator> {
    using Base = btree_set<DynamicTuple, DynamicTupleComparator, std::allocator<DynamicTuple>, 256,
            typename detail::default_strategy<DynamicTuple>::type, DynamicTupleComparator>;

public:
    explicit DynamicBTreeSet(std::size_t arity)
            : DynamicBTreeSet(arity, DynamicTupleComparator(arity), InternalTag{}) {}

    DynamicBTreeSet(std::size_t arity, std::vector<std::size_t> order)
            : DynamicBTreeSet(arity, DynamicTupleComparator(arity, std::move(order)), InternalTag{}) {}

    using Base::contains;
    using Base::insert;

    bool insert(const DynamicTuple& tuple) {
        checkArity(tuple);
        return Base::insert(tuple);
    }

    bool contains(const DynamicTuple& tuple) const {
        checkArity(tuple);
        return Base::contains(tuple);
    }

    std::size_t getArity() const {
        return arity;
    }

private:
    struct InternalTag {};

    DynamicBTreeSet(std::size_t arity, const DynamicTupleComparator& comparator, InternalTag)
            : Base(comparator, comparator), arity(arity) {}

    void checkArity(const DynamicTuple& tuple) const {
        if (tuple.size() != arity) {
            throw std::invalid_argument("tuple arity does not match B-tree arity");
        }
    }

    std::size_t arity;
};

}  // namespace souffle
