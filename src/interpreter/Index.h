/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

#pragma once

#include "souffle/utility/StreamUtil.h"
#include <cassert>
#include <cstddef>
#include <iosfwd>
#include <numeric>
#include <utility>
#include <vector>

namespace souffle::interpreter {

/** A runtime lexicographic order for relation attributes. */
class Order {
    using Attribute = std::size_t;
    using AttributeOrder = std::vector<Attribute>;
    AttributeOrder order;

public:
    Order() = default;
    Order(AttributeOrder pos) : order(std::move(pos)) {
        assert(valid());
    }

    static Order create(std::size_t arity) {
        AttributeOrder result(arity);
        std::iota(result.begin(), result.end(), std::size_t{0});
        return Order(std::move(result));
    }

    std::size_t size() const {
        return order.size();
    }

    bool valid() const {
        std::vector<bool> seen(order.size(), false);
        for (const auto attribute : order) {
            if (attribute >= order.size() || seen[attribute]) return false;
            seen[attribute] = true;
        }
        return true;
    }

    const AttributeOrder& getOrder() const {
        return order;
    }

    bool operator==(const Order& other) const {
        return order == other.order;
    }

    bool operator!=(const Order& other) const {
        return !(*this == other);
    }

    Attribute operator[](std::size_t idx) const {
        return order[idx];
    }

    friend std::ostream& operator<<(std::ostream& out, const Order& order);
};

inline std::ostream& operator<<(std::ostream& out, const Order& order) {
    return out << "[" << join(order.order) << "]";
}

/** Type-erased base class for relation index views. */
struct ViewWrapper {
    virtual ~ViewWrapper() = default;
};

}  // namespace souffle::interpreter
