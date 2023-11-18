/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2022, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file IntrinsicAggregator.h
 *
 ***********************************************************************/

#pragma once

#include "AggregateOp.h"
#include "Aggregator.h"
#include "souffle/utility/DynamicCasting.h"

namespace souffle::ram {

/**
 * @class IntrinsicAggregate
 * @brief Class for intrinsic aggregate functions
 *
 */
class IntrinsicAggregator : public Aggregator {
public:
    IntrinsicAggregator(AggregateOp fun) : function(fun) {}
    virtual ~IntrinsicAggregator() = default;

    /** @brief Get aggregation function */
    AggregateOp getFunction() const {
        return function;
    }

    IntrinsicAggregator* cloning() const override {
        return new IntrinsicAggregator(function);
    }

    bool equal(const Aggregator& agg) const override {
        const auto& other = asAssert<IntrinsicAggregator>(agg);
        return function == other.function;
    }

    void print(std::ostream& os, int /* tabpos */) const override {
        os << function << " ";
    }

protected:
    /** Aggregation function */
    const AggregateOp function;
};
}  // namespace souffle::ram
