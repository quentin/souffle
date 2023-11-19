/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2021, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AbstractAggregate.h
 *
 ***********************************************************************/

#pragma once

#include "ram/Aggregator.h"
#include "ram/Condition.h"
#include "ram/Expression.h"
#include "ram/Node.h"
#include "ram/TupleElement.h"
#include "souffle/utility/ContainerUtil.h"
#include "souffle/utility/MiscUtil.h"
#include <cassert>
#include <iosfwd>
#include <memory>
#include <ostream>
#include <utility>
#include <vector>

namespace souffle::ram {

/**
 * @class AbstractAggregate
 * @brief Abstract class for aggregation
 *
 * A particular function (e.g. MIN) is applied given a
 * that a condition holds
 */
class AbstractAggregate {
public:
    AbstractAggregate(Own<Aggregator> op, Own<Expression> expr, Own<Expression> second, Own<Condition> cond,
            VecOwn<Expression> orderBy)
            : function(std::move(op)), expression(std::move(expr)), second(std::move(second)),
              condition(std::move(cond)), orderBy(std::move(orderBy)) {
        assert(condition != nullptr && "Condition is a null-pointer");
        assert(expression != nullptr && "Expression is a null-pointer");
    }

    virtual ~AbstractAggregate() = default;

    /** @brief Get condition */
    const Condition& getCondition() const {
        assert(condition != nullptr && "Condition of aggregate is a null-pointer");
        return *condition;
    }

    const Aggregator& getAggregator() const {
        assert(function != nullptr && "Aggregator of aggregate is a null-pointer");
        return *function;
    }

    /** @brief Get target expression */
    const Expression& getExpression() const {
        assert(expression != nullptr && "Expression of aggregate is a null-pointer");
        return *expression;
    }

    const Expression* getSecondaryExpression() const {
        return second.get();
    }

    const VecOwn<Expression>& getOrderByExpressions() const {
        return orderBy;
    }

    Node::ConstChildNodes getChildNodes() const {
        return Node::ConstChildNodes(getChildren(), detail::RefCaster());
    }

    Node::ChildNodes getChildNodes() {
        return Node::ChildNodes(getChildren(), detail::ConstCaster());
    }

protected:
    void print(std::ostream& os, int tabpos) const {
        function->print(os, tabpos);
        if (expression) {
            os << *expression << " ";
            if (second) {
                os << ", " << *second << " ";
            }
        }
        if (!orderBy.empty()) {
            os << " ORDER BY (";
            for (size_t i = 0; i < orderBy.size(); ++i) {
                if (i > 0) {
                    os << ", ";
                }
                os << *orderBy.at(i);
            }
            os << ")";
        }
    }

    bool equal(const Node& node) const {
        const auto& other = asAssert<AbstractAggregate, AllowCrossCast>(node);
        return equal_ptr(function, other.function) && equal_ptr(expression, other.expression) &&
               equal_ptr(second, other.second) && equal_ptr(condition, other.condition) &&
               equal_targets(orderBy, other.orderBy);
    }

    std::vector<const Node*> getChildren() const {
        std::vector<const Node*> res = function->getChildren();
        res.push_back(expression.get());
        if (second) {
          res.push_back(second.get());
        }
        res.push_back(condition.get());
        append(res, makePtrRange(orderBy));
        return res;
    }

    /** Aggregation function */
    Own<Aggregator> function;

    /** Aggregation expression */
    Own<Expression> expression;

    Own<Expression> second;

    /** Aggregation tuple condition */
    Own<Condition> condition;

    /** Order by tuple elements*/
    VecOwn<Expression> orderBy;
};

}  // namespace souffle::ram
