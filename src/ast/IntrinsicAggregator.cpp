/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

#include "ast/IntrinsicAggregator.h"
#include "ast/Aggregator.h"
#include "souffle/utility/ContainerUtil.h"
#include "souffle/utility/MiscUtil.h"
#include "souffle/utility/NodeMapperFwd.h"
#include "souffle/utility/StreamUtil.h"
#include <cassert>
#include <ostream>
#include <utility>

namespace souffle::ast {
IntrinsicAggregator::IntrinsicAggregator(AggregateOp baseOperator, Own<Argument> expr, Own<Argument> second,
        VecOwn<Literal> body, VecOwn<Argument> orderby, SrcLocation loc)
        : Aggregator(NK_IntrinsicAggregator, std::move(expr), std::move(second), std::move(body), std::move(orderby), std::move(loc)),
          baseOperator(baseOperator) {}

void IntrinsicAggregator::print(std::ostream& os) const {
    os << baseOperator;
    if (targetExpression) {
        os << " " << *targetExpression;
    }
    if (second) {
        os << ", " << *second;
    }
    os << " : { " << join(body) << " }";
    if (!orderBy.empty()) {
        os << " orderby (";
        for (size_t i = 0; i < orderBy.size(); ++i) {
            if (i > 0) {
                os << ", ";
            }
            os << *orderBy.at(i);
        }
        os << ")";
    }
}

bool IntrinsicAggregator::equal(const Node& node) const {
    const auto& other = asAssert<IntrinsicAggregator>(node);
    return baseOperator == other.baseOperator && equal_ptr(targetExpression, other.targetExpression) &&
           equal_targets(body, other.body);
}

IntrinsicAggregator* IntrinsicAggregator::cloning() const {
    return new IntrinsicAggregator(
            baseOperator, clone(targetExpression), clone(second), clone(body), clone(orderBy), getSrcLoc());
}

bool IntrinsicAggregator::classof(const Node* n) {
    return n->getKind() == NK_IntrinsicAggregator;
}

}  // namespace souffle::ast
