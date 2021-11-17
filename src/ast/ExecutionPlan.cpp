/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

#include "ast/ExecutionPlan.h"

#include "souffle/utility/ContainerUtil.h"
#include "souffle/utility/MiscUtil.h"
#include "souffle/utility/StreamUtil.h"
#include <ostream>
#include <utility>

namespace souffle::ast {

/** Set execution order for a given rule version */
void ExecutionPlan::setOrderFor(int version, Own<ExecutionOrder> plan) {
    assert(plan != nullptr);
    plans[version] = std::move(plan);
}

/** Get orders */
std::map<int, const ExecutionOrder*> ExecutionPlan::getOrders() const {
    std::map<int, const ExecutionOrder*> result;
    for (auto& plan : plans) {
        result.insert(std::make_pair(plan.first, plan.second.get()));
    }
    return result;
}

void ExecutionPlan::setCustomFor(std::string s, Own<ExecutionOrder> plan) {
    assert(plan != nullptr);
    customs[s] = std::move(plan);
}

std::map<std::string, const ExecutionOrder*> ExecutionPlan::getCustoms() const {
    std::map<std::string, const ExecutionOrder*> result;
    for (auto& custom : customs) {
        result.insert(std::make_pair(custom.first, custom.second.get()));
    }
    return result;
}

void ExecutionPlan::apply(const NodeMapper& map) {
    for (auto& plan : plans) {
        plan.second = map(std::move(plan.second));
    }
    for (auto& custom : customs) {
        custom.second = map(std::move(custom.second));
    }
}

Node::NodeVec ExecutionPlan::getChildren() const {
    auto ran = makeTransformRange(plans, [](auto const& pair) { return pair.second.get(); });
    return {ran.begin(), ran.end()};
}

void ExecutionPlan::print(std::ostream& out) const {
    if (!plans.empty()) {
        out << " .plan ";
        out << join(plans, ", ",
                [](std::ostream& os, const auto& arg) { os << arg.first << ":" << *arg.second; });
    }
    if (sips.has_value()) {
        //out << " .sips " << sips.value() << std::endl;
    }
}

bool ExecutionPlan::equal(const Node& node) const {
    const auto& other = asAssert<ExecutionPlan>(node);
    return equal_targets(plans, other.plans) && equal_targets(customs, other.customs) && sips == other.sips;
}

ExecutionPlan* ExecutionPlan::cloning() const {
    auto res = mk<ExecutionPlan>(getSrcLoc());
    for (auto& plan : plans) {
        res->setOrderFor(plan.first, clone(plan.second));
    }
    for (auto& custom : customs) {
        res->setCustomFor(custom.first, clone(custom.second));
    }
    res->sips = sips;
    return res.release();
}
}  // namespace souffle::ast
