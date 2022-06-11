/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2021, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file NestedUserDefinedOperator.h
 *
 ************************************************************************/

#pragma once

#include "ram/Expression.h"
#include "ram/Node.h"
#include "ram/Operation.h"
#include "ram/TupleOperation.h"
#include "souffle/TypeAttribute.h"
#include "souffle/utility/MiscUtil.h"
#include "souffle/utility/StreamUtil.h"
#include <cassert>
#include <memory>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

namespace souffle::ram {

/**
 * @class NestedUserDefinedOperator
 * @brief Operator that represents an extrinsic (user-defined) functor
 * that generates mulitple results.
 */
class NestedUserDefinedOperator : public TupleOperation {
public:
    NestedUserDefinedOperator(Own<Operation> nested, std::size_t ident, std::string n,
            std::vector<TypeAttribute> argsTypes, TypeAttribute returnType, VecOwn<Expression> args)
            : TupleOperation(ident, std::move(nested)), name(std::move(n)), argsTypes(std::move(argsTypes)),
              returnType(returnType), args(std::move(args)) {
        assert(argsTypes.size() == args.size());
    }

    /** @brief Get operator name */
    const std::string& getName() const {
        return name;
    }

    /** @brief Get types of arguments */
    const std::vector<TypeAttribute>& getArgsTypes() const {
        return argsTypes;
    }

    /** @brief Get return type */
    TypeAttribute getReturnType() const {
        return returnType;
    }

    std::vector<Expression*> getArguments() const {
        return toPtrVector(args);
    }

    NestedUserDefinedOperator* cloning() const override {
        return new NestedUserDefinedOperator(
                clone(getOperation()), getTupleId(), name, argsTypes, returnType, clone(args));
    }

    void apply(const NodeMapper& map) override {
        TupleOperation::apply(map);
        for (auto&& x : args) {
            x = map(std::move(x));
        }
    }

protected:
    void print(std::ostream& os, int tabpos) const override {
        os << times(" ", tabpos);
        os << "@" << name << "("
           << join(args, ",", [](std::ostream& out, const Own<Expression>& arg) { out << *arg; })
           << ") INTO t" << getTupleId() << "\n";
        NestedOperation::print(os, tabpos + 1);
    }

    bool equal(const Node& node) const override {
        const auto& other = asAssert<NestedUserDefinedOperator>(node);
        return TupleOperation::equal(node) && name == other.name && argsTypes == other.argsTypes &&
               returnType == other.returnType && equal_targets(args, other.args);
    }

    NodeVec getChildren() const override {
        auto res = TupleOperation::getChildren();
        for (auto&& x : args) {
            res.push_back(x.get());
        }
        return res;
    }

    /** Name of user-defined operator */
    const std::string name;

    /** Argument types */
    const std::vector<TypeAttribute> argsTypes;

    /** Return type */
    const TypeAttribute returnType;

    /* Arguments */
    VecOwn<Expression> args;
};
}  // namespace souffle::ram
