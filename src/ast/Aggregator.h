/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2021, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file Aggregator.h
 *
 * Defines the aggregator class
 *
 ***********************************************************************/

#pragma once

#include "AggregateOp.h"
#include "ast/Argument.h"
#include "ast/Literal.h"
#include "ast/OrderByElement.h"
#include "parser/SrcLocation.h"
#include "souffle/utility/Types.h"

#include <iosfwd>
#include <optional>
#include <string>
#include <vector>

namespace souffle::ast {

/**
 * @class Aggregator
 * @brief Defines the aggregator class
 *
 * Example:
 *   sum y+x: {A(y),B(x)}
 *
 * Aggregates over a sub-query using an aggregate operator
 * and an expression.
 */
class Aggregator : public Argument {
public:
    Aggregator(NodeKind Kind, Own<Argument> expr = {}, Own<Argument> second = {}, VecOwn<Literal> body = {},
            VecOwn<OrderByElement> orderby = {}, SrcLocation loc = {});

    /** Return target expression */
    const Argument* getTargetExpression() const;

    Argument* getTargetExpression();

    const Argument* getSecondaryExpression() const;

    Argument* getSecondaryExpression();

    /** Return body literals */
    VecOwn<Literal>& bodyLiterals();

    /** Return body literals */
    std::vector<Literal*> getBodyLiterals() const;

    /** Set body literals, returns previous body literals */
    VecOwn<Literal> setBodyLiterals(VecOwn<Literal> bodyLiterals);

    void apply(const NodeMapper& map) override;

    virtual std::string getBaseOperatorName() const = 0;

    const VecOwn<OrderByElement>& getOrderByElements() const;

    static bool classof(const Node*);

protected:
    NodeVec getChildren() const override;

    /** Aggregate expression */
    Own<Argument> targetExpression;

    /** Secondary expression */
    Own<Argument> second;

    /** Body literal of sub-query */
    VecOwn<Literal> body;

    /** Order-by elements */
    VecOwn<OrderByElement> orderBy;
};

}  // namespace souffle::ast
