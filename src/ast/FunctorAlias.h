/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2023 The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */
#pragma once

#include "ast/Node.h"
#include "ast/QualifiedName.h"
#include "parser/SrcLocation.h"

#include <ostream>
#include <string>

namespace souffle::ast {
class FunctorAlias : public Node {
public:
    explicit FunctorAlias(const std::string& ident, QualifiedName target, SrcLocation loc = {});

    ~FunctorAlias() = default;

    const std::string& getIdentifier() const;

    const QualifiedName& getTarget() const;

    void setTarget(QualifiedName qname);

protected:
    void print(std::ostream& out) const override;

    bool equal(const Node& node) const override;

    FunctorAlias* cloning() const override;

private:
    /// the identifier of this functor alias
    const std::string identifier;

    /// the qualified name of the aliased functor
    QualifiedName target;
};
}  // namespace souffle::ast
