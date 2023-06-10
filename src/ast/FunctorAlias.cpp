/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2023 The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

#include "ast/FunctorAlias.h"

#include <ostream>
#include <string>

namespace souffle::ast {

FunctorAlias::FunctorAlias(const std::string& ident, QualifiedName target, SrcLocation loc)
        : Node(loc), identifier(ident), target(target) {}

FunctorAlias* FunctorAlias::cloning() const {
    return new FunctorAlias(identifier, target, getSrcLoc());
}
void FunctorAlias::print(std::ostream& os) const {
    os << ".functor " << identifier << " = " << target;
}

bool FunctorAlias::equal(const Node& node) const {
    const auto& other = asAssert<FunctorAlias>(node);
    return (identifier == other.identifier) && (target == other.target);
}

const std::string& FunctorAlias::getIdentifier() const {
    return identifier;
}

const QualifiedName& FunctorAlias::getTarget() const {
    return target;
}

void FunctorAlias::setTarget(QualifiedName qname) {
    target = qname;
}

}  // namespace souffle::ast
