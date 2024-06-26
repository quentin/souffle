/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

#include "ast/Variable.h"
#include "souffle/utility/DynamicCasting.h"
#include <ostream>
#include <utility>

namespace souffle::ast {
Variable::Variable(std::string name, SrcLocation loc)
        : Argument(NK_Variable, std::move(loc)), name(std::move(name)) {}

void Variable::setName(std::string name) {
    this->name = std::move(name);
}

void Variable::print(std::ostream& os) const {
    os << name;
}

bool Variable::equal(const Node& node) const {
    const auto& other = asAssert<Variable>(node);
    return name == other.name;
}

Variable* Variable::cloning() const {
    return new Variable(name, getSrcLoc());
}

bool Variable::classof(const Node* n) {
    return n->getKind() == NK_Variable;
}

}  // namespace souffle::ast
