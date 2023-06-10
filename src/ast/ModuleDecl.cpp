/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2023 The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */
#include "ast/ModuleDecl.h"
#include "souffle/utility/ContainerUtil.h"
#include "souffle/utility/MiscUtil.h"
#include "souffle/utility/NodeMapperFwd.h"
#include "souffle/utility/StreamUtil.h"

#include <ostream>
#include <string>

namespace souffle::ast {
ModuleType::ModuleType(SrcLocation loc) : Node(loc) {}

ModuleType* ModuleType::cloning() const {
    return new ModuleType(getSrcLoc());
}
void ModuleType::print(std::ostream& os) const {
    os << "";
}

bool ModuleType::equal(const Node& node) const {
    [[maybe_unused]] const auto& other = asAssert<ModuleType>(node);
    return true;
}

ModuleDecl::ModuleDecl(const std::string& name, Own<ModuleType> ty, Items itms, SrcLocation loc)
        : Node(loc), name(name), moduleType(std::move(ty)), items(std::move(itms)) {}

const std::string& ModuleDecl::getName() const {
    return name;
}

const ModuleType& ModuleDecl::getModuleType() const {
    return *moduleType;
}

Items& ModuleDecl::getItems() {
    return items;
}

ModuleDecl& ModuleDecl::addItem(Own<Node> item) {
    items.emplace_back(std::move(item));
    return *this;
}

void ModuleDecl::print(std::ostream& os) const {
    const auto show = [&](auto&& xs, char const* sep = "\n") {
        if (!xs.empty()) os << join(xs, sep) << "\n";
    };

    os << ".module " << getName() << " " << *moduleType << " {\n";
    show(items);
    os << "}\n";
}

Node::NodeVec ModuleDecl::getChildren() const {
    std::vector<const Node*> res;
    res.emplace_back(moduleType.get());
    append(res, makePtrRange(items));
    return res;
}

void ModuleDecl::apply(const NodeMapper& mapper) {
    moduleType = mapper(std::move(moduleType));
    mapAll(items, mapper);
}

bool ModuleDecl::equal(const Node&) const {
    throw "TODO";
}

ModuleDecl* ModuleDecl::cloning() const {
    ModuleDecl* res = new ModuleDecl(name, clone(moduleType), clone(items), getSrcLoc());
    return res;
}

}  // namespace souffle::ast
