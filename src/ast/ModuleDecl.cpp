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
ModuleApplication::ModuleApplication(
        ast::QualifiedName source, std::vector<QualifiedName> args, SrcLocation loc)
        : ModuleDef(loc), source(source), args(args) {}

const QualifiedName& ModuleApplication::getSource() const {
    return source;
}

void ModuleApplication::setSource(const QualifiedName& qn) {
    source = qn;
}

ModuleApplication* ModuleApplication::cloning() const {
    return new ModuleApplication(source, args, getSrcLoc());
}

void ModuleApplication::print(std::ostream& os) const {
    os << " = " << source;
    os << "(" << join(args, ", ") << ")";
}

bool ModuleApplication::equal(const Node& node) const {
    const auto& other = asAssert<ModuleApplication>(node);
    return source == other.source && args == other.args;
}

ModuleStruct::ModuleStruct(Items itms, SrcLocation loc) : ModuleDef(loc), items(std::move(itms)) {}

const Items& ModuleStruct::getItems() const {
    return items;
}

ModuleStruct& ModuleStruct::addItem(Own<Node> item) {
    items.emplace_back(std::move(item));
    return *this;
}

void ModuleStruct::print(std::ostream& os) const {
    const auto show = [&](auto&& xs, char const* sep = "\n") {
        if (!xs.empty()) os << join(xs, sep) << "\n";
    };

    os << "{\n";
    show(items);
    os << "}\n";
}

Node::NodeVec ModuleStruct::getChildren() const {
    std::vector<const Node*> res;
    append(res, makePtrRange(items));
    return res;
}

void ModuleStruct::apply(const NodeMapper& mapper) {
    mapAll(items, mapper);
}

bool ModuleStruct::equal(const Node& node) const {
    [[maybe_unused]] const auto& other = asAssert<ModuleStruct>(node);
    throw "TODO";
}

ModuleStruct* ModuleStruct::cloning() const {
    ModuleStruct* res = new ModuleStruct(clone(items), getSrcLoc());
    return res;
}

ModuleAlias::ModuleAlias(const QualifiedName& source, SrcLocation loc) : ModuleDef(loc), source(source) {}

const QualifiedName& ModuleAlias::getSource() const {
    return source;
}

void ModuleAlias::setSource(const QualifiedName& src) {
    source = src;
}

void ModuleAlias::print(std::ostream& os) const {
    os << " = " << source;
}

bool ModuleAlias::equal(const Node& node) const {
    [[maybe_unused]] const auto& other = asAssert<ModuleAlias>(node);
    throw "TODO";
}

ModuleAlias* ModuleAlias::cloning() const {
    return new ModuleAlias(source, getSrcLoc());
}

ModuleDecl::ModuleDecl(const std::string& name, std::optional<std::vector<QualifiedName>> params,
        Own<ModuleDef> def, SrcLocation loc)
        : Node(loc), name(name), params(std::move(params)), def(std::move(def)) {}

const std::string& ModuleDecl::getName() const {
    return name;
}

const ModuleDef* ModuleDecl::getDefinition() const {
    return def.get();
}

std::optional<std::vector<QualifiedName>>& ModuleDecl::getParameterList() {
    return params;
}

bool ModuleDecl::hasParameterList() const {
    return (bool)params;
}

void ModuleDecl::apply(const NodeMapper& mapper) {
    def = mapper(std::move(def));
}

bool ModuleDecl::equal(const Node&) const {
    throw "TODO";
}

ModuleDecl* ModuleDecl::cloning() const {
    ModuleDecl* res = new ModuleDecl(name, params, clone(def), getSrcLoc());
    return res;
}

void ModuleDecl::print(std::ostream& os) const {
    os << ".module " << name;
    if (hasParameterList()) {
        os << "(" << join(*params, ", ") << ")";
    }
    os << *def;
}

Node::NodeVec ModuleDecl::getChildren() const {
    std::vector<const Node*> res;
    res.push_back(def.get());
    return res;
}

ModuleDef::ModuleDef(SrcLocation loc) : Node(loc) {}

// void ModuleStruct::print(std::ostream& os) const {
//     const auto show = [&](auto&& xs, char const* sep = "\n") {
//         if (!xs.empty()) os << join(xs, sep) << "\n";
//     };
//
//     os << ".module " << getName() << " " << *moduleType << " {\n";
//     show(items);
//     os << "}\n";
// }

}  // namespace souffle::ast
