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
ModuleRef::ModuleRef(ast::QualifiedName name, std::optional<std::vector<QualifiedName>> args, SrcLocation loc)
        : Node(loc), name(name), args(args) {}

const QualifiedName& ModuleRef::getQualifiedName() const {
    return name;
}

void ModuleRef::setQualifiedName(const QualifiedName& qn) {
    name = qn;
}

ModuleRef* ModuleRef::cloning() const {
    return new ModuleRef(name, args, getSrcLoc());
}

void ModuleRef::print(std::ostream& os) const {
    os << name;
    if (hasArgumentList()) {
        os << "(" << join(*args, ", ") << ")";
    }
}

bool ModuleRef::hasArgumentList() const {
    return (bool)args;
}

bool ModuleRef::equal(const Node& node) const {
    const auto& other = asAssert<ModuleRef>(node);
    return name == other.name && args == other.args;
}

ModuleStruct::ModuleStruct(Items itms, SrcLocation loc) : ModuleDef(loc), items(std::move(itms)) {}

Items& ModuleStruct::getItems() {
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
    const auto& other = asAssert<ModuleStruct>(node);
    throw "TODO";
}

ModuleStruct* ModuleStruct::cloning() const {
    ModuleStruct* res = new ModuleStruct(clone(items), getSrcLoc());
    return res;
}

ModuleAlias::ModuleAlias(Own<ModuleRef> ref, SrcLocation loc) : ModuleDef(loc), ref(std::move(ref)) {}

const ModuleRef* ModuleAlias::getModuleRef() const {
    return ref.get();
}

void ModuleAlias::setModuleRef(Own<ModuleRef> r) {
    ref = std::move(r);
}

void ModuleAlias::print(std::ostream& os) const {
    os << " = " << *ref;
}

Node::NodeVec ModuleAlias::getChildren() const {
    std::vector<const Node*> res;
    res.push_back(ref.get());
    return res;
}

void ModuleAlias::apply(const NodeMapper& mapper) {
    ref = mapper(std::move(ref));
}

bool ModuleAlias::equal(const Node& node) const {
    const auto& other = asAssert<ModuleAlias>(node);
    throw "TODO";
}

ModuleAlias* ModuleAlias::cloning() const {
    return new ModuleAlias(clone(ref), getSrcLoc());
}

ModuleDecl::ModuleDecl(const std::string& name, std::optional<std::vector<QualifiedName>> params,
        Own<ModuleDef> def, SrcLocation loc)
        : Node(loc), name(name), params(std::move(params)), def(std::move(def)) {}

const std::string& ModuleDecl::getName() const {
    return name;
}

ModuleDef& ModuleDecl::getDefinition() {
    return *def;
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
