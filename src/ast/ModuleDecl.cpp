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

ModuleExpr::ModuleExpr(SrcLocation loc) : Node(std::move(loc)) {}

PathModuleExpr::PathModuleExpr(QualifiedName modulePath, SrcLocation loc)
        : ModuleExpr(std::move(loc)), path(std::move(modulePath)) {}

const QualifiedName& PathModuleExpr::getPath() const {
    return path;
}

void PathModuleExpr::print(std::ostream& os) const {
    os << path;
}

PathModuleExpr* PathModuleExpr::cloning() const {
    return new PathModuleExpr(path, getSrcLoc());
}

FunctorModuleExpr::FunctorModuleExpr(std::string name, Own<ModuleExpr> expr, SrcLocation loc)
        : ModuleExpr(loc), moduleName(name), moduleExpr(std::move(expr)) {}

FunctorModuleExpr::FunctorModuleExpr(Own<ModuleExpr> expr, SrcLocation loc)
        : ModuleExpr(loc), moduleName(std::nullopt), moduleExpr(std::move(expr)) {}

void FunctorModuleExpr::print(std::ostream& os) const {
    os << "functor (";
    if (moduleName) {
        os << *moduleName;
    }
    os << ") -> " << *moduleExpr;
}

void FunctorModuleExpr::apply(const NodeMapper& mapper) {
    moduleExpr = mapper(std::move(moduleExpr));
}

Node::NodeVec FunctorModuleExpr::getChildren() const {
    std::vector<const Node*> res;
    res.emplace_back(moduleExpr.get());
    return res;
}

FunctorModuleExpr* FunctorModuleExpr::cloning() const {
    if (moduleName) {
        return new FunctorModuleExpr(*moduleName, clone(moduleExpr), getSrcLoc());
    } else {
        return new FunctorModuleExpr(clone(moduleExpr), getSrcLoc());
    }
}
bool FunctorModuleExpr::isGenerative() const {
    return !moduleName.has_value();
}

bool FunctorModuleExpr::isApplicative() const {
    return moduleName.has_value();
}

const ModuleExpr* getExpr() const {
    return moduleExpr;
}

const std::optional<std::string>& getParameter() const {
    return moduleName;
}

BodyModuleExpr::BodyModuleExpr(Items moduleItems, SrcLocation loc)
        : ModuleExpr(loc), items(std::move(moduleItems)) {}

const Items& BodyModuleExpr::getItems() const {
    return items;
}

BodyModuleExpr& BodyModuleExpr::addItem(Own<Node> item) {
    items.emplace_back(std::move(item));
    return *this;
}

void BodyModuleExpr::print(std::ostream& os) const {
    auto show = [&](auto&& xs, char const* sep = "\n") {
        if (!xs.empty()) os << join(xs, sep) << "\n";
    };

    os << "{\n";
    show(items);
    os << "}";
}

void BodyModuleExpr::apply(const NodeMapper& mapper) {
    mapAll(items, mapper);
}

Node::NodeVec BodyModuleExpr::getChildren() const {
    std::vector<const Node*> res;
    append(res, makePtrRange(items));
    return res;
}

BodyModuleExpr* BodyModuleExpr::cloning() const {
    return new BodyModuleExpr(clone(items), getSrcLoc());
}

ApplicationModuleExpr::ApplicationModuleExpr(
        Own<ModuleExpr> moduleExpr, Own<ModuleExpr> moduleArg, SrcLocation loc)
        : ModuleExpr(loc), expr(std::move(moduleExpr)), argument(std::move(moduleArg)) {}

void ApplicationModuleExpr::print(std::ostream& os) const {
    os << *expr << "(" << *argument << ")";
}

Node::NodeVec ApplicationModuleExpr::getChildren() const {
    std::vector<const Node*> res;
    res.emplace_back(expr.get());
    res.emplace_back(argument.get());
    return res;
}

void ApplicationModuleExpr::apply(const NodeMapper& mapper) {
    expr = mapper(std::move(expr));
    argument = mapper(std::move(argument));
}

ApplicationModuleExpr* ApplicationModuleExpr::cloning() const {
    return new ApplicationModuleExpr(clone(expr), clone(argument), getSrcLoc());
}

GenerationModuleExpr::GenerationModuleExpr(Own<ModuleExpr> moduleExpr, SrcLocation loc)
        : ModuleExpr(loc), expr(std::move(moduleExpr)) {}

void GenerationModuleExpr::print(std::ostream& os) const {
    os << *expr << "()";
}

Node::NodeVec GenerationModuleExpr::getChildren() const {
    std::vector<const Node*> res;
    res.emplace_back(expr.get());
    return res;
}

void GenerationModuleExpr::apply(const NodeMapper& mapper) {
    expr = mapper(std::move(expr));
}

GenerationModuleExpr* GenerationModuleExpr::cloning() const {
    return new GenerationModuleExpr(clone(expr), getSrcLoc());
}

ModuleDefinition::ModuleDefinition(const std::string& name, Own<ModuleExpr> moduleExpr, SrcLocation loc)
        : Node(loc), name(name), expr(std::move(moduleExpr)) {}

void ModuleDefinition::print(std::ostream& os) const {
    os << ".module " << name << " = " << *expr;
}

Node::NodeVec ModuleDefinition::getChildren() const {
    std::vector<const Node*> res;
    res.emplace_back(expr.get());
    return res;
}

void ModuleDefinition::apply(const NodeMapper& mapper) {
    expr = mapper(std::move(expr));
}

ModuleDefinition* ModuleDefinition::cloning() const {
    return new ModuleDefinition(name, clone(expr), getSrcLoc());
}

const std::string& ModuleDefinition::getName() const {
    return name;
}

const ModuleExpr* ModuleDefinition::getModuleExpr() const {
    return expr.get();
}
}  // namespace souffle::ast
