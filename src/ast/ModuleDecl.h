#pragma once

#include "ast/Items.h"
#include "ast/Node.h"
#include "ast/QualifiedName.h"

#include <optional>
#include <ostream>
#include <string>
#include <vector>

namespace souffle::ast {

/// A module expression
class ModuleExpr : public Node {
public:
    ModuleExpr(SrcLocation loc = {});
    virtual ~ModuleExpr() = default;
};

/// ```
/// module-expr ::= module-path
/// ```
class PathModuleExpr : public ModuleExpr {
public:
    explicit PathModuleExpr(QualifiedName modulePath, SrcLocation loc = {});

    const QualifiedName& getPath() const;

protected:
    void print(std::ostream& os) const override;

private:
    PathModuleExpr* cloning() const override;

    QualifiedName path;
};

/// ```
/// module-expr ::= "functor" "(" module-name ")" "->" module-expr
///             |   "functor" "(" ")" "->" module-expr
/// ```
class FunctorModuleExpr : public ModuleExpr {
public:
    explicit FunctorModuleExpr(std::string moduleName, Own<ModuleExpr> moduleExpr, SrcLocation loc = {});
    explicit FunctorModuleExpr(Own<ModuleExpr> moduleExpr, SrcLocation loc = {});

    bool isGenerative() const;

    bool isApplicative() const;

    const ModuleExpr* getExpr() const;

    const std::optional<std::string>& getParameter() const;

protected:
    void print(std::ostream& os) const override;

    Node::NodeVec getChildren() const override;

    void apply(const NodeMapper& mapper) override;

private:
    FunctorModuleExpr* cloning() const override;

    std::optional<std::string> moduleName;
    Own<ModuleExpr> moduleExpr;
};

/// ```
/// module-expr ::= "{" module-item { module-item } "}"
/// ```
class BodyModuleExpr : public ModuleExpr {
public:
    explicit BodyModuleExpr(Items moduleItems, SrcLocation loc = {});

    const Items& getItems() const;

    BodyModuleExpr& addItem(Own<Node> item);

protected:
    void print(std::ostream& os) const override;

    Node::NodeVec getChildren() const override;

    void apply(const NodeMapper& mapper) override;

private:
    BodyModuleExpr* cloning() const override;

private:
    Items items;
};

/// ```
/// module-expr ::= module-expr "(" module-expr ")"
/// ```
class ApplicationModuleExpr : public ModuleExpr {
public:
    explicit ApplicationModuleExpr(
            Own<ModuleExpr> moduleExpr, Own<ModuleExpr> argument, SrcLocation loc = {});

    const ModuleExpr* getArgument() const;

    const ModuleExpr* getReceiver() const;

protected:
    void print(std::ostream& os) const override;

    Node::NodeVec getChildren() const override;

    void apply(const NodeMapper& mapper) override;

private:
    ApplicationModuleExpr* cloning() const override;

    Own<ModuleExpr> expr;
    Own<ModuleExpr> argument;
};

/// ```
/// module-expr ::= module-expr "(" ")"
/// ```
class GenerationModuleExpr : public ModuleExpr {
public:
    explicit GenerationModuleExpr(Own<ModuleExpr> moduleExpr, SrcLocation loc = {});

    const ModuleExpr* getReceiver() const;

protected:
    void print(std::ostream& os) const override;

    Node::NodeVec getChildren() const override;

    void apply(const NodeMapper& mapper) override;

private:
    GenerationModuleExpr* cloning() const override;

    Own<ModuleExpr> expr;
};

/// The definition of a module
///
/// ```
/// module-item ::= ".module" module-name "=" module-expr
/// ```
class ModuleDefinition : public Node {
public:
    explicit ModuleDefinition(const std::string& name, Own<ModuleExpr> moduleExpr, SrcLocation loc = {});

    const std::string& getName() const;

    const ModuleExpr* getModuleExpr() const;

protected:
    void print(std::ostream& os) const override;

    Node::NodeVec getChildren() const override;

    void apply(const NodeMapper& mapper) override;

private:
    ModuleDefinition* cloning() const override;

    std::string name;

    Own<ModuleExpr> expr;
};
}  // namespace souffle::ast
