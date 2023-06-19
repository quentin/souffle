#pragma once

#include "ast/Items.h"
#include "ast/Node.h"
#include "ast/QualifiedName.h"

#include <optional>
#include <ostream>
#include <string>
#include <vector>

namespace souffle::ast {

/// The definition of a module.
class ModuleDef : public Node {
public:
    explicit ModuleDef(SrcLocation loc);
    virtual ~ModuleDef() = default;
};

/// Module structure, contains some module items.
///
/// ```
/// ...module decl... {
///   ...module items...
/// }
/// ```
class ModuleStruct : public ModuleDef {
public:
    explicit ModuleStruct(Items items, SrcLocation loc = {});

    const Items& getItems() const;

    ModuleStruct& addItem(Own<Node> item);

protected:
    void print(std::ostream& os) const override;

    NodeVec getChildren() const override;

    void apply(const NodeMapper& mapper) override;

private:
    bool equal(const Node& node) const override;

    ModuleStruct* cloning() const override;

    Items items;
};

/// Module alias, references an existing module or instantiate a functor
/// module.
///
/// ```
/// ...module decl... = M::N
/// ...module decl... = F(a1, ..., aN)
/// ```
///
class ModuleAlias : public ModuleDef {
public:
    explicit ModuleAlias(const QualifiedName& source, SrcLocation loc = {});

    const QualifiedName& getSource() const;

    void setSource(const QualifiedName& src);

protected:
    void print(std::ostream& os) const override;

private:
    bool equal(const Node& node) const override;

    ModuleAlias* cloning() const override;

    QualifiedName source;
};

/// The application of a module functor
///
/// ## Example
///
/// ```souffle
/// A()
/// M::N(u, T::t)
/// ```
///
class ModuleApplication : public ModuleDef {
public:
    explicit ModuleApplication(
            ast::QualifiedName source, std::vector<QualifiedName> args, SrcLocation loc = {});

    const QualifiedName& getSource() const;

    void setSource(const QualifiedName&);

    const std::vector<QualifiedName>& getArguments() const;

    std::vector<QualifiedName>& getArguments();

private:
    void print(std::ostream& os) const override;

    bool equal(const Node& node) const override;

    ModuleApplication* cloning() const override;

    QualifiedName source;

    std::vector<QualifiedName> args;
};

/// The declaration of a module, possibly with some parameters.
///
/// ```
/// .module Name ...module-def...
/// .module Name () ...module-def...
/// .module Name (p1, ... , pN) ...module-def...
/// ```
class ModuleDecl : public Node {
public:
    explicit ModuleDecl(const std::string& name, std::optional<std::vector<QualifiedName>> params,
            Own<ModuleDef> def, SrcLocation loc = {});

    const std::string& getName() const;

    bool hasParameterList() const;

    std::optional<std::vector<QualifiedName>>& getParameterList();

    const ModuleDef* getDefinition() const;

protected:
    void print(std::ostream& os) const override;

    NodeVec getChildren() const override;

    void apply(const NodeMapper& mapper) override;

private:
    bool equal(const Node& node) const override;

    ModuleDecl* cloning() const override;

    std::string name;

    std::optional<std::vector<QualifiedName>> params;

    Own<ModuleDef> def;

    Items items;
};
}  // namespace souffle::ast
