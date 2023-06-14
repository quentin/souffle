#pragma once

#include "ast/Items.h"
#include "ast/Node.h"
#include "ast/QualifiedName.h"

#include <optional>
#include <ostream>
#include <string>
#include <vector>

namespace souffle::ast {

/// The reference to a module with possibly some arguments.
///
/// ## Example
///
/// ```souffle
/// M::N(u, T::t)
/// M::K
/// ```
///
class ModuleRef : public Node {
public:
    explicit ModuleRef(
            ast::QualifiedName name, std::optional<std::vector<QualifiedName>> args, SrcLocation loc = {});

    bool hasArgumentList() const;

    const QualifiedName& getQualifiedName() const;

    void setQualifiedName(const QualifiedName&);

    const std::optional<std::vector<QualifiedName>>& getArguments() const;

    std::optional<std::vector<QualifiedName>>& getArguments();

private:
    void print(std::ostream& os) const override;

    bool equal(const Node& node) const override;

    ModuleRef* cloning() const override;

    QualifiedName name;

    std::optional<std::vector<QualifiedName>> args;
};

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

    Items& getItems();

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
    explicit ModuleAlias(Own<ModuleRef> ref, SrcLocation loc = {});

    const ModuleRef* getModuleRef() const;

    void setModuleRef(Own<ModuleRef> ref);

protected:
    void print(std::ostream& os) const override;

    NodeVec getChildren() const override;

    void apply(const NodeMapper& mapper) override;

private:
    bool equal(const Node& node) const override;

    ModuleAlias* cloning() const override;

    Own<ModuleRef> ref;
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

    ModuleDef& getDefinition();

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
