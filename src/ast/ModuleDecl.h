#pragma once

#include "ast/Items.h"
#include "ast/Node.h"

#include <ostream>
#include <string>

namespace souffle::ast {

class ModuleType : public Node {
public:
    ModuleType(SrcLocation loc = {});

protected:
    void print(std::ostream& os) const override;

private:
    bool equal(const Node& node) const override;

    ModuleType* cloning() const override;
};

class ModuleDecl : public Node {
public:
    explicit ModuleDecl(const std::string& name, Own<ModuleType>, Items, SrcLocation loc = {});

    const std::string& getName() const;

    const ModuleType& getModuleType() const;

    Items& getItems();

    ModuleDecl& addItem(Own<Node> item);

protected:
    void print(std::ostream& os) const override;

    NodeVec getChildren() const override;

    void apply(const NodeMapper& mapper) override;

private:
    bool equal(const Node& node) const override;

    ModuleDecl* cloning() const override;

    std::string name;

    Own<ModuleType> moduleType;

    Items items;
};
}  // namespace souffle::ast
