#include "ast/OrderByElement.h"
#include "souffle/utility/ContainerUtil.h"
#include "souffle/utility/MiscUtil.h"
#include "souffle/utility/NodeMapperFwd.h"

#include <iostream>

namespace souffle::ast {

OrderByElement::OrderByElement(Own<Argument> arg, std::optional<std::string> collate_locale,
        std::optional<std::string> direction, SrcLocation loc)
        : Node(NK_OrderByElement, loc), argument(std::move(arg)), collateLocale(collate_locale),
          sortDirection(direction) {}

void OrderByElement::apply(const NodeMapper& map) {
    argument = map(std::move(argument));
}

Node::NodeVec OrderByElement::getChildren() const {
    return {argument.get()};
}

void OrderByElement::print(std::ostream& os) const {
    os << *argument;
    if (collateLocale) {
      os << " COLLATE \"" << *collateLocale << "\"";
    }
    if (sortDirection) {
      os << " " << *sortDirection;
    }
}

bool OrderByElement::equal(const Node& node) const {
    const auto& other = asAssert<OrderByElement>(node);
    return equal_ptr(argument, other.argument) && collateLocale == other.collateLocale &&
           sortDirection == other.sortDirection;
}

OrderByElement* OrderByElement::cloning() const {
    return new OrderByElement(clone(argument), collateLocale, sortDirection, getSrcLoc());
}

const Argument* OrderByElement::getArgument() const {
    return argument.get();
}

std::optional<std::string> OrderByElement::getCollateLocale() const {
    return collateLocale;
}

std::optional<std::string> OrderByElement::getSortDirection() const {
    return sortDirection;
}
}  // namespace souffle::ast
