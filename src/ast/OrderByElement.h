/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2023, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

#pragma once

#include "ast/Node.h"
#include "ast/Argument.h"
#include "parser/SrcLocation.h"

#include <optional>
#include <string>

namespace souffle::ast {

/// @class OrderByElement
/// @brief Element of an `orderby` term.
class OrderByElement : public Node {
public:
  OrderByElement(Own<Argument> arg, std::optional<std::string> collate_locale,
      std::optional<std::string> direction, SrcLocation loc);

  const Argument* getArgument() const;

  std::optional<std::string> getCollateLocale() const;

  std::optional<std::string> getSortDirection() const;

  void apply(const NodeMapper&) override;

protected:

  void print(std::ostream&) const override;

  NodeVec getChildren() const override;

private:
  bool equal(const Node&) const override;

  OrderByElement* cloning() const override;

  Own<Argument> argument;

  std::optional<std::string> collateLocale;
  /// "ASC" or "DESC"
  std::optional<std::string> sortDirection;
};

}
