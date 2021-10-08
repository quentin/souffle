/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ResolveRecordAccess.h
 *
 *
 ***********************************************************************/

#pragma once

#include "ast/BinaryConstraint.h"
#include "ast/Clause.h"
#include "ast/Literal.h"
#include "ast/TranslationUnit.h"
#include "ast/transform/Transformer.h"
#include "souffle/utility/ContainerUtil.h"
#include <memory>
#include <string>
#include <vector>

namespace souffle::ast::transform {


class ResolveRecordAccess : public Transformer {
public:
    std::string getName() const override {
        return "ResolveRecordAccess";
    }

private:
    ResolveRecordAccess* cloning() const override {
        return new ResolveRecordAccess();
    }

    bool transform(TranslationUnit& translationUnit) override;
};

}  // namespace souffle::ast::transform
