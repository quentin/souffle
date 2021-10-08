/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ResolveRecordAccess.cpp
 *
 ***********************************************************************/

#include "ast/transform/ResolveRecordAccess.h"
#include "ast/Argument.h"
#include "ast/BinaryConstraint.h"
#include "ast/BooleanConstraint.h"
#include "ast/Clause.h"
#include "ast/IntrinsicFunctor.h"
#include "ast/Literal.h"
#include "ast/Program.h"
#include "ast/RecordInit.h"
#include "ast/TranslationUnit.h"
#include "ast/utility/Visitor.h"
#include "souffle/BinaryConstraintOps.h"
#include "souffle/utility/ContainerUtil.h"
#include "souffle/utility/MiscUtil.h"
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <iterator>
#include <memory>
#include <utility>

namespace souffle::ast::transform {

bool ResolveRecordAccess::transform(TranslationUnit& translationUnit) {
    bool changed = false;
    Program& program = translationUnit.getProgram();

    visit(program, [&](const UserDefinedFunctor& intrinsic) {
        if (intrinsic.getName() == "hint") {
            Argument * arg = intrinsic.getArguments()[0];
            if (isA<RecordInit>(arg)) {
                changed = true;
            }
        }
    });

    struct removeFirst : public NodeMapper {
        Own<Node> operator()(Own<Node> node) const override {
            node->apply(*this);

            if (auto* intrinsic = as<UserDefinedFunctor>(node)) {
                if (intrinsic->getName() != "hint") {
                    return node;
                }
                Argument * arg = intrinsic->getArguments()[0];
                if (auto* record = as<RecordInit>(arg)) {
                    Argument * arg0 = record->getArguments()[0];
                    std::cout << *node << " -> " << *arg0 << std::endl;
                    return clone(arg0);
                }
            }

            return node;
        }
    };

    removeFirst update;
    program.apply(update);
    return changed;
}
}  // namespace souffle::ast::transform
