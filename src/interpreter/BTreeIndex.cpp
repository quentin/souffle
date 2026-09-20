/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file BTreeIndex.cpp
 *
 * Interpreter index with generic interface.
 *
 ***********************************************************************/

#include "interpreter/Relation.h"
#include "ram/Relation.h"
#include "ram/analysis/Index.h"
#include "souffle/utility/MiscUtil.h"

namespace souffle::interpreter {

Own<RelationWrapper> createBTreeRelation(
        const ram::Relation& id, const ram::analysis::IndexCluster& indexSelection) {
    return mk<DynamicRelation>(id, indexSelection);
}

}  // namespace souffle::interpreter
