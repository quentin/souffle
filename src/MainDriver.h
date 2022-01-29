/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2022, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file MainDriver.hpp
 *
 * Main driver interface for Souffle
 *
 ***********************************************************************/

#pragma once

#include "Global.h"
#include "ast/transform/Pipeline.h"
#include "ast2ram/UnitTranslator.h"
#include "ast2ram/utility/TranslatorContext.h"
#include "include/souffle/utility/Types.h"
#include "ram/Program.h"
#include "ram/transform/Transformer.h"

namespace souffle {

/** Return the package version string. */
const char* packageVersion();

/** Return the command-line options that can be passed to `MainConfig::processArgs()`. */
std::vector<MainOption> getMainOptions();

/** Execute entire Souffle driver. */
int main(Global& glb, const char* souffle_executable);

/** Construct and return an AST transformer pipeline. */
Own<ast::transform::PipelineTransformer> astTransformationPipeline(Global& glb);

/** Construct and return an AST to RAM unit translator. */
Own<ast2ram::UnitTranslator> getUnitTranslator(Global& glb);

/** Construct and return a RAM transformer pipeline. */
Own<ram::transform::Transformer> ramTransformerSequence(Global& glb);

/** Interpret the RAM translation unit using Souffle engine. */
bool interpretTranslationUnit(Global& glb, ram::TranslationUnit& ramTranslationUnit);

}  // namespace souffle
