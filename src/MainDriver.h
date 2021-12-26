
#pragma once

#include "Global.h"
#include "ast/transform/Pipeline.h"
#include "ast2ram/UnitTranslator.h"
#include "ast2ram/utility/TranslatorContext.h"
#include "include/souffle/utility/Types.h"
#include "ram/Program.h"
#include "ram/transform/Transformer.h"

namespace souffle {
const char* packageVersion();
int main(Global& glb, const char* souffle_executable);
Own<ast::transform::PipelineTransformer> astTransformationPipeline(Global& glb);
Own<ast2ram::UnitTranslator> getUnitTranslator(Global& glb);
Own<ram::transform::Transformer> ramTransformerSequence(Global& glb);
bool interpretTranslationUnit(Global& glb, ram::TranslationUnit& ramTranslationUnit);

}  // namespace souffle
