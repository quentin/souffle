/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2023, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/*
 * @file Demonstration of program types reflection
 */

#include "tests/test.h"

#include "MainDriver.h"
#include "parser/ParserDriver.h"
#include "souffle/io/IOSystem.h"

using namespace souffle;

using Facts = std::set<std::tuple<std::string, std::string>>;

/// A custom output write stream
struct TestWriteStream : public WriteStream {
    explicit TestWriteStream(const TypeDesc* tupleType, Facts& facts,
            const std::map<std::string, std::string>& rwOperation, const SymbolTable& symbolTable,
            const RecordTable& recordTable)
            : WriteStream(rwOperation, symbolTable, recordTable), tupleTy(tupleType), facts(facts),
              name(rwOperation.at("name")) {}

    void writeNullary() override {
        facts.emplace(name, "()");
    }

    void writeNextTuple(const RamDomain* tup) override {
        std::stringstream fact;
        for (std::size_t col = 0; col < tupleTy->size(); ++col) {
            if (col > 0) {
                fact << " ";
            }
            writeNextTupleElement(tupleTy->getElementType(col), tup[col], fact);
        }
        facts.emplace(name, fact.str());
    }

    void writeNextTupleElement(const TypeDesc* const desc, RamDomain value, std::stringstream& fact) {
        switch (desc->kind()) {
            case TypeKind::Primitive: writePrimitive(desc, value, fact); break;
            default:  // ignore
                break;
        }
    }

    void writePrimitive(const TypeDesc* const desc, RamDomain value, std::stringstream& fact) {
        if (desc->hasIdentifier("number")) {
            fact << std::to_string(value);
        } else if (desc->hasIdentifier("unsigned")) {
            fact << std::to_string(ramBitCast<RamUnsigned>(value));
        } else if (desc->hasIdentifier("float")) {
            fact << std::to_string(ramBitCast<RamFloat>(value));
        } else if (desc->hasIdentifier("symbol")) {
            fact << symbolTable.decode(value);
        }
    }

private:
    const TypeDesc* const tupleTy;
    Facts& facts;
    const std::string name;
};

/// Factory of the custom output write stream
struct TestWriteStreamFactory : public WriteStreamFactory {
    TestWriteStreamFactory(const TypeRegistry& tr, Facts& facts) : tyreg(tr), facts(facts) {}

    std::unique_ptr<WriteStream> getWriter(const std::map<std::string, std::string>& rwOperation,
            const SymbolTable& symbolTable, const RecordTable& recordTable) override {
        const TypeDesc* const tupleTy = tyreg.getTuple(rwOperation.at("name"));
        return std::make_unique<TestWriteStream>(tupleTy, facts, rwOperation, symbolTable, recordTable);
    }

    const std::string& getName() const override {
        return name;
    }

    const std::string name = "test";

private:
    const TypeRegistry& tyreg;
    Facts& facts;
};

namespace {
std::unique_ptr<ram::TranslationUnit> buildRamTU(Global& glb, const std::string& Code) {
    auto InputDl = std::make_shared<souffle::SingleFileFS>("input.dl", Code);

    glb.config().set("jobs", "1");
    glb.config().set("", "input.dl");
    // must set output-dir to something different from "-" in order to have
    // our custom output write stream called-back.
    glb.config().set("output-dir", ".");

    ErrorReport errReport;
    DebugReport dbgReport(glb);

    auto VFS = std::make_shared<OverlayFileSystem>(InputDl);
    auto astTranslationUnit =
            ParserDriver::parseTranslationUnitFromFS(glb, "input.dl", errReport, dbgReport, VFS);

    auto pipeline = astTransformationPipeline(glb);
    pipeline->apply(*astTranslationUnit);

    auto unitTranslator = getUnitTranslator(glb);
    auto ramTranslationUnit = unitTranslator->translateUnit(*astTranslationUnit);

    auto ramTransform = ramTransformerSequence(glb);
    ramTransform->apply(*ramTranslationUnit);

    return ramTranslationUnit;
}
}  // namespace

TEST(TypeReflection, Writer) {
    auto Code = R"datalog(
      .decl query(v:number, s:symbol)
      .output query(IO=test)
      query(1, "one").
      query(2, "two").
      query(3, "three").
    )datalog";

    Global glb;
    auto ramTranslationUnit = buildRamTU(glb, Code);

    auto& prog = ramTranslationUnit->getProgram();
    const TypeRegistry& tyreg = prog.getTypeRegistry();

    std::set<std::tuple<std::string, std::string>> outputFacts;
    std::set<std::tuple<std::string, std::string>> expectedFacts = {
            {"query", "1 one"},
            {"query", "2 two"},
            {"query", "3 three"},
    };

    auto testWriteStreamFactory = std::make_shared<TestWriteStreamFactory>(tyreg, outputFacts);
    IOSystem::getInstance().registerWriteStreamFactory(testWriteStreamFactory);

    const bool success = interpretTranslationUnit(glb, *ramTranslationUnit);
    EXPECT_TRUE(success);

    EXPECT_EQ(expectedFacts, outputFacts);
}

TEST(TypeReflection, Types) {
    auto Code = R"datalog(
      .type ANumber = number
      .type Rec = [head:ANumber, tail:Rec]
    )datalog";

    Global glb;
    auto ramTranslationUnit = buildRamTU(glb, Code);

    auto& prog = ramTranslationUnit->getProgram();
    const TypeRegistry& tyreg = prog.getTypeRegistry();

    {
        const TypeDesc* T = tyreg.get("ANumber");
        EXPECT_NE(nullptr, T);
        EXPECT_TRUE(T->isPrimitive());
        EXPECT_TRUE(T->hasIdentifier("number"));
    }

    {
        const TypeDesc* T = tyreg.get("Rec");
        EXPECT_NE(nullptr, T);
        EXPECT_TRUE(T->isRecord());
        EXPECT_EQ(2, T->size());
        EXPECT_EQ("head", *T->getElementName(0));
        EXPECT_EQ("tail", *T->getElementName(1));
    }
}
