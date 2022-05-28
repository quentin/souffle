/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2022, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/*
 * @file Demonstration of overlay and virtual file systems.
 */

#include "MainDriver.h"
#include "parser/ParserDriver.h"
#include "souffle/io/IOSystem.h"

using namespace souffle;

/// A custom output write stream
struct TestWriteStream : public WriteStream {
    explicit TestWriteStream(const std::map<std::string, std::string>& rwOperation,
            const SymbolTable& symbolTable, const RecordTable& recordTable)
            : WriteStream(rwOperation, symbolTable, recordTable) {}

    void writeNullary() override {
        std::cout << "()\n";
    }

    void writeNextTuple(const RamDomain* tup) override {
        for (std::size_t col = 0; col < arity; ++col) {
            if (col > 0) {
                std::cout << "\t";
            }
            writeNextTupleElement(typeAttributes.at(col), tup[col]);
        }
        std::cout << "\n";
    }

    void writeNextTupleElement(const std::string& type, RamDomain value) {
        switch (type[0]) {
            case 'i': std::cout << value; break;
            default: /* don't care about other types */ break;
        }
    }
};

/// Factory of the custom output write stream
struct TestWriteStreamFactory : public WriteStreamFactory {
    std::unique_ptr<WriteStream> getWriter(const std::map<std::string, std::string>& rwOperation,
            const SymbolTable& symbolTable, const RecordTable& recordTable) override {
        return std::make_unique<TestWriteStream>(rwOperation, symbolTable, recordTable);
    }

    const std::string& getName() const override {
        return name;
    }

    const std::string name = "test";
};

int main(int, char**) {
    auto InputDl = std::make_shared<souffle::SingleFileFS>("input.dl",
            R"datalog(
      .decl query(v:number)
      .output query(IO=test)
      .include "include/data.dl"
    )datalog");

    auto IncludeDl = std::make_shared<souffle::SingleFileFS>("include/data.dl",
            R"datalog(
      query(1).
      query(2).
      query(3).
    )datalog");

    try {
        Global glb;
        glb.config().set("jobs", "1");
        glb.config().set("", "input.dl");
        // must set output-dir to something different from "-" in order to have
        // our custom output write stream called-back.
        glb.config().set("output-dir", ".");

        auto testWriteStreamFactory = std::make_shared<TestWriteStreamFactory>();
        IOSystem::getInstance().registerWriteStreamFactory(testWriteStreamFactory);

        ErrorReport errReport;
        DebugReport dbgReport(glb);

        auto VFS = std::make_shared<OverlayFileSystem>(InputDl);
        VFS->pushOverlay(IncludeDl);
        auto astTranslationUnit =
                ParserDriver::parseTranslationUnitFromFS(glb, "input.dl", errReport, dbgReport, VFS);

        auto pipeline = astTransformationPipeline(glb);
        pipeline->apply(*astTranslationUnit);

        auto unitTranslator = getUnitTranslator(glb);
        auto ramTranslationUnit = unitTranslator->translateUnit(*astTranslationUnit);

        auto ramTransform = ramTransformerSequence(glb);
        ramTransform->apply(*ramTranslationUnit);

        const bool success = interpretTranslationUnit(glb, *ramTranslationUnit);
        if (!success) {
            std::exit(EXIT_FAILURE);
        }
    } catch (const std::exception& e) {
        std::cerr << e.what() << std::endl;
        std::exit(EXIT_FAILURE);
    }

    return 0;
}
