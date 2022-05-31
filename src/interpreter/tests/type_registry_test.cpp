/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2022, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/**
 * @file A test of the type registry and the usage of libsouffle main driver interface.
 * Demonstrate how to perform custom output without the need for JSON data.
 */

#include "tests/test.h"

#include "Global.h"
#include "MainDriver.h"
#include "interpreter/Engine.h"
#include "parser/ParserDriver.h"
#include "souffle/io/IOSystem.h"

#include <map>
#include <set>
#include <string>

namespace souffle::interpreter::test {

using namespace ram;
using namespace ast;

using RelationTuplesType = std::map<std::string, std::set<std::string>>;

/**
 * Custom output writer.
 *
 * Writes pretty-printed tuples to a set.
 */
class CustomWriter : public WriteAllInterface {
public:
    CustomWriter(RelationTuplesType& relationTuples,
            const TypeRegistry& typeRegistry, const SymbolTable& symbolTable, const RecordTable& recordTable)
            : RelTuples(relationTuples), TypeReg(typeRegistry), SymTable(symbolTable), RecTable(recordTable) {}

    void writeAll(const RelationBase& rel) override {
        auto& tuples = RelTuples[rel.getName()];
        const std::size_t arity = rel.getArity();
        const TypeDesc* desc = rel.getTypeDescriptor();
        rel.each([&](const RamDomain* tuple) {
            std::stringstream tupleStream;
            for (std::size_t i = 0; i < arity; ++i) {
                if (i > 0) {
                    tupleStream << ", ";
                }
                const TypeElement* elem = desc->getElement(i);
                tupleStream << elem->first << ": ";
                printValue(tupleStream, *elem->second, tuple[i]);
            }
            tuples.emplace(tupleStream.str());
        });
    }

private:
    bool isOrSubset(const TypeDesc& desc, std::string_view id) const {
        const TypeDesc* T = &desc;
        while (T->isSubset()) {
            if (T->hasIdentifier(id)) {
                return true;
            }
            T = T->aux();
        }
        return T->hasIdentifier(id);
    }

    void printValue(std::ostream& out, const TypeDesc& desc, const RamDomain v) const {
        if (isOrSubset(desc, "symbol")) {
            out << SymTable.decode(v);
        } else if (isOrSubset(desc, "number")) {
            out << v;
        } else if (isOrSubset(desc, "unsigned")) {
            out << ramBitCast<RamUnsigned>(v);
        } else if (isOrSubset(desc, "float")) {
            out << ramBitCast<RamFloat>(v);
        } else if (desc.isUnion()) {
            printValue(out, *desc.aux(), v);
        } else if (desc.isRecord()) {
            if (v == 0) {
              out << "nil";
            } else {
                out << "[";
                const RamDomain* const record = RecTable.unpack(v, desc.size());
                for (std::size_t i = 0; i < desc.size(); ++i) {
                    if (i > 0) {
                        out << ", ";
                    }
                    const TypeElement* elem = desc.getElement(i);
                    out << elem->first << ": ";
                    printValue(out, *elem->second, record[i]);
                }
                out << "]";
            }
        } else if (desc.isEnumADT()) {
            const TypeDesc* branch = desc.getElementType(v);
            printValue(out, *branch, 0 /* dummy value*/);
        } else if (desc.isADT()) {
            const RamDomain* pair = RecTable.unpack(v, 2);
            const TypeDesc* branch = desc.getElementType(pair[0]);
            printValue(out, *branch, pair[1]);
        } else if (desc.isBranch()) {
            out << "$" << desc.canonicalIdentifier() << "(";
            if (desc.size() == 1) {
                out << *desc.getElementName(0) << ": ";
                printValue(out, *desc.getElementType(0), v);
            } else if (desc.size() > 1) {
                const RamDomain* const record = RecTable.unpack(v, desc.size());
                for (std::size_t i = 0; i < desc.size(); ++i) {
                    if (i > 0) {
                        out << ", ";
                    }
                    const TypeElement* elem = desc.getElement(i);
                    out << elem->first << ": ";
                    printValue(out, *elem->second, record[i]);
                }
            }
            out << ")";
        }
    }

    RelationTuplesType& RelTuples;

    const TypeRegistry& TypeReg;
    const SymbolTable& SymTable;
    const RecordTable& RecTable;

};

/** Custom output writer factory */
class CustomWriteFactory : public WriteStreamFactory {
public:
    CustomWriteFactory(const TypeRegistry& tyReg) : typeRegistry(tyReg) {}

    Own<WriteAllInterface> getWriter(const std::map<std::string, std::string>&,
            const SymbolTable& symbolTable, const RecordTable& recordTable) override {
        return std::make_unique<CustomWriter>(relTuples, typeRegistry, symbolTable, recordTable);
    }

    const std::string& getName() const override {
        return Name;
    }

    bool hasTuple(const std::string& relationName, const std::string& tuple) const {
        auto it = relTuples.find(relationName);
        if (it == relTuples.end()) {
          return false;
        }
        return it->second.count(tuple) > 0;
    }

    std::size_t countTuples(const std::string& relationName) const {
        auto it = relTuples.find(relationName);
        if (it == relTuples.end()) {
          return 0;
        }
        return it->second.size();
    }

private:
    const std::string Name{"custom"};
    const TypeRegistry& typeRegistry;
    RelationTuplesType relTuples;
};

/** An helper class for the test */
class Helper {
public:
    /** Evaluate the program formed by the given code and mark the given list of relations as outputs */
    void evaluate(const std::string& code, std::initializer_list<std::string> rels) {
        Global glb;
        glb.config().set("jobs", "1");

        ErrorReport Err(false);
        DebugReport Dbg(glb);

        auto astTU = ParserDriver::parseTranslationUnit(glb, code, Err, Dbg);
        Err.exitIfErrors();

        {
            // hack: use a parser driver to add elements in the translation unit
            ParserDriver Driver(glb);
            Driver.translationUnit = std::move(astTU);

            for (const auto& rel : rels) {
                // custom output directive
                auto Dir = mk<Directive>(DirectiveType::output, rel);
                Dir->addParameter("IO", "custom");
                Dir->addParameter("fact-dir", ".");
                Driver.addDirective(std::move(Dir));
            }

            // move the translation unit back
            astTU = std::move(Driver.translationUnit);
        }

        auto astPipeline = astTransformationPipeline(glb);
        astPipeline->apply(*astTU);

        auto unitTranslator = getUnitTranslator(glb);
        auto ramTU = unitTranslator->translateUnit(*astTU);

        auto ramTransform = ramTransformerSequence(glb);
        ramTransform->apply(*ramTU);

        // register the custom write stream factory
        customFactory = std::make_shared<CustomWriteFactory>(ramTU->getProgram().getTypeRegistry());
        IOSystem::getInstance().registerWriteStreamFactory(customFactory);

        auto interpreter = mk<Engine>(*ramTU, 1, nullptr, nullptr);
        interpreter->executeMain();
    }

    /** Return true if the relation contains the given tuple */
    bool hasTuple(const std::string& relationName, const std::string& tuple) const {
        return customFactory->hasTuple(relationName, tuple);
    }

    /** Return true if the relation contains the given list of tuples */
    bool hasTuples(const std::string& relationName, std::initializer_list<std::string> tuples) const {
        for (const std::string& tuple : tuples) {
            if (!customFactory->hasTuple(relationName, tuple)) {
                return false;
            }
        }
        return true;
    }

    /** Return true if the relation contains stricly the given list of tuples */
    bool strictlyHasTuples(const std::string& relationName, std::initializer_list<std::string> tuples) const {
        if (customFactory->countTuples(relationName) != tuples.size()) {
          return false;
        }
        for (const std::string& tuple : tuples) {
            if (!customFactory->hasTuple(relationName, tuple)) {
                return false;
            }
        }
        return true;
    }

private:
  std::shared_ptr<CustomWriteFactory> customFactory;

};

TEST(TypeRegistry, Simple) {

    std::string Code = R"datalog(
    .decl query(x:symbol)
    query("hello").
    query("world").
    )datalog";

    Helper helper;
    helper.evaluate(Code, {"query"});

    EXPECT_TRUE(helper.hasTuple("query", "x: hello"));
    EXPECT_TRUE(helper.hasTuple("query", "x: world"));
}

TEST(TypeRegistry, Enum) {
  std::string Code = R"datalog(
  .type YesNo = Yes{} | No{}
  .decl enum(e:YesNo)
  enum($Yes()).
  enum($No()).
  )datalog";

  Helper helper;
  helper.evaluate(Code, {"enum"});

  EXPECT_TRUE(helper.hasTuples("enum",{
        "e: $Yes()",
        "e: $No()"
        }));
}

TEST(TypeRegistry, Tree) {
  std::string Code = R"datalog(
  .type Tree = Empty {}
             | Node {t1: Tree, val: unsigned, t2: Tree}
  .decl tree(t:Tree)
  tree(
    $Node(
      $Empty(), 1234, $Empty()
    )
  ).
  tree(
    $Node(
      $Node($Empty(), 1, $Node($Empty(), 2, $Empty())),
      3,
      $Node($Empty(), 4, $Empty())
    )
  ).
  )datalog";

  Helper helper;
  helper.evaluate(Code, {"tree"});

  EXPECT_TRUE(helper.hasTuples("tree",{
        "t: $Node(t1: $Empty(), val: 1234, t2: $Empty())",
        "t: $Node(t1: $Node(t1: $Empty(), val: 1, t2: $Node(t1: $Empty(), val: 2, t2: $Empty())), val: 3, t2: $Node(t1: $Empty(), val: 4, t2: $Empty()))"
        }));
}

TEST(TypeRegistry, RecursiveRecord) {
  std::string Code = R"datalog(
  .type List = [s:symbol, tail:List]
  .decl list(l:List)
  list(["a", ["b", ["c", nil]]]).
  )datalog";

  Helper helper;
  helper.evaluate(Code, {"list"});

  EXPECT_TRUE(helper.hasTuples("list",{
        "l: [s: a, tail: [s: b, tail: [s: c, tail: nil]]]"
        }));
}

TEST(TypeRegistry, EmptyRecord) {
  std::string Code = R"datalog(
  .type Empty = []
  .decl empty(e:Empty)
  empty([]).
  )datalog";

  Helper helper;
  helper.evaluate(Code, {"empty"});

  EXPECT_TRUE(helper.hasTuples("empty",{
        "e: []"
        }));
}

TEST(TypeRegistry, Nullary) {
  std::string Code = R"datalog(
  .decl set()
  set().
  .decl unset()
  )datalog";

  Helper helper;
  helper.evaluate(Code, {"set","unset"});

  EXPECT_TRUE(helper.strictlyHasTuples("set",{ "" }));
  EXPECT_TRUE(helper.strictlyHasTuples("unset",{ }));
}

}  // namespace souffle::interpreter::test
