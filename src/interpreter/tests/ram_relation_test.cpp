/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ram_relation_test.cpp
 *
 * Tests arithmetic evaluation by the Interpreter.
 *
 ***********************************************************************/

#include "tests/test.h"

#include "Global.h"
#include "RelationTag.h"
#include "interpreter/Engine.h"
#include "ram/Expression.h"
#include "ram/Erase.h"
#include "ram/IO.h"
#include "ram/Insert.h"
#include "ram/Program.h"
#include "ram/Query.h"
#include "ram/Relation.h"
#include "ram/Sequence.h"
#include "ram/SignedConstant.h"
#include "ram/Statement.h"
#include "ram/StringConstant.h"
#include "ram/TranslationUnit.h"
#include "reports/DebugReport.h"
#include "reports/ErrorReport.h"
#include "souffle/RamTypes.h"
#include "souffle/SymbolTable.h"
#include "souffle/utility/ContainerUtil.h"
#include "souffle/utility/json11.h"
#include <algorithm>
#include <cstddef>
#include <iomanip>
#include <iostream>
#include <limits>
#include <map>
#include <memory>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

namespace souffle::interpreter::test {

using namespace ram;

using json11::Json;

const std::string testInterpreterStore(
        std::vector<std::string> attribs, std::vector<std::string> attribsTypes, VecOwn<Expression> exprs) {
    Global glb;
    glb.config().set("jobs", "1");

    const std::size_t arity = attribs.size();

    VecOwn<ram::Relation> rels;
    Own<ram::Relation> myrel =
            mk<ram::Relation>("test", arity, 0, attribs, attribsTypes, RelationRepresentation::BTREE);

    Json types = Json::object{
            {"relation", Json::object{{"arity", static_cast<long long>(arity)},
                                 {"types", Json::array(attribsTypes.begin(), attribsTypes.end())}}}};

    std::map<std::string, std::string> dirs = {{"operation", "output"}, {"IO", "stdout"},
            {"attributeNames", "x\ty"}, {"name", "test"}, {"auxArity", "0"}, {"types", types.dump()}};

    std::map<std::string, std::string> ioDirs = std::map<std::string, std::string>(dirs);

    Own<ram::Statement> main = mk<ram::Sequence>(
            mk<ram::Query>(mk<ram::Insert>("test", std::move(exprs))), mk<ram::IO>("test", ioDirs));

    rels.push_back(std::move(myrel));
    std::map<std::string, Own<Statement>> subs;
    Own<ram::Program> prog = mk<Program>(std::move(rels), std::move(main), std::move(subs));

    ErrorReport errReport;
    DebugReport debugReport(glb);

    TranslationUnit translationUnit(glb, std::move(prog), errReport, debugReport);

    // configure and execute interpreter
    Own<Engine> interpreter = mk<Engine>(translationUnit, 1);

    std::streambuf* oldCoutStreambuf = std::cout.rdbuf();
    std::ostringstream sout;
    std::cout.rdbuf(sout.rdbuf());

    interpreter->executeMain();

    std::cout.rdbuf(oldCoutStreambuf);

    return sout.str();
}

const std::string testInterpreterProgram(std::vector<std::string> attributes,
        std::vector<std::string> attributeTypes, std::size_t auxiliaryArity,
        RelationRepresentation representation, VecOwn<ram::Statement> statements) {
    Global glb;
    glb.config().set("jobs", "1");

    const std::size_t arity = attributes.size();
    VecOwn<ram::Relation> relations;
    relations.push_back(mk<ram::Relation>("test", arity, auxiliaryArity, attributes, attributeTypes,
            representation));

    Own<ram::Statement> main;
    for (auto it = statements.rbegin(); it != statements.rend(); ++it) {
        if (!main) {
            main = std::move(*it);
        } else {
            main = mk<ram::Sequence>(std::move(*it), std::move(main));
        }
    }

    Json types = Json::object{
            {"relation", Json::object{{"arity", static_cast<long long>(arity)},
                                 {"types", Json::array(attributeTypes.begin(), attributeTypes.end())}}}};
    std::map<std::string, std::string> ioDirs = {{"operation", "output"}, {"IO", "stdout"},
            {"attributeNames", "x\ty"}, {"name", "test"},
            {"auxArity", std::to_string(auxiliaryArity)}, {"types", types.dump()}};
    main = mk<ram::Sequence>(std::move(main), mk<ram::IO>("test", ioDirs));

    std::map<std::string, Own<Statement>> subroutines;
    Own<Program> program = mk<Program>(std::move(relations), std::move(main), std::move(subroutines));
    ErrorReport errorReport;
    DebugReport debugReport(glb);
    TranslationUnit translationUnit(glb, std::move(program), errorReport, debugReport);
    Own<Engine> interpreter = mk<Engine>(translationUnit, 1);

    std::streambuf* oldCoutStreambuf = std::cout.rdbuf();
    std::ostringstream output;
    std::cout.rdbuf(output.rdbuf());
    interpreter->executeMain();
    std::cout.rdbuf(oldCoutStreambuf);
    return output.str();
}

TEST(IO_store, FloatSimple) {
    std::vector<std::string> attribs = {"a", "b"};
    std::vector<std::string> attribsTypes = {"f", "f"};

    VecOwn<Expression> exprs;
    exprs.push_back(mk<SignedConstant>(ramBitCast(static_cast<RamFloat>(0.5))));
    exprs.push_back(mk<SignedConstant>(ramBitCast(static_cast<RamFloat>(0.5))));

    std::string expected = R"(---------------
test
===============
0.5	0.5
===============
)";

    auto result = testInterpreterStore(attribs, attribsTypes, std::move(exprs));
    EXPECT_EQ(expected, result);
}

TEST(IO_store, DynamicArityBeyondStaticInterpreterRange) {
    constexpr std::size_t arity = 32;
    std::vector<std::string> attributes;
    std::vector<std::string> types(arity, "i");
    VecOwn<Expression> values;
    std::stringstream expected;
    expected << "---------------\ntest\n===============\n";
    for (std::size_t i = 0; i < arity; ++i) {
        attributes.push_back("a" + std::to_string(i));
        values.push_back(mk<SignedConstant>(static_cast<RamDomain>(i)));
        if (i != 0) expected << '\t';
        expected << i;
    }
    expected << "\n===============\n";

    EXPECT_EQ(expected.str(), testInterpreterStore(attributes, types, std::move(values)));
}

TEST(InterpreterDynamicBackends, BtreeDelete) {
    VecOwn<Expression> inserted;
    inserted.push_back(mk<SignedConstant>(1));
    inserted.push_back(mk<SignedConstant>(2));
    VecOwn<Expression> erased;
    erased.push_back(mk<SignedConstant>(1));
    erased.push_back(mk<SignedConstant>(2));

    VecOwn<ram::Statement> statements;
    statements.push_back(mk<ram::Query>(mk<ram::Insert>("test", std::move(inserted))));
    statements.push_back(mk<ram::Query>(mk<ram::Erase>("test", std::move(erased))));

    const auto output = testInterpreterProgram(
            {"a", "b"}, {"i", "i"}, 0, RelationRepresentation::BTREE_DELETE, std::move(statements));
    EXPECT_EQ("---------------\ntest\n===============\n===============\n", output);
}

TEST(InterpreterDynamicBackends, ProvenanceUpdate) {
    VecOwn<ram::Statement> statements;
    for (const auto [rule, level] : {std::pair<RamDomain, RamDomain>{7, 5}, {3, 5}, {1, 7}}) {
        VecOwn<Expression> values;
        values.push_back(mk<SignedConstant>(1));
        values.push_back(mk<SignedConstant>(2));
        values.push_back(mk<SignedConstant>(rule));
        values.push_back(mk<SignedConstant>(level));
        statements.push_back(mk<ram::Query>(mk<ram::Insert>("test", std::move(values))));
    }

    const auto output = testInterpreterProgram({"x", "y", "@rule_number", "@level_number"},
            {"i", "i", "i", "i"}, 2, RelationRepresentation::BTREE, std::move(statements));
    EXPECT_EQ("---------------\ntest\n===============\n1\t2\t3\t5\n===============\n", output);
}

TEST(InterpreterDynamicBackends, EquivalenceRelationClosure) {
    VecOwn<ram::Statement> statements;
    for (const auto [lhs, rhs] : {std::pair<RamDomain, RamDomain>{1, 2}, {2, 3}}) {
        VecOwn<Expression> values;
        values.push_back(mk<SignedConstant>(lhs));
        values.push_back(mk<SignedConstant>(rhs));
        statements.push_back(mk<ram::Query>(mk<ram::Insert>("test", std::move(values))));
    }

    const auto output = testInterpreterProgram(
            {"a", "b"}, {"i", "i"}, 0, RelationRepresentation::EQREL, std::move(statements));
    EXPECT_EQ(std::size_t{9}, static_cast<std::size_t>(std::count(output.begin(), output.end(), '\n') - 4));
    EXPECT_NE(std::string::npos, output.find("1\t1\n"));
    EXPECT_NE(std::string::npos, output.find("1\t3\n"));
    EXPECT_NE(std::string::npos, output.find("3\t1\n"));
}

TEST(IO_store, Signed) {
    std::vector<RamDomain> randomNumbers = testutil::generateValues<RamDomain>();
    const std::size_t len = randomNumbers.size();

    // a0 a1 a2...
    std::vector<std::string> attribs(len, "a");
    for (std::size_t i = 0; i < len; ++i) {
        attribs[i].append(std::to_string(i));
    }

    std::vector<std::string> attribsTypes(len, "i");

    VecOwn<Expression> exprs;
    for (RamDomain i : randomNumbers) {
        exprs.push_back(mk<SignedConstant>(i));
    }

    std::stringstream expected;
    expected << "---------------"
             << "\n"
             << "test"
             << "\n"
             << "==============="
             << "\n"
             << randomNumbers[0];

    for (std::size_t i = 1; i < len; ++i) {
        expected << "\t" << randomNumbers[i];
    }
    expected << "\n"
             << "==============="
             << "\n";

    auto result = testInterpreterStore(attribs, attribsTypes, std::move(exprs));
    EXPECT_EQ(expected.str(), result);
}

TEST(IO_store, Float) {
    const std::vector<RamFloat> randomNumbers = testutil::generateValues<RamFloat>();
    const std::size_t len = randomNumbers.size();

    // a0 a1 a2...
    std::vector<std::string> attribs(len, "a");
    for (std::size_t i = 0; i < len; ++i) {
        attribs[i].append(std::to_string(i));
    }

    std::vector<std::string> attribsTypes(len, "f");

    VecOwn<Expression> exprs;
    for (RamFloat f : randomNumbers) {
        exprs.push_back(mk<SignedConstant>(ramBitCast(f)));
    }

    std::stringstream expected;
    expected << std::setprecision(std::numeric_limits<RamFloat>::max_digits10);

    expected << "---------------"
             << "\n"
             << "test"
             << "\n"
             << "==============="
             << "\n"
             << randomNumbers[0];

    for (std::size_t i = 1; i < randomNumbers.size(); ++i) {
        expected << "\t" << randomNumbers[i];
    }
    expected << "\n"
             << "==============="
             << "\n";

    auto result = testInterpreterStore(attribs, attribsTypes, std::move(exprs));
    EXPECT_EQ(expected.str(), result);
}

TEST(IO_store, Unsigned) {
    const std::vector<RamUnsigned> randomNumbers = testutil::generateValues<RamUnsigned>();
    const std::size_t len = randomNumbers.size();

    // a0 a1 a2...
    std::vector<std::string> attribs(len, "a");
    for (std::size_t i = 0; i < len; ++i) {
        attribs[i].append(std::to_string(i));
    }

    std::vector<std::string> attribsTypes(len, "u");

    VecOwn<Expression> exprs;
    for (RamUnsigned u : randomNumbers) {
        exprs.push_back(mk<SignedConstant>(ramBitCast(u)));
    }

    std::stringstream expected;
    expected << "---------------"
             << "\n"
             << "test"
             << "\n"
             << "==============="
             << "\n"
             << randomNumbers[0];

    for (std::size_t i = 1; i < randomNumbers.size(); ++i) {
        expected << "\t" << randomNumbers[i];
    }
    expected << "\n"
             << "==============="
             << "\n";

    auto result = testInterpreterStore(attribs, attribsTypes, std::move(exprs));
    EXPECT_EQ(expected.str(), result);
}

// Test (store) with different delimiter
TEST(IO_store, SignedChangedDelimiter) {
    const std::vector<RamDomain> randomNumbers = testutil::generateValues<RamDomain>();
    const std::size_t len = randomNumbers.size();
    const std::string delimiter{", "};

    Global glb;
    glb.config().set("jobs", "1");

    VecOwn<ram::Relation> rels;

    // a0 a1 a2...
    std::vector<std::string> attribs(len, "a");
    for (std::size_t i = 0; i < len; ++i) {
        attribs[i].append(std::to_string(i));
    }

    std::vector<std::string> attribsTypes(len, "i");

    Own<ram::Relation> myrel =
            mk<ram::Relation>("test", len, 0, attribs, attribsTypes, RelationRepresentation::BTREE);

    Json types = Json::object{
            {"relation", Json::object{{"arity", static_cast<long long>(attribsTypes.size())},
                                 {"types", Json::array(attribsTypes.begin(), attribsTypes.end())}}}};

    std::map<std::string, std::string> dirs = {{"operation", "output"}, {"IO", "stdout"},
            {"attributeNames", "x\ty"}, {"name", "test"}, {"auxArity", "0"}, {"delimiter", delimiter},
            {"types", types.dump()}};

    std::map<std::string, std::string> ioDirs = std::map<std::string, std::string>(dirs);

    VecOwn<Expression> exprs;
    for (RamDomain i : randomNumbers) {
        exprs.push_back(mk<SignedConstant>(i));
    }

    Own<ram::Statement> main = mk<ram::Sequence>(
            mk<ram::Query>(mk<ram::Insert>("test", std::move(exprs))), mk<ram::IO>("test", ioDirs));

    rels.push_back(std::move(myrel));
    std::map<std::string, Own<Statement>> subs;
    Own<Program> prog = mk<Program>(std::move(rels), std::move(main), std::move(subs));

    ErrorReport errReport;
    DebugReport debugReport(glb);

    TranslationUnit translationUnit(glb, std::move(prog), errReport, debugReport);

    // configure and execute interpreter
    Own<Engine> interpreter = mk<Engine>(translationUnit, 1);

    std::streambuf* oldCoutStreambuf = std::cout.rdbuf();
    std::ostringstream sout;
    std::cout.rdbuf(sout.rdbuf());

    interpreter->executeMain();

    std::cout.rdbuf(oldCoutStreambuf);

    std::stringstream expected;
    expected << "---------------"
             << "\n"
             << "test"
             << "\n"
             << "==============="
             << "\n"
             << randomNumbers[0];

    for (std::size_t i = 1; i < randomNumbers.size(); ++i) {
        expected << delimiter << randomNumbers[i];
    }
    expected << "\n"
             << "==============="
             << "\n";

    EXPECT_EQ(expected.str(), sout.str());
}

TEST(IO_store, MixedTypes) {
    Global glb;
    glb.config().set("jobs", "1");

    VecOwn<ram::Relation> rels;

    std::vector<std::string> attribs{"t", "o", "s", "i", "a"};

    std::vector<std::string> attribsTypes{"i", "u", "f", "f", "s"};

    Own<ram::Relation> myrel =
            mk<ram::Relation>("test", 5, 0, attribs, attribsTypes, RelationRepresentation::BTREE);

    Json types = Json::object{
            {"relation", Json::object{{"arity", static_cast<long long>(attribsTypes.size())},
                                 {"types", Json::array(attribsTypes.begin(), attribsTypes.end())}}}};

    std::map<std::string, std::string> dirs = {{"operation", "output"}, {"IO", "stdout"}, {"auxArity", "0"},
            {"attributeNames", "x\ty"}, {"name", "test"}, {"types", types.dump()}};
    std::map<std::string, std::string> ioDirs = std::map<std::string, std::string>(dirs);

    ErrorReport errReport;
    DebugReport debugReport(glb);

    VecOwn<Expression> exprs;
    RamFloat floatValue = 27.75;
    exprs.push_back(mk<SignedConstant>(3));
    exprs.push_back(mk<SignedConstant>(ramBitCast(static_cast<RamUnsigned>(27))));
    exprs.push_back(mk<SignedConstant>(ramBitCast(static_cast<RamFloat>(floatValue))));
    exprs.push_back(mk<SignedConstant>(ramBitCast(static_cast<RamFloat>(floatValue))));
    exprs.push_back(mk<ram::StringConstant>("meow"));

    Own<ram::Statement> main = mk<ram::Sequence>(
            mk<ram::Query>(mk<ram::Insert>("test", std::move(exprs))), mk<ram::IO>("test", ioDirs));

    rels.push_back(std::move(myrel));
    std::map<std::string, Own<Statement>> subs;
    Own<Program> prog = mk<Program>(std::move(rels), std::move(main), std::move(subs));

    TranslationUnit translationUnit(glb, std::move(prog), errReport, debugReport);

    // configure and execute interpreter
    Own<Engine> interpreter = mk<Engine>(translationUnit, 1);

    std::streambuf* oldCoutStreambuf = std::cout.rdbuf();
    std::ostringstream sout;
    std::cout.rdbuf(sout.rdbuf());

    interpreter->executeMain();

    std::cout.rdbuf(oldCoutStreambuf);

    std::stringstream expected;
    expected << std::setprecision(std::numeric_limits<RamFloat>::max_digits10);
    expected << "---------------"
             << "\n"
             << "test"
             << "\n"
             << "==============="
             << "\n"
             << 3 << "\t" << 27 << "\t" << floatValue << "\t" << floatValue << "\t"
             << "meow"
             << "\n"
             << "==============="
             << "\n";

    EXPECT_EQ(expected.str(), sout.str());
}

TEST(IO_load, Signed) {
    std::streambuf* backupCin = std::cin.rdbuf();
    std::istringstream testInput("5	3");
    std::cin.rdbuf(testInput.rdbuf());

    Global glb;
    glb.config().set("jobs", "1");

    VecOwn<ram::Relation> rels;

    std::vector<std::string> attribs = {"a", "b"};
    std::vector<std::string> attribsTypes = {"i", "i"};
    Own<ram::Relation> myrel =
            mk<ram::Relation>("test", 2, 0, attribs, attribsTypes, RelationRepresentation::BTREE);

    Json types = Json::object{
            {"relation", Json::object{{"arity", static_cast<long long>(attribsTypes.size())},
                                 {"types", Json::array(attribsTypes.begin(), attribsTypes.end())}}}};

    std::map<std::string, std::string> readDirs = {{"operation", "input"}, {"IO", "stdin"}, {"auxArity", "0"},
            {"attributeNames", "x\ty"}, {"name", "test"}, {"types", types.dump()}};
    std::map<std::string, std::string> readIoDirs = std::map<std::string, std::string>(readDirs);

    std::map<std::string, std::string> writeDirs = {{"operation", "output"}, {"IO", "stdout"},
            {"auxArity", "0"}, {"attributeNames", "x\ty"}, {"name", "test"}, {"types", types.dump()}};
    std::map<std::string, std::string> writeIoDirs = std::map<std::string, std::string>(writeDirs);

    Own<ram::Statement> main =
            mk<ram::Sequence>(mk<ram::IO>("test", readIoDirs), mk<ram::IO>("test", writeIoDirs));

    rels.push_back(std::move(myrel));
    std::map<std::string, Own<Statement>> subs;
    Own<Program> prog = mk<Program>(std::move(rels), std::move(main), std::move(subs));

    ErrorReport errReport;
    DebugReport debugReport(glb);

    TranslationUnit translationUnit(glb, std::move(prog), errReport, debugReport);

    // configure and execute interpreter
    Own<Engine> interpreter = mk<Engine>(translationUnit, 1);

    std::streambuf* oldCoutStreambuf = std::cout.rdbuf();
    std::ostringstream sout;
    std::cout.rdbuf(sout.rdbuf());

    interpreter->executeMain();

    std::cout.rdbuf(oldCoutStreambuf);

    std::string expected = R"(---------------
test
===============
5	3
===============
)";
    EXPECT_EQ(expected, sout.str());

    std::cin.rdbuf(backupCin);
}

TEST(IO_load, Float) {
    std::streambuf* backupCin = std::cin.rdbuf();
    std::istringstream testInput("0.5	0.5");
    std::cin.rdbuf(testInput.rdbuf());

    Global glb;
    glb.config().set("jobs", "1");

    VecOwn<ram::Relation> rels;

    std::vector<std::string> attribs = {"a", "b"};
    std::vector<std::string> attribsTypes = {"f", "f"};
    Own<ram::Relation> myrel =
            mk<ram::Relation>("test", 2, 0, attribs, attribsTypes, RelationRepresentation::BTREE);

    Json types = Json::object{
            {"relation", Json::object{{"arity", static_cast<long long>(attribsTypes.size())},
                                 {"types", Json::array(attribsTypes.begin(), attribsTypes.end())}}}};

    std::map<std::string, std::string> readDirs = {{"operation", "input"}, {"IO", "stdin"}, {"auxArity", "0"},
            {"attributeNames", "x\ty"}, {"name", "test"}, {"types", types.dump()}};
    std::map<std::string, std::string> readIoDirs = std::map<std::string, std::string>(readDirs);

    std::map<std::string, std::string> writeDirs = {{"operation", "output"}, {"IO", "stdout"},
            {"auxArity", "0"}, {"attributeNames", "x\ty"}, {"name", "test"}, {"types", types.dump()}};
    std::map<std::string, std::string> writeIoDirs = std::map<std::string, std::string>(writeDirs);

    Own<ram::Statement> main =
            mk<ram::Sequence>(mk<ram::IO>("test", readIoDirs), mk<ram::IO>("test", writeIoDirs));

    rels.push_back(std::move(myrel));
    std::map<std::string, Own<Statement>> subs;
    Own<Program> prog = mk<Program>(std::move(rels), std::move(main), std::move(subs));

    ErrorReport errReport;
    DebugReport debugReport(glb);

    TranslationUnit translationUnit(glb, std::move(prog), errReport, debugReport);

    // configure and execute interpreter
    Own<Engine> interpreter = mk<Engine>(translationUnit, 1);

    std::streambuf* oldCoutStreambuf = std::cout.rdbuf();
    std::ostringstream sout;
    std::cout.rdbuf(sout.rdbuf());

    interpreter->executeMain();

    std::cout.rdbuf(oldCoutStreambuf);

    std::string expected = R"(---------------
test
===============
0.5	0.5
===============
)";
    EXPECT_EQ(expected, sout.str());

    std::cin.rdbuf(backupCin);
}

TEST(IO_load, Unsigned) {
    std::streambuf* backupCin = std::cin.rdbuf();
    std::istringstream testInput("6	6");
    std::cin.rdbuf(testInput.rdbuf());

    Global glb;
    glb.config().set("jobs", "1");

    VecOwn<ram::Relation> rels;

    std::vector<std::string> attribs = {"a", "b"};
    std::vector<std::string> attribsTypes = {"u", "u"};
    Own<ram::Relation> myrel =
            mk<ram::Relation>("test", 2, 0, attribs, attribsTypes, RelationRepresentation::BTREE);

    Json types = Json::object{
            {"relation", Json::object{{"arity", static_cast<long long>(attribsTypes.size())},
                                 {"types", Json::array(attribsTypes.begin(), attribsTypes.end())}}}};

    std::map<std::string, std::string> readDirs = {{"operation", "input"}, {"IO", "stdin"}, {"auxArity", "0"},
            {"attributeNames", "x\ty"}, {"name", "test"}, {"types", types.dump()}};
    std::map<std::string, std::string> readIoDirs = std::map<std::string, std::string>(readDirs);

    std::map<std::string, std::string> writeDirs = {{"operation", "output"}, {"IO", "stdout"},
            {"auxArity", "0"}, {"attributeNames", "x\ty"}, {"name", "test"}, {"types", types.dump()}};
    std::map<std::string, std::string> writeIoDirs = std::map<std::string, std::string>(writeDirs);

    Own<ram::Statement> main =
            mk<ram::Sequence>(mk<ram::IO>("test", readIoDirs), mk<ram::IO>("test", writeIoDirs));

    rels.push_back(std::move(myrel));
    std::map<std::string, Own<Statement>> subs;
    Own<Program> prog = mk<Program>(std::move(rels), std::move(main), std::move(subs));

    ErrorReport errReport;
    DebugReport debugReport(glb);

    TranslationUnit translationUnit(glb, std::move(prog), errReport, debugReport);

    // configure and execute interpreter
    Own<Engine> interpreter = mk<Engine>(translationUnit, 1);

    std::streambuf* oldCoutStreambuf = std::cout.rdbuf();
    std::ostringstream sout;
    std::cout.rdbuf(sout.rdbuf());

    interpreter->executeMain();

    std::cout.rdbuf(oldCoutStreambuf);

    std::string expected = R"(---------------
test
===============
6	6
===============
)";
    EXPECT_EQ(expected, sout.str());

    std::cin.rdbuf(backupCin);
}

TEST(IO_load, MixedTypesLoad) {
    std::streambuf* backupCin = std::cin.rdbuf();
    std::istringstream testInput("meow	-3	3	0.5");
    std::cin.rdbuf(testInput.rdbuf());

    Global glb;
    glb.config().set("jobs", "1");

    VecOwn<ram::Relation> rels;

    std::vector<std::string> attribs = {"l", "u", "b", "a"};
    std::vector<std::string> attribsTypes = {"s", "i", "u", "f"};
    Own<ram::Relation> myrel =
            mk<ram::Relation>("test", 4, 0, attribs, attribsTypes, RelationRepresentation::BTREE);

    Json types = Json::object{
            {"relation", Json::object{{"arity", static_cast<long long>(attribsTypes.size())},
                                 {"types", Json::array(attribsTypes.begin(), attribsTypes.end())}}}};

    std::map<std::string, std::string> readDirs = {{"operation", "input"}, {"IO", "stdin"}, {"auxArity", "0"},
            {"attributeNames", "x\ty"}, {"name", "test"}, {"types", types.dump()}};
    std::map<std::string, std::string> readIoDirs = std::map<std::string, std::string>(readDirs);

    std::map<std::string, std::string> writeDirs = {{"operation", "output"}, {"IO", "stdout"},
            {"auxArity", "0"}, {"attributeNames", "x\ty"}, {"name", "test"}, {"types", types.dump()}};
    std::map<std::string, std::string> writeIoDirs = std::map<std::string, std::string>(writeDirs);

    Own<ram::Statement> main =
            mk<ram::Sequence>(mk<ram::IO>("test", readIoDirs), mk<ram::IO>("test", writeIoDirs));

    rels.push_back(std::move(myrel));
    std::map<std::string, Own<Statement>> subs;
    Own<Program> prog = mk<Program>(std::move(rels), std::move(main), std::move(subs));

    ErrorReport errReport;
    DebugReport debugReport(glb);

    TranslationUnit translationUnit(glb, std::move(prog), errReport, debugReport);

    // configure and execute interpreter
    Own<Engine> interpreter = mk<Engine>(translationUnit, 1);

    std::streambuf* oldCoutStreambuf = std::cout.rdbuf();
    std::ostringstream sout;
    std::cout.rdbuf(sout.rdbuf());

    interpreter->executeMain();

    std::cout.rdbuf(oldCoutStreambuf);

    std::string expected = R"(---------------
test
===============
meow	-3	3	0.5
===============
)";

    EXPECT_EQ(expected, sout.str());

    std::cin.rdbuf(backupCin);
}

}  // namespace souffle::interpreter::test
