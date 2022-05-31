/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2022, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

#include "souffle/SouffleInterface.h"

#include <string>
#include <set>

using namespace souffle;

void error(std::string txt) {
    std::cerr << "error: " << txt << "\n";
    exit(1);
}

int main(int argc, char** argv) {
    if (argc != 2) {
        error("wrong number of arguments!");
    }

    SouffleProgram* prog = ProgramFactory::newInstance("relation_base");
    if (prog == nullptr) {
        error("cannot load 'relation_base' program");
    }

    prog->loadAll(argv[1]);

    prog->run();

    Relation* program = prog->getRelation("program");
    if (program->size() != 3) {
        error("size of 'program' relation should be 3");
    }

    Relation* result = prog->getRelation("result");
    const TypeDesc* desc = result->getTypeDescriptor();

    if (result->getArity() != 2) {
        error("arity of 'result' should be 2");
    }

    if (result->getName() != "result") {
        error("name of 'result' should be 'result'");
    }

    if (desc == nullptr) {
        error("type descriptor of 'result' should not be null");
    }

    if (result->size() != 3) {
        error("size of 'result' relation should be 3");
    }

    std::set<std::pair<std::string, RamDomain>> results;
    result->each([&](const RamDomain* tuple) {
        results.emplace(prog->getSymbolTable().decode(tuple[0]), tuple[1]);
    });

    if (results.size() != 3) {
        error("there should be 3 tuples");
    }

    if (results.count(std::make_pair(std::string("5*5*5"), static_cast<RamDomain>(125))) != 1) {
        error("missing tuple (5*5*5, 125) in 'result'");
    }
    if (results.count(std::make_pair(std::string("1+5*5*5"), static_cast<RamDomain>(126))) != 1) {
        error("missing tuple (1+5*5*5, 126) in 'result'");
    }
    if (results.count(std::make_pair(std::string("4*(12+1)"), static_cast<RamDomain>(52))) != 1) {
        error("missing tuple (4*(12+1), 52) in 'result'");
    }

    prog->printAll();

    delete prog;

    return 0;
}
