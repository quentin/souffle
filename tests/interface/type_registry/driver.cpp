/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2022, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

#include "souffle/SouffleInterface.h"

using namespace souffle;

void error(std::string txt) {
    std::cerr << "error: " << txt << "\n";
    exit(1);
}

int main(int argc, char** argv) {
    if (argc != 2) {
        error("wrong number of arguments!");
    }

    SouffleProgram* prog = ProgramFactory::newInstance("type_registry");
    if (prog == nullptr) {
        error("cannot load type_registry program");
    }

    const TypeRegistry& TR = prog->getTypeRegistry();

    const TypeDesc* numberT = TR.get("number");
    if (numberT == nullptr) {
        error("cannot get type 'number'");
    }

    const TypeDesc* unusedT = TR.get("unused");
    if (unusedT == nullptr) {
        error("cannot get type 'unused'");
    }

    if (!unusedT->isSubset()) {
        error("type 'unused' should be a Subset");
    }

    const TypeDesc* unusedSuperT = unusedT->aux();
    if (numberT != unusedSuperT) {
        error("super-type of 'unused' should equal type of 'number'");
    }

    const TypeDesc* OptT = TR.get("Opt");
    if (OptT == nullptr) {
        error("cannot get type 'Opt'");
    }

    if (!OptT->isADT()) {
        error("type 'Opt' should be an ADT");
    }

    if (OptT->size() != 2) {
        error("type 'Opt' should have size 2");
    }

    const TypeDesc* NoneT = OptT->getElementType(0);
    if (NoneT == nullptr) {
        error("type 'Opt' should have an element at index 0");
    }
    if (!NoneT->isBranch()) {
        error("type 'Opt' element 0 should be a branch");
    }
    if (!NoneT->hasIdentifier(std::string_view("None"))) {
        error("type 'Opt' element 0 should have identifier 'None'");
    }
    if (NoneT->size() != 0) {
        error("branch 'None' should have 0 element");
    }

    const TypeDesc* SomeT = OptT->getElementType(1);
    if (SomeT == nullptr) {
        error("type 'Opt' should have an element at index 1");
    }
    if (!SomeT->isBranch()) {
        error("type 'Opt' element 1 should be a branch");
    }
    if (!SomeT->hasIdentifier(std::string("Some"))) {
        error("type 'Opt' element 1 should have identifier 'Some'");
    }
    if (SomeT->size() != 1) {
        error("branch 'Some' should have 1 element");
    }

    const TypeDesc* Some0T = SomeT->getElementType(0);
    if (Some0T != numberT) {
        error("branch 'Some' element 0 should be the type of 'number'");
    }
    if (*SomeT->getElementName(0) != "root") {
        error("branch 'Some' element 0 should have identifier 'root'");
    }

    prog->loadAll(argv[1]);
    prog->run();
    prog->printAll();
    delete prog;

    return 0;
}
