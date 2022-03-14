/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2018, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file Synthesiser.h
 *
 * Declares synthesiser classes to synthesise C++ code from a RAM program.
 *
 ***********************************************************************/

#pragma once

#include "ram/Operation.h"
#include "ram/Relation.h"
#include "ram/Statement.h"
#include "ram/TranslationUnit.h"
#include "ram/utility/Visitor.h"
#include "souffle/RecordTable.h"
#include "souffle/SouffleTypes.h"
#include "souffle/utility/ContainerUtil.h"
#include "synthesiser/Relation.h"
#include <cstddef>
#include <iomanip>
#include <map>
#include <memory>
#include <ostream>
#include <set>
#include <string>

namespace souffle::synthesiser {

/**
 * A RAM synthesiser: synthesises a C++ program from a RAM program.
 */
class Synthesiser {
private:
    /** Record Table */

    /** RAM translation unit */
    ram::TranslationUnit& translationUnit;

    /** Global */
    Global& glb;

    /** RAM identifier to C++ identifier map */
    std::map<const std::string, const std::string> identifiers;

    /** Frequency profiling of searches */
    std::map<std::string, unsigned> idxMap;

    /** Frequency profiling of non-existence checks */
    std::map<std::string, std::size_t> neIdxMap;

    /** Cache for generated types for relations */
    std::set<std::string> typeCache;

    /** Relation map */
    std::map<std::string, const ram::Relation*> relationMap;

    /** Symbol map */
    mutable std::map<std::string, RamUnsigned> symbolMap;

    /** Symbol map */
    mutable std::vector<std::string> symbolIndex;

    /** Is set to true if there is a need to include std::regex */
    bool UsingStdRegex = false;

    /** Set of packed and unpacked records arities */
    std::set<std::size_t> arities;

protected:
    /** Convert RAM identifier */
    const std::string convertRamIdent(const std::string& name);

    /** Get relation name */
    const std::string getRelationName(const ram::Relation& rel);
    const std::string getRelationName(const ram::Relation* rel);

    /** Get context name */
    const std::string getOpContextName(const ram::Relation& rel);

    /** Get relation struct definition */
    void generateRelationTypeStruct(std::ostream& out, Own<Relation> relationType);

    /** Get referenced relations */
    std::set<const ram::Relation*> getReferencedRelations(const ram::Operation& op);

    /** Generate code */
    void emitCode(std::ostream& out, const ram::Statement& stmt);

    /** Generate type registry code */
    void emitTypes(std::ostream& out, const ram::Program& prog);

    /** Lookup frequency counter */
    unsigned lookupFreqIdx(const std::string& txt);

    /** Lookup read counter */
    std::size_t lookupReadIdx(const std::string& txt);

    /** Lookup relation by relation name */
    const ram::Relation* lookup(const std::string& relName) {
        auto it = relationMap.find(relName);
        assert(it != relationMap.end() && "relation not found");
        return it->second;
    }

    /** Lookup symbol index */
    RamUnsigned convertSymbol2Idx(const std::string& symbol) const {
        auto it = symbolMap.find(symbol);
        if (it != symbolMap.end()) {
            return it->second;
        } else {
            symbolIndex.push_back(symbol);
            std::size_t idx = symbolMap.size();
            symbolMap[symbol] = RamUnsigned(idx);
            return RamUnsigned(idx);
        }
    }

    std::string convertSymbolToIdentifier(const std::string& symbol) const;

    /** return the hexadecimal representation of the given value, 16 characters long. */
    std::string toHex(const std::size_t value) const;

    /** return the C++ raw-string literal for the given string. */
    std::string rawStr(const std::string& str) const;

public:
    explicit Synthesiser(ram::TranslationUnit& tUnit) : translationUnit(tUnit), glb(tUnit.global()) {
        visit(tUnit.getProgram(),
                [&](const ram::Relation& relation) { relationMap[relation.getName()] = &relation; });
    }

    virtual ~Synthesiser() = default;

    /** Get translation unit */
    ram::TranslationUnit& getTranslationUnit() {
        return translationUnit;
    }

    /** Generate code */
    void generateCode(std::ostream& os, const std::string& id, bool& withSharedLibrary);
};
}  // namespace souffle::synthesiser
