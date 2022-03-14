/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2014, Oracle and/or its affiliates. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file RecordTable.h
 *
 * Data container implementing a map between records and their references.
 * Records are separated by arity, i.e., stored in different RecordMaps.
 *
 ***********************************************************************/

#pragma once

#include "souffle/RamTypes.h"
#include "souffle/utility/span.h"
#include <initializer_list>

namespace souffle {

/** The interface of any Record Table. */
class RecordTable {
public:
    virtual ~RecordTable() {}

    virtual void setNumLanes(const std::size_t NumLanes) = 0;

    virtual RamDomain pack(const RamDomain* Tuple, const std::size_t Arity) = 0;

    virtual RamDomain pack(const std::initializer_list<RamDomain>& List) = 0;

    virtual const RamDomain* unpack(const RamDomain Ref, const std::size_t Arity) const = 0;

    /**
     * @brief Gives a hint about the number of threads that may use this
     * record table concurrently.
     *
     * The given number of threads may be used by the implementation as a hint
     * to setup or dimension internal data structures for optimal concurrency.
     */
    virtual void setNumThreads(const std::size_t){};
};

/** @brief helper to convert tuple to record reference for the synthesiser */
template <class RecordTableT, std::size_t Arity>
RamDomain pack(RecordTableT&& recordTab, Tuple<RamDomain, Arity> const& tuple) {
    return recordTab.pack(tuple.data(), Arity);
}

/** @brief helper to convert tuple to record reference for the synthesiser */
template <class RecordTableT, std::size_t Arity>
RamDomain pack(RecordTableT&& recordTab, span<const RamDomain, Arity> tuple) {
    return recordTab.pack(tuple.data(), Arity);
}

/** @brief helper to pack using an initialization-list of RamDomain values. */
template <class RecordTableT>
RamDomain pack(RecordTableT&& recordTab, const std::initializer_list<RamDomain>&& initlist) {
    return recordTab.pack(std::data(initlist), initlist.size());
}

}  // namespace souffle
