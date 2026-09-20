/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file Relation.h
 *
 * Defines Interpreter Relations
 *
 ***********************************************************************/

#pragma once

#include "interpreter/Index.h"
#include "ram/analysis/Index.h"
#include "souffle/RamTypes.h"
#include "souffle/SouffleInterface.h"
#include "souffle/datastructure/DynamicBTree.h"
#include "souffle/datastructure/EquivalenceRelation.h"
#include "souffle/utility/MiscUtil.h"
#include <cstddef>
#include <cstdint>
#include <algorithm>
#include <deque>
#include <iterator>
#include <memory>
#include <mutex>
#include <set>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace souffle::interpreter {

/**
 * Wrapper for InterpreterRelation.
 *
 * This class unifies the InterpreterRelation template classes.
 * It also defines virtual interfaces for ProgInterface and some virtual helper functions for interpreter
 * execution.
 */
struct RelationWrapper {
    using arity_type = souffle::Relation::arity_type;

public:
    RelationWrapper(arity_type arity, arity_type auxiliaryArity, std::string relName)
            : relName(std::move(relName)), arity(arity), auxiliaryArity(auxiliaryArity) {}

    virtual ~RelationWrapper() = default;

    // -- Define methods and interfaces for ProgInterface. --
public:
    /**
     * A virtualized iterator class that can be used by the Proginterface.
     * Define behaviors to uniformly access the underlying tuple regardless its structure and arity.
     */
    class iterator_base {
    public:
        virtual ~iterator_base() = default;

        virtual iterator_base& operator++() = 0;

        virtual const RamDomain* operator*() = 0;

        /*
         * A clone method is required by ProgInterface.
         */
        virtual iterator_base* clone() const = 0;

        virtual bool equal(const iterator_base& other) const = 0;
    };

    /**
     * The iterator interface.
     *
     * Other class should use this iterator class to traverse the relation.
     */
    class Iterator {
        Own<iterator_base> iter;

    public:
        Iterator(const Iterator& other) : iter(other.iter->clone()) {}
        Iterator(iterator_base* iter) : iter(iter) {}

        Iterator& operator++() {
            ++(*iter);
            return *this;
        }

        const RamDomain* operator*() {
            return **iter;
        }

        bool operator==(const Iterator& other) const {
            return iter->equal(*other.iter);
        }

        bool operator!=(const Iterator& other) const {
            return !(*this == other);
        }
    };

    virtual Iterator begin() const = 0;

    virtual Iterator end() const = 0;

    virtual void insert(const RamDomain*) = 0;

    virtual bool contains(const RamDomain*) const = 0;

    virtual std::size_t size() const = 0;

    virtual void purge() = 0;

    const std::string& getName() const {
        return relName;
    }

    arity_type getArity() const {
        return arity;
    }

    arity_type getAuxiliaryArity() const {
        return auxiliaryArity;
    }

    virtual void printStats(std::ostream& o) const = 0;

    // -- Defines methods and interfaces for Interpreter execution. --
public:
    using IndexViewPtr = Own<ViewWrapper>;

    /**
     * Return the order of an index.
     */
    virtual Order getIndexOrder(std::size_t) const = 0;

    /**
     * Obtains a view on an index of this relation, facilitating hint-supported accesses.
     *
     * This function is virtual because view creation require at least one indirect dispatch.
     */
    virtual IndexViewPtr createView(const std::size_t&) const = 0;

protected:
    std::string relName;

    arity_type arity;
    arity_type auxiliaryArity;
};

/** A B-tree relation whose tuple and index arities are supplied at runtime. */
class DynamicRelation : public RelationWrapper {
public:
    using Tuple = souffle::DynamicTuple;
    using Index = souffle::DynamicBTreeSet;
    using iterator = Index::iterator;
    using IndexRange = souffle::range<iterator>;

    class View : public ViewWrapper {
        const Index& index;

    public:
        explicit View(const Index& index) : index(index) {}

        bool contains(const Tuple& tuple) const {
            return index.contains(tuple);
        }

        bool contains(const Tuple& low, const Tuple& high) const {
            return !index.range(low, high).empty();
        }

        IndexRange range(const Tuple& low, const Tuple& high) const {
            return index.range(low, high);
        }
    };

    DynamicRelation(const ram::Relation& id, const ram::analysis::IndexCluster& indexSelection,
            bool provenance = false)
            : DynamicRelation(id.getName(), id.getArity(), id.getAuxiliaryArity(), indexSelection,
                      id.getRepresentation() == RelationRepresentation::EQREL, provenance) {}

    DynamicRelation(const std::string& name, std::size_t arity, std::size_t auxiliaryArity,
            const ram::analysis::IndexCluster& indexSelection, bool equivalenceRelation = false,
            bool provenance = false)
            : RelationWrapper(arity, auxiliaryArity, name), equivalenceRelation(equivalenceRelation),
              provenanceRelation(provenance) {
        if (auxiliaryArity > arity) {
            throw std::invalid_argument("relation auxiliary arity must not exceed relation arity");
        }
        if (equivalenceRelation && (arity != 2 || auxiliaryArity != 0)) {
            throw std::invalid_argument("equivalence relations must be binary and have no auxiliary attributes");
        }
        if (provenanceRelation && (arity < 2 || auxiliaryArity < 2)) {
            throw std::invalid_argument("provenance relations require rule and level attributes");
        }
        for (const auto& selectedOrder : indexSelection.getAllOrders()) {
            auto order = selectedOrder;
            for (std::size_t col = 0; col < this->arity; ++col) {
                if (std::find(order.begin(), order.end(), col) == order.end()) {
                    order.push_back(col);
                }
            }
            orders.emplace_back(std::move(order));
            // Rows are encoded into index order before insertion. The B-tree
            // therefore compares their stored columns in natural order.
            indexes.emplace_back(this->arity);
        }
        assert(!indexes.empty());
    }

    Index& getIndex(std::size_t indexPos) {
        return indexes.at(indexPos);
    }

    const Index& getIndex(std::size_t indexPos) const {
        return indexes.at(indexPos);
    }

    souffle::DynamicTuple encode(const Tuple& tuple, std::size_t indexPos) const {
        if (tuple.size() != arity) throw std::invalid_argument("tuple arity does not match relation arity");
        souffle::DynamicTuple encoded(arity);
        const auto& order = orders.at(indexPos).getOrder();
        for (std::size_t i = 0; i < arity; ++i) encoded[i] = tuple[order[i]];
        return encoded;
    }

    bool insert(const Tuple& tuple) {
        if (tuple.size() != arity) throw std::invalid_argument("tuple arity does not match relation arity");
        std::unique_lock<std::mutex> updateLock(mutationMutex, std::defer_lock);
        if (equivalenceRelation || provenanceRelation || auxiliaryArity != 0) updateLock.lock();
        if (equivalenceRelation) {
            if (tuple.size() != 2) throw std::invalid_argument("equivalence relation must have arity 2");
            const bool inserted = eqrel.insert(tuple);
            rebuildIndexes();
            return inserted;
        }
        if (provenanceRelation || auxiliaryArity != 0) return insertOrUpdate(tuple);
        if (!indexes.front().insert(encode(tuple, 0))) return false;
        for (std::size_t i = 1; i < indexes.size(); ++i) indexes[i].insert(encode(tuple, i));
        return true;
    }

    bool erase(const Tuple& tuple) {
        if (tuple.size() != arity) throw std::invalid_argument("tuple arity does not match relation arity");
        std::lock_guard<std::mutex> lock(mutationMutex);
        if (indexes.front().erase(encode(tuple, 0)) == 0) return false;
        for (std::size_t i = 1; i < indexes.size(); ++i) indexes[i].erase(encode(tuple, i));
        return true;
    }

    bool contains(const Tuple& tuple) const {
        if (tuple.size() != arity) throw std::invalid_argument("tuple arity does not match relation arity");
        if (equivalenceRelation) return eqrel.contains(tuple);
        return indexes.front().contains(encode(tuple, 0));
    }

    void extendAndInsert(DynamicRelation& other) {
        assert(equivalenceRelation && other.equivalenceRelation);
        std::scoped_lock lock(mutationMutex, other.mutationMutex);
        eqrel.extendAndInsert(other.eqrel);
        rebuildIndexes();
        other.rebuildIndexes();
    }

    IndexRange scan() const {
        return {indexes.front().begin(), indexes.front().end()};
    }

    IndexRange range(std::size_t indexPos, const Tuple& low, const Tuple& high) const {
        return indexes.at(indexPos).range(low, high);
    }

    std::vector<IndexRange> partitionScan(std::size_t partitionCount) const {
        return indexes.front().partition(partitionCount);
    }

    std::vector<IndexRange> partitionRange(
            std::size_t indexPos, const Tuple& low, const Tuple& high, std::size_t partitionCount) const {
        return indexes.at(indexPos).partitionRange(low, high, partitionCount);
    }

    std::size_t size() const override {
        if (equivalenceRelation) return eqrel.size();
        return indexes.front().size();
    }

    bool empty() const {
        if (equivalenceRelation) return eqrel.empty();
        return indexes.front().empty();
    }

    void purge() override {
        std::lock_guard<std::mutex> lock(mutationMutex);
        if (equivalenceRelation) eqrel.clear();
        for (auto& index : indexes) index.clear();
    }

    void insert(const RamDomain* tuple) override {
        insert(Tuple(tuple, tuple + arity));
    }

    bool contains(const RamDomain* tuple) const override {
        return contains(Tuple(tuple, tuple + arity));
    }

    class iterator_base : public RelationWrapper::iterator_base {
        iterator iter;
        std::vector<std::size_t> order;
        mutable Tuple decoded;

    public:
        iterator_base(iterator iter, std::vector<std::size_t> order)
                : iter(std::move(iter)), order(std::move(order)), decoded(this->order.size()) {}

        iterator_base& operator++() override {
            ++iter;
            return *this;
        }

        const RamDomain* operator*() override {
            const auto& row = *iter;
            for (std::size_t i = 0; i < order.size(); ++i) {
                assert(order[i] < decoded.size());
                decoded[order[i]] = row[i];
            }
            return decoded.data();
        }

        iterator_base* clone() const override {
            return new iterator_base(iter, order);
        }

        bool equal(const RelationWrapper::iterator_base& other) const override {
            if (auto* rhs = as<iterator_base>(other)) return iter == rhs->iter;
            return false;
        }
    };

    Iterator begin() const override {
        return Iterator(new iterator_base(indexes.front().begin(), orders.front().getOrder()));
    }

    Iterator end() const override {
        return Iterator(new iterator_base(indexes.front().end(), orders.front().getOrder()));
    }

    void printStats(std::ostream& out) const override {
        for (std::size_t i = 0; i < indexes.size(); ++i) {
            out << "Index " << i << ":\n";
            indexes[i].printStats(out);
        }
    }

    Order getIndexOrder(std::size_t indexPos) const override {
        return orders.at(indexPos);
    }

    IndexViewPtr createView(const std::size_t& indexPos) const override {
        return mk<View>(indexes.at(indexPos));
    }

    static View* castView(ViewWrapper* view) {
        return static_cast<View*>(view);
    }

private:
    bool insertOrUpdate(const Tuple& tuple) {
        bool changed = false;
        const std::size_t keyArity = arity - auxiliaryArity;
        for (std::size_t indexPos = 0; indexPos < indexes.size(); ++indexPos) {
            auto& index = indexes[indexPos];
            auto encoded = encode(tuple, indexPos);
            auto found = index.end();
            for (auto it = index.begin(); it != index.end(); ++it) {
                if (std::equal((*it).begin(), (*it).begin() + keyArity, encoded.begin())) {
                    found = it;
                    break;
                }
            }
            if (found == index.end()) {
                index.insert(encoded);
                changed = true;
                continue;
            }

            Tuple updated = *found;
            bool update = false;
            if (provenanceRelation) {
                const auto& order = orders[indexPos].getOrder();
                const auto level = static_cast<std::size_t>(std::find(order.begin(), order.end(), arity - 1) -
                                                            order.begin());
                const auto rule = static_cast<std::size_t>(std::find(order.begin(), order.end(), arity - 2) -
                                                           order.begin());
                const auto newLevel = ramBitCast<RamSigned>(encoded[level]);
                const auto oldLevel = ramBitCast<RamSigned>(updated[level]);
                const auto newRule = ramBitCast<RamSigned>(encoded[rule]);
                const auto oldRule = ramBitCast<RamSigned>(updated[rule]);
                update = newLevel < oldLevel || (newLevel == oldLevel && newRule < oldRule);
                if (update) {
                    updated[rule] = encoded[rule];
                    updated[level] = encoded[level];
                }
            } else {
                const std::size_t firstAuxiliary = arity - auxiliaryArity;
                for (std::size_t col = firstAuxiliary; col < arity; ++col) {
                    if (updated[col] != encoded[col]) {
                        updated[col] = encoded[col];
                        update = true;
                    }
                }
            }
            if (update) {
                index.erase(*found);
                index.insert(updated);
                changed = true;
            }
        }
        return changed;
    }

    void rebuildIndexes() {
        for (auto& index : indexes) index.clear();
        if (!equivalenceRelation) return;
        for (const auto& tuple : eqrel) {
            for (std::size_t i = 0; i < indexes.size(); ++i) indexes[i].insert(encode(tuple, i));
        }
    }

    std::vector<Index> indexes;
    std::vector<Order> orders;
    std::mutex mutationMutex;
    bool equivalenceRelation;
    bool provenanceRelation;
    souffle::EquivalenceRelation<Tuple> eqrel;
};

// The type of relation factory functions.
using RelationFactory = Own<RelationWrapper> (*)(
        const ram::Relation& id, const ram::analysis::IndexCluster& indexSelection);

// A factory for BTree based relation.
Own<RelationWrapper> createBTreeRelation(
        const ram::Relation& id, const ram::analysis::IndexCluster& indexSelection);

// A factory for BTreeDelete based relation.
Own<RelationWrapper> createBTreeDeleteRelation(
        const ram::Relation& id, const ram::analysis::IndexCluster& indexSelection);

// A factory for BTree provenance index.
Own<RelationWrapper> createProvenanceRelation(
        const ram::Relation& id, const ram::analysis::IndexCluster& indexSelection);

// A factory for Brie based index.
Own<RelationWrapper> createBrieRelation(
        const ram::Relation& id, const ram::analysis::IndexCluster& indexSelection);

// A factory for Eqrel index.
Own<RelationWrapper> createEqrelRelation(
        const ram::Relation& id, const ram::analysis::IndexCluster& indexSelection);
}  // namespace souffle::interpreter
