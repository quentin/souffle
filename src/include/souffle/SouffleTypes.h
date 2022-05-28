/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2023, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/**
 * @file SouffleTypes.h
 *
 * Reflection for Souffle program's types and tuples.
 */
#pragma once

#include <algorithm>
#include <cassert>
#include <iostream>
#include <list>
#include <map>
#include <memory>
#include <string>
#include <vector>

namespace souffle {

/**
 * Kind of a souffle type descriptor.
 *
 * Note that there is no type descriptor for Alias types.
 * All aliases of a type are represented by a single descriptor.
 */
enum class TypeKind {
    /// primitive type
    Primitive,
    /// subset type
    Subset,
    /// union type
    Union,
    /// relation tuple type
    Tuple,
    /// algebraic data type
    ADT,
    /// algebraic data type branch
    Branch,
    /// record type
    Record
};

struct TypeDesc;

/** A pair of a type identifier and a pointer to its type descriptor */
using TypeElement = std::pair<std::string, const TypeDesc*>;

/** A list of TypeElement*/
using TypeElementList = std::vector<TypeElement>;

/** A list of type identifiers */
using IdentifierList = std::list<std::string>;

/**
 * Souffle type descriptor.
 *
 * Type descriptors are built by the `TypeRegistry` class.
 *
 * A type descriptor describes one or several types of the program.
 *
 * A type and all its aliases are represented by a single type descriptor.
 *
 * The `canonical` identifier is the declaration identifier of a type,
 * except for alias types that are simply listed as additionnal identifiers of the
 * original type.
 *
 * Depending on its kind, a descriptor may have some sub-`elements` (other
 * descriptors):
 * - For a record, an algebraic data type branch or a tuple: the list of fields
 *   in declaration order.
 * - For an algebraic data type: the list of branches sorted lexicographically.
 * - For a union type: the list of sub-types.
 *
 */
struct TypeDesc {
    TypeDesc() = delete;

    /** Return the kind of this type descriptor */
    TypeKind kind() const {
        return Kind;
    }

    /** Indicate if this describes a primitive type */
    bool isPrimitive() const {
        return Kind == TypeKind::Primitive;
    }

    /** Indicate if this describes a subtype type */
    bool isSubset() const {
        return Kind == TypeKind::Subset;
    }

    /** Indicate if this describes a union type */
    bool isUnion() const {
        return Kind == TypeKind::Union;
    }

    /** Indicate if this describes the type of a relation's tuple */
    bool isTuple() const {
        return Kind == TypeKind::Tuple;
    }

    /** Indicate if this describes an algebraic data type */
    bool isADT() const {
        return Kind == TypeKind::ADT;
    }

    /** Indicate if this describes a record type */
    bool isRecord() const {
        return Kind == TypeKind::Record;
    }

    /** Indicate if this describes an algebraic data type branch */
    bool isBranch() const {
        return Kind == TypeKind::Branch;
    }

    /** Indicate if this describes an enumeration-like algebraic data type. */
    bool isEnumADT() const {
        return (Kind == TypeKind::ADT && NonEnumADT);
    }

    /** Return the canonical identifier of the described type */
    const std::string& canonicalIdentifier() const {
        return CanonicalIdentifier;
    }

    /** Indicate if the the described type has the given identifier either as its canonical identifier or as
     * an alias identifier */
    bool hasIdentifier(std::string_view Id) const {
        if (Id == CanonicalIdentifier) {
            return true;
        }
        return Identifiers.end() != std::find(Identifiers.begin(), Identifiers.end(), Id);
    }

    /**
     * Return the associated auxiliary type descriptor.
     *
     * Depending on the kind of this descriptor, the auxiliary type represents:
     * - a Branch -> aux is the ADT type
     * - a Subset -> aux is the super type
     * - an Union -> aux is the common primitive type
     */
    const TypeDesc* aux() const {
        return Aux;
    }

    /** Return the number of elements */
    std::size_t size() const {
        return Elements.size();
    }

    /** Return the list of elements */
    const TypeElementList& elements() const {
        return Elements;
    }

    /** Return the list of identifiers (not including the canonical identifier) */
    const IdentifierList& identifiers() const {
        return Identifiers;
    }

    /** Return the Pos-th element or nullptr */
    const TypeElement* getElement(const std::size_t Pos) const {
        if (Elements.size() <= Pos) {
            return nullptr;
        }
        return &Elements[Pos];
    }

    /** Return the first element with the exact given name, or nullptr */
    const TypeElement* getElement(const std::string_view Name) const {
        const auto It = std::find_if(Elements.begin(), Elements.end(),
                [&](const TypeElement& Elem) { return Elem.first == Name; });
        if (It == Elements.end()) {
            return nullptr;
        }
        return &(*It);
    }

    /** Return the name of the Pos-th element, or nullptr */
    const std::string* getElementName(const std::size_t Pos) const {
        if (Elements.size() <= Pos) {
            return nullptr;
        }
        return &Elements[Pos].first;
    }

    /** Return the type descriptor of the Pos-th element, or nullptr */
    const TypeDesc* getElementType(const std::size_t Pos) const {
        if (Elements.size() <= Pos) {
            return nullptr;
        }
        return Elements[Pos].second;
    }

    /** Return the type descriptor of the first element with the exact given name, or nullptr */
    const TypeDesc* getElementType(const std::string_view Name) const {
        const auto It = std::find_if(Elements.begin(), Elements.end(),
                [&](const TypeElement& Elem) { return Elem.first == Name; });
        if (It == Elements.end()) {
            return nullptr;
        }
        return It->second;
    }

    /** Print the described type */
    std::ostream& print(std::ostream& Out) const {
        for (const auto& Id : Identifiers) {
            Out << ".type " << Id << " = " << canonicalIdentifier() << "\n";
        }

        if (isSubset()) {
            Out << ".type " << canonicalIdentifier() << " <: " << Aux->canonicalIdentifier() << "\n";
        } else if (isUnion()) {
            Out << ".type " << canonicalIdentifier() << " = ";
            bool First = true;
            for (const auto& Element : Elements) {
                if (!First) {
                    Out << "| ";
                }
                Out << Element.second->canonicalIdentifier();
                First = false;
            }
            Out << "\n";
        } else if (isRecord()) {
            Out << ".type " << canonicalIdentifier() << " = [";
            bool First = true;
            for (const auto& Element : Elements) {
                if (!First) {
                    Out << ", ";
                }
                Out << Element.first << ":" << Element.second->canonicalIdentifier();
                First = false;
            }
            Out << "]\n";
        } else if (isADT()) {
            Out << ".type " << canonicalIdentifier() << " = ";
            bool First = true;
            for (const auto& Element : Elements) {
                if (!First) {
                    Out << " | ";
                }
                Element.second->print(Out);
                First = false;
            }
            Out << "\n";
        } else if (isBranch()) {
            Out << canonicalIdentifier() << " {";
            bool First = true;
            for (const auto& Element : Elements) {
                if (!First) {
                    Out << ", ";
                }
                Out << Element.first << ":" << Element.second->canonicalIdentifier();
                First = false;
            }
            Out << "}";
        }
        return Out;
    }

private:
    friend class TypeRegistry;

    /// the kind of this type
    const TypeKind Kind;

    /// auxiliary type, usage depends on the kind of this type:
    /// Branch -> Aux is the ADT type
    /// Subset -> Aux is the super type
    /// Union -> Aux is the common primitive type
    const TypeDesc* const Aux;

    /// identifier.
    const std::string CanonicalIdentifier;

    /// The sorted list of equivalent type identifiers (aliases)
    IdentifierList Identifiers;

    /**
     * The list of elements of this type.
     *
     * For a record, an ADT branch or a tuple: the list of fields in declaration order.
     * For an ADT: the list of branches sorted lexicographically.
     * For a Union: the list of sub-types.
     */
    TypeElementList Elements;

    /// `true` if this type is an ADT that has at least one non-empty branch..
    bool NonEnumADT = false;

    TypeDesc(const TypeKind K, const TypeDesc& B, const std::string& Id)
            : Kind(K), Aux(&B), CanonicalIdentifier(Id) {
        assert(K == TypeKind::Subset || K == TypeKind::Branch || K == TypeKind::Union);
    }

    TypeDesc(const TypeKind K, const std::string& Id) : Kind(K), Aux(nullptr), CanonicalIdentifier(Id) {}

    void addIdentifier(const std::string& Id) {
        if (Id == CanonicalIdentifier) {
            return;
        }

        auto Lb = std::lower_bound(Identifiers.begin(), Identifiers.end(), Id);
        if (Lb != Identifiers.end() && *Lb == Id) {
            return;
        }

        Identifiers.insert(Lb, Id);
    }

    bool addElement(const std::string& Id, const TypeDesc* Ty) {
        if (std::find_if(Elements.begin(), Elements.end(),
                    [&](const TypeElement& Elem) { return Elem.first == Id; }) != Elements.end()) {
            return false;
        }

        Elements.emplace_back(Id, Ty);

        if (Kind == TypeKind::ADT) {
            if (Ty->size() > 0) {
                NonEnumADT = true;
            }
            // must sort elements
            std::sort(Elements.begin(), Elements.end(),
                    [](const TypeElement& A, const TypeElement& B) { return A.first < B.first; });
        }
        return true;
    }
};

/**
 * Souffle type registry.
 *
 * Builds and maintains a set of `TypeDesc` descriptors of Souffle types.
 */
class TypeRegistry {
public:
    TypeRegistry() {
        auto Number = newPrimitive("number");
        newEquivalent("__numberConstant", Number);

        auto Unsigned = newPrimitive("unsigned");
        newEquivalent("__unsignedConstant", Unsigned);

        auto Float = newPrimitive("float");
        newEquivalent("__floatConstant", Float);

        auto Symbol = newPrimitive("symbol");
        newEquivalent("__symbolConstant", Symbol);
    }

    /** Print all the stored canonical type descriptors */
    std::ostream& printAll(std::ostream& Out) const {
        for (const auto& Pair : CanonicalTypeDescriptors) {
            Pair.second->print(Out);
        }
        return Out;
    }

    /** Return the number of stored canonical type descriptors (except tuples, branches and aliases) */
    std::size_t numCanonicalTypes() const {
        return CanonicalTypeDescriptors.size();
    }

    /** Return all the stored canonical type descriptors (except tuples, branches and aliases) */
    const TypeDesc* getCanonicalType(std::size_t I) const {
        if (I >= CanonicalTypeDescriptors.size()) {
            return nullptr;
        }
        return std::next(CanonicalTypeDescriptors.begin(), I)->second.get();
    }

    /** Return the type descriptor for the given type name (except branches and tuples). */
    const TypeDesc* get(const std::string& Id) const {
        auto It = TypeDescriptors.find(Id);
        if (It == TypeDescriptors.end()) {
            return nullptr;
        }
        return It->second.get();
    }

    /** Return the type descriptor for a tuple of the given relation name (except branches and tuples). */
    const TypeDesc* getTuple(const std::string& Id) const {
        auto It = TupleDescriptors.find(Id);
        if (It == TupleDescriptors.end()) {
            return nullptr;
        }
        return It->second.get();
    }

    /** Return the list of all the canonnical type descriptors */
    const std::vector<const TypeDesc*> getTypes() const {
        std::vector<const TypeDesc*> Result;
        for (const auto& Pair : CanonicalTypeDescriptors) {
            Result.emplace_back(Pair.second.get());
        }
        return Result;
    }

    /** Add an element to an existing type descriptor or return false. */
    bool addElement(const TypeDesc* Dest, const std::string& ElemId, const TypeDesc* ElemTy) {
        TypeDesc* T;
        if (Dest->isTuple()) {
            auto It = TupleDescriptors.find(Dest->canonicalIdentifier());
            if (It == TupleDescriptors.end()) {
                return false;
            }
            T = It->second.get();
        } else if (Dest->isBranch()) {
            T = const_cast<TypeDesc*>(Dest);
        } else {
            auto It = TypeDescriptors.find(Dest->canonicalIdentifier());
            if (It == TypeDescriptors.end()) {
                return false;
            }
            T = It->second.get();
        }

        if (T != Dest) {
            return false;
        }

        if (T->kind() == TypeKind::Record || T->kind() == TypeKind::Tuple || T->kind() == TypeKind::ADT ||
                T->kind() == TypeKind::Branch || T->kind() == TypeKind::Union) {
            return T->addElement(ElemId, ElemTy);
        } else {
            return false;
        }
    }

    /** Build a new primitive type or return nullptr */
    const TypeDesc* newPrimitive(const std::string& Id) {
        if (TypeDescriptors.count(Id) != 0) {
            return nullptr;
        }

        std::shared_ptr<TypeDesc> T(new TypeDesc(TypeKind::Primitive, Id));
        TypeDescriptors.emplace(Id, T);
        CanonicalTypeDescriptors.emplace(Id, T);
        return T.get();
    }

    /** Declare a new alias for a type or return false */
    bool newEquivalent(const std::string& Id, const TypeDesc* Base) {
        if (TypeDescriptors.count(Id) != 0) {
            return false;
        }

        auto It = TypeDescriptors.find(Base->canonicalIdentifier());
        if (It == TypeDescriptors.end()) {
            return false;
        }
        assert(It->second.get() == Base);
        if (It->second.get() != Base) {
            return false;
        }

        auto& TypeRef = It->second;
        TypeRef->addIdentifier(Id);
        TypeDescriptors.emplace(Id, TypeRef);

        return true;
    }

    /** Build a new alias type or return nullptr */
    const TypeDesc* newSubset(const std::string& Id, const TypeDesc* Base) {
        if (TypeDescriptors.count(Id) != 0) {
            return nullptr;
        }

        std::shared_ptr<TypeDesc> T(new TypeDesc(TypeKind::Subset, *Base, Id));
        TypeDescriptors.emplace(Id, T);
        CanonicalTypeDescriptors.emplace(Id, T);
        return T.get();
    }

    /** Build a new union type containing no sub-type initially or return nullptr */
    const TypeDesc* newUnion(const std::string& Id, const TypeDesc* Primitive) {
        if (TypeDescriptors.count(Id) != 0) {
            return nullptr;
        }

        std::shared_ptr<TypeDesc> T(new TypeDesc(TypeKind::Union, *Primitive, Id));
        TypeDescriptors.emplace(Id, T);
        CanonicalTypeDescriptors.emplace(Id, T);
        return T.get();
    }

    /** Build a new record type containing no field initially or return nullptr */
    const TypeDesc* newRecord(const std::string& Id) {
        if (TypeDescriptors.count(Id) != 0) {
            return nullptr;
        }

        std::shared_ptr<TypeDesc> T(new TypeDesc(TypeKind::Record, Id));
        TypeDescriptors.emplace(Id, T);
        CanonicalTypeDescriptors.emplace(Id, T);
        return T.get();
    }

    /** Build a new algebraic data type containing no branch initially or return nullptr */
    const TypeDesc* newADT(const std::string& Id) {
        if (TypeDescriptors.count(Id) != 0) {
            return nullptr;
        }

        std::shared_ptr<TypeDesc> T(new TypeDesc(TypeKind::ADT, Id));
        TypeDescriptors.emplace(Id, T);
        CanonicalTypeDescriptors.emplace(Id, T);
        return T.get();
    }

    /** Build a new branch for an existing algebraic data type or return nullptr */
    const TypeDesc* newBranch(const std::string& Id, const TypeDesc* BaseADT) {
        if (BranchDescriptors.count(Id) != 0) {
            return nullptr;
        }

        auto It = TypeDescriptors.find(BaseADT->canonicalIdentifier());
        if (It == TypeDescriptors.end()) {
            return nullptr;
        }
        assert(It->second.get() == BaseADT);
        if (It->second.get() != BaseADT) {
            return nullptr;
        }

        auto& TypeRef = It->second;

        std::shared_ptr<TypeDesc> T(new TypeDesc(TypeKind::Branch, *TypeRef, Id));
        if (TypeRef->addElement(Id, T.get())) {
            BranchDescriptors.emplace(Id, T);
            return T.get();
        } else {
            return nullptr;
        }
    }

    /** Build a new tuple type containing no field initially or return nullptr */
    const TypeDesc* newTuple(const std::string& RelId) {
        if (TupleDescriptors.count(RelId) != 0) {
            return nullptr;
        }

        std::shared_ptr<TypeDesc> T(new TypeDesc(TypeKind::Tuple, RelId));
        TupleDescriptors.emplace(RelId, T);
        return T.get();
    }

private:
    /** The mapping from canonical type identifier to type descriptors */
    std::map<std::string, std::shared_ptr<TypeDesc>> CanonicalTypeDescriptors;
    /** The mapping from type identifiers (including canonical and aliases) to type descriptors */
    std::map<std::string, std::shared_ptr<TypeDesc>> TypeDescriptors;
    /** The mapping from relation identifiers to tuple descriptors */
    std::map<std::string, std::shared_ptr<TypeDesc>> TupleDescriptors;
    /** The mapping from ADT branch identifiers to branch descriptors */
    std::map<std::string, std::shared_ptr<TypeDesc>> BranchDescriptors;
};

}  // namespace souffle
