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

namespace interface {

enum TypeKind {
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
using TypeElement = std::pair<std::string, const TypeDesc*>;
using TypeElementList = std::vector<TypeElement>;
using IdentifierList = std::list<std::string>;

struct TypeDesc {
    TypeDesc() = delete;

    TypeKind kind() const {
        return Kind;
    }

    bool isPrimitive() const {
        return Kind == Primitive;
    }

    bool isSubset() const {
        return Kind == Subset;
    }

    bool isUnion() const {
        return Kind == Union;
    }

    bool isTuple() const {
        return Kind == Tuple;
    }

    bool isADT() const {
        return Kind == ADT;
    }

    bool isRecord() const {
        return Kind == Record;
    }

    bool isBranch() const {
        return Kind == Branch;
    }

    bool isEnumADT() const {
        if (Kind != ADT) {
            return false;
        }
        // all branches must be empty in an enum ADT.
        for (const auto& Pair : Elements) {
            if (Pair.second->size() > 0) {
                return false;
            }
        }
        return true;
    }

    const std::string& canonicalIdentifier() const {
        return CanonicalIdentifier;
    }

    bool hasIdentifier(const std::string& Id) const {
        if (Id == CanonicalIdentifier) {
            return true;
        }
        return Identifiers.end() != std::find(Identifiers.begin(), Identifiers.end(), Id);
    }

    bool hasIdentifier(std::string_view Id) const {
        if (Id == CanonicalIdentifier) {
            return true;
        }
        return Identifiers.end() != std::find(Identifiers.begin(), Identifiers.end(), Id);
    }

    const TypeDesc* aux() const {
        return Aux;
    }

    std::size_t size() const {
        return Elements.size();
    }

    const TypeElementList& elements() const {
        return Elements;
    }

    const IdentifierList& identifiers() const {
        return Identifiers;
    }

    const TypeDesc* getElementType(const std::size_t Pos) const {
        if (Elements.size() <= Pos) {
            return nullptr;
        }
        return Elements[Pos].second;
    }

    const TypeDesc* getElementType(const std::string_view Name) const {
        const auto It = std::find_if(Elements.begin(), Elements.end(),
                [&](const TypeElement& Elem) { return Elem.first == Name; });
        if (It == Elements.end()) {
            return nullptr;
        }
        return It->second;
    }

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
    /// Branch -> aux is the ADT type
    /// Subset -> aux is the super type
    const TypeDesc* const Aux;

    /// identifier.
    const std::string CanonicalIdentifier;

    /// The list of equivalent type identifiers (aliases)
    IdentifierList Identifiers;

    TypeElementList Elements;

    TypeDesc(const TypeKind K, const TypeDesc& B, const std::string& Id)
            : Kind(K), Aux(&B), CanonicalIdentifier(Id) {
        assert(K == Subset || K == Branch);
    }

    TypeDesc(const TypeKind K, const std::string& Id) : Kind(K), Aux(nullptr), CanonicalIdentifier(Id) {}

    void addIdentifier(const std::string& Id) {
        if (Id == CanonicalIdentifier) {
            return;
        }
        if (std::find(Identifiers.begin(), Identifiers.end(), Id) != Identifiers.end()) {
            return;
        }
        Identifiers.emplace_back(Id);
        return;
    }

    bool addElement(const std::string& Id, const TypeDesc* Ty) {
        if (std::find_if(Elements.begin(), Elements.end(),
                    [&](const TypeElement& Elem) { return Elem.first == Id; }) != Elements.end()) {
            return false;
        }

        Elements.emplace_back(Id, Ty);

        if (Kind == ADT) {
            // must sort elements
            std::sort(Elements.begin(), Elements.end(),
                    [](const TypeElement& A, const TypeElement& B) { return A.first < B.first; });
        }
        return true;
    }
};

struct TypeRegistry {
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

    std::ostream& printAll(std::ostream& Out) const {
        for (const auto& Pair : CanonicalTypeDescriptors) {
            Pair.second->print(Out);
        }
        return Out;
    }

    std::size_t numCanonicalTypes() const {
        return CanonicalTypeDescriptors.size();
    }

    const TypeDesc* getCanonicalType(std::size_t I) const {
        if (I >= CanonicalTypeDescriptors.size()) {
            return nullptr;
        }
        return std::next(CanonicalTypeDescriptors.begin(), I)->second.get();
    }

    /** Return the type descriptor for  the given type name. */
    const TypeDesc* get(const std::string& Id) const {
        auto It = TypeDescriptors.find(Id);
        if (It == TypeDescriptors.end()) {
            return nullptr;
        }
        return It->second.get();
    }

    /** Return the type descriptor for a tuple of the given relation name. */
    const TypeDesc* getTuple(const std::string& Id) const {
        auto It = TupleDescriptors.find(Id);
        if (It == TupleDescriptors.end()) {
            return nullptr;
        }
        return It->second.get();
    }

    /** Return the list of all the types. */
    const std::vector<const TypeDesc*> getTypes() const {
        std::vector<const TypeDesc*> Result;
        for (const auto& Pair : CanonicalTypeDescriptors) {
            Result.emplace_back(Pair.second.get());
        }
        return Result;
    }

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

        if (T->kind() == Record || T->kind() == Tuple || T->kind() == ADT || T->kind() == Branch ||
                T->kind() == Union) {
            return T->addElement(ElemId, ElemTy);
        } else {
            return false;
        }
    }

    const TypeDesc* newPrimitive(const std::string& Id) {
        if (TypeDescriptors.count(Id) != 0) {
            return nullptr;
        }

        std::shared_ptr<TypeDesc> T(new TypeDesc(Primitive, Id));
        TypeDescriptors.emplace(Id, T);
        CanonicalTypeDescriptors.emplace(Id, T);
        return T.get();
    }

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

    const TypeDesc* newSubset(const std::string& Id, const TypeDesc* Base) {
        if (TypeDescriptors.count(Id) != 0) {
            return nullptr;
        }

        std::shared_ptr<TypeDesc> T(new TypeDesc(Subset, *Base, Id));
        TypeDescriptors.emplace(Id, T);
        CanonicalTypeDescriptors.emplace(Id, T);
        return T.get();
    }

    const TypeDesc* newUnion(const std::string& Id) {
        if (TypeDescriptors.count(Id) != 0) {
            return nullptr;
        }

        std::shared_ptr<TypeDesc> T(new TypeDesc(Union, Id));
        TypeDescriptors.emplace(Id, T);
        CanonicalTypeDescriptors.emplace(Id, T);
        return T.get();
    }

    const TypeDesc* newRecord(const std::string& Id) {
        if (TypeDescriptors.count(Id) != 0) {
            return nullptr;
        }

        std::shared_ptr<TypeDesc> T(new TypeDesc(Record, Id));
        TypeDescriptors.emplace(Id, T);
        CanonicalTypeDescriptors.emplace(Id, T);
        return T.get();
    }

    const TypeDesc* newADT(const std::string& Id) {
        if (TypeDescriptors.count(Id) != 0) {
            return nullptr;
        }

        std::shared_ptr<TypeDesc> T(new TypeDesc(ADT, Id));
        TypeDescriptors.emplace(Id, T);
        CanonicalTypeDescriptors.emplace(Id, T);
        return T.get();
    }

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

        std::shared_ptr<TypeDesc> T(new TypeDesc(Branch, *TypeRef, Id));
        if (TypeRef->addElement(Id, T.get())) {
            BranchDescriptors.emplace(Id, T);
            return T.get();
        } else {
            return nullptr;
        }
    }

    const TypeDesc* newTuple(const std::string& RelId) {
        if (TupleDescriptors.count(RelId) != 0) {
            return nullptr;
        }

        std::shared_ptr<TypeDesc> T(new TypeDesc(Tuple, RelId));
        TupleDescriptors.emplace(RelId, T);
        return T.get();
    }

private:
    std::map<std::string, std::shared_ptr<TypeDesc>> CanonicalTypeDescriptors;
    std::map<std::string, std::shared_ptr<TypeDesc>> TypeDescriptors;
    std::map<std::string, std::shared_ptr<TypeDesc>> TupleDescriptors;
    std::map<std::string, std::shared_ptr<TypeDesc>> BranchDescriptors;
};

}  // namespace interface
}  // namespace souffle
