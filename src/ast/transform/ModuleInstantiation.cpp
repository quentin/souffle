/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2023, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

#include "ast/transform/ModuleInstantiation.h"
#include "ast/AlgebraicDataType.h"
#include "ast/utility/Visitor.h"
#include "souffle/datastructure/SymbolTableImpl.h"
#include "souffle/utility/span.h"

#include <deque>
#include <map>
#include <queue>
#include <variant>

namespace souffle::ast::transform {

namespace {

using souffle::span;

/// identifier of an definition
using DefId = std::size_t;

/// an interned identifier
struct Ident {
    /// index in the symbol table
    RamDomain idx;
};

/// namespaces
enum class NS {
    /// the namespace of component declarations
    Comp,
    /// the namespace of user-defined functors
    UFun,
    /// the namespace of types, component instances, modules
    Type,
    /// the namespace of relations, variables, intrinsic functors, aggregators, ADT branches
    Value,
};

/// key of a name binding
struct BindingKey {
    /// the identifier
    Ident ident;

    /// the namespace
    NS ns;

    bool operator<(const BindingKey& other) const {
        return (ident.idx < other.ident.idx) || (ident.idx == other.ident.idx && ns < other.ns);
    }
};

struct ModuleInstance;

/// the kind of definitions
enum class DefKind {
    // ADT Branch
    Branch,
    // Component
    Comp,
    // Instance (component instance)
    Init,
    // User functor
    Func,
    // Module
    Mod,
    // Relation
    Rel,
    // Type definition
    Type,
};

/// a definition
struct Def {
    DefKind kind;
    DefId def;
};

/// the four primary types of souffle
enum class PrimaryType { Number, Unsigned, Float, Symbol };

/// a primary type
struct PrimTy {
    PrimaryType ty;
};

/// a resolution
using Res = std::variant<Def, PrimTy>;

DefId toDefId(const Res& res) {
    if (std::holds_alternative<Def>(res)) {
        return std::get<Def>(res).def;
    } else {
        throw std::runtime_error("toDefId()");
    }
}

struct Module {
    ModuleInstance* mod;
};

/// either a resolution or a module
using NameBindingKind = std::variant<Res, Module>;

struct NameBinding {
    NameBindingKind kind;
    SrcLocation loc;

    Res res() const;

    /// return the module instance if the name binding
    std::optional<ModuleInstance*> mod() {
        if (std::holds_alternative<Module>(kind)) {
            return std::get<Module>(kind).mod;
        }
        return std::nullopt;
    }
};

struct NameResolution {
    /// resolved binding when available
    std::optional<NameBinding> binding;
};

/// The instance of a module
struct ModuleInstance {
    /// identifier of this module instance
    DefId def;

    /// qualified name of this module instance
    ast::QualifiedName name;

    /// module declaration of this module instance,
    /// may be nullptr for the top module
    ModuleDecl* decl;

    /// parent module instance, may be nullptr for the top module
    ModuleInstance* parent;

    /// bindings from keys to resolution in this module instance
    std::map<BindingKey, NameResolution> resolutions;
};

/// Visit and possibly mutate paths (qualified name + namespace)
struct PathMutVisitor : public Visitor<void, Node> {
    virtual ~PathMutVisitor() = default;

    virtual void visitPath(
            const QualifiedName& qname, NS ns, const std::function<void(const QualifiedName&)>& updater) = 0;

private:
#define UPDATER(CALLEXPR) [&](const QualifiedName& qn) { CALLEXPR(qn); }

    void visit_(type_identity<Atom>, Atom& n) override {
        visitPath(n.getQualifiedName(), NS::Value, UPDATER(n.setQualifiedName));
    }

    void visit_(type_identity<Directive>, Directive& n) override {
        visitPath(n.getQualifiedName(), NS::Value, UPDATER(n.setQualifiedName));
    }

    void visit_(type_identity<Attribute>, Attribute& n) override {
        visitPath(n.getTypeName(), NS::Type, UPDATER(n.setTypeName));
    }

    void visit_(type_identity<BranchInit>, BranchInit& n) override {
        visitPath(n.getBranchName(), NS::Value, UPDATER(n.setBranchName));
    }

    void visit_(type_identity<ComponentType>, ComponentType& n) override {
        throw "todo";
    }

    void visit_(type_identity<RecordType>, RecordType& n) override {
        auto fields = n.getFields();
        for (std::size_t i = 0; i < fields.size(); ++i) {
            visitPath(fields[i]->getTypeName(), NS::Type,
                    [&](const QualifiedName& qn) { n.setFieldType(i, qn); });
        }
    }

    void visit_(type_identity<BranchType>, BranchType& n) override {
        auto fields = n.getFields();
        for (std::size_t i = 0; i < fields.size(); ++i) {
            visitPath(fields[i]->getTypeName(), NS::Type,
                    [&](const QualifiedName& qn) { n.setFieldType(i, qn); });
        }
    }

    void visit_(type_identity<UnionType>, UnionType& n) override {
        auto types = n.getTypes();
        for (std::size_t i = 0; i < types.size(); ++i) {
            visitPath(types[i], NS::Type, [&](const QualifiedName& qn) { n.setType(i, qn); });
        }
    }

    void visit_(type_identity<SubsetType>, SubsetType& n) override {
        visitPath(n.getBaseType(), NS::Type, UPDATER(n.setBaseType));
    }

    void visit_(type_identity<AliasType>, AliasType& n) override {
        visitPath(n.getAliasType(), NS::Type, UPDATER(n.setAliasType));
    }

    void visit_(type_identity<FunctorDeclaration>, FunctorDeclaration& n) override {
        for (Own<Attribute>& attr : n.getParams()) {
            visitPath(attr->getTypeName(), NS::Type, UPDATER(attr->setTypeName));
        }
        visitPath(n.getReturnType().getTypeName(), NS::Type,
                [&](const QualifiedName& qn) { n.getReturnType().setTypeName(qn); });
    }

    void visit_(type_identity<TypeCast>, TypeCast& n) override {
        visitPath(n.getType(), NS::Type, UPDATER(n.setType));
    }

    void visit_(type_identity<Component>, Component& c) override {
        // TODO do not rewrite within components
    }
#undef UPDATER
};

struct TransformerImpl;

struct PathExpander : public PathMutVisitor {
    explicit PathExpander(TransformerImpl* t, ModuleInstance& parent) : t(t), parent(parent) {}

    void visitPath(const QualifiedName& qname, NS ns,
            const std::function<void(const QualifiedName&)>& updater) override;

private:
    TransformerImpl* t;
    ModuleInstance& parent;
};

struct TransformerImpl {
    explicit TransformerImpl(TranslationUnit& t) : tu(t), symtable{"number", "unsigned", "float", "symbol"} {}

public:
    bool transform() {
        Program& program = tu.getProgram();

        // create a module for the program that contains top-level items
        createModule(createDef(program.getTopModule()), QualifiedName(), program.getTopModule(), nullptr);

        {
            // collect definitions in all modules
            std::size_t idx = 0;
            while (idx < moduleInstances.size()) {
                ModuleInstance& inst = moduleInstances[idx++];
                collectDefinitions(inst);
            }
        }

        // expand each module
        for (std::size_t idx = 0; idx < moduleInstances.size(); ++idx) {
            ModuleInstance& inst = moduleInstances[idx];
            expandModule(inst);
        }

        program.setTopModule(nullptr);

        return true;
    }

    QualifiedName expandPath(const QualifiedName& qname, const NS ns, ModuleInstance& parent) {
        PathResult pathRes = resolveQualifiedName(qname, ns, parent);
        if (std::holds_alternative<Res>(pathRes)) {
            const Res& res = std::get<Res>(pathRes);
            if (std::holds_alternative<Def>(res)) {
                const DefId def = toDefId(res);
                return qualifiedNameMap.at(def);
            } else if (std::holds_alternative<PrimTy>(res)) {
                const PrimTy primTy = std::get<PrimTy>(res);
                switch (primTy.ty) {
                    case PrimaryType::Number: return "number";
                    case PrimaryType::Unsigned: return "unsigned";
                    case PrimaryType::Float: return "float";
                    case PrimaryType::Symbol: return "symbol";
                    default: throw std::runtime_error("unexpected primary type");
                }
            } else {
                throw std::runtime_error("unexpected Res");
            }
        } else if (std::holds_alternative<Module>(pathRes)) {
            return std::get<Module>(pathRes).mod->name;
        } else if (std::holds_alternative<ComponentPath>(pathRes)) {
            return std::get<ComponentPath>(pathRes).qname;
        } else if (std::holds_alternative<Failed>(pathRes)) {
            // error already emitted by `resolveQualifiedName`
            return qname;
        } else {
            throw "unexpected";
        }
    }

private:
    TranslationUnit& tu;

    DefId nextDefId = 1;

    /// mapping from definition identifier to AST node
    std::map<DefId, Node*> defNodeMap;

    /// mapping from an AST node to its definition identifier
    std::map<Node*, DefId> nodeDefMap;

    /// store module instances
    std::deque<ModuleInstance> moduleInstances;

    /// mapping the definition identifier of a module to the module instance
    std::map<DefId, ModuleInstance*> moduleInstanceMap;

    /// mapping from a definition identifier to its parent module
    std::map<DefId, ModuleInstance*> parentMap;

    /// mapping from a definition identifier to its full qualified name
    std::map<DefId, QualifiedName> qualifiedNameMap;

    SymbolTableImpl symtable;

    /// Create a definition from a node
    DefId createDef(Node* n) {
        const DefId def = nextDefId++;
        defNodeMap.emplace(def, n);
        nodeDefMap.emplace(n, def);
        return def;
    }

    /// Return the current resolution for the key.
    NameResolution& resolution(ModuleInstance* parent, const BindingKey& key) {
        const auto inserted = parent->resolutions.try_emplace(key, NameResolution{std::nullopt});
        return inserted.first->second;
    }

    /// Add a definition.
    void define(ModuleInstance* parent, const std::string& name, NS ns, NameBinding binding) {
        Ident ident{symtable.encode(name)};
        BindingKey key{ident, ns};
        const auto inserted = parent->resolutions.try_emplace(key, NameResolution{binding});
        if (!inserted.second) {
            // key is already bound to something
            std::vector<DiagnosticMessage> additionals;
            const NameResolution& existingResolution = inserted.first->second;
            if (existingResolution.binding) {
                const SrcLocation existingLoc = existingResolution.binding->loc;
                additionals.emplace_back("Previous definition", existingLoc);
            }

            Diagnostic err(Diagnostic::Type::ERROR, DiagnosticMessage("Redefinition of " + name, binding.loc),
                    std::move(additionals));
            tu.getErrorReport().addDiagnostic(err);
        }

        const DefId def = toDefId(binding.res());

        parentMap.emplace(def, parent);

        QualifiedName qname(parent->name);
        qname.append(name);
        qualifiedNameMap.emplace(def, std::move(qname));
    }

    /// Create a new module
    ModuleInstance& createModule(
            const DefId def, QualifiedName name, ModuleDecl* decl, ModuleInstance* parent) {
        ModuleInstance& inst = moduleInstances.emplace_back(ModuleInstance{def, name, decl, parent, {}});
        moduleInstanceMap.emplace(inst.def, &inst);
        return inst;
    }

    /// collect definitions of a module
    void collectDefinitions(ModuleInstance& inst) {
        for (Own<Node>& item : inst.decl->getItems()) {
            if (ModuleDecl* md = as<ModuleDecl>(item)) {
                const std::string name = md->getName();
                ast::QualifiedName qname(inst.name);
                qname.append(name);
                const DefId def = createDef(md);
                ModuleInstance& mod = createModule(def, qname, md, &inst);
                moduleInstanceMap.emplace(def, &mod);

                NameBinding binding{Module{&mod}, md->getSrcLoc()};
                define(&inst, name, NS::Type, binding);
            } else if (Relation* rel = as<Relation>(item)) {
                // @todo only allow non-qualified names?
                const DefId def = createDef(rel);
                NameBinding binding{Res{Def{DefKind::Rel, def}}, rel->getSrcLoc()};
                define(&inst, rel->getQualifiedName().toString(), NS::Value, binding);
            } else if (FunctorDeclaration* fd = as<FunctorDeclaration>(item)) {
                const DefId def = createDef(fd);
                NameBinding binding{Res{Def{DefKind::Func, def}}, fd->getSrcLoc()};
                define(&inst, fd->getName(), NS::UFun, binding);
            } else if (Type* ty = as<Type>(item)) {
                // @todo only allow non-qualified names?
                const DefId def = createDef(ty);
                NameBinding binding{Res{Def{DefKind::Type, def}}, ty->getSrcLoc()};
                define(&inst, ty->getQualifiedName().toString(), NS::Type, binding);

                if (AlgebraicDataType* adt = as<AlgebraicDataType>(ty)) {
                    // define each branch in the Value namespace
                    for (BranchType* br : adt->getBranches()) {
                        const DefId brDef = createDef(br);
                        NameBinding brBinding{Res{Def{DefKind::Branch, brDef}}, br->getSrcLoc()};
                        // @todo only allow non-qualified names?
                        define(&inst, br->getBranchName().toString(), NS::Value, brBinding);
                    }
                }
            } else if (Component* cm = as<ast::Component>(item)) {
                const DefId def = createDef(cm);
                NameBinding binding{Res{Def{DefKind::Comp, def}}, cm->getSrcLoc()};
                define(&inst, cm->getComponentType()->getName(), NS::Comp, binding);
            } else if (ComponentInit* ci = as<ast::ComponentInit>(item)) {
                const DefId def = createDef(ci);
                NameBinding binding{Res{Def{DefKind::Init, def}}, ci->getSrcLoc()};
                define(&inst, ci->getInstanceName(), NS::Value, binding);
            }
        }
    }

    void expandModule(ModuleInstance& inst) {
        Program& program = tu.getProgram();
        for (Own<Node>& item : inst.decl->getItems()) {
            if (isA<ModuleDecl>(item)) {
                // nothing to do for module declaration they don't exist in the
                // program after the expansion.
            } else if (Own<Relation> n = own_cast<Relation>(item)) {
                const DefId def = nodeDefMap.at(n.get());
                expandRelation(def, n.get(), inst);
                program.addRelation(std::move(n));
            } else if (Own<Type> n = own_cast<Type>(item)) {
                const DefId def = nodeDefMap.at(n.get());
                expandType(def, n.get(), inst);
                program.addType(std::move(n));
            } else if (Own<Directive> n = own_cast<Directive>(item)) {
                expandDirective(n.get(), inst);
                program.addDirective(std::move(n));
            } else if (Own<Clause> n = own_cast<Clause>(item)) {
                expandClause(*n, inst);
                program.addClause(std::move(n));
            } else if (Own<Component> n = own_cast<Component>(item)) {
                // we do not apply expansion to components
                program.addComponent(std::move(n));
            } else if (Own<ComponentInit> n = own_cast<ComponentInit>(item)) {
                // we do not apply expansion to components init
                program.addInstantiation(std::move(n));
            } else if (Own<Pragma> n = own_cast<Pragma>(item)) {
                // TODO
                program.addPragma(std::move(n));
            } else if (Own<FunctorDeclaration> n = own_cast<FunctorDeclaration>(item)) {
                expandFunctorDeclaration(*n, inst);
                program.addFunctorDeclaration(std::move(n));
            } else {
                std::cerr << "TODO expand " << *item << "\n";
            }
        }
    }

    /// The failed resolution of a path
    struct Failed {};

    struct ComponentPath {
        QualifiedName qname;
    };

    /// The result of resolving a path
    using PathResult = std::variant<Module, Res, ComponentPath, Failed>;

    void expandRelation(const DefId def, Relation* rel, ModuleInstance& parent) {
        // expand attribute types
        rel->setQualifiedName(qualifiedNameMap.at(def));
        PathExpander vis(this, parent);
        visit(rel, vis);
    }

    void expandType(const DefId def, Type* ty, ModuleInstance& parent) {
        ty->setQualifiedName(qualifiedNameMap.at(def));
        (void)parent;
        if (AlgebraicDataType* adt = as<AlgebraicDataType>(ty)) {
            for (BranchType* br : adt->getBranches()) {
                br->setBranchName(qualifiedNameMap.at(nodeDefMap.at(br)));
            }
        }
        PathExpander vis(this, parent);
        visit(ty, vis);
    }

    void expandDirective(Directive* dir, ModuleInstance& parent) {
        PathExpander vis(this, parent);
        visit(dir, vis);
    }

    void expandClause(Clause& cl, ModuleInstance& parent) {
        PathExpander vis(this, parent);
        visit(cl, vis);
    }

    void expandFunctorDeclaration(FunctorDeclaration& fd, ModuleInstance& parent) {
        PathExpander vis(this, parent);
        visit(fd, vis);
    }

    QualifiedName expectFullQualifiedName(const PathResult& pathRes) {
        if (std::holds_alternative<Res>(pathRes)) {
            const Res& res = std::get<Res>(pathRes);
            if (std::holds_alternative<Def>(res)) {
                const DefId def = toDefId(res);
                return qualifiedNameMap.at(def);
            } else if (std::holds_alternative<PrimTy>(res)) {
                const PrimTy primTy = std::get<PrimTy>(res);
                switch (primTy.ty) {
                    case PrimaryType::Number: return "number";
                    case PrimaryType::Unsigned: return "unsigned";
                    case PrimaryType::Float: return "float";
                    case PrimaryType::Symbol: return "symbol";
                    default: throw std::runtime_error("unexpected primary type");
                }
            } else {
                throw std::runtime_error("unexpected Res");
            }
        } else if (std::holds_alternative<Module>(pathRes)) {
            return std::get<Module>(pathRes).mod->name;
        } else if (std::holds_alternative<ComponentPath>(pathRes)) {
            return std::get<ComponentPath>(pathRes).qname;
        } else {
            throw std::runtime_error("unresolved");
        }
    }

    /// Resolve a qualified name
    PathResult resolveQualifiedName(const QualifiedName& qname, const NS ns, ModuleInstance& parent) {
        return resolvePath(qname.getQualifiers(), ns, parent);
    }

    PathResult resolvePath(span<const std::string> segments, const NS ns, ModuleInstance& parent) {
        if (segments.empty()) {
            Diagnostic err(Diagnostic::Type::ERROR, DiagnosticMessage("empty path"));
            tu.getErrorReport().addDiagnostic(err);
            return Failed{};
        }

        ModuleInstance* mod = nullptr;

        for (std::size_t i = 0; i < segments.size(); ++i) {
            const bool isLast = (i + 1 == segments.size());
            const NS segmentNS = isLast ? ns : NS::Type;
            const Ident segmentIdent{symtable.encode(segments[i])};

            std::optional<NameBinding> binding = [&]() {
                if (mod == nullptr) {
                    return resolveIdentInLexicalScope(parent, segmentIdent, segmentNS);
                } else {
                    return resolveIdentInModule(*mod, segmentIdent, segmentNS);
                }
            }();

            if (!binding) {
                Diagnostic err(Diagnostic::Type::ERROR, DiagnosticMessage("Cannot resolve " + segments[i]));
                tu.getErrorReport().addDiagnostic(err);
                return Failed{};
            } else if (std::optional<ModuleInstance*> nextMod = binding->mod()) {
                mod = *nextMod;
            } else {
                Res res = binding->res();
                if (isLast) {
                    return res;
                } else if (i == 0 && std::holds_alternative<Def>(res) &&
                           std::get<Def>(res).kind == DefKind::Init) {
                    return ComponentPath{QualifiedName(segments)};
                } else {
                    Diagnostic err(
                            Diagnostic::Type::ERROR, DiagnosticMessage("Cannot resolve " + segments[i]));
                    tu.getErrorReport().addDiagnostic(err);
                    return Failed{};
                }
            }
        }

        return Module{mod};
    }

    std::optional<NameBinding> resolveIdentInLexicalScope(ModuleInstance& parent, Ident ident, const NS ns) {
        /// search recusively in scopes
        ModuleInstance* mod = &parent;
        while (mod) {
            if (std::optional<NameBinding> binding = resolveIdentInModule(*mod, ident, ns)) {
                return binding;
            }
            mod = mod->parent;
        }

        const auto& sym = symtable.decode(ident.idx);
        if (sym == "number") {
            return NameBinding{Res{PrimTy{PrimaryType::Number}}, SrcLocation{}};
        }
        if (sym == "unsigned") {
            return NameBinding{Res{PrimTy{PrimaryType::Unsigned}}, SrcLocation{}};
        }
        if (sym == "float") {
            return NameBinding{Res{PrimTy{PrimaryType::Float}}, SrcLocation{}};
        }
        if (sym == "symbol") {
            return NameBinding{Res{PrimTy{PrimaryType::Symbol}}, SrcLocation{}};
        }

        return std::nullopt;
    }

    std::optional<NameBinding> resolveIdentInModule(ModuleInstance& mod, Ident ident, const NS ns) {
        BindingKey key{ident, ns};
        auto it = mod.resolutions.find(key);
        if (it == mod.resolutions.end()) {
            return std::nullopt;
        }
        return it->second.binding;
    }
};

Res NameBinding::res() const {
    if (std::holds_alternative<Res>(kind)) {
        return std::get<Res>(kind);
    } else if (std::holds_alternative<Module>(kind)) {
        return Def{DefKind::Mod, std::get<Module>(kind).mod->def};
    } else {
        throw std::runtime_error("res()");
    }
}

}  // namespace

bool ModuleInstantiationTransformer::transform(TranslationUnit& tu) {
    TransformerImpl impl{tu};
    return impl.transform();
}

void PathExpander::visitPath(
        const QualifiedName& qname, NS ns, const std::function<void(const QualifiedName&)>& updater) {
    updater(t->expandPath(qname, ns, parent));
}

}  // namespace souffle::ast::transform
