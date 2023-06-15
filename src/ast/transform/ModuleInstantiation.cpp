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

#include <algorithm>
#include <deque>
#include <functional>
#include <limits>
#include <list>
#include <map>
#include <optional>
#include <queue>
#include <variant>
#include <vector>

namespace souffle::ast::transform {

namespace {

SymbolTableImpl symtable{"number", "unsigned", "float", "symbol"};

using souffle::span;

/// identifier of an AST node
using NodeId = std::size_t;

/// identifier of an definition
using DefId = std::size_t;

#define DEBUG_IDENT
/// an interned identifier
struct Ident {
    /// index in the symbol table
    const RamDomain idx;
#ifdef DEBUG_IDENT
    const std::string_view _str;

    static Ident from(const std::string& str) {
        RamDomain idx = symtable.encode(str);
        return Ident{idx, symtable.decode(idx)};
    }

    std::string_view str() const {
        return _str;
    }

    std::string toString() const {
        return std::string(_str);
    }
#else
    static Ident from(const std::string& str) {
        return Ident{symtable.encode(str)};
    }

    std::string_view str() const {
        return symtable.decode(idx);
    }

    std::string toString() const {
        return symtable.decode(idx);
    }
#endif
};

/// namespaces
enum class NS {
    /// the namespace of ADT branches
    Branch,
    /// the namespace of component declarations
    Comp,
    /// the namespace of user-defined functors
    UFun,
    /// the namespace of types, component instances, modules
    Type,
    /// the namespace of relations, variables, intrinsic functors, aggregators
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
    UFun,
    // Module
    Mod,
    // Relation
    Rel,
    // Type definition
    Type,
};

NS toNS(const DefKind kind);

/// a definition
struct Def {
    DefKind kind;
    DefId def;
};

/// A module
struct Module {
    ModuleInstance* mod;
};

/// the four primary types of souffle
enum class PrimaryType { Number, Unsigned, Float, Symbol };

/// a primary type
struct PrimTy {
    PrimaryType ty;
};

/// resolution error indicator
struct Err {};

/// a resolution
struct Res : std::variant<Def, PrimTy, Err> {
    using variant::variant;

    DefId toDefId() const {
        if (std::holds_alternative<Def>(*this)) {
            return std::get<Def>(*this).def;
        } else {
            throw std::runtime_error("toDefId()");
        }
    }
};

/// the import of a definition
struct Import {
    /// the module where this import must be resolved in
    ModuleInstance* parent;

    /// the source identifier of this import in the source module
    Ident source;

    /// the target identifier of this import in the `parent` module.
    Ident target;

    /// the kind of definition expected by this import
    DefKind defKind;

    /// the path of the source module to import from
    span<const std::string> modulePath;

    /// resolution of `modulePath`
    std::optional<ModuleInstance*> importedModule;

    /// source location of the import path
    SrcLocation loc;
};

/// either a resolution or a module or an import
using NameBindingKind = std::variant<Res, Module, Import*>;

struct NameBinding {
    NameBindingKind kind;
    SrcLocation loc;

    Res res() const;

    /// return the module instance if the name binding
    std::optional<ModuleInstance*> mod() {
        if (std::holds_alternative<Module>(kind)) {
            return std::get<Module>(kind).mod;
        }
        // @todo Import ?
        return std::nullopt;
    }
};

/// The resolution of a name binding.
struct NameResolution {
    /// imports that may resolve this name
    std::list<Import*> imports;

    /// resolved binding when available
    std::optional<NameBinding*> binding;

    /// add an import that may resolve this name
    void addImport(Import* imprt) {
        imports.emplace_back(imprt);
    }
};

/// The instance of a module
struct ModuleInstance {
    /// identifier of this module instance
    DefId def;

    /// qualified name of this module instance
    ast::QualifiedName name;

    /// parent module instance, may be nullptr for the top module
    ModuleInstance* parent;

    /// bindings from keys to resolution in this module instance
    std::map<BindingKey, NameResolution> resolutions;

    /// AST nodes in this module
    std::vector<NodeId> nodes;
};

/// Visit and possibly mutate paths (qualified name + namespace)
struct PathMutVisitor : public Visitor<void, Node> {
    virtual ~PathMutVisitor() = default;

    virtual void visitPath(const QualifiedName& qname, NS ns, const SrcLocation& loc,
            const std::function<void(const QualifiedName&)>& updater) = 0;

private:
#define UPDATER(CALLEXPR) [&](const QualifiedName& qn) { CALLEXPR(qn); }

    void visit_(type_identity<Atom>, Atom& n) override {
        visitPath(n.getQualifiedName(), NS::Value, n.getSrcLoc(), UPDATER(n.setQualifiedName));
    }

    void visit_(type_identity<Directive>, Directive& n) override {
        visitPath(n.getQualifiedName(), NS::Value, n.getSrcLoc(), UPDATER(n.setQualifiedName));
    }

    void visit_(type_identity<Attribute>, Attribute& n) override {
        visitPath(n.getTypeName(), NS::Type, n.getSrcLoc(), UPDATER(n.setTypeName));
    }

    void visit_(type_identity<BranchInit>, BranchInit& n) override {
        visitPath(n.getBranchName(), NS::Branch, n.getSrcLoc(), UPDATER(n.setBranchName));
    }

    void visit_(type_identity<ComponentType>, ComponentType& n) override {
        throw "todo";
    }

    void visit_(type_identity<RecordType>, RecordType& n) override {
        auto fields = n.getFields();
        for (std::size_t i = 0; i < fields.size(); ++i) {
            visitPath(fields[i]->getTypeName(), NS::Type, fields[i]->getSrcLoc(),
                    [&](const QualifiedName& qn) { n.setFieldType(i, qn); });
        }
    }

    void visit_(type_identity<BranchType>, BranchType& n) override {
        auto fields = n.getFields();
        for (std::size_t i = 0; i < fields.size(); ++i) {
            visitPath(fields[i]->getTypeName(), NS::Type, fields[i]->getSrcLoc(),
                    [&](const QualifiedName& qn) { n.setFieldType(i, qn); });
        }
    }

    void visit_(type_identity<UnionType>, UnionType& n) override {
        auto types = n.getTypes();
        for (std::size_t i = 0; i < types.size(); ++i) {
            visitPath(types[i], NS::Type, n.getSrcLoc(), [&](const QualifiedName& qn) { n.setType(i, qn); });
        }
    }

    void visit_(type_identity<SubsetType>, SubsetType& n) override {
        visitPath(n.getBaseType(), NS::Type, n.getSrcLoc(), UPDATER(n.setBaseType));
    }

    void visit_(type_identity<AliasType>, AliasType& n) override {
        visitPath(n.getAliasType(), NS::Type, n.getSrcLoc(), UPDATER(n.setAliasType));
    }

    void visit_(type_identity<FunctorDeclaration>, FunctorDeclaration& n) override {
        for (Own<Attribute>& attr : n.getParams()) {
            visitPath(attr->getTypeName(), NS::Type, attr->getSrcLoc(), UPDATER(attr->setTypeName));
        }
        visitPath(n.getReturnType().getTypeName(), NS::Type, n.getReturnType().getSrcLoc(),
                [&](const QualifiedName& qn) { n.getReturnType().setTypeName(qn); });
    }

    void visit_(type_identity<TypeCast>, TypeCast& n) override {
        visitPath(n.getType(), NS::Type, n.getSrcLoc(), UPDATER(n.setType));
    }

    void visit_(type_identity<Component>, Component& c) override {
        // TODO do not rewrite within components
    }
#undef UPDATER
};

struct TransformerImpl;

struct PathExpander : public PathMutVisitor {
    explicit PathExpander(TransformerImpl* t, ModuleInstance& parent) : t(t), parent(parent) {}

    void visitPath(const QualifiedName& qname, NS ns, const SrcLocation& loc,
            const std::function<void(const QualifiedName&)>& updater) override;

private:
    TransformerImpl* t;
    ModuleInstance& parent;
};

struct TransformerImpl {
    explicit TransformerImpl(TranslationUnit& t) : tu(t) {
        numberNameBinding = allocNameBinding(Res{PrimTy{PrimaryType::Number}}, SrcLocation{});
        unsignedNameBinding = allocNameBinding(Res{PrimTy{PrimaryType::Unsigned}}, SrcLocation{});
        floatNameBinding = allocNameBinding(Res{PrimTy{PrimaryType::Float}}, SrcLocation{});
        symbolNameBinding = allocNameBinding(Res{PrimTy{PrimaryType::Symbol}}, SrcLocation{});
    }

public:
    bool transform() {
        collectDefinitions();
        resolveImports();
        expandModules();
        return true;
    }

    QualifiedName expandPath(
            const QualifiedName& qname, const NS ns, const SrcLocation& loc, ModuleInstance& parent) {
        PathResult pathRes = resolveQualifiedName(qname, ns, loc, parent);
        if (std::holds_alternative<Res>(pathRes)) {
            const Res& res = std::get<Res>(pathRes);
            if (std::holds_alternative<Def>(res)) {
                const DefId def = res.toDefId();
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

    /// next definition identifier
    DefId nextDefId = 1;

    /// next AST node identifier
    NodeId nextNodeId = 1;

    /// mapping from definition identifier to AST node identifier
    std::map<DefId, NodeId> defNodeMap;

    /// mapping from an AST node identifier to its definition identifier
    std::map<NodeId, DefId> nodeDefMap;

    /// store module instances
    std::deque<ModuleInstance> moduleInstances;

    /// mapping the definition identifier of a module to the module instance
    std::map<DefId, ModuleInstance*> moduleInstanceMap;

    /// mapping from a definition identifier to its parent module
    std::map<DefId, ModuleInstance*> parentMap;

    /// mapping from a definition identifier to its full qualified name
    std::map<DefId, QualifiedName> qualifiedNameMap;

    /// mapping from a node identifier to an AST node
    std::map<NodeId, Node*> nodeMap;

    /// imports not yet resolved
    std::list<Import*> unresolvedImports;

    /// all the imports
    std::deque<Import> imports;

    /// all the name bindings
    std::queue<NameBinding> nameBindings;

    NameBinding* numberNameBinding = nullptr;
    NameBinding* unsignedNameBinding = nullptr;
    NameBinding* floatNameBinding = nullptr;
    NameBinding* symbolNameBinding = nullptr;

    /// Recursively collect definitions and instantiate modules
    void collectDefinitions() {
        Program& program = tu.getProgram();

        // create a module for the program that contains top-level items
        ModuleInstance& top = createModule(createDef(0), QualifiedName(), nullptr);

        // collect definitions and more module instances
        std::list<std::pair<ModuleInstance*, Items*>> modules;
        modules.emplace_back(&top, &program.getTopModule()->getItems());
        while (!modules.empty()) {
            std::pair<ModuleInstance*, Items*> task = modules.front();
            modules.pop_front();
            auto newModules = collectDefinitions(*task.first, *task.second);
            modules.splice(modules.end(), newModules);
        }
    }

    /// Resolve the imported definitions
    void resolveImports() {
        bool resolvedSome = true;
        // iterate as long as progress is made
        while (resolvedSome) {
            resolvedSome = false;
            auto it = unresolvedImports.begin();
            while (it != unresolvedImports.end()) {
                if (resolveImport(*it)) {
                    resolvedSome = true;
                    it = unresolvedImports.erase(it);
                } else {
                    ++it;
                }
            }
        }
        if (!unresolvedImports.empty()) {
            Diagnostic err(
                    Diagnostic::Type::ERROR, DiagnosticMessage("Some definition could not be resolved"));
            tu.getErrorReport().addDiagnostic(err);
        }
    }

    /// Flatten the modules into a program
    void expandModules() {
        Program& program = tu.getProgram();

        // expand each module
        for (std::size_t idx = 0; idx < moduleInstances.size(); ++idx) {
            ModuleInstance& inst = moduleInstances[idx];
            expandModule(inst);
        }

        program.setTopModule(nullptr);
    }

    /// Create a definition from a node
    DefId createDef(const NodeId id) {
        const DefId def = nextDefId++;
        defNodeMap.emplace(def, id);
        nodeDefMap.emplace(id, def);
        return def;
    }

    /// Return the current resolution for the key.
    ///
    /// Create an unresolved binding if it does not exists yet.
    NameResolution& resolution(ModuleInstance& parent, const BindingKey& key) {
        const auto inserted = parent.resolutions.try_emplace(key, NameResolution{{}, std::nullopt});
        return inserted.first->second;
    }

    /// Add a definition.
    void define(ModuleInstance* parent, Ident ident, NS ns, NameBinding* binding) {
        const BindingKey key{ident, ns};
        std::optional<NameBinding*> maybeExistingBinding = tryDefine(parent, key, binding);
        if (maybeExistingBinding) {
            // key is already bound to something
            std::vector<DiagnosticMessage> additionals;
            const NameBinding* existingBinding = *maybeExistingBinding;
            const SrcLocation existingLoc = existingBinding->loc;
            additionals.emplace_back("Previous definition", existingLoc);

            Diagnostic err(Diagnostic::Type::ERROR,
                    DiagnosticMessage(std::string("Redefinition of ") + ident.toString(), binding->loc),
                    std::move(additionals));
            tu.getErrorReport().addDiagnostic(err);
        }

        const DefId def = binding->res().toDefId();

        parentMap.emplace(def, parent);

        QualifiedName qname(parent->name);
        qname.append(ident.toString());
        qualifiedNameMap.emplace(def, std::move(qname));
    }

    /// Define a binding if it does not have a definition yet, otherwise return the existing binding
    std::optional<NameBinding*> tryDefine(
            ModuleInstance* parent, const BindingKey& key, NameBinding* binding) {
        const auto inserted = parent->resolutions.try_emplace(key, NameResolution{{}, binding});
        if (!inserted.second) {
            // resolution already exists
            NameResolution& resolution = inserted.first->second;
            if (resolution.binding) {
                // already bound to something
                NameBinding* oldBinding = *resolution.binding;
                return oldBinding;
            } else {
                resolution.binding = binding;
            }
        }

        return std::nullopt;
    }

    /// Create a new module
    ModuleInstance& createModule(const DefId def, QualifiedName name, ModuleInstance* parent) {
        ModuleInstance& inst = moduleInstances.emplace_back(
                ModuleInstance{def, name, parent, /*resolutions*/ {}, /*nodes*/ {}});
        moduleInstanceMap.emplace(inst.def, &inst);
        return inst;
    }

    /// Add an import binding in `parent` module.
    ///
    /// `source` is the binding name in the module imported from.
    /// `target` is the alias binding to be created in `parent` once the import is resolved
    void addImport(ModuleInstance& parent, const Ident source, const Ident target, const DefKind kind,
            span<const std::string> modulePath, std::optional<ModuleInstance*> importedModule,
            SrcLocation loc) {
        imports.emplace_back(Import{&parent, source, target, kind, modulePath, importedModule, loc});
        Import* imprt = &imports.back();
        unresolvedImports.emplace_back(imprt);

        const BindingKey key{target, toNS(kind)};
        resolution(parent, key).addImport(imprt);
    }

    /// Collect definitions of a module
    ///
    /// Return the list of new module instances found in the current module.
    std::list<std::pair<ModuleInstance*, Items*>> collectDefinitions(ModuleInstance& inst, Items& items) {
        std::list<std::pair<ModuleInstance*, Items*>> newModules;

        for (Own<Node>& item : items) {
            const NodeId id = nextNodeId++;
            inst.nodes.emplace_back(id);

            Node* n = item.get();
            nodeMap.emplace(id, n);

            if (ModuleDecl* md = as<ModuleDecl>(n)) {
                const std::string name = md->getName();
                ast::QualifiedName qname(inst.name);
                qname.append(name);
                const DefId def = createDef(id);
                ModuleDef& moduleDef = md->getDefinition();
                if (ModuleStruct* mstruct = as<ModuleStruct>(moduleDef)) {
                    ModuleInstance& mod = createModule(def, qname, &inst);
                    newModules.emplace_back(&mod, &mstruct->getItems());
                    NameBinding* binding = allocNameBinding(Module{&mod}, md->getSrcLoc());
                    define(&inst, Ident::from(name), NS::Type, binding);

                } else if (ModuleAlias* malias = as<ModuleAlias>(moduleDef)) {
                    const Ident source = Ident::from(malias->getSource().getQualifiers().front());
                    const Ident target = Ident::from(name);
                    std::optional<ModuleInstance*> importedModule;
                    span<const std::string> modPath;

                    if (malias->getSource().getQualifiers().size() == 1) {
                        importedModule = &inst;
                    } else {
                        modPath = span<const std::string>(malias->getSource().getQualifiers().data() + 1,
                                malias->getSource().getQualifiers().size() - 1);
                    }

                    addImport(
                            inst, source, target, DefKind::Mod, modPath, importedModule, malias->getSrcLoc());

                } else if (ModuleApplication* mapp = as<ModuleApplication>(moduleDef)) {
                    // must resolve the source module and then generate a module for it
                    // @todo rename in ModuleGeneration ?
                    throw "TODO";
                } else {
                    throw "unexpected";
                }
            } else if (Relation* rel = as<Relation>(n)) {
                // @todo only allow non-qualified names?
                const DefId def = createDef(id);
                NameBinding* binding = allocNameBinding(Res{Def{DefKind::Rel, def}}, rel->getSrcLoc());
                define(&inst, Ident::from(rel->getQualifiedName().toString()), NS::Value, binding);
            } else if (FunctorDeclaration* fd = as<FunctorDeclaration>(n)) {
                const DefId def = createDef(id);
                NameBinding* binding = allocNameBinding(Res{Def{DefKind::UFun, def}}, fd->getSrcLoc());
                define(&inst, Ident::from(fd->getName()), NS::UFun, binding);
            } else if (Type* ty = as<Type>(n)) {
                // @todo only allow non-qualified names?
                // @todo TypeAlias !!
                const DefId def = createDef(id);
                NameBinding* binding = allocNameBinding(Res{Def{DefKind::Type, def}}, ty->getSrcLoc());
                define(&inst, Ident::from(ty->getQualifiedName().toString()), NS::Type, binding);

                if (AlgebraicDataType* adt = as<AlgebraicDataType>(ty)) {
                    // define each branch in the Value namespace
                    for (BranchType* br : adt->getBranches()) {
                        const NodeId brId = nextNodeId++;
                        const DefId brDef = createDef(brId);
                        NameBinding* brBinding =
                                allocNameBinding(Res{Def{DefKind::Branch, brDef}}, br->getSrcLoc());
                        // @todo only allow non-qualified names?
                        define(&inst, Ident::from(br->getBranchName().toString()), NS::Branch, brBinding);
                    }
                }
            } else if (Component* cm = as<ast::Component>(n)) {
                const DefId def = createDef(id);
                NameBinding* binding = allocNameBinding(Res{Def{DefKind::Comp, def}}, cm->getSrcLoc());
                define(&inst, Ident::from(cm->getComponentType()->getName()), NS::Comp, binding);

            } else if (ComponentInit* ci = as<ast::ComponentInit>(n)) {
                const DefId def = createDef(id);
                NameBinding* binding = allocNameBinding(Res{Def{DefKind::Init, def}}, ci->getSrcLoc());
                define(&inst, Ident::from(ci->getInstanceName()), NS::Type, binding);
            }
        }

        return newModules;
    }

    /// Try to resolve the given import.
    ///
    /// Return true if the import has been resolved, false otherwise.
    bool resolveImport(Import* imprt) {
        const NS ns = toNS(imprt->defKind);

        ModuleInstance* mod = nullptr;
        if (imprt->importedModule) {
            mod = *imprt->importedModule;
        }

        if (mod == nullptr) {
            // try to resolve the source module
            PathResult res = resolvePath(imprt->modulePath, ns, imprt->loc, *imprt->parent);
            if (std::holds_alternative<Module>(res)) {
                mod = std::get<Module>(res).mod;
            } else if (std::holds_alternative<Failed>(res)) {
                // cannot resolve the module yet, maybe later
                return false;
            } else {
                // expected a module, got something that is not a module
                // @todo return a code so that we don't even try to resolve this import again
                return false;
            }
        }

        // try to resolve the source binding in the source module without
        // creating it if it does not exists
        std::optional<NameBinding*> maybeBinding = resolveIdentInModule(*mod, imprt->source, ns);
        if (!maybeBinding) {
            return false;
        }

        define(imprt->parent, imprt->target, ns, *maybeBinding);
        {
            const BindingKey key{imprt->target, ns};
            auto& imports = resolution(*imprt->parent, key).imports;
            std::remove(imports.begin(), imports.end(), imprt);
        }

        return true;
    }

    void expandModule(ModuleInstance& inst) {
        Program& program = tu.getProgram();
        for (const NodeId id : inst.nodes) {
            Node* item = nodeMap.at(id);
            if (isA<ModuleDecl>(item)) {
                // nothing to do for module declaration they don't exist in the
                // program after the expansion.
            } else if (Relation* n = as<Relation>(item)) {
                const DefId def = nodeDefMap.at(id);
                Own<Relation> rel = clone(n);
                expandRelation(def, rel.get(), inst);
                program.addRelation(std::move(rel));

            } else if (Type* n = as<Type>(item)) {
                const DefId def = nodeDefMap.at(id);
                Own<Type> ty = clone(n);
                expandType(def, ty.get(), inst);
                program.addType(std::move(ty));

            } else if (Directive* n = as<Directive>(item)) {
                Own<Directive> dir = clone(n);
                expandDirective(*dir, inst);
                program.addDirective(std::move(dir));

            } else if (Clause* n = as<Clause>(item)) {
                Own<Clause> cl = clone(n);
                expandClause(*cl, inst);
                program.addClause(std::move(cl));

            } else if (Component* n = as<Component>(item)) {
                // we do not apply expansion to components
                Own<Component> cmp = clone(n);
                program.addComponent(std::move(cmp));

            } else if (ComponentInit* n = as<ComponentInit>(item)) {
                // we do not apply expansion to components init
                Own<ComponentInit> ini = clone(n);
                program.addInstantiation(std::move(ini));

            } else if (Pragma* n = as<Pragma>(item)) {
                // TODO
                Own<Pragma> pr = clone(n);
                program.addPragma(std::move(pr));

            } else if (FunctorDeclaration* n = as<FunctorDeclaration>(item)) {
                Own<FunctorDeclaration> fd = clone(n);
                expandFunctorDeclaration(*fd, inst);
                program.addFunctorDeclaration(std::move(fd));

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
        if (AlgebraicDataType* adt = as<AlgebraicDataType>(ty)) {
            for (BranchType* br : adt->getBranches()) {
                QualifiedName qname(parent.name);
                qname.append(br->getBranchName().toString());
                br->setBranchName(qname);
            }
        }
        PathExpander vis(this, parent);
        visit(ty, vis);
    }

    void expandDirective(Directive& dir, ModuleInstance& parent) {
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
                const DefId def = res.toDefId();
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
    PathResult resolveQualifiedName(
            const QualifiedName& qname, const NS ns, const SrcLocation& loc, ModuleInstance& parent) {
        return resolvePath(qname.getQualifiers(), ns, loc, parent);
    }

    PathResult resolvePath(
            span<const std::string> segments, const NS ns, const SrcLocation& loc, ModuleInstance& parent) {
        if (segments.empty()) {
            Diagnostic err(Diagnostic::Type::ERROR, DiagnosticMessage("empty path", loc));
            tu.getErrorReport().addDiagnostic(err);
            return Failed{};
        }

        ModuleInstance* mod = nullptr;

        for (std::size_t i = 0; i < segments.size(); ++i) {
            const bool isLast = (i + 1 == segments.size());
            const NS segmentNS = isLast ? ns : NS::Type;
            const Ident segmentIdent = Ident::from(segments[i]);

            std::optional<NameBinding*> binding = [&]() {
                if (mod == nullptr) {
                    return resolveIdentInLexicalScope(parent, segmentIdent, segmentNS);
                } else {
                    return resolveIdentInModule(*mod, segmentIdent, segmentNS);
                }
            }();

            if (!binding) {
                Diagnostic err(
                        Diagnostic::Type::ERROR, DiagnosticMessage("Cannot resolve " + segments[i], loc));
                tu.getErrorReport().addDiagnostic(err);
                return Failed{};
            } else if (std::optional<ModuleInstance*> nextMod = (*binding)->mod()) {
                mod = *nextMod;
            } else {
                Res res = (*binding)->res();
                if (isLast) {
                    return res;
                } else if (i == 0 && std::holds_alternative<Def>(res) &&
                           std::get<Def>(res).kind == DefKind::Init) {
                    return ComponentPath{QualifiedName(segments)};
                } else {
                    Diagnostic err(
                            Diagnostic::Type::ERROR, DiagnosticMessage("Cannot resolve " + segments[i], loc));
                    tu.getErrorReport().addDiagnostic(err);
                    return Failed{};
                }
            }
        }

        return Module{mod};
    }

    std::optional<NameBinding*> resolveIdentInLexicalScope(ModuleInstance& parent, Ident ident, const NS ns) {
        /// search recusively in scopes
        ModuleInstance* mod = &parent;
        while (mod) {
            if (std::optional<NameBinding*> binding = resolveIdentInModule(*mod, ident, ns)) {
                return binding;
            }
            mod = mod->parent;
        }

        const auto& sym = symtable.decode(ident.idx);
        if (sym == "number") {
            return numberNameBinding;
        }
        if (sym == "unsigned") {
            return unsignedNameBinding;
        }
        if (sym == "float") {
            return floatNameBinding;
        }
        if (sym == "symbol") {
            return symbolNameBinding;
        }

        return std::nullopt;
    }

    /// Try to resolve the name in the module but do not create a binding if there is no
    /// binding for that name yet.
    std::optional<NameBinding*> resolveIdentInModule(ModuleInstance& mod, Ident ident, const NS ns) {
        BindingKey key{ident, ns};
        auto it = mod.resolutions.find(key);
        if (it == mod.resolutions.end()) {
            return std::nullopt;
        }
        return it->second.binding;
    }

    /// Allocate a fresh `NameBinding`
    NameBinding* allocNameBinding(NameBindingKind kind, SrcLocation loc) {
        nameBindings.emplace(NameBinding{kind, loc});
        return &nameBindings.back();
    }
};

Res NameBinding::res() const {
    if (std::holds_alternative<Res>(kind)) {
        return std::get<Res>(kind);
    } else if (std::holds_alternative<Module>(kind)) {
        return Def{DefKind::Mod, std::get<Module>(kind).mod->def};
    } else if (std::holds_alternative<Import*>(kind)) {
        return Res{Err{}};
    } else {
        throw std::runtime_error("res()");
    }
}

/// return the namespace associated to the definition kind
NS toNS(const DefKind kind) {
    switch (kind) {
        case DefKind::Branch: return NS::Branch;
        case DefKind::Comp: return NS::Comp;
        case DefKind::Init: return NS::Type;
        case DefKind::UFun: return NS::UFun;
        case DefKind::Mod: return NS::Type;
        case DefKind::Rel: return NS::Value;
        case DefKind::Type: return NS::Type;
        default: throw "unexpected";
    }
}

}  // namespace

bool ModuleInstantiationTransformer::transform(TranslationUnit& tu) {
    TransformerImpl impl{tu};
    return impl.transform();
}

void PathExpander::visitPath(const QualifiedName& qname, NS ns, const SrcLocation& loc,
        const std::function<void(const QualifiedName&)>& updater) {
    updater(t->expandPath(qname, ns, loc, parent));
}

}  // namespace souffle::ast::transform
