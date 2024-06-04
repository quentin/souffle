#pragma once

#include "Global.h"
#include "ast/QualifiedName.h"

#include <cassert>
#include <functional>
#include <memory>

namespace souffle {

struct SessionGlobals;

/// Thread local storage for the current thread's Souffle session.
///
/// Do not access directly, use `SessionGlobals::with` instead.
extern thread_local SessionGlobals* SESSION_GLOBALS;

struct SessionGlobals {
  /// Set the current thread's session and call f.
  template <typename F>
  static decltype(auto) set(SessionGlobals s, F&& f) {
      assert(SESSION_GLOBALS == nullptr);
      auto finalize = [](SessionGlobals* obj) {
          SESSION_GLOBALS = nullptr;
          delete obj;
      };
      std::shared_ptr<SessionGlobals> instance(new SessionGlobals{s}, finalize);
      SESSION_GLOBALS = instance.get();
      return f();
  }

  /// Call f and pass the current thread's session.
  template <typename F>
  static decltype(auto) with(F&& f) {
      assert(SESSION_GLOBALS != nullptr);
      return f(*SESSION_GLOBALS);
  }

  /// Create a default session, set as the current thread's session and call f.
  template <typename F>
  static decltype(auto) createDefaultThen(F&& f) {
      Global glb;
      std::unique_ptr<ast::QNInterner> interner = ast::QNInterner::make();
      return SessionGlobals::set(SessionGlobals{glb, interner.get()}, std::forward<F>(f));
  }

  template <typename F>
  static decltype(auto) createDefaultThenWith(F&& f) {
      Global glb;
      std::unique_ptr<ast::QNInterner> interner = ast::QNInterner::make();
      return SessionGlobals::set(SessionGlobals{glb, interner.get()}, [&]() { return with(f); });
  }

  /// Return true if a session is currently set.
  static bool isSet();

  Global& glb;
  ast::QNInterner* const interner;
};
}  // namespace souffle
