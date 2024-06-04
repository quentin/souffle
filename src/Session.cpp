#include "Session.h"

namespace souffle {
thread_local SessionGlobals* SESSION_GLOBALS = nullptr;

bool SessionGlobals::isSet() {
  return SESSION_GLOBALS != nullptr;
}

}  // namespace souffle
