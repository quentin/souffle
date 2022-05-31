#pragma once

#include "souffle/SouffleInterface.h"

namespace souffle {

class WriteAllInterface {
public:
  virtual ~WriteAllInterface() = default;

  virtual void writeAll(const RelationBase& relation) = 0;
};

}  // namespace souffle
