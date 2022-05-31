#pragma once

#include "souffle/SouffleInterface.h"

namespace souffle {

class ReadAllInterface {
public:
    virtual ~ReadAllInterface() = default;

    virtual void readAll(RelationBase& relation) = 0;
};

}  // namespace souffle
