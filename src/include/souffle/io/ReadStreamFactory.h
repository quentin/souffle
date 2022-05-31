#pragma once

#include "souffle/io/ReadAllInterface.h"

#include <map>
#include <string>

namespace souffle {

class ReadStreamFactory {
public:
    virtual Own<ReadAllInterface> getReader(
            const std::map<std::string, std::string>&, SymbolTable&, RecordTable&) = 0;
    virtual const std::string& getName() const = 0;
    virtual ~ReadStreamFactory() = default;
};

}
