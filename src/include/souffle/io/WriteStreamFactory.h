#pragma once

#include "souffle/io/WriteAllInterface.h"

#include <map>
#include <string>

namespace souffle {

class WriteStreamFactory {
public:
    virtual Own<WriteAllInterface> getWriter(const std::map<std::string, std::string>& rwOperation,
            const SymbolTable& symbolTable, const RecordTable& recordTable) = 0;

    virtual const std::string& getName() const = 0;
    virtual ~WriteStreamFactory() = default;
};

}
