#include "souffle/SouffleFunctor.h"

#include <string>

using namespace std;
using namespace souffle;

extern "C" {
RamDomain myrange(souffle::SymbolTable*, souffle::RecordTable*, const std::function<void()>& Fn,
        const RamDomain* args, RamDomain* tuple) {
    const RamDomain from = args[0];
    const RamUnsigned count = ramBitCast<RamUnsigned>(args[1]);

    tuple[0] = from;
    for (RamUnsigned i = 0; i < count; ++i) {
        Fn();
        ++tuple[0];
    }

    return (RamDomain)count;
}

RamDomain myrange_step_f(souffle::SymbolTable*, souffle::RecordTable*, const std::function<void()>& Fn,
        const RamDomain* args, RamDomain* tuple) {
    const RamFloat from = ramBitCast<RamFloat>(args[0]);
    const RamUnsigned count = ramBitCast<RamUnsigned>(args[1]);
    const RamFloat step = ramBitCast<RamFloat>(args[2]);

    RamFloat v = from;
    for (RamUnsigned i = 0; i < count; ++i) {
        tuple[0] = ramBitCast(v);
        Fn();
        v += step;
    }

    return (RamDomain)count;
}

RamDomain safe_div_f(souffle::SymbolTable*, souffle::RecordTable*, const std::function<void()>& Fn,
        const RamDomain* args, RamDomain* tuple) {
    const RamFloat numerator = ramBitCast<RamFloat>(args[0]);
    const RamFloat denominator = ramBitCast<RamFloat>(args[1]);

    if (denominator == 0.0) {
        return 0;
    } else {
        tuple[0] = ramBitCast(numerator / denominator);
        Fn();
        return 1;
    }
}

RamDomain to_str_f(souffle::SymbolTable* symbolTable, souffle::RecordTable* recordTable,
        const std::function<void()>& Fn, const RamDomain* args, RamDomain* tuple) {
    const RamFloat v = ramBitCast<RamFloat>(args[0]);
    const RamSigned precision = ramBitCast<RamSigned>(args[1]);

    if (precision < 0) {
        return 0;
    }

    char buffer[100];
    int cx = snprintf(buffer, 100, "%.*e", (int)precision, (double)v);
    if (cx >= 0 && cx < 100) {
        tuple[0] = recordTable->pack({symbolTable->encode(buffer), cx});
        Fn();
        return 1;
    } else {
        return 0;
    }
}

RamDomain index_of(souffle::SymbolTable* symbolTable, souffle::RecordTable*, const std::function<void()>& Fn,
        const RamDomain* args, RamDomain* tuple) {
    const RamDomain sourceRef = args[0];
    const RamDomain patternRef = args[1];

    const std::string& source = symbolTable->decode(sourceRef);
    const std::string& pattern = symbolTable->decode(patternRef);

    if (source.empty() || pattern.empty()) {
        return 0;
    }

    std::size_t count = 0;
    std::size_t from = 0;
    while (from < source.size()) {
        std::size_t at = source.find(pattern, from);
        if (at == std::string::npos) {
            break;
        }
        from = at + 1;
        tuple[0] = ramBitCast((RamUnsigned)at);
        Fn();
    }

    return ramBitCast((RamUnsigned)count);
}
}
