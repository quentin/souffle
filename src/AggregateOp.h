/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AggregateOp.h
 *
 * Defines aggregation operators for AST and RAM
 *
 ***********************************************************************/

#pragma once

#include "souffle/TypeAttribute.h"
#include "souffle/utility/MiscUtil.h"
#include <cstdint>
#include <ostream>
#include <utility>

namespace souffle {

/** Types of aggregation functions */
enum class AggregateOp {
    MAX,
    MIN,
    SUM,

    FMAX,
    FMIN,
    FSUM,
    MEAN,

    UMAX,
    UMIN,
    USUM,

    CONCAT,
    COUNT,
    STRICTCONCAT,

    RANK,
    ARANK,
    FRANK,
    RRANK,
    SRANK,
    URANK,
};

inline std::ostream& operator<<(std::ostream& os, AggregateOp op) {
    switch (op) {
        case AggregateOp::CONCAT: return os << "concat";
        case AggregateOp::STRICTCONCAT: return os << "strictconcat";

        case AggregateOp::COUNT: return os << "count";

        case AggregateOp::MEAN: return os << "mean";

        case AggregateOp::MAX:
        case AggregateOp::UMAX:
        case AggregateOp::FMAX: return os << "max";

        case AggregateOp::MIN:
        case AggregateOp::UMIN:
        case AggregateOp::FMIN: return os << "min";

        case AggregateOp::SUM:
        case AggregateOp::USUM:
        case AggregateOp::FSUM: return os << "sum";

        case AggregateOp::RANK:
        case AggregateOp::ARANK:
        case AggregateOp::FRANK:
        case AggregateOp::RRANK:
        case AggregateOp::SRANK:
        case AggregateOp::URANK:
                                return os << "rank";
    }

    UNREACHABLE_BAD_CASE_ANALYSIS();
}

// `[min, max]` # of arguments for each function
inline std::pair<uint8_t, uint8_t> aggregateArity(AggregateOp op) {
    switch (op) {
        case AggregateOp::COUNT: return {0, 0};

        case AggregateOp::CONCAT:
        case AggregateOp::STRICTCONCAT: return {1, 2};

        case AggregateOp::FMAX:
        case AggregateOp::FMIN:
        case AggregateOp::FSUM:
        case AggregateOp::MAX:
        case AggregateOp::MEAN:
        case AggregateOp::MIN:
        case AggregateOp::SUM:
        case AggregateOp::UMAX:
        case AggregateOp::UMIN:
        case AggregateOp::USUM: return {1, 1};

        case AggregateOp::RANK:
        case AggregateOp::ARANK:
        case AggregateOp::FRANK:
        case AggregateOp::RRANK:
        case AggregateOp::SRANK:
        case AggregateOp::URANK: return {2, 2};
    }

    UNREACHABLE_BAD_CASE_ANALYSIS();
}

/**
 * Get return type of the aggregate.
 */
inline TypeAttribute getTypeAttributeAggregate(const AggregateOp op) {
    switch (op) {
        case AggregateOp::COUNT:
        case AggregateOp::MAX:
        case AggregateOp::MIN:
        case AggregateOp::SUM: return TypeAttribute::Signed;

        case AggregateOp::MEAN:
        case AggregateOp::FMAX:
        case AggregateOp::FMIN:
        case AggregateOp::FSUM: return TypeAttribute::Float;

        case AggregateOp::UMAX:
        case AggregateOp::UMIN:
        case AggregateOp::USUM: return TypeAttribute::Unsigned;

        case AggregateOp::CONCAT:
        case AggregateOp::STRICTCONCAT: return TypeAttribute::Symbol;

        case AggregateOp::RANK: return TypeAttribute::Signed;
        case AggregateOp::ARANK: return TypeAttribute::ADT;
        case AggregateOp::FRANK: return TypeAttribute::Float;
        case AggregateOp::RRANK: return TypeAttribute::Record;
        case AggregateOp::SRANK: return TypeAttribute::Symbol;
        case AggregateOp::URANK: return TypeAttribute::Unsigned;
    }

    UNREACHABLE_BAD_CASE_ANALYSIS();
}

inline bool isOverloadedAggregator(const AggregateOp op) {
    switch (op) {
        case AggregateOp::MAX:
        case AggregateOp::MIN:
        case AggregateOp::RANK:
        case AggregateOp::SUM: return true;

        case AggregateOp::COUNT:
        case AggregateOp::MEAN:
        case AggregateOp::CONCAT:
        case AggregateOp::STRICTCONCAT:
        case AggregateOp::ARANK:
        case AggregateOp::FRANK:
        case AggregateOp::RRANK:
        case AggregateOp::SRANK:
        case AggregateOp::URANK: return false;

        default: return false;
    }
}

/**
 * Convert aggregator to a give type.
 * Eg. sum, float → fsum.
 **/
inline AggregateOp convertOverloadedAggregator(const AggregateOp op, const TypeAttribute type) {
#define CASE_NUMERIC(op)                                                \
    case AggregateOp::op:                                               \
        if (type == TypeAttribute::Signed) return AggregateOp::op;      \
        if (type == TypeAttribute::Unsigned) return AggregateOp::U##op; \
        if (type == TypeAttribute::Float) return AggregateOp::F##op;    \
        fatal("invalid overload");

    switch (op) {
        default:
            fatal("agg op is not overloadable");

            // clang-format off
        CASE_NUMERIC(MAX)
        CASE_NUMERIC(MIN)
        CASE_NUMERIC(SUM)
            // clang-format on
        case AggregateOp::RANK:
          if (type == TypeAttribute::Signed) return AggregateOp::RANK;
          if (type == TypeAttribute::ADT) return AggregateOp::ARANK;
          if (type == TypeAttribute::Float) return AggregateOp::FRANK;
          if (type == TypeAttribute::Record) return AggregateOp::RRANK;
          if (type == TypeAttribute::Symbol) return AggregateOp::SRANK;
          if (type == TypeAttribute::Unsigned) return AggregateOp::URANK;
          fatal("invalid overload");
    }

#undef CASE_NUMERIC
}

inline bool isAnyConcatAggregator(const AggregateOp op) {
    return (op == AggregateOp::CONCAT 
            || op == AggregateOp::STRICTCONCAT);
}

inline bool isAnyRankAggregator(const AggregateOp op) {
    return (op == AggregateOp::RANK || op == AggregateOp::ARANK || op == AggregateOp::FRANK ||
            op == AggregateOp::RRANK || op == AggregateOp::SRANK || op == AggregateOp::URANK);
}

}  // namespace souffle
