/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2026, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

#include "interpreter/Util.h"
#include "souffle/datastructure/DynamicBTree.h"
#include <algorithm>
#include <array>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <iostream>
#include <numeric>
#include <random>
#include <set>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <vector>

namespace {

using Clock = std::chrono::steady_clock;

enum class ComparisonProfile { early, late };

const char* comparisonProfileName(ComparisonProfile profile) {
    return profile == ComparisonProfile::early ? "early_discriminator" : "late_discriminator";
}

struct DynamicTupleLess {
    explicit DynamicTupleLess(std::size_t arity) : comparator(arity) {}

    bool operator()(const souffle::DynamicTuple& lhs, const souffle::DynamicTuple& rhs) const {
        return comparator.less(lhs, rhs);
    }

    souffle::DynamicTupleComparator comparator;
};

struct Timings {
    double insertMs;
    double lookupMs;
    double scanMs;
    std::size_t hits;
    std::size_t size;
    std::uint64_t checksum;
};

struct Workload {
    std::size_t attempts;
    std::size_t duplicatePercent;
    ComparisonProfile comparisonProfile;
    std::size_t distinctCount;
    std::vector<std::size_t> rowIds;
};

double elapsedMilliseconds(Clock::time_point start, Clock::time_point finish) {
    return std::chrono::duration<double, std::milli>(finish - start).count();
}

Workload makeWorkload(std::size_t count, std::size_t duplicatePercent, ComparisonProfile comparisonProfile) {
    if (count == 0 || duplicatePercent > 90) {
        throw std::invalid_argument("tuple count must be positive and duplicate rate at most 90 percent");
    }
    const auto distinctCount = std::max<std::size_t>(1, (count * (100 - duplicatePercent) + 50) / 100);
    std::vector<std::size_t> rowIds(count);
    for (std::size_t i = 0; i < distinctCount; ++i) {
        rowIds[i] = i;
    }
    for (std::size_t i = distinctCount; i < count; ++i) {
        rowIds[i] = (i - distinctCount) % distinctCount;
    }
    std::mt19937 generator(20260417);
    std::shuffle(rowIds.begin(), rowIds.end(), generator);
    return {count, duplicatePercent, comparisonProfile, distinctCount, std::move(rowIds)};
}

souffle::RamDomain tupleValue(
        std::size_t arity, std::size_t row, std::size_t column, ComparisonProfile comparisonProfile) {
    if (comparisonProfile == ComparisonProfile::early) {
        return static_cast<souffle::RamDomain>(column == 0 ? row : (row * 17 + column * 31) % 1000003);
    }
    return static_cast<souffle::RamDomain>(column + 1 == arity ? row : 0);
}

template <typename Tuple>
void fillTuple(Tuple& tuple, std::size_t arity, std::size_t row, ComparisonProfile comparisonProfile) {
    for (std::size_t column = 0; column < arity; ++column) {
        tuple[column] = tupleValue(arity, row, column, comparisonProfile);
    }
}

template <typename Key>
std::vector<Key> makeRows(std::size_t arity, const Workload& workload) {
    std::vector<Key> distinct(workload.distinctCount);
    for (std::size_t row = 0; row < distinct.size(); ++row) {
        if constexpr (std::is_same_v<Key, souffle::DynamicTuple>) {
            distinct[row].resize(arity);
        }
        fillTuple(distinct[row], arity, row, workload.comparisonProfile);
    }

    std::vector<Key> rows;
    rows.reserve(workload.attempts);
    for (const auto rowId : workload.rowIds) {
        rows.push_back(distinct[rowId]);
    }
    return rows;
}

std::uint64_t expectedChecksum(
        std::size_t arity, std::size_t distinctCount, ComparisonProfile comparisonProfile) {
    std::uint64_t checksum = 0;
    std::vector<souffle::RamDomain> tuple(arity);
    for (std::size_t row = 0; row < distinctCount; ++row) {
        fillTuple(tuple, arity, row, comparisonProfile);
        for (const auto value : tuple) {
            checksum += static_cast<std::uint64_t>(value);
        }
    }
    return checksum;
}

template <typename Set, typename Key>
Timings runSetOnce(
        const std::vector<Key>& rows, std::size_t expectedSize, std::uint64_t expectedSum, Set set) {
    auto start = Clock::now();
    for (const auto& row : rows) {
        set.insert(row);
    }
    auto finish = Clock::now();
    const auto insertMs = elapsedMilliseconds(start, finish);

    std::size_t hits = 0;
    start = Clock::now();
    for (const auto& row : rows) {
        hits += set.find(row) != set.end();
    }
    finish = Clock::now();
    const auto lookupMs = elapsedMilliseconds(start, finish);

    std::uint64_t checksum = 0;
    start = Clock::now();
    for (const auto& row : set) {
        for (std::size_t column = 0; column < row.size(); ++column) {
            checksum += static_cast<std::uint64_t>(row[column]);
        }
    }
    finish = Clock::now();
    const auto scanMs = elapsedMilliseconds(start, finish);

    if (hits != rows.size() || set.size() != expectedSize || checksum != expectedSum) {
        throw std::runtime_error("benchmark set returned an unexpected result");
    }
    return {insertMs, lookupMs, scanMs, hits, set.size(), checksum};
}

Timings runSortedVectorOnce(
        const std::vector<souffle::DynamicTuple>& rows, std::size_t expectedSize, std::uint64_t expectedSum) {
    std::vector<souffle::DynamicTuple> sorted;
    auto start = Clock::now();
    sorted.reserve(rows.size());
    sorted.insert(sorted.end(), rows.begin(), rows.end());
    std::sort(sorted.begin(), sorted.end());
    sorted.erase(std::unique(sorted.begin(), sorted.end()), sorted.end());
    auto finish = Clock::now();
    const auto insertMs = elapsedMilliseconds(start, finish);

    std::size_t hits = 0;
    start = Clock::now();
    for (const auto& row : rows) {
        hits += std::binary_search(sorted.begin(), sorted.end(), row);
    }
    finish = Clock::now();
    const auto lookupMs = elapsedMilliseconds(start, finish);

    std::uint64_t checksum = 0;
    start = Clock::now();
    for (const auto& row : sorted) {
        for (const auto value : row) {
            checksum += static_cast<std::uint64_t>(value);
        }
    }
    finish = Clock::now();
    const auto scanMs = elapsedMilliseconds(start, finish);

    if (hits != rows.size() || sorted.size() != expectedSize || checksum != expectedSum) {
        throw std::runtime_error("sorted-vector benchmark returned an unexpected result");
    }
    return {insertMs, lookupMs, scanMs, hits, sorted.size(), checksum};
}

int compareFlatRow(const std::vector<souffle::RamDomain>& flat, std::size_t flatRow,
        const souffle::DynamicTuple& tuple) {
    const auto arity = tuple.size();
    const auto offset = flatRow * arity;
    for (std::size_t column = 0; column < arity; ++column) {
        if (flat[offset + column] != tuple[column]) {
            return flat[offset + column] < tuple[column] ? -1 : 1;
        }
    }
    return 0;
}

int compareFlatRows(
        const std::vector<souffle::RamDomain>& flat, std::size_t lhs, std::size_t rhs, std::size_t arity) {
    const auto lhsOffset = lhs * arity;
    const auto rhsOffset = rhs * arity;
    for (std::size_t column = 0; column < arity; ++column) {
        if (flat[lhsOffset + column] != flat[rhsOffset + column]) {
            return flat[lhsOffset + column] < flat[rhsOffset + column] ? -1 : 1;
        }
    }
    return 0;
}

Timings runFlatVectorOnce(std::size_t arity, const Workload& workload, std::uint64_t expectedSum) {
    std::vector<souffle::RamDomain> input;
    std::vector<std::size_t> order;
    auto start = Clock::now();
    input.resize(workload.attempts * arity);
    for (std::size_t row = 0; row < workload.attempts; ++row) {
        const auto rowId = workload.rowIds[row];
        for (std::size_t column = 0; column < arity; ++column) {
            input[row * arity + column] = tupleValue(arity, rowId, column, workload.comparisonProfile);
        }
    }
    order.resize(workload.attempts);
    std::iota(order.begin(), order.end(), std::size_t{0});
    std::sort(order.begin(), order.end(),
            [&](std::size_t lhs, std::size_t rhs) { return compareFlatRows(input, lhs, rhs, arity) < 0; });
    std::size_t distinctCount = order.empty() ? 0 : 1;
    for (std::size_t i = 1; i < order.size(); ++i) {
        distinctCount += compareFlatRows(input, order[i - 1], order[i], arity) != 0;
    }

    std::vector<souffle::RamDomain> temporaryRow(arity);
    for (std::size_t startRow = 0; startRow < order.size(); ++startRow) {
        if (order[startRow] == startRow) {
            continue;
        }
        std::copy_n(input.begin() + startRow * arity, arity, temporaryRow.begin());
        auto currentRow = startRow;
        while (order[currentRow] != startRow) {
            const auto nextRow = order[currentRow];
            std::copy_n(input.begin() + nextRow * arity, arity, input.begin() + currentRow * arity);
            order[currentRow] = currentRow;
            currentRow = nextRow;
        }
        std::copy(temporaryRow.begin(), temporaryRow.end(), input.begin() + currentRow * arity);
        order[currentRow] = currentRow;
    }
    std::size_t uniqueRows = 0;
    for (std::size_t row = 0; row < workload.attempts; ++row) {
        if (uniqueRows == 0 || compareFlatRows(input, uniqueRows - 1, row, arity) != 0) {
            if (uniqueRows != row) {
                std::copy_n(input.begin() + row * arity, arity, input.begin() + uniqueRows * arity);
            }
            ++uniqueRows;
        }
    }
    if (uniqueRows != distinctCount) {
        throw std::runtime_error("flat-vector benchmark deduplicated an unexpected number of tuples");
    }
    input.resize(distinctCount * arity);
    auto finish = Clock::now();
    const auto insertMs = elapsedMilliseconds(start, finish);

    std::size_t hits = 0;
    start = Clock::now();
    souffle::DynamicTuple query(arity);
    for (const auto rowId : workload.rowIds) {
        fillTuple(query, arity, rowId, workload.comparisonProfile);
        std::size_t first = 0;
        std::size_t last = input.size() / arity;
        while (first < last) {
            const auto middle = first + (last - first) / 2;
            const auto comparison = compareFlatRow(input, middle, query);
            if (comparison < 0) {
                first = middle + 1;
            } else {
                last = middle;
            }
        }
        hits += first < input.size() / arity && compareFlatRow(input, first, query) == 0;
    }
    finish = Clock::now();
    const auto lookupMs = elapsedMilliseconds(start, finish);

    std::uint64_t checksum = 0;
    start = Clock::now();
    for (const auto value : input) {
        checksum += static_cast<std::uint64_t>(value);
    }
    finish = Clock::now();
    const auto scanMs = elapsedMilliseconds(start, finish);

    if (hits != workload.attempts || distinctCount != workload.distinctCount || checksum != expectedSum) {
        throw std::runtime_error(
                "flat-vector benchmark returned an unexpected result: hits=" + std::to_string(hits) +
                ", distinct=" + std::to_string(distinctCount) + ", checksum=" + std::to_string(checksum) +
                ", expected checksum=" + std::to_string(expectedSum) + ", arity=" + std::to_string(arity) +
                ", duplicate rate=" + std::to_string(workload.duplicatePercent) +
                ", profile=" + comparisonProfileName(workload.comparisonProfile));
    }
    return {insertMs, lookupMs, scanMs, hits, distinctCount, checksum};
}

template <typename Runner>
Timings medianTimings(Runner run, std::size_t repetitions) {
    std::vector<Timings> samples;
    samples.reserve(repetitions);
    for (std::size_t i = 0; i < repetitions; ++i) {
        samples.push_back(run());
    }
    auto median = [&](auto member) {
        std::vector<double> values;
        values.reserve(samples.size());
        for (const auto& sample : samples) {
            values.push_back(sample.*member);
        }
        std::sort(values.begin(), values.end());
        return values[values.size() / 2];
    };
    return {median(&Timings::insertMs), median(&Timings::lookupMs), median(&Timings::scanMs),
            samples.front().hits, samples.front().size, samples.front().checksum};
}

void printRow(
        const char* implementation, std::size_t arity, const Workload& workload, const Timings& timings) {
    std::cout << implementation << ',' << arity << ',' << workload.attempts << ','
              << workload.duplicatePercent << ',' << comparisonProfileName(workload.comparisonProfile) << ','
              << timings.size << ',' << timings.insertMs << ',' << timings.lookupMs << ',' << timings.scanMs
              << ',' << timings.hits << ',' << timings.checksum << '\n';
}

void benchmarkDynamic(std::size_t arity, const Workload& workload, std::size_t repetitions) {
    const auto expectedSum = expectedChecksum(arity, workload.distinctCount, workload.comparisonProfile);
    Timings btree;
    Timings stdSet;
    Timings sortedVector;
    {
        const auto rows = makeRows<souffle::DynamicTuple>(arity, workload);
        btree = medianTimings(
                [&]() {
                    return runSetOnce(
                            rows, workload.distinctCount, expectedSum, souffle::DynamicBTreeSet(arity));
                },
                repetitions);
        using Set = std::set<souffle::DynamicTuple, DynamicTupleLess>;
        stdSet = medianTimings(
                [&]() {
                    return runSetOnce(
                            rows, workload.distinctCount, expectedSum, Set(DynamicTupleLess(arity)));
                },
                repetitions);
        sortedVector = medianTimings(
                [&]() { return runSortedVectorOnce(rows, workload.distinctCount, expectedSum); },
                repetitions);
    }
    const auto flatVector =
            medianTimings([&]() { return runFlatVectorOnce(arity, workload, expectedSum); }, repetitions);
    printRow("dynamic_btree", arity, workload, btree);
    printRow("dynamic_std_set", arity, workload, stdSet);
    printRow("sorted_vector", arity, workload, sortedVector);
    printRow("flat_vector", arity, workload, flatVector);
}

template <std::size_t Arity>
void benchmarkStatic(const Workload& workload, std::size_t repetitions) {
    using Key = souffle::Tuple<souffle::RamDomain, Arity>;
    using Set = souffle::interpreter::Btree<Arity, 0>;
    const auto rows = makeRows<Key>(Arity, workload);
    const auto expectedSum = expectedChecksum(Arity, workload.distinctCount, workload.comparisonProfile);
    const auto btree = medianTimings(
            [&]() { return runSetOnce(rows, workload.distinctCount, expectedSum, Set{}); }, repetitions);
    using StdSet = std::set<Key>;
    const auto stdSet = medianTimings(
            [&]() { return runSetOnce(rows, workload.distinctCount, expectedSum, StdSet{}); }, repetitions);
    printRow("static_btree", Arity, workload, btree);
    printRow("static_std_set", Arity, workload, stdSet);
}

void benchmarkStaticByArity(std::size_t arity, const Workload& workload, std::size_t repetitions) {
    switch (arity) {
        case 1: benchmarkStatic<1>(workload, repetitions); break;
        case 2: benchmarkStatic<2>(workload, repetitions); break;
        case 3: benchmarkStatic<3>(workload, repetitions); break;
        case 4: benchmarkStatic<4>(workload, repetitions); break;
        case 5: benchmarkStatic<5>(workload, repetitions); break;
        case 6: benchmarkStatic<6>(workload, repetitions); break;
        case 7: benchmarkStatic<7>(workload, repetitions); break;
        case 8: benchmarkStatic<8>(workload, repetitions); break;
        case 9: benchmarkStatic<9>(workload, repetitions); break;
        case 10: benchmarkStatic<10>(workload, repetitions); break;
        case 11: benchmarkStatic<11>(workload, repetitions); break;
        case 12: benchmarkStatic<12>(workload, repetitions); break;
        case 13: benchmarkStatic<13>(workload, repetitions); break;
        case 14: benchmarkStatic<14>(workload, repetitions); break;
        case 15: benchmarkStatic<15>(workload, repetitions); break;
        case 16: benchmarkStatic<16>(workload, repetitions); break;
        case 17: benchmarkStatic<17>(workload, repetitions); break;
        case 18: benchmarkStatic<18>(workload, repetitions); break;
        case 19: benchmarkStatic<19>(workload, repetitions); break;
        case 20: benchmarkStatic<20>(workload, repetitions); break;
        case 21: benchmarkStatic<21>(workload, repetitions); break;
        case 22: benchmarkStatic<22>(workload, repetitions); break;
        default: throw std::invalid_argument("no static interpreter B-tree benchmark for this arity");
    }
}

}  // namespace

int main(int argc, char** argv) {
    const std::size_t count = argc > 1 ? std::stoull(argv[1]) : 25000;
    constexpr std::size_t repetitions = 5;
    constexpr std::array<std::size_t, 3> duplicateRates{0, 50, 90};
    constexpr std::array<ComparisonProfile, 2> comparisonProfiles{
            ComparisonProfile::early, ComparisonProfile::late};
    constexpr std::array<std::size_t, 23> arities{
            1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 32};

    std::cout
            << "implementation,arity,attempts,duplicate_percent,comparison_profile,distinct_tuples,insert_ms,"
               "lookup_ms,scan_ms,hits,checksum\n";
    for (const auto duplicateRate : duplicateRates) {
        for (const auto comparisonProfile : comparisonProfiles) {
            const auto workload = makeWorkload(count, duplicateRate, comparisonProfile);
            for (const auto arity : arities) {
                benchmarkDynamic(arity, workload, repetitions);
                if (arity <= 22) {
                    benchmarkStaticByArity(arity, workload, repetitions);
                }
            }
        }
    }
}
