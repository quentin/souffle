#!/usr/bin/env python3
"""Generate benchmark charts from dynamic_btree_benchmark CSV output."""

import argparse
import csv
import shutil
import subprocess
import tempfile
from collections import defaultdict
from pathlib import Path


IMPLEMENTATIONS = (
    ("dynamic_btree", "Dynamic B-tree", "#0072B2", 5),
    ("static_btree", "Static B-tree", "#D55E00", 7),
    ("dynamic_std_set", "Dynamic std::set", "#009E73", 9),
    ("static_std_set", "Static std::set", "#CC79A7", 11),
    ("sorted_vector", "Sorted vector", "#E69F00", 13),
    ("flat_vector", "Flat vector", "#56B4E9", 15),
)
OPERATIONS = (("insert_ms", "Insertion"), ("lookup_ms", "Lookup"), ("scan_ms", "Scan"))
DUPLICATE_RATES = (0, 50, 90)
PROFILES = (
    ("early_discriminator", "Early discriminator", ""),
    ("late_discriminator", "Late discriminator", "-late"),
)


def quote_gnuplot(value: str) -> str:
    return '"' + value.replace("\\", "\\\\").replace('"', '\\"') + '"'


def load_results(path: Path):
    with path.open(newline="", encoding="utf-8") as source:
        rows = list(csv.DictReader(source))
    if not rows:
        raise ValueError(f"no benchmark rows found in {path}")

    required = {
        "implementation",
        "arity",
        "attempts",
        "duplicate_percent",
        "comparison_profile",
        "distinct_tuples",
        "insert_ms",
        "lookup_ms",
        "scan_ms",
    }
    missing = required.difference(rows[0])
    if missing:
        raise ValueError(f"CSV is missing required columns: {', '.join(sorted(missing))}")
    profiles = {row["comparison_profile"] for row in rows}
    expected_profiles = {profile for profile, _, _ in PROFILES}
    if profiles != expected_profiles:
        raise ValueError(f"CSV must include both comparison profiles: {', '.join(sorted(expected_profiles))}")

    attempts = {int(row["attempts"]) for row in rows}
    if len(attempts) != 1:
        raise ValueError("all rows in the CSV must use the same attempt count")
    return rows, attempts.pop()


def write_data_files(rows, directory: Path):
    grouped = defaultdict(dict)
    for row in rows:
        key = (row["comparison_profile"], int(row["duplicate_percent"]), int(row["arity"]))
        grouped[key][row["implementation"]] = row

    data_files = {}
    connector_files = {}
    point_files = {}
    for profile, _, _ in PROFILES:
        for column, _ in OPERATIONS:
            for rate in DUPLICATE_RATES:
                path = directory / f"{profile}-{column[:-3]}-{rate}.dat"
                connector_path = directory / f"{profile}-{column[:-3]}-{rate}-connectors.dat"
                point_path = directory / f"{profile}-{column[:-3]}-{rate}-arity32.dat"
                with path.open("w", encoding="utf-8") as output:
                    output.write("# arity " + " ".join(item[0] for item in IMPLEMENTATIONS) + "\n")
                    for display_arity in range(1, 25):
                        arity = 32 if display_arity == 24 else display_arity
                        by_implementation = grouped.get((profile, rate, arity), {})
                        fields = [str(display_arity)]
                        for implementation, _, _, _ in IMPLEMENTATIONS:
                            result = by_implementation.get(implementation)
                            if display_arity == 24 and result is not None:
                                fields.append("NaN")
                            else:
                                fields.append(result[column] if result is not None else "NaN")
                        output.write(" ".join(fields) + "\n")

                connectors = []
                point_values = ["NaN"] * len(IMPLEMENTATIONS)
                by_implementation = grouped.get((profile, rate, 32), {})
                arity22 = grouped.get((profile, rate, 22), {})
                for index, (implementation, _, _, _) in enumerate(IMPLEMENTATIONS):
                    end = by_implementation.get(implementation)
                    start = arity22.get(implementation)
                    if end is None:
                        continue
                    point_values[index] = end[column]
                    if start is not None:
                        connectors.append(f"22 {start[column]}\n24 {end[column]}\n\n")
                connector_path.write_text("".join(connectors), encoding="utf-8")
                point_path.write_text("24 " + " ".join(point_values) + "\n", encoding="utf-8")
                data_files[(profile, column, rate)] = path
                connector_files[(profile, column, rate)] = connector_path
                point_files[(profile, column, rate)] = point_path
    return data_files, connector_files, point_files


def make_plot_script(rows, data_files, connector_files, point_files, output_directory: Path, attempts: int) -> str:
    lines = [
        'set terminal pngcairo size 1500,1650 enhanced font "DejaVu Sans,12"',
        "set xrange [1:25]",
        'set xtics ("2" 2, "4" 4, "6" 6, "8" 8, "10" 10, "12" 12, "14" 14, '
        '"16" 16, "18" 18, "20" 20, "22" 22, "32" 24)',
        'set grid ytics lc rgb "#dddddd"',
        "set key top left opaque",
        'set xlabel "Tuple arity"',
    ]
    lines.extend(
        [
            'set arrow 1 from 22, graph 0 to 22, graph 1 nohead dt 2 lw 1.5 lc rgb "#555555"',
            'set label 1 "static arity limit" at 22, graph 0.92 right tc rgb "#555555"',
            'set arrow 2 from 22.43, graph 0 to 22.48, graph 0.035 nohead lw 1.5 lc rgb "#333333"',
            'set arrow 3 from 22.52, graph 0 to 22.57, graph 0.035 nohead lw 1.5 lc rgb "#333333"',
        ]
    )

    suffix = f"{attempts // 1000}k" if attempts >= 1000 and attempts % 1000 == 0 else str(attempts)
    for profile, profile_title, filename_suffix in PROFILES:
        for column, operation in OPERATIONS:
            operation_name = column[:-3]
            output = output_directory / (
                f"dynamic-btree-{operation_name}{filename_suffix}-duplicates-{suffix}.png"
            )
            lines.extend(
                [
                    "set output " + quote_gnuplot(str(output.resolve())),
                    f'set multiplot layout 3,1 title "{profile_title}: {operation} — '
                    f'{attempts:,} attempts, median of 5 runs" font ",16"',
                    'set ylabel "Time (ms)"',
                ]
            )
            for rate in DUPLICATE_RATES:
                representative = next(
                    row
                    for row in rows
                    if row["comparison_profile"] == profile and int(row["duplicate_percent"]) == rate
                )
                distinct = int(representative["distinct_tuples"])
                lines.append(f'set title "Duplicate attempts: {rate}% ({distinct:,} distinct tuples)"')
                data_path = data_files[(profile, column, rate)]
                clauses = []
                for index, (_, label, color, point) in enumerate(IMPLEMENTATIONS):
                    source = quote_gnuplot(str(data_path)) if index == 0 else '""'
                    title = f"title {quote_gnuplot(label)}" if rate == DUPLICATE_RATES[0] else "notitle"
                    clauses.append(
                        f"{source} using 1:{index + 2} with linespoints lw 2 pt {point} ps 0.6 "
                        f"lc rgb {quote_gnuplot(color)} {title}"
                    )
                connector_path = quote_gnuplot(str(connector_files[(profile, column, rate)]))
                clauses.append(
                    f"{connector_path} using 1:2 with lines dt 2 lw 1.5 lc rgb \"#555555\" notitle"
                )
                point_path = quote_gnuplot(str(point_files[(profile, column, rate)]))
                for index, (_, _, color, point) in enumerate(IMPLEMENTATIONS):
                    clauses.append(
                        f"{point_path} using 1:{index + 2} with points pt {point} ps 0.8 "
                        f"lc rgb {quote_gnuplot(color)} notitle"
                    )
                lines.append("plot " + ", \\\n".join(clauses))
            lines.append("unset multiplot")
    return "\n".join(lines) + "\n"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("csv", type=Path, help="CSV emitted by dynamic_btree_benchmark")
    parser.add_argument(
        "--output-dir",
        type=Path,
        help="directory for PNG charts (default: the CSV's directory)",
    )
    args = parser.parse_args()

    gnuplot = shutil.which("gnuplot")
    if gnuplot is None:
        parser.error("gnuplot is required to render charts")
    rows, attempts = load_results(args.csv)
    output_directory = args.output_dir or args.csv.parent
    output_directory.mkdir(parents=True, exist_ok=True)

    with tempfile.TemporaryDirectory(prefix="dynamic-btree-plots-") as temporary:
        temporary_directory = Path(temporary)
        data_files, connector_files, point_files = write_data_files(rows, temporary_directory)
        plot_script = temporary_directory / "benchmark.gp"
        plot_script.write_text(
            make_plot_script(rows, data_files, connector_files, point_files, output_directory, attempts),
            encoding="utf-8",
        )
        subprocess.run([gnuplot, str(plot_script)], check=True)

    suffix = f"{attempts // 1000}k" if attempts >= 1000 and attempts % 1000 == 0 else str(attempts)
    for _, _, filename_suffix in PROFILES:
        for column, _ in OPERATIONS:
            operation_name = column[:-3]
            print(
                output_directory
                / f"dynamic-btree-{operation_name}{filename_suffix}-duplicates-{suffix}.png"
            )


if __name__ == "__main__":
    main()
