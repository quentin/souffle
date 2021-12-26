/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2021, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

#include "MainDriver.h"

using namespace souffle;

namespace {

bool processArgs(Global& glb, int argc, char** argv) {
    /* have all to do with command line arguments in its own scope, as these are accessible through the global
     * configuration only */
    std::stringstream header;
    header << "============================================================================" << std::endl;
    header << "souffle -- A datalog engine." << std::endl;
    header << "Usage: souffle [OPTION] FILE." << std::endl;
    header << "----------------------------------------------------------------------------" << std::endl;
    header << "Options:" << std::endl;

    std::stringstream footer;
    footer << "----------------------------------------------------------------------------" << std::endl;
    footer << "Version: " << packageVersion() << "" << std::endl;
    footer << "----------------------------------------------------------------------------" << std::endl;
    footer << "Copyright (c) 2016-21 The Souffle Developers." << std::endl;
    footer << "Copyright (c) 2013-16 Oracle and/or its affiliates." << std::endl;
    footer << "All rights reserved." << std::endl;
    footer << "============================================================================" << std::endl;

    // command line options, the environment will be filled with the arguments passed to them, or
    // the empty string if they take none
    // main option, the datalog program itself, has an empty key
    std::vector<MainOption> options{{"", 0, "", "", false, ""},
            {"fact-dir", 'F', "DIR", ".", false, "Specify directory for fact files."},
            {"include-dir", 'I', "DIR", ".", true, "Specify directory for include files."},
            {"output-dir", 'D', "DIR", ".", false,
                    "Specify directory for output files. If <DIR> is `-` then stdout is used."},
            {"jobs", 'j', "N", "1", false,
                    "Run interpreter/compiler in parallel using N threads, N=auto for system "
                    "default."},
            {"compile", 'c', "", "", false,
                    "Generate C++ source code, compile to a binary executable, then run this "
                    "executable."},
            {"generate", 'g', "FILE", "", false,
                    "Generate C++ source code for the given Datalog program and write it to "
                    "<FILE>. If <FILE> is `-` then stdout is used."},
            {"inline-exclude", '\x7', "RELATIONS", "", false,
                    "Prevent the given relations from being inlined. Overrides any `inline` qualifiers."},
            {"swig", 's', "LANG", "", false,
                    "Generate SWIG interface for given language. The values <LANG> accepts is java and "
                    "python. "},
            {"library-dir", 'L', "DIR", "", true, "Specify directory for library files."},
            {"libraries", 'l', "FILE", "", true, "Specify libraries."},
            {"no-warn", 'w', "", "", false, "Disable warnings."},
            {"magic-transform", 'm', "RELATIONS", "", false,
                    "Enable magic set transformation changes on the given relations, use '*' "
                    "for all."},
            {"magic-transform-exclude", '\x8', "RELATIONS", "", false,
                    "Disable magic set transformation changes on the given relations. Overrides "
                    "`magic-transform`. Implies `inline-exclude` for the given relations."},
            {"macro", 'M', "MACROS", "", false, "Set macro definitions for the pre-processor"},
            {"disable-transformers", 'z', "TRANSFORMERS", "", false, "Disable the given AST transformers."},
            {"dl-program", 'o', "FILE", "", false,
                    "Generate C++ source code, written to <FILE>, and compile this to a "
                    "binary executable (without executing it)."},
            {"live-profile", '\1', "", "", false, "Enable live profiling."},
            {"profile", 'p', "FILE", "", false, "Enable profiling, and write profile data to <FILE>."},
            {"profile-use", 'u', "FILE", "", false,
                    "Use profile log-file <FILE> for profile-guided optimization."},
            {"profile-frequency", '\2', "", "", false, "Enable the frequency counter in the profiler."},
            {"debug-report", 'r', "FILE", "", false, "Write HTML debug report to <FILE>."},
            {"pragma", 'P', "OPTIONS", "", true, "Set pragma options."},
            {"provenance", 't', "[ none | explain | explore ]", "", false,
                    "Enable provenance instrumentation and interaction."},
            {"verbose", 'v', "", "", false, "Verbose output."}, {"version", '\3', "", "", false, "Version."},
            {"show", '\4', "[ <see-list> ]", "", true,
                    "Print selected program information.\n"
                    "Modes:\n"
                    "\tinitial-ast\n"
                    "\tinitial-ram\n"
                    "\tparse-errors\n"
                    "\tprecedence-graph\n"
                    "\tprecedence-graph-text\n"
                    "\tscc-graph\n"
                    "\tscc-graph-text\n"
                    "\ttransformed-ast\n"
                    "\ttransformed-ram\n"
                    "\ttype-analysis"},
            {"parse-errors", '\5', "", "", false, "Show parsing errors, if any, then exit."},
            {"help", 'h', "", "", false, "Display this help message."},
            {"legacy", '\6', "", "", false, "Enable legacy support."}};
    glb.config().processArgs(argc, argv, header.str(), footer.str(), options);

    return true;
}

}  // namespace

int main(int argc, char** argv) {
    Global glb;

    processArgs(glb, argc, argv);

    return main(glb, argv[0]);
}
