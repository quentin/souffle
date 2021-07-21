// Implementation of getopt_long for Windows.

#ifdef USE_CUSTOM_GETOPTLONG

#include "GetOptLong.h"
#include <stdio.h>
#include <string.h>

char* optarg = nullptr;
int optind = 0;
int opterr = 1;
int optopt = 0;

namespace {

char* const EMPTY = const_cast<char*>("");

char* nextchar = EMPTY;

// detected non-options are moved at argv[first_nonopt .. end_nonopt - 1]
// position in argv of the first non-option.
int first_nonopt;
// one position past the last non-option in argv.
int end_nonopt;

int parse_long_option(const int argc, char* const argv[], const struct option* longopts, int* longindex) {
    char* const current = nextchar;
    ++optind;

    char* const hasequal = strchr(current, '=');
    size_t namelength = (hasequal ? (hasequal - current) : strlen(current));

    int i;
    int match = -1;
    for (i = 0; longopts[i].name != nullptr; ++i) {
        if (strncmp(longopts[i].name, current, namelength)) {
            continue;
        }
        if (strlen(longopts[i].name) != namelength) {
            continue;
        }

        match = i;
        break;
    }

    if (match == -1) {
        // cannot find long option
        if (opterr) {
            fprintf(stderr, "unknown option -- %.*s\n", static_cast<int>(namelength), current);
        }
        optopt = 0;
        return (int)'?';
    }

    if (longopts[match].has_arg == 0) {
        // no argument expected
        if (hasequal) {
            if (opterr) {
                fprintf(stderr, "unexpected argument -- %.*s\n", static_cast<int>(namelength), current);
            }
            if (longopts[match].flag == nullptr) {
                optopt = longopts[match].val;
            } else {
                optopt = 0;
            }
            return (int)'?';
        }
    }

    if (longopts[match].has_arg == 1 || longopts[match].has_arg == 2) {
        if (hasequal) {
            // argument is in the same argv after '=' sign
            optarg = hasequal + 1;
        } else if (optind < argc) {
            // Argument may be in next argv
            // If argument is optional, leave optarg to null, user is in charge
            // of verifying the value of argv[optind] and increment optind
            // if the argument is valid.
            if (longopts[match].has_arg == 1) {
                // mandatory argument
                optarg = argv[optind++];
            }
        } else {
            // no argument found
            if (longopts[match].has_arg == 1) {
                if (opterr) {
                    fprintf(stderr, "missing mandatory argument -- %.*s\n", static_cast<int>(namelength),
                            current);
                }
                optopt = 0;
                return (int)':';
            }
        }
    }  // unexpected value of has_arg is not verified

    if (longindex) *longindex = match;
    if (longopts[match].flag) {
        *longopts[match].flag = longopts[match].val;
        return 0;
    } else {
        return longopts[match].val;
    }
}

// Permute current non-option with next available option if any.
// Return 1 if a permutation occured and optind is the index of an option in
// argc, 0 otherwise.
int permute(int argc, char** argv) {
    // find next option
    int next_opt;

    for (next_opt = optind; next_opt < argc; ++next_opt) {
        if (argv[next_opt][0] == '-') {
            break;
        }
    }

    if (next_opt == argc) {
        // no more options
        return 0;
    }

    // move argc[next_opt] to argc[optind]
    // shift argc[optind .. next_opt - 1] to argc[optind + 1 .. next_opt]
    char* const option = argv[next_opt];
    int i;
    for (i = next_opt; i > optind; --i) {
        argv[i] = argv[i - 1];
    }
    argv[optind] = option;

    return 1;
}

}  // anonymous namespace

int getopt_long(
        int argc, char* const argv[], const char* optstring, const struct option* longopts, int* longindex) {
    if (optind == 0) {  // full reset
        optind = 1;
        nextchar = EMPTY;
        first_nonopt = 1;
        end_nonopt = 1;
    }

    if (optstring[0] == '+' || optstring[0] == '-') {
        throw "Mode +/- of optstring is not supported.";
    }

    if (optind >= argc) {
        // all command-line arguments have been parsed
        return -1;
    }

    if (*nextchar == 0) {
        nextchar = argv[optind];
        if (*nextchar != '-') {
            // not starting with an option
            // try to permute with the next option if available
            if (permute(argc, const_cast<char**>(argv))) {
                nextchar = argv[optind];
            } else {
                // definitely no more options
                nextchar = EMPTY;
                return -1;
            }
        }
    }

    optarg = nullptr;

    if (nextchar == argv[optind] && *nextchar == '-') {
        ++nextchar;
        if (*nextchar == '-' && *(++nextchar)) {
            // search long option
            optopt = parse_long_option(argc, argv, longopts, longindex);
            nextchar = EMPTY;
            return optopt;
        } else if (*nextchar == 0) {
            // missing option character
            nextchar = EMPTY;
            return -1;
        }
    }

    // search short option
    const char* option;
    optopt = *nextchar++;
    if ((option = strchr(optstring, optopt)) == nullptr) {
        // cannot find option
        if (opterr) {
            fprintf(stderr, "unknown option -- %c\n", optopt);
        }
        return (int)'?';
    }
    ++option;

    if (*option++ != ':') {
        // no argument required
        if (!*nextchar) {
            ++optind;
        }
    } else {
        if (*nextchar) {
            // if argument is in the same argv, always set optarg
            optarg = nextchar;
            ++optind;
            nextchar = EMPTY;
        } else if (argc <= ++optind) {
            // no argument found
            nextchar = EMPTY;
            if (*option != ':') {
                // option has mandatory argument
                if (opterr) {
                    fprintf(stderr, "missing mandatory argument -- %c\n", optopt);
                }
                return (int)':';
            } else {
                // option has optional argument
                optarg = nullptr;
            }
        } else {
            // argument is in next argv
            if (*argv[optind] == '-' && *option != ':') {
                // argument is mandatory, but must not start with a dash
                if (opterr) {
                    fprintf(stderr, "missing mandatory argument -- %c\n", optopt);
                }
                return (int)':';
            }
            if (*option != ':') {
                // argument  is mandatory
                optarg = argv[optind++];
            } else {
                // Argument is optional but not in the same argv, set optarg to null.
                // User is in charge of interpreting argv[optind] and increment it
                // if it considers its a valid argument.
                optarg = nullptr;
            }

            nextchar = EMPTY;
        }
    }

    return optopt;
}

#endif
