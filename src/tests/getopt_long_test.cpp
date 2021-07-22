
#include "tests/test.h"

#define USE_CUSTOM_GETOPTLONG
#include "GetOptLong.cpp"

namespace souffle::test {

TEST(GetOptLong, Short) {
    optind = 1;
    opterr = 0;

    char* const argv[] = {const_cast<char*>("test"), const_cast<char*>("-z"), const_cast<char*>("-x"),
            const_cast<char*>("argx"), const_cast<char*>("-yargy"), const_cast<char*>("-z"),
            const_cast<char*>("argz"), const_cast<char*>("-zargz")};

    const int argc = 8;
    const char* optstr = "x:y:z::";
    struct option longopts[1];
    memset(longopts, 0, sizeof(struct option));

    char c;
    c = getopt_long(argc, argv, optstr, longopts, nullptr);
    EXPECT_EQ(c, 'z');
    EXPECT_EQ(optarg, nullptr);

    c = getopt_long(argc, argv, optstr, longopts, nullptr);
    EXPECT_EQ(c, 'x');
    EXPECT_EQ(std::string(optarg), std::string("argx"));

    c = getopt_long(argc, argv, optstr, longopts, nullptr);
    EXPECT_EQ(c, 'y');
    EXPECT_EQ(std::string(optarg), std::string("argy"));

    c = getopt_long(argc, argv, optstr, longopts, nullptr);
    EXPECT_EQ(c, 'z');
    // z has an optional argument and it's not in the same argv cell so optarg is set to null
    // and optind is the position of the possible argument in argv
    EXPECT_EQ(std::string(argv[optind]), std::string("argz"));
    // accept the argument
    optind += 1;

    c = getopt_long(argc, argv, optstr, longopts, nullptr);
    EXPECT_EQ(c, 'z');
    // z has an optional argument and it in the same argv cell so optarg is set
    EXPECT_EQ(std::string(optarg), std::string("argz"));

    c = getopt_long(argc, argv, optstr, longopts, nullptr);
    EXPECT_EQ(c, -1);
}

TEST(GetOptLong, SimplePermutation) {
    optind = 1;
    opterr = 0;

    // expect arguments to be permuted and show up as if -x, -y, arg1, arg2, arg3, arg4
    char* const argv[] = {
            const_cast<char*>("test"),
            const_cast<char*>("arg1"),
            const_cast<char*>("-x"),
            const_cast<char*>("arg2"),
            const_cast<char*>("arg3"),
            const_cast<char*>("-y"),
            const_cast<char*>("arg4"),
    };

    const int argc = 7;

    struct option longopts[1];
    const char* optstr = "xy";
    memset(longopts, 0, sizeof(struct option));

    char c;
    c = getopt_long(argc, argv, optstr, longopts, nullptr);
    EXPECT_EQ(c, 'x');
    c = getopt_long(argc, argv, optstr, longopts, nullptr);
    EXPECT_EQ(c, 'y');
    c = getopt_long(argc, argv, optstr, longopts, nullptr);
    EXPECT_EQ(c, -1);
    EXPECT_EQ(optind, 3);
    EXPECT_EQ(std::string(argv[optind]), std::string("arg1"));
    EXPECT_EQ(std::string(argv[optind + 1]), std::string("arg2"));
    EXPECT_EQ(std::string(argv[optind + 2]), std::string("arg3"));
    EXPECT_EQ(std::string(argv[optind + 3]), std::string("arg4"));
}

#if 0 // disabled because not passing yet
TEST(GetOptLong, ComplexPermutation) {
    optind = 1;
    opterr = 0;

    // expect arguments to be permuted and show up as if -x argx, -y, arg1
    char* const argv[] = {
            const_cast<char*>("test"),
            const_cast<char*>("arg1"),
            const_cast<char*>("-x"),
            const_cast<char*>("argx"),
            const_cast<char*>("-y"),
    };

    const int argc = 5;
    const char* optstr = "x:y::";
    struct option longopts[1];
    memset(longopts, 0, sizeof(struct option));

    char c;
    c = getopt_long(argc, argv, optstr, longopts, nullptr);
    EXPECT_EQ(c, 'x');
    EXPECT_NE(optarg, nullptr);
    EXPECT_EQ(std::string(optarg), std::string("argx"));

    c = getopt_long(argc, argv, optstr, longopts, nullptr);
    EXPECT_EQ(c, 'y');
    EXPECT_EQ(optarg, nullptr);

    c = getopt_long(argc, argv, optstr, longopts, nullptr);
    EXPECT_EQ(c, -1);
    EXPECT_EQ(optarg, nullptr);
    EXPECT_EQ(optind, 4);
    EXPECT_EQ(std::string(argv[optind]), std::string("arg1"));
}
#endif

}  // namespace souffle::test
