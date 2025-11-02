#!/bin/bash

# GLaDOS Test Runner - Because manually testing is for chumps

# Colors for fancy output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

# Defaults
VERBOSE=0
STOP_ON_FAIL=0
TEST_PATTERN="*.c"
TEST_DIR="tests/dotC"
COMPILER="stack exec glados-exe --"
QUIET=0

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            echo "GLaDOS Test Runner"
            echo ""
            echo "Usage: $0 [options]"
            echo ""
            echo "Options:"
            echo "  -v, --verbose       Show what went wrong"
            echo "  -q, --quiet         Only show failures"
            echo "  -s, --stop-on-fail  Quit on first failure"
            echo "  -p, --pattern       Test file pattern (default: *.c)"
            echo "  --linker            Run linker tests instead"
            echo "  -h, --help          This thing you're reading"
            echo ""
            echo "Examples:"
            echo "  $0                  Run all tests"
            echo "  $0 -v               Verbose mode"
            echo "  $0 -p \"*simple*\"    Run only simple tests"
            exit 0
            ;;
        -v|--verbose)
            VERBOSE=1
            shift
            ;;
        -q|--quiet)
            QUIET=1
            shift
            ;;
        -s|--stop-on-fail)
            STOP_ON_FAIL=1
            shift
            ;;
        -p|--pattern)
            TEST_PATTERN="$2"
            shift 2
            ;;
        --linker)
            TEST_DIR="tests/linker"
            shift
            ;;
        *)
            echo "Unknown option: $1"
            echo "Try -h for help"
            exit 1
            ;;
    esac
done

# Stats tracking
TOTAL=0
PASSED=0
FAILED=0
START_TIME=$(date +%s)
FAILED_TESTS=()

# Make sure stack is installed
if ! command -v stack &> /dev/null; then
    echo -e "${RED}stack is not installed, can't continue${NC}"
    exit 1
fi

echo -e "${BLUE}${BOLD}GLaDOS Test Runner${NC}"
echo ""

# Build first, or nothing will work
echo -e "${CYAN}Building...${NC}"
if stack build 2>&1 | grep -q "error:"; then
    echo -e "${RED}Build failed!${NC}"
    exit 1
fi
echo -e "${GREEN}Build successful${NC}"
echo ""

# Find all test files
TEST_FILES=$(find "$TEST_DIR" -name "$TEST_PATTERN" -type f 2>/dev/null | sort)

if [ -z "$TEST_FILES" ]; then
    echo -e "${YELLOW}No test files found matching '$TEST_PATTERN' in $TEST_DIR${NC}"
    exit 1
fi

TOTAL=$(echo "$TEST_FILES" | wc -l)
echo -e "Found ${BOLD}$TOTAL${NC} test(s)"
echo ""

# Run a single test
run_regular_test() {
    local test_file="$1"
    local test_num="$2"
    local test_name=$(basename "$test_file" .c)

    # Check if this test is supposed to fail
    local expect_error=0
    grep -iq "Expected:.*error" "$test_file" && expect_error=1

    [ $QUIET -eq 0 ] && echo -ne "[$test_num/$TOTAL] ${CYAN}$test_name${NC} ... "

    # Run the compiler
    output=$($COMPILER < "$test_file" 2>&1)
    exit_code=$?

    # Did it pass?
    passed=0
    if [ $expect_error -eq 1 ]; then
        # Should have failed
        [ $exit_code -ne 0 ] && passed=1
    else
        # Should have succeeded
        [ $exit_code -eq 0 ] && passed=1
    fi

    # Report results
    if [ $passed -eq 1 ]; then
        PASSED=$((PASSED + 1))
        [ $QUIET -eq 0 ] && echo -e "${GREEN}PASS${NC}"
    else
        FAILED=$((FAILED + 1))
        FAILED_TESTS+=("$test_name")
        echo -e "${RED}FAIL${NC}"
        [ $VERBOSE -eq 1 ] && echo "$output" | sed 's/^/  /'
        [ $STOP_ON_FAIL -eq 1 ] && return 1
    fi
    return 0
}

# Run a linker test (more complex - compile, link, then run)
run_linker_test() {
    local test_file="$1"
    local test_num="$2"
    local test_name=$(basename "$test_file" .c)
    local temp_dir=$(mktemp -d)

    # Should this test fail?
    local expect_error=0
    grep -iq "Expected:.*error" "$test_file" && expect_error=1

    [ $QUIET -eq 0 ] && echo -ne "[$test_num/$TOTAL] ${CYAN}$test_name${NC} ... "

    # Figure out what files to link together
    local link_with=$(grep "LinkWith:" "$test_file" | head -1 | sed 's/.*LinkWith: *//' | sed 's/ *$//')
    local all_files="$test_file"
    [ -n "$link_with" ] && all_files="$all_files $TEST_DIR/$link_with"

    # Compile each file to object code
    local compile_failed=0
    local object_files=""
    for src in $all_files; do
        local obj="$temp_dir/$(basename "$src" .c).gbo"
        if ! $COMPILER --compile-obj < "$src" > "$obj" 2>&1; then
            compile_failed=1
            break
        fi
        object_files="$object_files $obj"
    done

    # Now try to link and run
    local passed=0
    if [ $compile_failed -eq 0 ]; then
        local bytecode="$temp_dir/linked.gbc"
        if $COMPILER --link $object_files > "$bytecode" 2>&1; then
            if $COMPILER --run "$bytecode" 2>&1; then
                [ $expect_error -eq 0 ] && passed=1
            else
                [ $expect_error -eq 1 ] && passed=1
            fi
        else
            [ $expect_error -eq 1 ] && passed=1
        fi
    elif [ $expect_error -eq 1 ]; then
        passed=1
    fi

    rm -rf "$temp_dir"

    # Report
    if [ $passed -eq 1 ]; then
        PASSED=$((PASSED + 1))
        [ $QUIET -eq 0 ] && echo -e "${GREEN}PASS${NC}"
    else
        FAILED=$((FAILED + 1))
        FAILED_TESTS+=("$test_name")
        echo -e "${RED}FAIL${NC}"
        [ $STOP_ON_FAIL -eq 1 ] && return 1
    fi
    return 0
}

# Actually run the tests
test_num=0
for test_file in $TEST_FILES; do
    test_num=$((test_num + 1))

    # Skip library files in linker mode (they're not actual tests)
    if [ "$TEST_DIR" = "tests/linker" ] && grep -q "LinkTest: lib" "$test_file"; then
        TOTAL=$((TOTAL - 1))
        test_num=$((test_num - 1))
        continue
    fi

    # Pick the right test runner
    if [ "$TEST_DIR" = "tests/linker" ]; then
        run_linker_test "$test_file" $test_num || break
    else
        run_regular_test "$test_file" $test_num || break
    fi
done

# Print summary
end_time=$(date +%s)
duration=$((end_time - START_TIME))
pass_rate=0
[ $TOTAL -gt 0 ] && pass_rate=$(awk "BEGIN {printf \"%.0f\", ($PASSED/$TOTAL)*100}")

echo ""
echo -e "${BOLD}Results:${NC}"
echo -e "  Total:     $TOTAL"
echo -e "  ${GREEN}Passed:    $PASSED${NC}"
echo -e "  ${RED}Failed:    $FAILED${NC}"
echo -e "  Pass rate: ${pass_rate}%"
echo -e "  Time:      ${duration}s"

# List failures if any
if [ $FAILED -gt 0 ]; then
    echo ""
    echo -e "${RED}What failed:${NC}"
    for test in "${FAILED_TESTS[@]}"; do
        echo -e "  - $test"
    done
fi

echo ""
if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}All good!${NC}"
    exit 0
else
    echo -e "${RED}$FAILED test(s) didn't make it${NC}"
    exit 1
fi
