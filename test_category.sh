#!/bin/bash

# Test specific parts of GLaDOS

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

show_help() {
    echo "Test specific parts of GLaDOS"
    echo ""
    echo "Usage: $0 <category> [options]"
    echo ""
    echo "Categories:"
    echo "  float         Floating point tests"
    echo "  errors        Error handling tests"
    echo "  functions     Function-related tests (test_3_*)"
    echo "  variables     Variable tests (test_4_*)"
    echo "  control       Control flow (if/while/for)"
    echo "  calls         Function calls (test_6_*)"
    echo "  statements    Statement tests (test_7_*)"
    echo "  init          Initialization tests (test_9_*)"
    echo "  linker        All linker tests"
    echo "  basic         Basic/simple tests"
    echo ""
    echo "Options:"
    echo "  -v, --verbose    Show detailed output"
    echo "  -s, --stop       Stop on first failure"
    echo "  -h, --help       Show this help"
    echo ""
    echo "Examples:"
    echo "  $0 float         Test all float functionality"
    echo "  $0 errors -v     Test error handling with verbose output"
    echo "  $0 linker        Test the linker"
}

# Parse args
CATEGORY=""
OPTS=""

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            show_help
            exit 0
            ;;
        -v|--verbose)
            OPTS="$OPTS -v"
            shift
            ;;
        -s|--stop)
            OPTS="$OPTS -s"
            shift
            ;;
        *)
            if [ -z "$CATEGORY" ]; then
                CATEGORY="$1"
            else
                echo "Unknown option: $1"
                exit 1
            fi
            shift
            ;;
    esac
done

if [ -z "$CATEGORY" ]; then
    echo "No category specified"
    echo "Try -h for help"
    exit 1
fi

# Figure out what pattern to use
PATTERN=""
TEST_DIR="tests/dotC"

case $CATEGORY in
    float)
        PATTERN="*float*"
        echo -e "${CYAN}Testing floating point functionality${NC}"
        ;;
    errors)
        PATTERN="*err*"
        echo -e "${CYAN}Testing error handling${NC}"
        ;;
    functions)
        PATTERN="test_3_*"
        echo -e "${CYAN}Testing function functionality${NC}"
        ;;
    variables)
        PATTERN="test_4_*"
        echo -e "${CYAN}Testing variable handling${NC}"
        ;;
    control)
        echo -e "${CYAN}Testing control flow${NC}"
        # Run multiple patterns
        ./run_tests.sh -p "test_5_*" $OPTS
        exit_code=$?
        ./run_tests.sh -p "control_flow.c" $OPTS
        [ $exit_code -ne 0 ] && exit $exit_code
        exit $?
        ;;
    calls)
        PATTERN="test_6_*"
        echo -e "${CYAN}Testing function calls${NC}"
        ;;
    statements)
        PATTERN="test_7_*"
        echo -e "${CYAN}Testing statements${NC}"
        ;;
    init)
        PATTERN="test_9_*"
        echo -e "${CYAN}Testing initialization${NC}"
        ;;
    linker)
        echo -e "${CYAN}Testing linker${NC}"
        ./run_tests.sh --linker $OPTS
        exit $?
        ;;
    basic)
        PATTERN="test1*"
        echo -e "${CYAN}Testing basics${NC}"
        ;;
    *)
        echo "Unknown category: $CATEGORY"
        echo "Try -h for available categories"
        exit 1
        ;;
esac

# Run the tests with the pattern
./run_tests.sh -p "$PATTERN" $OPTS
