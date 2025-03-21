#!/bin/bash

# RTLF Test Script
# This script tests all functionality of RTLF with various parameter combinations
# and cleans up after itself

# Terminal colors for better readability
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Make sure we're in the right directory
cd "$(dirname "$0")"

# Ensure rtlf.R is executable
chmod +x rtlf.R

# Create temporary test directory
TEST_DIR="./rtlf_test_$(date +%s)"
mkdir -p "$TEST_DIR"

echo -e "${BLUE}========== RTLF Test Suite ==========${NC}"
echo -e "${BLUE}Temporary test directory: ${TEST_DIR}${NC}"
echo

# Function to run a test and report results
run_test() {
    local test_name="$1"
    local command="$2"
    local expected_exit="$3"
    
    echo -e "${YELLOW}TEST: ${test_name}${NC}"
    echo "Command: $command"
    
    # Run the command and capture exit code
    eval "$command" > "$TEST_DIR/output.log" 2>&1
    local exit_code=$?
    
    # Check expected exit code
    if [ "$exit_code" = "$expected_exit" ]; then
        echo -e "${GREEN}✓ PASS (Exit code: $exit_code)${NC}"
    else
        echo -e "${RED}✗ FAIL (Expected: $expected_exit, Actual: $exit_code)${NC}"
        echo "Log output:"
        cat "$TEST_DIR/output.log"
    fi
    echo
}

# Test 1: Basic help
run_test "Display help" \
         "./rtlf.R --help" \
         "0"

# Test 2: Single file analysis (difference detected)
run_test "Single file analysis (difference detected)" \
         "./rtlf.R -i examples/example-1.csv -o ${TEST_DIR}/result1.json" \
         "11"

# Test 3: Single file analysis (no difference)
run_test "Single file analysis (no difference)" \
         "./rtlf.R -i examples/example-2.csv -o ${TEST_DIR}/result2.json" \
         "10"

# Test 4: Different alpha value
run_test "Custom alpha value" \
         "./rtlf.R -i examples/example-1.csv -a 0.05 -o ${TEST_DIR}/result3.json" \
         "11"

# Test 5: Quiet mode
run_test "Quiet mode" \
         "./rtlf.R -i examples/example-1.csv -q -o ${TEST_DIR}/result4.json" \
         "11"

# Test 6: Column format
run_test "Column format" \
         "./rtlf.R -i examples/example-columns.csv -o ${TEST_DIR}/result5.json" \
         "11"

# Test 7: Single-row format
run_test "Single-row format" \
         "./rtlf.R -i examples/example-single-row.csv -o ${TEST_DIR}/result6.json" \
         "11"

# Test 8: Semicolon separator
run_test "Semicolon separator" \
         "./rtlf.R -i examples/example-semicolon.csv -o ${TEST_DIR}/result7.json" \
         "11"

# Test 9: Directory processing (default single-threaded)
run_test "Directory processing (single-threaded)" \
         "./rtlf.R -i examples/ -o ${TEST_DIR}/dir_results1/" \
         "11"

# Test 10: Directory processing with explicit threads
run_test "Directory processing (multi-threaded)" \
         "./rtlf.R -i examples/ -t 2 -o ${TEST_DIR}/dir_results2/" \
         "11"

# Test 11: Directory processing with auto-detected threads
run_test "Directory processing (auto-detected threads)" \
         "./rtlf.R -i examples/ -t 0 -o ${TEST_DIR}/dir_results3/" \
         "11"

# Test 12: Directory processing with quiet
run_test "Directory processing (quiet mode)" \
         "./rtlf.R -i examples/ -q -o ${TEST_DIR}/dir_results4/" \
         "11"

# Test 13: Pattern matching
run_test "Pattern matching" \
         "./rtlf.R -i examples/ -p \"*-1.csv\" -o ${TEST_DIR}/dir_results5/" \
         "11"

# Test 14: Different output formats
run_test "CSV output format" \
         "./rtlf.R -i examples/example-1.csv -o ${TEST_DIR}/result.csv" \
         "11"

run_test "RDATA output format" \
         "./rtlf.R -i examples/example-1.csv -o ${TEST_DIR}/result.RDATA" \
         "11"

# Test the validity of the JSON output
echo -e "${YELLOW}TEST: Validating JSON output${NC}"
if [ -f "${TEST_DIR}/result1.json" ]; then
    # Try to parse the JSON with jq if available
    if command -v jq >/dev/null 2>&1; then
        if jq . "${TEST_DIR}/result1.json" > /dev/null 2>&1; then
            echo -e "${GREEN}✓ PASS: JSON is valid${NC}"
        else
            echo -e "${RED}✗ FAIL: JSON is not valid${NC}"
        fi
    else
        echo -e "${YELLOW}⚠ WARNING: jq not installed, can't validate JSON${NC}"
    fi
else
    echo -e "${RED}✗ FAIL: JSON output file not found${NC}"
fi
echo

# Print summary of tests
echo -e "${BLUE}========== Test Summary ==========${NC}"
echo -e "${BLUE}Test results are available in: ${TEST_DIR}${NC}"
echo -e "${GREEN}You can examine the outputs in this directory${NC}"
echo -e "${YELLOW}To clean up the test directory, run:${NC}"
echo -e "rm -rf ${TEST_DIR}"

# Prompt for cleanup
read -p "Do you want to clean up the test directory now? (y/n): " cleanup
if [ "$cleanup" = "y" ] || [ "$cleanup" = "Y" ]; then
    rm -rf "$TEST_DIR"
    echo -e "${GREEN}Test directory removed.${NC}"
else
    echo -e "${YELLOW}Test directory kept for inspection.${NC}"
fi
