#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

# Directory paths
E2E_TEST_DIR="../e2e"
TEST_RESULTS="../test_results"
EXECUTABLE="../../build/$(BUILD_TYPE)/c99compiler"

# Create test results directory
mkdir -p "$TEST_RESULTS"

# Function to compare outputs
compare_outputs() {
    local test_case_dir=$1
    local source_file=$2
    local expected_output_file=$3
    local test_name=$(basename "$test_case_dir")
    local actual_output_file="$TEST_RESULTS/${test_name}.out"

    echo "Running test: $test_name"

    # Compile the input file using your compiler
    "$EXECUTABLE" "$source_file" -o "$TEST_RESULTS/${test_name}.exe"

    if [ $? -ne 0 ]; then
        echo "Compilation failed for test: $test_name"
        return
    fi

    # Run the compiled executable and capture output
    "./$TEST_RESULTS/${test_name}.exe" > "$actual_output_file" 2>&1

    # Compare actual output with expected output
    if diff -q "$actual_output_file" "$expected_output_file" > /dev/null; then
        echo "Test Passed: $test_name"
    else
        echo "Test Failed: $test_name"
        echo "Differences:"
        diff "$actual_output_file" "$expected_output_file"
    fi
}

# Iterate over all e2e test cases
for test_case in "$E2E_TEST_DIR"/*/; do
    test_case_dir=$(realpath "$test_case")
    test_name=$(basename "$test_case_dir")
    source_file="$test_case_dir/main.c"
    expected_output_file="$test_case_dir/main.out"

    if [ -f "$source_file" ] && [ -f "$expected_output_file" ]; then
        compare_outputs "$test_case_dir" "$source_file" "$expected_output_file"
    else
        echo "Missing input or expected output for test: $test_name"
    fi
done
