# Makefile for C99 Compiler Project

# ===========================
# Compiler and Flags
# ===========================

# Compiler
CXX := clang++

# Default Build Type
BUILD_TYPE ?= Debug

# Compilation Flags
CXXFLAGS := -std=c++17 -Wall -Wextra -Iinclude -MMD -MP

# Debug vs Release Flags
ifeq ($(BUILD_TYPE),Debug)
    CXXFLAGS += -g -O0
    LDFLAGS :=
else ifeq ($(BUILD_TYPE),Release)
    CXXFLAGS += -O3
    LDFLAGS := 
else
    $(error Unsupported BUILD_TYPE: $(BUILD_TYPE))
endif

# ===========================
# Directories
# ===========================

# Source Directory
SRC_DIR := src

# Include Directory
INCLUDE_DIR := include

# Build Directory
BUILD_DIR := build/$(BUILD_TYPE)

# Test Directory
TEST_DIR := tests

# ===========================
# Files
# ===========================

# Find all .cpp source files recursively in src/
SRCS := $(shell find $(SRC_DIR) -name '*.cpp')

# Generate corresponding object files in build/
# For example: src/parser/Parser.cpp -> build/Debug/src/parser/Parser.o
OBJS := $(patsubst $(SRC_DIR)/%.cpp, $(BUILD_DIR)/%.o, $(SRCS))

# Dependency files
DEPS := $(OBJS:.o=.d)

# Target Executable
TARGET := c99compiler

# ===========================
# Test Files
# ===========================

# Find all test .cpp files
TEST_SRCS := $(shell find $(TEST_DIR) -name '*_test.cpp')

# Generate corresponding test object files
TEST_OBJS := $(patsubst $(TEST_DIR)/%.cpp, $(BUILD_DIR)/tests/%.o, $(TEST_SRCS))

# Test Executable
TEST_TARGET := run_tests

# ===========================
# OS Detection
# ===========================

UNAME_S := $(shell uname -s)

# ===========================
# Default Target
# ===========================

.PHONY: all
all: $(BUILD_DIR)/$(TARGET)

# ===========================
# Linking
# ===========================

# Link all object files to create the executable
$(BUILD_DIR)/$(TARGET): $(OBJS)
	$(CXX) $(CXXFLAGS) -o $@ $^ $(LDFLAGS)

# ===========================
# Compilation
# ===========================

# Pattern rule to compile .cpp files to .o files
# Includes automatic dependency generation
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.cpp
	@mkdir -p $(dir $@)            # Create the directory for the object file if it doesn't exist
	$(CXX) $(CXXFLAGS) -c -o $@ $<

# ===========================
# Test Compilation and Linking
# ===========================

# Compile test object files
$(BUILD_DIR)/tests/%.o: $(TEST_DIR)/%.cpp
	@mkdir -p $(dir $@)
	$(CXX) $(CXXFLAGS) -c -o $@ $<

# Link test executable
$(BUILD_DIR)/$(TEST_TARGET): $(TEST_OBJS) $(OBJS)
	@echo "Checking for Google Test libraries..."
	@if [ "$(UNAME_S)" = "Darwin" ]; then \
		GTEST_LIB1="/usr/local/lib/libgtest.a"; \
		GTEST_LIB2="/opt/homebrew/lib/libgtest.a"; \
	else \
		GTEST_LIB1="/usr/lib/libgtest.a"; \
		GTEST_LIB2="/usr/local/lib/libgtest.a"; \
	fi; \
	if [ -f $$GTEST_LIB1 ]; then \
		echo "Found Google Test at $$GTEST_LIB1"; \
		GTEST_LIB=$$GTEST_LIB1; \
	elif [ -f $$GTEST_LIB2 ]; then \
		echo "Found Google Test at $$GTEST_LIB2"; \
		GTEST_LIB=$$GTEST_LIB2; \
	else \
		echo >&2 "Google Test library not found."; \
		if [ "$(UNAME_S)" = "Darwin" ]; then \
			echo >&2 "Run 'brew install googletest' to install Google Test."; \
		else \
			echo >&2 "Please install Google Test manually."; \
		fi; \
		exit 1; \
	fi
	$(CXX) $(CXXFLAGS) -o $@ $^ $(LDFLAGS) $$GTEST_LIB -lgtest_main -pthread

# ===========================
# Clean Target
# ===========================

.PHONY: clean
clean:
	rm -rf build

# ===========================
# Rebuild Target
# ===========================

.PHONY: rebuild
rebuild: clean all

# ===========================
# Run Executable
# ===========================

.PHONY: run
run: $(BUILD_DIR)/$(TARGET)
	@echo "Running $(TARGET)..."
	./$(BUILD_DIR)/$(TARGET)

# ===========================
# Format Target
# ===========================

.PHONY: format
format:
	@echo "Formatting code with clang-format..."
	@find $(SRC_DIR) $(TEST_DIR) -name '*.cpp' -o -name '*.h' | xargs clang-format -i

# ===========================
# Test Target
# ===========================

.PHONY: test
test: $(BUILD_DIR)/$(TEST_TARGET)
	@echo "Running unit tests..."
	./$(BUILD_DIR)/$(TEST_TARGET)

# ===========================
# Help Target
# ===========================

.PHONY: help
help:
	@echo "Available targets:"
	@echo "  all        - Build the project (default)"
	@echo "  clean      - Remove build artifacts"
	@echo "  rebuild    - Clean and rebuild the project"
	@echo "  run        - Run the executable"
	@echo "  test       - Run unit tests"
	@echo "  format     - Format code with clang-format"
	@echo "  help       - Show this help message"

# ===========================
# 6. Include Dependencies
# ===========================

# Include the dependency files if they exist
-include $(DEPS)

# ===========================
# Phony Targets
# ===========================

.PHONY: all clean rebuild run test format help
