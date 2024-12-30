# MAKEFILE

# ===========================
# 1. Compiler and Flags
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
# 2. Directories
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
# 3. Files
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
# 4. Test Files
# ===========================

# Find all test .cpp files
TEST_SRCS := $(shell find $(TEST_DIR) -name '*_test.cpp')

# Generate corresponding test object files
TEST_OBJS := $(patsubst $(TEST_DIR)/%.cpp, $(BUILD_DIR)/tests/%.o, $(TEST_SRCS))

# Test Executable
TEST_TARGET := run_tests

# ===========================
# 5. Default Target
# ===========================

.PHONY: all
all: $(BUILD_DIR)/$(TARGET)

# ===========================
# 6. Linking
# ===========================

# Link all object files to create the executable
$(BUILD_DIR)/$(TARGET): $(OBJS)
	$(CXX) $(CXXFLAGS) -o $@ $^ $(LDFLAGS)

# ===========================
# 7. Compilation
# ===========================

# Pattern rule to compile .cpp files to .o files
# Includes automatic dependency generation
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.cpp
	@mkdir -p $(dir $@)            # Create the directory for the object file if it doesn't exist
	$(CXX) $(CXXFLAGS) -c -o $@ $<

# ===========================
# 8. Test Compilation and Linking
# ===========================

# Compile test object files
$(BUILD_DIR)/tests/%.o: $(TEST_DIR)/%.cpp
	@mkdir -p $(dir $@)
	$(CXX) $(CXXFLAGS) -c -o $@ $<

# Link test executable
$(BUILD_DIR)/$(TEST_TARGET): $(TEST_OBJS) $(OBJS)
	$(CXX) $(CXXFLAGS) -o $@ $^ $(LDFLAGS) -lgtest -lgtest_main -pthread

# ===========================
# 9. Clean Target
# ===========================

.PHONY: clean
clean:
	rm -rf build

# ===========================
# 10. Rebuild Target
# ===========================

.PHONY: rebuild
rebuild: clean all

# ===========================
# 11. Run Executable
# ===========================

.PHONY: run
run: $(BUILD_DIR)/$(TARGET)
	@echo "Running $(TARGET)..."
	./$(BUILD_DIR)/$(TARGET)

# ===========================
# 12. Lint Target
# ===========================

.PHONY: lint
lint:
	@echo "Running clang-tidy..."
	@find $(SRC_DIR) $(TEST_DIR) -name '*.cpp' | xargs clang-tidy -p build/$(BUILD_TYPE)

# ===========================
# 13. Test Target
# ===========================

.PHONY: test
test: $(BUILD_DIR)/$(TEST_TARGET)
	@echo "Running unit tests..."
	./$(BUILD_DIR)/$(TEST_TARGET)

# ===========================
# 14. Include Dependencies
# ===========================

# Include the dependency files if they exist
-include $(DEPS)

# ===========================
# 15. Phony Targets
# ===========================

.PHONY: all clean rebuild run lint test
