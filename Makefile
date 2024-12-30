# Makefile for C99 Compiler Project using CMake

# ===========================
# 1. Configuration
# ===========================

# Default build type (can be overridden by specifying BUILD_TYPE=Release)
BUILD_TYPE ?= Debug

# Number of parallel jobs (defaults to number of CPU cores)
JOBS := $(shell sysctl -n hw.ncpu 2>/dev/null || echo 4)

# Build directory
BUILD_DIR := build/$(BUILD_TYPE)

# CMake executable
CMAKE := cmake

# ===========================
# 2. Phony Targets
# ===========================

.PHONY: all configure build clean rebuild run unit_test e2e_test test format help

# ===========================
# 3. Default Target
# ===========================

all: build

# ===========================
# 4. Configure Target
# ===========================

configure:
	@echo "Configuring the project with CMake..."
	@mkdir -p $(BUILD_DIR)
	@$(CMAKE) -S . -B $(BUILD_DIR) -DCMAKE_BUILD_TYPE=$(BUILD_TYPE)

# ===========================
# 5. Build Target
# ===========================

build: configure
	@echo "Building the project..."
	@$(CMAKE) --build $(BUILD_DIR) -- -j$(JOBS)

# ===========================
# 6. Clean Target
# ===========================

clean:
	@echo "Cleaning build artifacts..."
	@rm -rf build

# ===========================
# 7. Rebuild Target
# ===========================

rebuild: clean all

# ===========================
# 8. Run Executable Target
# ===========================

run: build
	@echo "Running the executable..."
	@./$(BUILD_DIR)/c99compiler

# ===========================
# 9. Unit Test Execution Target
# ===========================

unit_test: build
	@echo "Running unit tests..."
	@$(CMAKE) --build $(BUILD_DIR) --target run_unit_tests -- -j$(JOBS)

# ===========================
# 10. End-to-End (e2e) Test Execution Target
# ===========================

e2e_test: build
	@echo "Running end-to-end tests..."
	@$(CMAKE) --build $(BUILD_DIR) --target e2e_test -- -j$(JOBS)

# ===========================
# 11. Combined Test Target
# ===========================

test: unit_test e2e_test

# ===========================
# 12. Code Formatting Target
# ===========================

format:
	@echo "Formatting code with clang-format..."
	@find src tests -name '*.cpp' -o -name '*.h' | xargs clang-format -i

# ===========================
# 13. Help Target
# ===========================

help:
	@echo "Available targets:"
	@echo "  all        - Build the project (default)"
	@echo "  configure  - Configure the project with CMake"
	@echo "  build      - Configure and build the project"
	@echo "  clean      - Remove build artifacts"
	@echo "  rebuild    - Clean and rebuild the project"
	@echo "  run        - Run the executable"
	@echo "  unit_test  - Run unit tests"
	@echo "  e2e_test   - Run end-to-end tests"
	@echo "  test       - Run both unit and e2e tests"
	@echo "  format     - Format code with clang-format"
	@echo "  help       - Show this help message"
