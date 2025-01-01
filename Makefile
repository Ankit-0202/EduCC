# =========================================================
# EduCC - A Teaching C99 Compiler in C++
# Updated Makefile (with LLVM integration)
# =========================================================

##################################################
# Make sure you have the following in your shell configuration file,
# or you can run them locally in your terminal.
# export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
# export LDFLAGS="-L/opt/homebrew/opt/llvm/lib"
# export CPPFLAGS="-I/opt/homebrew/opt/llvm/include"
# export PKG_CONFIG_PATH="/opt/homebrew/opt/llvm/lib/pkgconfig"

# --- Compiler and Tools ---
CXX       := g++
CXXFLAGS  := -std=c++17 -O2 -Wall -Wextra -pedantic -Wno-unused-parameter

# If you have clang++, you can switch to:
# CXX       := clang++
# CXXFLAGS  := -std=c++17 -O2 -Wall -Wextra -pedantic

# --- LLVM Flags (via llvm-config) ---
LLVM_CXXFLAGS := $(shell llvm-config --cxxflags)
LLVM_LDFLAGS  := $(shell llvm-config --ldflags --libs core orcjit native)

# We append them to our flags:
CXXFLAGS  += $(LLVM_CXXFLAGS)
LDFLAGS    = $(LLVM_LDFLAGS)

# --- Project Structure ---
TARGET    := educc
SRC_DIR   := src
OBJ_DIR   := build
INCLUDE_DIR := include

# We'll store all object files in build/ preserving directory structure
VPATH := $(SRC_DIR)

# Collect all .cpp files from the src directory (recursively):
SOURCES := $(shell find $(SRC_DIR) -name '*.cpp')

# Replace src/ with build/ in the object paths, e.g. src/lexer/Lexer.cpp => build/lexer/Lexer.o
OBJECTS := $(patsubst $(SRC_DIR)/%.cpp, $(OBJ_DIR)/%.o, $(SOURCES))

# Include flags:
INC_FLAGS := -I$(INCLUDE_DIR)

# By default, build the compiler
all:
	make $(TARGET)

# Link step
$(TARGET): $(OBJECTS)
	@echo "Linking $(TARGET)..."
	$(CXX) $(CXXFLAGS) -o $@ $^ $(LDFLAGS)

# Compile step
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.cpp
	@mkdir -p $(dir $@)
	@echo "Compiling $<..."
	$(CXX) $(CXXFLAGS) $(INC_FLAGS) -c $< -o $@

# Cleaning
clean:
	@echo "Cleaning..."
	rm -rf $(OBJ_DIR) 
	rm -f $(TARGET)

# A simple test rule to demonstrate usage
# 1) Builds the compiler
# 2) Runs the compiler on sample.c -> sample.ll
# 3) Uses clang to compile sample.ll -> sample (native executable)
# 4) Runs ./sample
test: all
	@echo "Compiling sample.c with educc -> sample.ll"
	@./$(TARGET) sample.c sample.ll
	@echo "Now compiling sample.ll with clang -> sample"
	@clang sample.ll -o sample
	@echo "Running ./sample..."
	@./sample

# Phony targets
.PHONY: all clean test
