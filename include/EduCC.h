/**
 * @file EduCC.h
 * @brief Common includes and definitions for the EduCC compiler.
 *
 * This header is intended to be included by most source files in the project,
 * providing standard library includes, macros, utility functions, and
 * project-wide definitions.
 */

#ifndef EDUCC_H
#define EDUCC_H

// ============================================================================
//  Standard Library Includes
// ============================================================================
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <cstdint>
#include <memory>
#include <unordered_map>
#include <unordered_set>
#include <stdexcept>
#include <sstream>
#include <algorithm>
#include <cassert>

// ============================================================================
//  Global Compiler Constants / Macros
// ============================================================================

/**
 * @brief Compiler version string.
 */
#define EDUCC_VERSION "0.1.0"

/**
 * @brief Quickly check whether we are in debug mode.
 * Use this macro in your code to conditionally enable debug output.
 */
#ifndef NDEBUG
    #define EDUCC_DEBUG 1
#else
    #define EDUCC_DEBUG 0
#endif

/**
 * @brief Simple debug print macro. 
 * Only prints when EDUCC_DEBUG is set (i.e., nonzero).
 * Usage: DEBUG_PRINT("Some message: %d", 42);
 */
#if EDUCC_DEBUG
    #define DEBUG_PRINT(fmt, ...) \
        do { fprintf(stderr, "[DEBUG] " fmt "\n", ##__VA_ARGS__); } while(0)
#else
    #define DEBUG_PRINT(fmt, ...) \
        do { } while(0)
#endif

// ============================================================================
//  Utility Functions / Global Declarations
// ============================================================================

/**
 * @brief A simple function to print an internal compiler error message and exit.
 * 
 * @param message The error message to print.
 */
inline void educc_internal_error(const std::string &message)
{
    std::cerr << "[EDUCC INTERNAL ERROR] " << message << std::endl;
    std::exit(EXIT_FAILURE);
}

#endif // EDUCC_H
