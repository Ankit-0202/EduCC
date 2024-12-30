/**
 * @file SymbolTable.h
 * @brief Declarations for symbol table management in EduCC.
 *
 * A symbol table keeps track of variables, functions, and other identifiers
 * in their respective scopes during semantic analysis.
 */

#ifndef EDUCC_SYMBOLTABLE_H
#define EDUCC_SYMBOLTABLE_H

#include "EduCC.h"
#include <string>
#include <unordered_map>
#include <vector>
#include <memory>

/**
 * @enum SymbolKind
 * @brief Identifies the kind of symbol (variable, function, etc.).
 */
enum class SymbolKind
{
    Variable,
    Function
};

/**
 * @struct Symbol
 * @brief Represents a single named entity in the symbol table.
 */
struct Symbol
{
    SymbolKind kind;
    std::string name;
    std::string type;  ///< E.g. "int", "float", "void", etc.
    // You can store more details about function parameters, etc.

    Symbol(SymbolKind k, const std::string &n, const std::string &t)
        : kind(k), name(n), type(t)
    {}
};

/**
 * @class SymbolTable
 * @brief A hierarchical symbol table to manage scopes.
 *
 * Each SymbolTable can have a pointer to its "parent" scope, forming a chain.
 * Insert/lookups start in the current scope, then walk up the chain.
 */
class SymbolTable
{
private:
    std::unordered_map<std::string, std::unique_ptr<Symbol>> m_symbols;
    std::shared_ptr<SymbolTable> m_parent;

public:
    /**
     * @brief Constructs a SymbolTable with an optional parent (outer scope).
     */
    SymbolTable(std::shared_ptr<SymbolTable> parent = nullptr)
        : m_parent(parent) {}

    /**
     * @brief Adds a new symbol to the current scope.
     * @throw std::runtime_error if symbol is already declared in this scope.
     */
    void addSymbol(const Symbol &sym);

    /**
     * @brief Looks up a symbol by name, recursively searching parent scopes if necessary.
     * @return A pointer to the Symbol if found, or nullptr if not found.
     */
    const Symbol *lookup(const std::string &name) const;

    /**
     * @brief Gets the parent scope (if any).
     */
    std::shared_ptr<SymbolTable> parent() const { return m_parent; }
};

#endif // EDUCC_SYMBOLTABLE_H
