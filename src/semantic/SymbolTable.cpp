/**
 * @file SymbolTable.cpp
 * @brief Implementation of the SymbolTable class.
 */

#include "semantic/SymbolTable.h"

void SymbolTable::addSymbol(const Symbol &sym)
{
    auto it = m_symbols.find(sym.name);
    if (it != m_symbols.end()) {
        // Symbol is already declared in this scope
        throw std::runtime_error("Redeclaration error: symbol '" + sym.name + "' already declared in this scope.");
    }

    // Insert a copy of sym into our map
    m_symbols[sym.name] = std::make_unique<Symbol>(sym);
}

const Symbol *SymbolTable::lookup(const std::string &name) const
{
    auto it = m_symbols.find(name);
    if (it != m_symbols.end()) {
        return it->second.get();
    }
    // not found, try parent
    if (m_parent) {
        return m_parent->lookup(name);
    }
    return nullptr; // not found in any scope
}
