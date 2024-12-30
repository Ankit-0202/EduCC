#include "semantic/SymbolTable.h"

namespace Semantic {

// Constructor initializes with a global scope
SymbolTable::SymbolTable() {
  // Initialize with the global scope
  enterScope();
}

// Destructor
SymbolTable::~SymbolTable() {
  // Clear all scopes
  scopes.clear();
}

// Enter a new scope
void SymbolTable::enterScope() {
  scopes.emplace_back(); // Push a new empty scope
}

// Exit the current scope
void SymbolTable::exitScope() {
  if (!scopes.empty()) {
    scopes.pop_back();
  }
}

// Declare a new symbol in the current scope
bool SymbolTable::declare(const std::string &name, SymbolKind kind,
                          const std::string &type) {
  if (scopes.empty()) {
    // No scope to declare in
    return false;
  }

  auto &currentScope = scopes.back();
  if (currentScope.find(name) != currentScope.end()) {
    // Symbol already declared in current scope
    return false;
  }

  currentScope[name] = std::make_shared<Symbol>(kind, name, type);
  return true;
}

// Lookup a symbol in all enclosing scopes
std::shared_ptr<Symbol> SymbolTable::lookup(const std::string &name) const {
  for (auto scope_it = scopes.rbegin(); scope_it != scopes.rend(); ++scope_it) {
    auto it = scope_it->find(name);
    if (it != scope_it->end()) {
      return it->second;
    }
  }
  return nullptr; // Not found
}

} // namespace Semantic
