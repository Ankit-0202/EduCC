#ifndef SYMBOLTABLE_H
#define SYMBOLTABLE_H

#include <string>
#include <unordered_map>
#include <memory>
#include <vector>

// Namespace for semantic analysis
namespace Semantic {

// Enumeration for symbol kinds
enum class SymbolKind {
    VARIABLE,
    FUNCTION
};

// Structure representing a symbol
struct Symbol {
    SymbolKind kind;
    std::string name;
    std::string type;
    // Additional attributes like parameters for functions can be added

    Symbol(SymbolKind symbolKind, const std::string& symbolName, const std::string& symbolType)
        : kind(symbolKind), name(symbolName), type(symbolType) {}
};

// Symbol Table class managing scopes and symbols
class SymbolTable {
public:
    SymbolTable();
    ~SymbolTable();

    // Enter a new scope
    void enterScope();

    // Exit the current scope
    void exitScope();

    // Declare a new symbol in the current scope
    bool declare(const std::string& name, SymbolKind kind, const std::string& type);

    // Lookup a symbol in all enclosing scopes
    std::shared_ptr<Symbol> lookup(const std::string& name) const;

private:
    // Stack of scopes, each scope is a map from name to Symbol
    std::vector<std::unordered_map<std::string, std::shared_ptr<Symbol>>> scopes;
};

} // namespace Semantic

#endif // SYMBOLTABLE_H
