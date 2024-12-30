#ifndef OPTIMIZER_H
#define OPTIMIZER_H

#include <unordered_map>
#include <string>
#include "ir/IR.h"

namespace Optimizer {

class Optimizer {
public:
    Optimizer();
    ~Optimizer();

    // Entry point for optimization
    void optimize(IR::IntermediateRepresentation& ir);

private:
    // Declaration of constants map
    std::unordered_map<std::string, double> constants;

    // Optimization passes
    void peepholeOptimization(IR::IntermediateRepresentation& ir);
    void constantFolding(IR::IntermediateRepresentation& ir);

    // Helper functions
    bool isConstant(const std::string& operand, double& value);
    bool isTemporary(const std::string& operand);
};

} // namespace Optimizer

#endif // OPTIMIZER_H
