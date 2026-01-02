# Copilot ESMF/NUOPC Instructions

## 🚀 Quick Start
- Use ESMF/NUOPC to build modular, interoperable Earth system model components in Fortran or C.
- Start from official ESMF/NUOPC templates and prototypes for new components and couplers.
- Use explicit interfaces, intent attributes, and named error/status codes throughout.
- Document all modules, routines, and data structures with Doxygen-style comments.
- Always manage memory explicitly: allocate/deallocate all resources and document ownership.
- Design for parallelism (MPI, OpenMP) and ensure thread/process safety.
- Use ESMF logging and error reporting for diagnostics.

---

## Documentation Requirements
- Use Doxygen `!>` comments for all modules, subroutines, and functions.
- Document all arguments, return values, and error/status codes.
- Reference ESMF/NUOPC API docs for interface details.
- Provide usage examples and expected outputs in comments or documentation.

## Error Handling & Memory Management
- Always use explicit error/status codes and propagate them up the call stack.
- Use named constants for error codes (never magic numbers).
- Deallocate all allocatable arrays and pointers when no longer needed.
- Document which routine is responsible for allocation/deallocation.
- Use ESMF error handling macros and logging utilities.

## Parallelism & Interoperability
- Review all new code for thread/process safety (OpenMP, MPI, ESMF parallelism).
- Use ISO_C_BINDING for C/Fortran interoperability where needed.
- Document any interfaces intended for use by other languages or frameworks.
- Use ESMF/NUOPC FieldBundles, States, and Grids for data exchange.

## Testing & CI
- All new features must include unit and integration tests (see task-implementation instructions).
- Use ESMF/NUOPC test harnesses and driver scripts for component testing.
- All tests must pass in CI before merging.

## Do/Don’t Table
| Do | Don’t |
|----|-------|
| Use explicit interfaces and intent attributes | Use implicit typing |
| Document all routines and modules | Leave code undocumented |
| Free all memory you allocate | Cause memory leaks |
| Use named error/status codes | Use magic numbers |
| Write unit tests for new features | Skip testing |

## Common Pitfalls
- Missing or incomplete docstrings.
- Not freeing memory or not handling allocation errors.
- Not using `intent` attributes for all arguments.
- Not propagating error/status codes.
- Not testing edge cases or error conditions.

## Templates & Examples
- Use provided ESMF/NUOPC templates and prototype apps: https://github.com/esmf-org/nuopc-app-prototypes
- See [ESMF Reference Manual](https://earthsystemmodeling.org/docs/release/latest/ESMF_refdoc/), [ESMF User Guide](https://earthsystemmodeling.org/docs/release/latest/ESMF_usrdoc/), and [NUOPC Reference](https://earthsystemmodeling.org/docs/release/latest/NUOPC_refdoc/) for detailed API usage and examples.

---

## AI Prompt Engineering & Safety Best Practices

For comprehensive guidelines on prompt engineering, AI safety, bias mitigation, and responsible AI usage, see:

- [ai-prompt-enginering-safety-best-practices.instructions.md](./ai-prompt-engineering-safety-best-practices.instructions.md)

All contributors and users of Copilot should follow these best practices to ensure safe, effective, and ethical use of AI in this project.

---

For more details, see the full language- and task-specific instructions in this directory.
