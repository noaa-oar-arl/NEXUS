# Copilot Instructions.md

## 🚀 Quick Start
- All new code must be clear, modular, and well-documented.
- Use Doxygen (Fortran) or NumPy-style (Python) docstrings for every public routine, class, and module.
- Explicitly manage memory (allocation, deallocation) and error/status codes.
- Design for parallelism and scalability (OpenMP, MPI, ESMF, dask, etc.).
- All new features must include unit tests and pass CI before merging.
- Don't modify whitespace or formatting in existing code without justification.

---

## Documentation Requirements
- **Fortran:** Use Doxygen `!>` comments for all modules, subroutines, and functions. See [fortran.instructions.md](./fortran.instructions.md).
- **Python:** Use NumPy-style docstrings. See [python.instructions.md](./python.instructions.md).
- Documentation must be compatible with MkDocs + mkdocstrings/mkdoxygen.
- Reference and link to well-documented modules as examples.

## Error Handling & Memory Management
- Always use explicit error/status codes and propagate them up the call stack.
- Use named constants for error codes.
- Always deallocate allocatable arrays and pointers when no longer needed.
- Document which routine is responsible for allocation/deallocation.

## Parallelism & Interoperability
- Review all new code for thread/process safety (OpenMP, MPI, ESMF, dask, etc.).
- Use ISO_C_BINDING for C/Fortran interoperability where needed.
- Document any interfaces intended for use by other languages.

## Testing & CI
- All new features must include unit tests (see [task-implementation.instructions.md](./task-implementation.instructions.md)).
- All tests must pass in CI before merging.

## MkDocs Integration
- Documentation is parsed by mkdocstrings/mkdoxygen for publication.
- Use correct formatting and tags for docstrings to ensure proper rendering.

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
- Use provided code and docstring templates for new modules, routines, and tests.
- See [fortran.instructions.md](./fortran.instructions.md) and [python.instructions.md](./python.instructions.md) for examples.

---

## AI Prompt Engineering & Safety Best Practices

For comprehensive guidelines on prompt engineering, AI safety, bias mitigation, and responsible AI usage, see:

- [ai-prompt-enginering-safety-best-practices.instructions.md](./ai-prompt-enginering-safety-best-practices.instructions.md)

All contributors and users of Copilot should follow these best practices to ensure safe, effective, and ethical use of AI in this project.

---

For more details, see the full language- and task-specific instructions in this directory.
