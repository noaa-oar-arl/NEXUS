---
description: 'Python coding conventions and guidelines'
applyTo: '**/*.py'
---

# Python Coding Standards and Best Practices

## 🚀 Quick Start
- Write clear, modular, and well-documented code.
- Use NumPy-style docstrings for all public functions, classes, and modules.
- Include type hints for all function arguments and return values.
- Handle errors with exceptions, not silent failures.
- All new features must include unit tests and pass CI before merging.

---

## Documentation Requirements
- Use NumPy-style docstrings for all public APIs. See [numpydoc guide](https://numpydoc.readthedocs.io/en/latest/format.html).
- Document all arguments, return values, exceptions, and side effects.
- Include example usage in docstrings for complex functions.
- Ensure docstrings are compatible with MkDocs + mkdocstrings.
- Reference well-documented modules as examples.

## Type Hints & Static Analysis
- Use Python type hints for all function arguments and return values.
- Run `mypy` or similar tools to check type correctness.
- Prefer explicit types for clarity and maintainability.

## Error Handling
- Use exceptions for error handling, not return codes.
- Raise specific exception types (e.g., `ValueError`, `TypeError`).
- Document all raised exceptions in the docstring.

## Testing & CI
- Write unit tests for all new features (use `pytest` or `unittest`).
- Place tests in a dedicated `tests/` directory.
- Test edge cases and error conditions.
- All tests must pass in CI before merging.

## Style Guide
- Follow [PEP 8](https://peps.python.org/pep-0008/) for code style.
- Use 4 spaces per indentation level.
- Limit lines to 100 characters.
- Use descriptive variable and function names (snake_case).
- Use docstrings for all modules, classes, and functions.
- Use f-strings for string formatting.

## Modularization & Imports
- Organize code into small, focused modules and functions.
- Use explicit relative imports within packages.
- Avoid wildcard imports (`from module import *`).
- Group standard library, third-party, and local imports separately.

## Automation
- Use CI/CD to run tests and check documentation on every PR.
- Use `black` and `isort` for code formatting and import sorting.
- Use `mypy` for type checking.
- Run `flake8` or `pylint` or `ruff` for linting.

## Do/Don’t Table
| Do | Don’t |
|----|-------|
| Use type hints and docstrings | Leave code undocumented |
| Write unit tests for new features | Skip testing |
| Use exceptions for errors | Fail silently or use return codes |
| Follow PEP 8 | Ignore style guidelines |
| Use f-strings | Use old-style `%` formatting |

## Common Pitfalls
- Missing or incomplete docstrings.
- Not testing edge cases or error conditions.
- Not using type hints or static analysis.
- Not handling exceptions or documenting them.
- Mixing tabs and spaces for indentation.

## Templates & Examples
- Use provided code and docstring templates for new modules, classes, and functions.
- See well-documented modules in the codebase for reference.

---
