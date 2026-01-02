---
description: 'Documentation and content creation standards'
applyTo:
  - '**/*.F90'
  - '**/*.f90'
  - '**/*.F'
  - '**/*.f'
  - '**/*.f03'
  - '**/*.f08'
  - '**/*.f2008'
  - '**/*.f2018'
---
## Fortran Content Rules

The following markdown content rules are enforced in the validators:

- Write clear and concise descriptions.
- Ensure functions have descriptive names
- Provide docstrings following doxygen comments
- Break down complex functions into smaller, more manageable functions.
- **Modules**: Use modules for code organization and encapsulation.
- **Standard**: Always use **Fortran 2008+**.
- **Interfaces**: Use interfaces for procedure declarations and type definitions.
- **Abstract Types**: Use abstract types for polymorphism and code reuse.
- **Fortran 2008 Features**: Take advantage of Fortran 2008 features like coarrays and submodules.
- **Formatting**: Always indent with 2 spaces.
- **Error Handling**: Always use best practices for error handling.
- **Declarations**:
  - Always include `implicit none`.
  - Place **all variable declarations at the top**.
  - Always use explicit `intent(in)`, `intent(out)`, `intent(inout)`.
- **Parallel Programming**:
  - Shared memory: Use **OpenMP (`!$omp`)**.
  - Distributed memory: Use **MPI** (non-blocking comms preferred).
  - Use **coarrays** where appropriate.
- **Memory Management**:
  - Prefer `allocatable` arrays (never raw pointers unless required by API).
  - Always `deallocate` before reallocation.
  - Use derived types with allocatable components.

## Formatting and Structure

Follow these guidelines for formatting and structuring your markdown content:
- **Documentation**:
  - Doxygen `!>` comments required for all public routines, modules, and interfaces.
  - Structure comments for **mkdoxygen → MkDocs** output.
- **ESMF v8.8+**:
  - Components built as `GridComp`, `CplComp`, `State`.
  - Use `ESMF_Grid`, `ESMF_Mesh`, or `ESMF_LocStream`.
  - Include metadata (units, masks, CF conventions).
  - Use ESMF logging (`ESMF_LogErr`) and finalize with `ESMF_Finalize()`.
  - Use `ESMF_LogFoundError` for error checking.

## General Instructions

- Always prioritize readability and clarity.
- For algorithm-related code, include explanations of the approach used.
- Write code with good maintainability practices, including comments on why certain design decisions were made.
- Handle edge cases and write clear exception handling.
- For libraries or external dependencies, mention their usage and purpose in comments.
- Use consistent naming conventions and follow language-specific best practices.
- Write concise, efficient, and idiomatic code that is also easily understandable.

## Edge Cases and Testing

- Always include test cases for critical paths of the application.
- Account for common edge cases like empty inputs, invalid data types, and large datasets.
- Include comments for edge cases and the expected behavior in those cases.
- Write unit tests for functions and document them with docstrings explaining the test cases.


## 🤖 Copilot Behavior Instructions
When generating code:
- **Fortran**:
  - Always Fortran 2008+.
  - Always `implicit none`, with declarations at the top.
  - Always include **Doxygen comments** for mkdoxygen.
  - Use `allocatable` arrays.
  - Suggest OpenMP/MPI for parallel sections and ensure that all procedures are thread-safe.
  - Use **ESMF v8.8+ constructs** for coupling and regridding.

- **Documentation**:
  - Generate docstrings/comments using doxygen.

---

## Example: Best Practices for Doxygen Comments in Fortran

Below is an example Fortran function with Doxygen-style documentation, following best practices for clarity, completeness, and integration with automated documentation tools.

```fortran
!> @brief Compute the area of a rectangle.
!!
!! This function calculates the area given the rectangle's width and height.
!! It demonstrates best practices for Doxygen comments in Fortran, including
!! parameter descriptions, return value, and revision history.
!!
!! @param[in]  width   Width of the rectangle (meters)
!! @param[in]  height  Height of the rectangle (meters)
!! @return     area    Area of the rectangle (square meters)
!!
!! @author     Jane Doe
!! @date       2025-08-27
!! @version    1.0
!! @note       This is a simple example for documentation purposes.
!!
!! @example
!!   real :: a
!!   a = rectangle_area(3.0, 4.0)  ! a = 12.0
!!
real function rectangle_area(width, height)
  real, intent(in) :: width   !< Width of the rectangle (m)
  real, intent(in) :: height  !< Height of the rectangle (m)

  rectangle_area = width * height
end function rectangle_area
```

**Best Practices:**
- Use `!>` for Doxygen comment blocks and `!!` for continued lines.
- Always document all parameters, return values, and provide a brief summary.
- Include author, date, version, and example usage if relevant.
- Use `@param[in]`, `@param[out]`, `@return`, and other Doxygen tags for clarity.
- Place the Doxygen block immediately before the function or subroutine.
- Use inline comments for variable descriptions as needed.

---

## Fortran Style Guide and Best Practices

### 1. Coding Style
- Use `implicit none` in every program unit.
- Indent code blocks by 2 or 4 spaces (be consistent).
- Use lowercase for variable and procedure names (e.g., `my_var`, `compute_flux`).
- Use CamelCase for module names (e.g., `AtmosPhysics_Mod`).
- Use underscores to separate words in variable and procedure names.
- Limit line length to 100 characters.
- Place one statement per line.
- Use spaces around operators and after commas.
- Comment complex logic and all public APIs.

### 2. Explicit Interfaces and Types
- Always use explicit interfaces for public routines (via modules or interface blocks).
- Prefer modern Fortran modules over `include` files.
- Use `intent(in)`, `intent(out)`, or `intent(inout)` for all dummy arguments.
- Use `real(kind=...)` and `integer(kind=...)` for all floating-point and integer variables.

### 3. Error Handling Patterns
- Use integer return codes for error/status reporting.
- Define named constants for error codes (e.g., `SUCCESS = 0`, `ERR_FILE_NOT_FOUND = 10`).
- Propagate errors up the call stack and handle them at the top level.
- Document all error codes in the interface.

### 4. Unit and Integration Testing
- Write unit tests for all new routines (e.g., using pFUnit or custom test harnesses).
- Place tests in a dedicated `tests/` directory.
- Document how to run and extend tests in the repository README.
- Test edge cases and error conditions.

### 5. Modularization and Encapsulation
- Organize code into small, focused modules with clear APIs.
- Minimize use of global/module variables; prefer passing arguments.
- Use `private` by default in modules, and `public` only for intended APIs.
- Group related procedures and data in modules.

### 6. Memory Management
- Always deallocate allocatable arrays and pointers when no longer needed.
- Document which routine is responsible for allocation and deallocation.
- Avoid memory leaks by checking allocation status before allocating or deallocating.

### 7. Parallelism and Performance
- Use OpenMP/MPI/ESMF parallel constructs where appropriate.
- Document thread-safety and parallelization assumptions in routines.
- Avoid global state in parallel regions.
- Profile and optimize performance-critical code.

### 8. Interoperability
- Use `ISO_C_BINDING` for C/Fortran interoperability.
- Document any interfaces intended for use by other languages.
- Use `bind(c)` for procedures and derived types shared with C.

### 9. Automation
- Use CI/CD to run tests and check documentation on every PR.
- Automate formatting and linting if possible (e.g., with `fprettify`, `fortran-linter`).
- Require all tests to pass before merging.


