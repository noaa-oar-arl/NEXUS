# Copilot Bash Instructions

## 🚀 Quick Start
- Write clear, modular, and well-documented Bash scripts.
- Use comments to explain non-obvious logic and all function definitions.
- Prefer POSIX-compliant syntax for portability unless project requires Bash-specific features.
- Always check for errors after running commands; use `set -euo pipefail` at the top of scripts for safety.
- Use functions for repeated logic and group related commands.
- Validate input arguments and provide helpful usage messages.
- Avoid hardcoding paths; use variables and parameter expansion.
- Use `trap` to handle cleanup on exit or error.
- All new scripts must include basic tests or usage examples.
- Do not modify whitespace or formatting in existing scripts without justification.

---

## Documentation Requirements
- Add a header comment to every script with purpose, usage, and author info.
- Comment all functions and complex logic blocks.
- Use consistent indentation (2 or 4 spaces, no tabs unless required).
- Document environment variables and dependencies.

## Error Handling & Safety
- Use `set -euo pipefail` for robust error handling.
- Check exit codes (`$?`) after critical commands if not using `set -e`.
- Use `trap` to clean up temporary files or revert changes on error/exit.
- Validate all user input and arguments.
- Avoid using `rm -rf` or destructive commands without checks and confirmations.

## Portability & Interoperability
- Prefer POSIX-compliant syntax for maximum portability.
- Use `#!/usr/bin/env bash` as the shebang for Bash scripts.
- Avoid Bash-only features unless necessary; document when used.
- Test scripts on multiple platforms if possible.

## Testing & CI
- Include usage examples or test cases in comments or as part of the script.
- Scripts should be linted with `shellcheck` and pass without errors.
- All scripts must be tested in CI before merging.

## Do/Don’t Table
| Do | Don’t |
|----|-------|
| Use `set -euo pipefail` | Ignore error handling |
| Comment all functions and logic | Leave code undocumented |
| Validate all input | Assume input is always valid |
| Use functions for repeated logic | Repeat code unnecessarily |
| Use `shellcheck` for linting | Ignore lint warnings |

## Common Pitfalls
- Missing or incomplete comments and usage info.
- Not handling errors or failing to clean up on exit.
- Using Bash-only features without documentation.
- Hardcoding paths or values.
- Not validating user input or arguments.

## Templates & Examples
- Use provided script and function templates for new Bash scripts.
- See [python.instructions.md](./python.instructions.md) and [fortran.instructions.md](./fortran.instructions.md) for documentation style examples.

---

## AI Prompt Engineering & Safety Best Practices

For comprehensive guidelines on prompt engineering, AI safety, bias mitigation, and responsible AI usage, see:

- [ai-prompt-enginering-safety-best-practices.instructions.md](./ai-prompt-enginering-safety-best-practices.instructions.md)

All contributors and users of Copilot should follow these best practices to ensure safe, effective, and ethical use of AI in this project.

---

For more details, see the full language- and task-specific instructions in this directory.
