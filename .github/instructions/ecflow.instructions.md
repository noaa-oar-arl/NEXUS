# Copilot ecFlow Instructions

## 🚀 Quick Start
- Use ecFlow to define, schedule, and monitor complex workflows (suites) with dependencies and time triggers.
- Write suite definitions in Python or ECF script, using modular, reusable families and tasks.
- Use variables and includes to avoid duplication and improve maintainability.
- Document all suites, families, and tasks with comments explaining their purpose and logic.
- Use the Python API for programmatic suite generation and advanced logic.
- Test suites with small, mock jobs before production runs.

---

## Documentation Requirements
- Add comments to all suite, family, and task definitions.
- Document all variables, triggers, and dependencies.
- Use descriptive names for suites, families, tasks, and variables.
- Provide example usage and expected outputs in comments or documentation.

## Error Handling & Reliability
- Ensure all tasks/scripts return correct exit codes (0 for success, non-zero for failure).
- Use `complete`, `abort`, and `repeat` attributes to control task and suite behavior on failure.
- Use `late`, `clock`, and `time` attributes to handle time-based triggers and deadlines.
- Use event, meter, and label attributes for robust monitoring and control.
- Make tasks idempotent where possible to support safe reruns.

## Modularity & Maintainability
- Use includes and variables for all repeated paths, parameters, and constants.
- Group related tasks into families for clarity and reusability.
- Use triggers and dependencies to clearly define workflow logic.
- Avoid circular dependencies and ensure all referenced nodes are defined.

## Resource & Scheduling Controls
- Use `limit`, `inlimit`, and `defstatus` to control concurrency and resource usage.
- Specify time, date, and event-based triggers for precise scheduling.
- Use `zombie` and `autocancel` attributes to handle stuck or orphaned tasks.

## Testing & CI
- Validate suite definitions with `ecflow_client --check` or similar tools before use.
- Test workflows with small cycle ranges and mock scripts before production runs.
- Use ecFlowUI or CLI tools to monitor and debug suite execution.
- All workflow changes should be reviewed and tested before merging.

## Do/Don’t Table
| Do | Don’t |
|----|-------|
| Use variables/includes for constants | Hardcode paths or values |
| Modularize with families | Repeat similar task blocks |
| Document all nodes and triggers | Leave suite undocumented |
| Use `complete`/`abort` for logic | Ignore error/cleanup needs |
| Use triggers for dependencies | Implicit or unclear dependencies |

## Common Pitfalls
- Missing or incorrect suite headers or includes.
- Not handling task failures or cleanup on rerun.
- Overly complex or monolithic tasks; lack of modularity.
- Unclear or undocumented dependencies.
- Not validating or testing suite definitions before production.

## Templates & Examples
- Use provided Python or ECF templates for new suites and tasks.
- See [official ecFlow documentation](https://ecflow.readthedocs.io/en/5.14.1/) for detailed usage and examples.

---

## AI Prompt Engineering & Safety Best Practices

For comprehensive guidelines on prompt engineering, AI safety, bias mitigation, and responsible AI usage, see:

- [ai-prompt-enginering-safety-best-practices.instructions.md](./ai-prompt-enginering-safety-best-practices.instructions.md)

All contributors and users of Copilot should follow these best practices to ensure safe, effective, and ethical use of AI in this project.

---

For more details, see the full language- and task-specific instructions in this directory.
