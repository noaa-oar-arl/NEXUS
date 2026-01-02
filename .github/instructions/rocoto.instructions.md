# Copilot Rocoto Instructions

## 🚀 Quick Start
- Use Rocoto XML to define workflows, tasks, dependencies, and resources for weather and climate simulations.
- Always include the required XML header and use ENTITIES for constants to improve maintainability.
- Modularize workflows: break large processes into small, reusable tasks with well-defined input/output.
- Use <metatask> for templating and to avoid repetitive XML blocks.
- Document all tasks, cycles, and dependencies with XML comments for clarity.
- Prefer explicit, readable time and resource specifications (e.g., use `0:01:00:00` instead of seconds).
- Use the `rocotorun` command iteratively (e.g., via cron) to advance workflow state.

---

## Documentation Requirements
- Add XML comments to explain workflow structure, task purpose, and non-obvious dependencies.
- Document all ENTITIES and their intended use at the top of the XML file.
- Use descriptive names for tasks, metatasks, and variables.
- Provide example usage and expected outputs in comments.

## Error Handling & Reliability
- Ensure all workflow tasks return correct exit codes (0 for success, non-zero for failure).
- Avoid embedding automation or retry logic in scripts; let Rocoto handle retries via `maxtries`.
- Use <rewind> tags for cleanup actions when tasks are rerun.
- Use <hangdependency> to detect and handle hung jobs.
- Make tasks idempotent whenever possible to support safe reruns.

## Modularity & Maintainability
- Use ENTITIES for all repeated paths, parameters, and constants.
- Use <metatask> and <var> for templating similar tasks (e.g., ensemble members, forecast hours).
- Group related cycles with <cycledef group="..."> and use the `cycledefs` attribute in tasks.
- Use <dependency> tags to clearly define task, data, and time dependencies.
- Avoid circular dependencies and ensure all referenced tasks/metatasks are defined above their use.

## Resource & Throttling Controls
- Use <cyclethrottle>, <corethrottle>, and <taskthrottle> in <workflow> to control concurrency and resource usage.
- Specify <cores>, <nodes>, <walltime>, <memory>, <queue>, and <account> for each task as appropriate.
- Use <deadline> and <log> tags for robust tracking and monitoring.

## Testing & CI
- Validate XML with `xmllint` or similar tools before use.
- Test workflows with small cycle ranges and mock scripts before production runs.
- Use `rocotostat` and `rocotocheck` to monitor workflow status and debug issues.
- All workflow changes should be reviewed and tested before merging.

## Do/Don’t Table
| Do | Don’t |
|----|-------|
| Use ENTITIES for constants | Hardcode paths or values |
| Modularize with <metatask> | Repeat similar task blocks |
| Document all tasks and cycles | Leave XML undocumented |
| Use <rewind> for cleanup | Ignore rerun/cleanup needs |
| Use <dependency> for logic | Implicit or unclear dependencies |

## Common Pitfalls
- Missing or incorrect XML headers or ENTITIES.
- Not handling task failures or cleanup on rerun.
- Overly complex or monolithic tasks; lack of modularity.
- Unclear or undocumented dependencies.
- Not validating XML or testing workflows before production.

## Templates & Examples
- Use provided XML templates for new workflows and tasks.
- See [official Rocoto documentation](https://christopherwharrop.github.io/rocoto/) for detailed tag usage and examples.

---

## AI Prompt Engineering & Safety Best Practices

For comprehensive guidelines on prompt engineering, AI safety, bias mitigation, and responsible AI usage, see:

- [ai-prompt-enginering-safety-best-practices.instructions.md](./ai-prompt-enginering-safety-best-practices.instructions.md)

All contributors and users of Copilot should follow these best practices to ensure safe, effective, and ethical use of AI in this project.

---

For more details, see the full language- and task-specific instructions in this directory.
