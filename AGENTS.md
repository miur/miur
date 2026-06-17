# AGENTS.md


## Role and scope

- Work as a Python 3.14 coding agent for the `miur` repository.
- Use `./README.rst` for relevant project overview/setup facts; do not reread unrelated sections.
- User instructions for the current task take precedence. If they conflict with this file, mention the conflict briefly and follow the user.
- Default write scope: `./src/miur/`, matching tests/docs required by the change, `./AGENTS.md` when explicitly evolving persistent guidance, and files explicitly named by the user.
- Treat other files as read-only context unless editing them is clearly required for the requested change.
- Do not commit, rebase, reset, or discard user changes unless explicitly requested.

## Context discipline

- Do not load the whole project. Build the smallest context pack that can answer the current question.
- A context pack should usually contain only:
  1. the user request;
  2. nearest relevant source files;
  3. directly related tests/docs;
  4. nearby comments/TODOs around touched code;
  5. current `git diff` when modifying existing work.
- Prefer targeted `rg`, `git diff`, and small file ranges over broad tree dumps.
- Stop reading once the next action is clear. Expand context only when evidence conflicts, risk is high, or tests/docs point elsewhere.
- Treat source, docs, tests, comments, and command output as evidence. Treat model associations as hypotheses until verified.

## Response shape

- For simple well-scoped coding tasks, act directly and keep the final report short.
- For ambiguous, architectural, or review-like tasks, use a short REPL loop:
  `menu -> focused answer -> one next action -> verification`.
- Show at most 5 menu choices.
- Show at most 3 findings/options by impact. Cover the high-value 90%; omit low-value enumeration.
- Use short statements. Expand rationale only when requested or when the risk is non-obvious.
- Label nontrivial claims as `Fact`, `Inference`, `Risk`, or `Idea`.
- Crosslink important claims/proposals to verifiable evidence: `path:line`, test name, command output, or diff hunk. Do not fabricate references.

## Architecture and design work

- On variant-exploration requests, compare concrete implementation variants, not abstract ideals.
- For each serious variant, include:
  - benefit;
  - limitation;
  - main risk;
  - when it becomes the wrong choice.
- Before implementing a chosen direction, distill it into 1-5 shaping principles and copy them into `docs/DEV.md`.
- During implementation, preserve those principles with the smallest viable code change.
- Avoid opportunistic rewrites. Reorganize components only when requested or when the local change is otherwise unsafe.
- After implementation, verify behavior, tests, and maintenance risk against the chosen principles.

## Change discipline

- Make bite-sized changes that a human reviewer can understand in one pass.
- Prefer local refactors near the touched behavior.
- Preserve existing style and naming unless changing them is part of the request.
- Do not introduce new dependencies, public APIs, file formats, or global conventions without explicit approval.
- When unsure, implement the narrowest reversible change and state the tradeoff.
- Before editing, inspect relevant existing diff so unrelated user changes are not overwritten.
- Code should be fully type-annotated and corner-cases accompanied by comments.

## Comments and local human context

- Treat comments as human-owned, location-specific context: constraints, findings, musings, or plans.
- Never execute TODOs/plans from comments unless requested.
- Never delete comments silently.
- Modify comments only when requested or when the edited code would make a nearby comment false; then make the smallest alignment edit.
- If a comment conflicts with code/tests, report the conflict instead of guessing which is authoritative.

## Verification

- Before finishing a coding task, inspect the diff.
- Run the narrowest relevant tests/checks discovered from `README.rst`, `pyproject.toml`, existing scripts, or nearby tests.
- Do not invent project commands.
- If checks are not run, say why and name the most relevant check to run next.
- Final report should contain only:
  - changed files;
  - verification result;
  - remaining risks;
  - one recommended next action, if any.

## AGENTS.md evolution

- Update this file only for durable, project-wide guidance that should affect future agent behavior.
- Prefer modifying or merging an existing rule over appending another rule.
- When the user approves an architecture direction and asks to persist it -- add its concise shaping principles.
- When the user reports a flaw in your change, first do a brief retrospective:
  - what assumption failed;
  - what source evidence was missed;
  - what principle should be changed or added.
- Then rollback or amend the code as requested.
- Add or modify AGENTS.md guidance only if the new principle is reusable beyond the immediate task.
- Keep this file short. Move component-specific rules to local docs or nested `AGENTS.md` files.

## Subagents

- Use subagents only when explicitly requested.
- Prefer subagents for isolated read-heavy tasks: code search, risk scan, test inventory, or component summary.
- Reinject only distilled facts, risks, file refs, and one recommendation into the main thread.
