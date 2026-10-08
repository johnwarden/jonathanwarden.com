<!-- shared-agents:start (do not edit; run bin/sync-agents-md) -->
# Mental models

Use these when reasoning about design and implementation decisions. Do not name them in code comments or in your output unless naming one clarifies a choice.

- The Map Is Not the Territory: Trust reality over representations; update your map when reality changes.
- Circle of Competence: Know where your competence ends; use it when a decision may exceed your real expertise.
- First Principles Thinking: Strip away inherited assumptions and rebuild from what must be true; use it when convention limits better solutions.
- Second-Order Thinking: Look beyond the immediate effect; use it when downstream consequences may outweigh the first-order payoff.
- Probabilistic Thinking: Hold beliefs with calibrated confidence and update them with evidence; use it under uncertainty or when confidence is high.
- Inversion: Work backward from failure and eliminate its causes; use it when avoiding failure is easier than defining the perfect path.
- Occam's Razor: Prefer fewer unsupported assumptions; use it when multiple explanations fit the facts.
- Feedback Loops: Track what reinforces or stabilizes behavior; use feedback to adjust instead of repeating blindly.
- Bottlenecks: Optimize the constraint limiting the whole system; ignore faster parts until the bottleneck moves.
- Margin of Safety: Build buffers for being wrong; use them where unexpected failure would be costly.
- Law of Diminishing Returns: Expect each extra gain to cost more; stop optimizing when another use of effort offers higher returns.
- Multiply by Zero: Find factors whose failure can negate everything else; protect them before optimizing less critical strengths.
- Global and Local Maxima: Don't confuse the best nearby option with the best overall; accept temporary setbacks when escaping a local optimum may unlock a better one.
- Trade-offs: Treat every choice as giving up alternatives; decide by comparing opportunity costs against your real priorities.

Adapted from Felix Dietze, "Mental Models for LLMs" (https://felx.me/posts/mental-models-for-llms/), itself condensed from Farnam Street's mental models collection.
<!-- shared-agents:end -->

# AGENTS.md

Instructions for any coding agent (human-assisted or autonomous) working in this repository.

Keep this file **agent-general**. Tool-specific setup (Cursor Cloud `environment.json`, session-start hooks, IDE-only notes) belongs under `.cursor/`, not here.

## Trunk

The integration branch this repo fast-forwards onto is `main`. Everywhere this file says **trunk**, that means `main`. Per-repo exceptions belong in the install notes / `.cursor/trunk`, not in this line.

## Merge

Squash the PR to **one commit**, then **fast-forward** onto trunk. That squash commit **is** HEAD of trunk.

- No merge commits
- Rebase-merge is **not** the path (it keeps N commits)
- GitHub’s “Squash and merge” is the button
- **Do not merge** unless Jonathan explicitly says so for that PR (`gh pr merge --squash` is still a merge)
- Never `gh pr merge --merge`. Do not `--rebase` unless he says so for that PR

The squash SHA differs from the PR head. Treat the **code** as identical. Do not write SHA-dependent tests.

## CI and deploy

Test on the PR (the code that becomes trunk). After squash+FF, **deploy immediately**. Do **not** re-run format/compile/test on push to trunk (that is how a post-merge red happens after deploy already shipped). Trunk workflows may deploy. Those deploy workflows need `concurrency: group: deploy-production` and `cancel-in-progress: true` so two pushes cannot race and land the older SHA last.

Branch protection must **require** those PR checks so untested code cannot merge.

When CI fails on a PR, notify or resume the agent that owns that branch. Do not poll. Do not merge to “fix” CI.

## GitHub settings (human, once per repo)

Settings → General → Pull Requests:

- Allow merge commits: **off**
- Allow squash merging: **on**
- Allow rebase merging: **off**

Settings → Branches → rule on trunk:

- Require linear history: **on**
- Require the PR checks before merge

Bots do not flip admin settings from a shared machine.

## Git hooks

If this repo has `.githooks`, environment setup must set `core.hooksPath=.githooks`. Do **not** `git commit` or `git push --no-verify` unless Jonathan says so. CI is the backstop, not the only gate.

## Incomplete work

The Bot that owns this repo owns open PRs, CI, merge conflicts, and drafts. Check at the weekday 8:56 America/Denver run and whenever a signal arrives. Act without waiting to be nudged. Stay silent if nothing is new.

When trunk moves: rebase remaining **non-parked** feature/`cursor/*` PRs. Skip PRs Jonathan has parked (do not nag, do not rebase).

## Do not

- Put tokens, keys, or secrets in this repo, in docs, or in chat
- Merge, spend, publish, or send external mail unless Jonathan says so
- Enable a live bot or production flag unless he says so

## Project

Hugo personal site (Stack theme), live at https://jonathanwarden.com/, GitHub Pages.

Some Social Protocols essays are Hugo-mounted from the `syndication-sources/social-protocols` submodule. Canonical URLs for those essays point at social-protocols.org. Do not clone other repos into this workspace.

Do not draft new posts or rewrite Jonathan’s voice. Take markdown he wrote, open PRs, wait for his yes before merge.

There is no README and no `.githooks`. This is a static site with no secrets; do not add `secrets.sh` or Bitwarden wiring.

Devbox+direnv is mandatory locally. Run `direnv exec . just check` before claiming done. Commands are `just` recipes:

- `just serve` — `hugo server` (initializes theme and syndication submodules first)
- `just build` — production Hugo build (`hugo --gc --minify`)
- `just check` — verify the production build
- `just linkcheck` — optional link check (needs the local server on port 1313)

Deploy: `.github/workflows/hugo.yaml` (`Deploy Hugo site to Pages`) on push to `main` and `workflow_dispatch`. Checkout uses `submodules: recursive`. Build is Hugo extended **0.145.0**: `hugo --gc --minify --baseURL …`, then GitHub Pages. There is no PR test workflow.
