# Repository Guidelines

## Project Structure & Module Organization

This Doom Emacs setup keeps all user-facing configuration under `config.org`, which tangles into `init.el`, `config.el`, `packages.el`, and helper autoloads. Shared utilities live in `autoload.el` and `src/`, while feature-specific tweaks reside under `modules/private/*` (for example, `modules/private/org/` holds org-mode packages, autoloads, and per-topic `+*.org` overrides). Reusable snippets, templates, and assets are stored in `snippets/`, `file-templates/`, and `project-templates/`. Keep tests in `tests.el` and any experimental elisp in `src/project-modes/` until it stabilizes.

## Coding Style & Naming Conventions
Elisp here follows Doom defaults: two-space indentation, lexical-binding where possible, and functional helpers from `dash.el`, `s.el`, and `ht.el`. Private symbols always start with the `my` prefix (`my-namespace/function`, `my|interactive`, `my@macro`, `my*hook`) so upstream code never collides. Prefer template literals via `(t! "Version <<VERSION>>")`, favor pure functions, and keep side effects confined to module-specific files. Document unusual forms inline using terse comments.

Most of the code here will follow a functional mindset using [dash.el](https://github.com/magnars/dash.el) and [s.el](https://github.com/magnars/s.el). I will prefer those functions over built in functions if they are nicer to use.

# Variable Naming

All my private functions have the prefix `my`. It's the only prefix where you can assume that packages/emacs/doom won't override it and vice-versa.

## Schema

-   `my-namespace` Its not pretty, but it's the only way to work with emacs-lisps global scope for everything.
    -   `my-namespace::sub-namespace`
-   `my:variable`: Variable
-   `my/function`: Private function
    -   `my-namespace/function`: Private function with namespace
-   `my|function`: Interactive function
    -   `my-namespace|function`: Interactive function
-   `my@function`: Macro
-   `my*hook`: Hook function

## Testing Guidelines
Unit tests live in `tests.el` and rely on Buttercup’s `describe`/`it` style. Run them headlessly with `emacs -Q --batch -l tests.el -f buttercup-run`, or inside Doom using `M-x buttercup-run-at-point`. Mirror production file names in the test descriptions (e.g., `describe "my-string/trim"`) and add expectations for failure modes, not just the happy path.
