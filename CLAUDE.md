# MBOA Analysis Development Guide

## Project Structure
- R package with DESCRIPTION file, NAMESPACE, and Roxygen documentation
- Functions organized in R/ folder by data processing stages (ingest, prepare, transform, etc.)
- Uses config.yml for data file paths configuration

## Development Commands
- Load all functions: `devtools::load_all()`
- Generate documentation: `devtools::document()`
- Check package: `devtools::check()`
- Find undocumented functions: `source("utils/find_functions_without_documentation.R")`
- Package management: `source("utils/manage_packages.R")`

## Code Style
- Use 4-space indentation
- Function names use snake_case (e.g., `ingest_students_demographics`)
- Use Roxygen2 with markdown for all function documentation
- Include @description, @param, @return tags in documentation
- Use `globalVariables()` for NSE variables used with dplyr
- Explicit imports with `@importFrom` tags
- Use |> pipeline operator (not %>%)
- Place function parameters on separate lines for clarity
- Prefer the happy path principle with guard clauses above nested if's
- Try to avoid nested for loops as well
- Use tidyverse styleguide when in doubt
- Never use library calls, check if package is loaded in `utils/manage_packages.R`

## Naming Conventions
- Function names should be verbs (get_*, save_*, load_*)
- Variable names should be descriptive and self-documenting
- Use consistent prefixes for related functions
- Have a preference for pacakges already mentioned in utils/manage_packages.R
- Secondly properply supported packages, firstly from posit (like tidyverse and tidymodels)

## Dependencies
- Uses renv for dependency management
- Add new dependencies to vectors in utils/manage_packages.R
- Handle conditional package loading with `requireNamespace()`

## Error Handling
- Use validation checks before operations
- Employ tryCatch blocks for file operations
- Format error and warnings with messages from cli-package
- Use rlang::abort instead of stop
- Handle NULL values defensively

## Quarto style
- All codeblocks should start with `#| label:` and then a descriptive and unique label
- Think of code blocks as a first iteration towards a stand-alone function
- Preparation and visualisation should often have different codeblocks
- Use Dutch language in between code blocks to explain the rationale, not the code

## Git Workflow
- Always pull before committing: `git pull`
- Use short, concise commit messages with the format:
  - `fix: short description` (for bug fixes)
  - `feat: short description` (for new features)
  - `docs: short description` (for documentation changes)
  - `chore: short description` (for maintenance tasks)
- Example: `git commit -m "fix: add user to group to solve permission issue"`