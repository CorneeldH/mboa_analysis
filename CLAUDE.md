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
- Function names use snake_case (e.g., `ingest_students_demographics`)
- Use Roxygen2 with markdown for all function documentation
- Include @description, @param, @return tags in documentation
- Use `globalVariables()` for NSE variables used with dplyr
- Explicit imports with `@importFrom` tags

## Dependencies
- Uses renv for dependency management
- Add new dependencies to vectors in utils/manage_packages.R
- Handle conditional package loading with `requireNamespace()`