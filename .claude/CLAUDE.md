# Claude Configuration for shinyloadtest

This file provides Claude AI assistant with context and guidelines for working with the shinyloadtest package.

## Purpose

This configuration helps Claude:
- Understand the package structure and purpose
- Follow project conventions and style
- Use appropriate development commands
- Provide more accurate and context-aware assistance

See also `.github/copilot-instructions.md` for GitHub Copilot-specific guidance.

---

## Overview

`shinyloadtest` is an R package for load testing Shiny applications. It helps developers:
1. Assess how many concurrent users their Shiny apps can support
2. Identify performance bottlenecks
3. Guide optimization efforts

## Core Functionality

### Three Main Components

1. **Recording**: Capture user sessions from a Shiny application
   - Records WebSocket messages and HTTP requests
   - Saves session data for replay
   - Main function: `record_session()`

2. **Load Generation**: Replay recorded sessions at scale
   - Uses shinycannon (external tool) to generate load
   - Simulates multiple concurrent users
   - Collects timing and performance metrics

3. **Analysis**: Process and visualize results
   - Parse load test output
   - Generate performance reports
   - Create visualizations of app performance
   - Main functions: `load_runs()`, `shinyloadtest_report()`

## Architecture

### Key Modules

- **Recording System** (`R/shiny-recorder.R`)
  - Implements a proxy server to record sessions
  - Handles WebSocket and HTTP traffic
  - Manages authentication

- **Analysis Engine** (`R/analysis.R`)
  - Processes load test results
  - Calculates performance metrics
  - Generates statistics

- **Visualization** (`R/plotting.R`)
  - Creates performance plots
  - Visualizes timing distributions
  - Shows concurrency patterns

- **Report Generation** (`R/make_report.R`)
  - Produces HTML reports
  - Integrates plots and metrics
  - Provides actionable insights

- **Utilities**
  - `R/url.R` - URL manipulation and construction
  - `R/detect.R` - Shiny app detection
  - `R/auth.R` - Authentication handling
  - `R/enum.R` - Enumeration types

## Development Workflow

### Typical Development Cycle

1. Make changes to R code
2. Update documentation with `make document`
3. Run tests with `devtools::test()`
4. Install locally with `make devinstall`
5. Test manually if needed
6. Run R CMD check before committing

### Important Commands

```bash
make devinstall  # Quick install for development
make document    # Update documentation
make rcmdcheck   # Full CRAN check
make site        # Build package website
```

## Testing Strategy

### Test Coverage

- Unit tests for core functions
- Integration tests for recording/playback
- URL construction tests
- Report generation tests
- Error handling tests

### Running Tests

```bash
# All tests
R -e "devtools::test()"

# Specific test file
R -e "testthat::test_file('tests/testthat/test-url-construction.R')"
```

## Dependencies

### Runtime Dependencies

- **R6**: Object-oriented programming
- **websocket**: WebSocket client/server
- **httpuv**: HTTP server
- **dplyr**: Data manipulation
- **ggplot2**: Plotting
- **vroom**: Fast file reading
- **jsonlite**: JSON parsing

### Development Dependencies

- **devtools**: Development tools
- **testthat**: Testing framework
- **pkgdown**: Website generation
- **roxygen2**: Documentation

### External Tools

- **shinycannon**: Load generation tool (separate project)
- **pandoc**: Document conversion

## Code Style

### Conventions

- Use tidyverse style guide
- snake_case for functions and variables
- Roxygen2 for documentation
- 80 character line limit (when feasible)

### Documentation

- All exported functions must have roxygen2 docs
- Include examples where appropriate
- Document parameters and return values
- Add `@export` tag for public functions

### Error Handling

- Use `cli` package for user-facing messages
- Validate inputs early
- Provide helpful error messages
- Use `rlang::abort()` for errors

## Common Tasks

### Adding a New Analysis Function

1. Add function to `R/analysis.R`
2. Write roxygen2 documentation
3. Add unit tests to `tests/testthat/test-analysis.R`
4. Update `make document`
5. Add example to vignette if user-facing

### Adding a New Plot Type

1. Add function to `R/plotting.R`
2. Use ggplot2 for consistency
3. Follow existing plot styling
4. Document with examples
5. Test with demo data

### Updating Demo Data

```bash
make demo_data
```

This regenerates the example datasets used in vignettes and examples.

## Performance Considerations

- Load tests can generate large datasets
- Use `vroom` for fast file reading
- Consider memory usage in analysis functions
- Optimize plotting for large datasets

## Release Process

1. Update version in DESCRIPTION
2. Update NEWS.md with user-facing changes
3. Run full test suite
4. Run R CMD check as CRAN
5. Update cran-comments.md
6. Submit to CRAN

## Troubleshooting

### Common Issues

1. **Tests failing**: Check if demo data is up to date
2. **Documentation not updating**: Run `devtools::clean_vignettes()` first
3. **Package won't load**: Reinstall with `make devinstall`
4. **Plots not rendering**: Check if svglite is installed

## Related Projects

- **shinycannon**: The load generation tool (Go-based)
- **shiny**: The web framework being tested
- **httpuv**: Underlying HTTP server

## Resources

- [Package website](https://rstudio.github.io/shinyloadtest/)
- [GitHub repository](https://github.com/rstudio/shinyloadtest)
- [Issue tracker](https://github.com/rstudio/shinyloadtest/issues)
- [Shiny documentation](https://shiny.posit.co/)

## Tips for Claude

When working on this package:
- Always read existing code before suggesting changes
- Follow the established patterns in the codebase
- Test changes with the demo data
- Consider backwards compatibility
- Document any new user-facing functions
- Update NEWS.md for user-visible changes
- Use the Makefile commands for common tasks
- Run tests before committing
