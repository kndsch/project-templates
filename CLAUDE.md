# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands

### Using Task (Recommended)
Use @Taskfile.yml for available commands. Prefer the task versions for running commands

## Project Architecture

This is a Haskell-based project template processor that applies variable substitution and transformations to template files.

### Core Architecture
The application follows a monad transformer stack pattern:
- `AppT` monad transformer: `StateT AppState (ReaderT AppConfig m)`
- Reader monad for immutable configuration (`AppConfig`)
- State monad for mutable application state (`AppState`)
- IO for effects, with structured logging via `MonadLogger`

### Key Components

#### Template Processing Pipeline
1. **Configuration Loading** (`ProjectTemplates.App.Config`): Loads CLI options, validates template directories
2. **Variable Collection** (`ProjectTemplates.App.Variables`): Gathers variable definitions from config and user input
3. **Template Processing** (`ProjectTemplates.Templates.Processor`): Parses `{{variable|transform}}` syntax and applies substitutions
4. **File Processing** (`ProjectTemplates.App.Process`): Applies templates to both file contents and filenames

#### Module Organization
- `ProjectTemplates.Run` - Main application entry point and orchestration
- `ProjectTemplates.App.*` - Application layer (config, state, CLI, file handling)
- `ProjectTemplates.Templates.*` - Template processing engine (parser, transforms, config)

#### Template Syntax
- Variables: `{{variable_name}}`
- Transforms: `{{variable_name|transform1|transform2}}`
- Supports alphanumeric characters, underscore, and hyphen in variable names

### Application Flow
1. Parse CLI arguments and build configuration
2. Read template configuration file (`config.yaml` in template directory)
3. Collect variable definitions (from config and interactive prompts)
4. Execute pre-processing hooks
5. Process all template files (apply variable substitution to content and filenames)
6. Execute post-processing hooks

### Testing
- Test framework: Tasty with Hspec
- Test files in `tests/` directory
- Main test module: `TemplatesSpec.hs`

### Nix Integration
- Uses `haskell-flake` for Nix integration
- Development shell includes `fourmolu`, `cabal-fmt`, and HLS support
- Build with `nix build`, develop with `nix develop`
