# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is Adam's personal Emacs configuration repository, tracking since 1999. It contains a comprehensive collection of Emacs Lisp configuration files, custom utilities, and automation scripts for Emacs development and usage.

## Development Commands

### Build and Compilation
```bash
# Byte-compile all .el files and generate autoloads
cd lib/emacs && make all

# Compile specific files  
cd lib/emacs && make filename.elc

# Clean compiled files
cd lib/emacs && make clean

# Install compiled files to version-specific directories
cd lib/emacs && make install

# Force reinstall (clean + install)
cd lib/emacs && make force-install

# Generate autoload definitions
cd lib/emacs && make loaddefs
```

### Testing and Debugging
```bash
# Debug compilation of specific file
cd lib/emacs && make filename.elc.debug

# Test emacs startup without full initialization
EMACS_BATCH=1 emacs -q --no-site-file

# Quick startup mode for testing
QUICK_EMACS=1 emacs

# Debug init process
DEBUG_EMACS_INIT=1 emacs
```

### Deployment
```bash
# Deploy configuration using GNU Stow
stow -d . -t ~ .
```

## Architecture

### Bootstrap and Loading System

The configuration uses a sophisticated hook-based loading system:

1. **`.emacs`** - Entry point that loads the hook system via `as-pre-init-d.el`
2. **`.emacs.d/init.d/as-pre-init-d.el`** - Bootstrap that sets up load paths and core infrastructure:
   - Loads `as-load-paths.el` to configure all library directories
   - Initializes `as-progress.el` for startup progress tracking
   - Sets up `as-package-loading.el` for package management with straight.el
3. **External Hook Discovery** - Uses shell environment (`$ZDOT_FIND_HOOKS`) to dynamically discover and load all `.el` files in `init.d/`

### Package Management

- **Primary**: `straight.el` for version-aware dependency tracking
- **Configuration**: `use-package` for declarative package setup with autoloads and key bindings
- **Legacy support**: Custom Makefile-based compilation system in `lib/emacs/`

### Directory Structure

- **`.emacs.d/init.d/`** - Main configuration modules (80+ files), all auto-loaded
- **`.emacs.d/lib/`** - Custom libraries intended for eventual packaging
- **`lib/emacs/`** - Legacy utilities and build system
  - `minor-modes/msf-abbrev/` - Extensive abbreviation system with mode-specific expansions
  - `utils/` - General-purpose Emacs utilities
- **`bin/`** - Shell scripts for Emacs interaction and automation

### Configuration Conventions

- **Naming**: `as-` prefix for all custom functions, variables, and files
- **Modular design**: Each `init.d/` file handles a specific domain (languages, features, integrations)
- **Version awareness**: Separate custom files per Emacs version in `.emacs.d/custom/`
- **Environment integration**: Heavy use of shell environment and external tools

### Key Configuration Areas

- **Language support**: Dedicated files for major languages (Python, Perl, Ruby, Go, JavaScript, etc.)
- **Org-mode integration**: Multiple files for agenda, capture, effort tracking, and integrations
- **Development tools**: Git, LSP, linting, compilation, and project management
- **Communication**: Mail (notmuch, mutt), messaging, and external integrations
- **UI/UX**: Themes, fonts, mode-line, and window management

### Build System Details

The `lib/emacs/Makefile` provides:
- **Automatic discovery** of `.el` files for compilation
- **Load path management** for compilation dependencies  
- **Version-specific installation** to `~/lib/emacs/GNU_Emacs/VERSION/SYSTEM/`
- **Autoload generation** for custom functions across the codebase
- **Third-party package integration** support

## Important Files

- **`.emacs`** - Main entry point and hook system loader
- **`.emacs.d/init.d/as-vars.el`** - Core variable definitions and directory paths
- **`.emacs.d/init.d/as-package-loading.el`** - Package management setup
- **`lib/emacs/Makefile`** - Build system for byte-compilation and installation