# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a personal dotfiles repository managed by [dotdrop](https://github.com/deadc0de6/dotdrop), a dotfile management tool that enables deployment of configuration files across different machines and profiles.

## Core Commands

### Dotdrop Management
- `./dotdrop.sh install` - Install dotfiles for the current profile
- `./dotdrop.sh install -p <profile>` - Install dotfiles for a specific profile
- `./dotdrop.sh compare` - Compare installed dotfiles with repository versions
- `./dotdrop.sh update` - Update repository with changes from installed dotfiles
- `./dotdrop.sh profiles` - List available profiles

### Available Profiles
- `hyper-city` - Home laptop setup (60 dotfiles) - default profile
- `ismd-work` - Work desktop setup (67 dotfiles)
- `ismd-black` - Alternative setup (67 dotfiles)

### Fish Shell Abbreviations
Key abbreviations defined in `conf/config/fish/config.fish`:
- `dc` → `dotdrop compare`
- `di` → `dotdrop install`
- `ga` → `git add`
- `gci` → `git commit`

## Repository Structure

### Configuration Organization
- `conf/` - Contains all dotfile templates organized by application
- `config.yaml` - Main dotdrop configuration mapping source files to destinations
- `dotdrop.sh` - Main entry point script that manages the dotdrop submodule

### Key Configuration Areas

#### Emacs Configuration
- `conf/config/emacs/config.org` - Comprehensive Emacs configuration in Org mode
- `conf/config/emacs/init.el` - Emacs initialization file
- `conf/config/emacs/early-init.el` - Early initialization
- `conf/config/emacs/scripts/` - Custom Emacs scripts and utilities

#### Desktop Environment
- `conf/config/hypr/` - Hyprland window manager configuration
- `conf/config/waybar/` - Waybar status bar configuration
- `conf/config/fish/` - Fish shell configuration and functions

#### Development Tools
- `conf/gitconfig` - Git configuration with delta diff viewer
- `conf/bin/` - Custom scripts and utilities

## Architecture Patterns

### Profile-Based Management
The repository uses dotdrop's profile system to manage different configurations:
- Each profile defines a subset of dotfiles appropriate for that environment
- Profiles can share common configurations while having environment-specific overrides
- The `config.yaml` file maps source files in `conf/` to their destination paths

### Modular Configuration
- Emacs configuration is organized as a literate configuration in Org mode
- Shell configurations include modular function definitions
- Scripts are organized by functionality in `conf/bin/`

### Template System
Dotdrop supports templating, allowing dynamic configuration generation based on:
- Environment variables
- Host-specific settings
- Profile-specific values

## Development Workflow

### Making Changes
1. Edit files in the `conf/` directory
2. Use `./dotdrop.sh compare` to see differences
3. Use `./dotdrop.sh install` to deploy changes
4. Use `./dotdrop.sh update` to sync changes back from installed files

### Adding New Dotfiles
1. Use `./dotdrop.sh import <path>` to add new configuration files
2. The tool will automatically update `config.yaml` with the new mapping
3. Specify the appropriate profile with `-p <profile>` if needed

### Testing Changes
- Use `./dotdrop.sh install -t` to install to a temporary directory for testing
- Use `./dotdrop.sh install -d` for dry-run mode to preview changes
- Use `./dotdrop.sh compare` to verify differences before applying

## Important Notes

- The repository uses git submodules for dotdrop itself
- Claude Code alias is available as `claude` in the fish shell configuration
- The default profile is `hyper-city`
- All executables in `conf/bin/` are automatically made executable when installed
- Some configurations include environment-specific ignore patterns