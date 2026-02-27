# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

A flake-based NixOS configuration managing two machines (`hp-desktop`, `tuxedo-laptop`) with a shared Home Manager user configuration for user `decio`. Uses nixpkgs 25.11 stable with select packages from unstable.

## Common Commands

```bash
# Apply configuration to current machine
sudo nixos-rebuild switch --flake .#tuxedo-laptop
sudo nixos-rebuild switch --flake .#hp-desktop

# Test without making it the boot default
sudo nixos-rebuild test --flake .#tuxedo-laptop

# Build without activating
sudo nixos-rebuild build --flake .#tuxedo-laptop

# Update all flake inputs
nix flake update

# Format Nix files
nix fmt
```

## Architecture

### Entry Point
`flake.nix` declares two `nixosConfigurations` outputs. Both share the same Home Manager setup (the `home` let-binding) which imports `./home` for user `decio` and `./home/theme` as a shared module.

### Configuration Layers
1. **Machine configs** (`machines/<host>/configuration.nix`) — host-specific settings (bootloader, hostname, hardware services). Each imports shared `configuration.nix` from the root.
2. **Shared system config** (`configuration.nix`) — networking, fonts, Docker, Hyprland/GDM, Bluetooth, PostgreSQL, keyboard layouts, common system packages.
3. **Home Manager** (`home/`) — all user-level configuration.

### Home Manager Structure (`home/`)
- `default.nix` — root; declares user packages, GTK/cursor/icon theme (Catppuccin/Colloid-Dark/Papirus), imports all submodules
- `programs/` — per-app configs (alacritty, git, neovim, zsh, fzf, rofi, zed)
- `hyprland/` — Hyprland WM config (binds, rules, settings), hyprpanel
- `services/` — user services (gnome-keyring, gpg-agent, dunst, hyprlock, hypridle, hyprpaper, polkit-agent)
- `theme/default.nix` — NixOS module that exposes shared theme options consumed across machines

### Unstable Packages
`pkgs-unstable` is passed via `extraSpecialArgs` to all Home Manager modules. Use `pkgs-unstable.<package>` in home modules when a newer version is needed (e.g. Obsidian, Zed, Windsurf, Gleam, Haskell tools currently use it).

### Overlays
An overlay system exists in `overlays/` but is currently commented out in both machine configs. To enable, uncomment the overlay line in the relevant `machines/<host>/configuration.nix`.

## Key Conventions

- The formatter is `nixpkgs-fmt` (run via `nix fmt`).
- `home-manager.backupFileExtension = "hmbackup_01"` — existing files will be renamed with this suffix on conflict.
- Electron 25.9.0 is explicitly permitted as an insecure package (required by some apps).
- Both machine configs follow the same pattern: import root `configuration.nix`, declare hardware module, set hostname and machine-specific options.
