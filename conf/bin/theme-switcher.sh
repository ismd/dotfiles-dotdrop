#!/bin/bash
#
# Unified Theme Switcher
# This script switches themes across multiple applications
#

set -e

# Default values
THEME_NAME=""
LIST_THEMES=false
HELP=false
VERBOSE=false

# Theme directory
THEMES_DIR="$HOME/.config/themes"

# Supported applications
SUPPORTED_APPS=("gtk" "emacs" "alacritty" "hyprland" "waybar" "dunst")

# Function to print usage
print_usage() {
  echo "Usage: theme-switcher [OPTIONS] [THEME_NAME]"
  echo ""
  echo "Options:"
  echo "  -l, --list          List available themes"
  echo "  -h, --help          Show this help message"
  echo "  -v, --verbose       Enable verbose output"
  echo ""
  echo "Examples:"
  echo "  theme-switcher -l                # List available themes"
  echo "  theme-switcher dark              # Switch to dark theme"
  echo "  theme-switcher light -v          # Switch to light theme with verbose output"
}

# Function to list available themes
list_themes() {
  echo "Available themes:"
  if [ -d "$THEMES_DIR" ]; then
    for theme in "$THEMES_DIR"/*; do
 if [ -d "$theme" ]; then
              echo "  - $(basename "$theme")"
            fi
    done
  else
    echo "  No themes found. Create themes in $THEMES_DIR"
    mkdir -p "$THEMES_DIR"
  fi
}

# Function to apply GTK theme
apply_gtk_theme() {
  local theme_dir="$THEMES_DIR/$THEME_NAME/gtk"
  if [ -d "$theme_dir" ]; then
    if [ "$VERBOSE" = true ]; then
      echo "Applying GTK theme from $theme_dir"
    fi
    
    # Apply GTK2 theme
    if [ -f "$theme_dir/gtkrc-2.0" ]; then
      cp "$theme_dir/gtkrc-2.0" "$HOME/.config/gtkrc-2.0"
    fi
    
    # Apply GTK3/4 theme
    if [ -f "$theme_dir/settings.ini" ]; then
      mkdir -p "$HOME/.config/gtk-3.0"
      cp "$theme_dir/settings.ini" "$HOME/.config/gtk-3.0/settings.ini"
      
      mkdir -p "$HOME/.config/gtk-4.0"
      cp "$theme_dir/settings.ini" "$HOME/.config/gtk-4.0/settings.ini"
    fi
  else
    echo "Warning: GTK theme directory not found at $theme_dir"
  fi
}

# Function to apply Emacs theme
apply_emacs_theme() {
  local theme_dir="$THEMES_DIR/$THEME_NAME/emacs"
  if [ -d "$theme_dir" ]; then
    if [ "$VERBOSE" = true ]; then
      echo "Applying Emacs theme from $theme_dir"
    fi
    
    # Apply Emacs theme
    cp "$theme_dir/theme.el" "$HOME/.config/emacs/theme.el"

    if pgrep -x "emacs" > /dev/null; then
      emacsclient -e "(load-file \"$theme_dir/theme.el\")" > /dev/null
      emacsclient -e "(load-theme theme-name t)" > /dev/null
      emacsclient -e "(message \"Theme changed to $THEME_NAME\")" > /dev/null

      if [ "$VERBOSE" = true ]; then
        echo "Emacs theme modified and applied via emacsclient"
      fi
    else
      if [ "$VERBOSE" = true ]; then
        echo "Emacs not running, modified theme will be applied on next start"
      fi
    fi
  else
    echo "Warning: Emacs theme directory not found at $theme_dir"
  fi
}

# Function to apply Waybar theme
apply_waybar_theme() {
    local theme_dir="$THEMES_DIR/$THEME_NAME/waybar"
    if [ -d "$theme_dir" ]; then
        if [ "$VERBOSE" = true ]; then
            echo "Applying Waybar theme from $theme_dir"
        fi
        
        mkdir -p "$HOME/.config/waybar"
        
        if [ -f "$theme_dir/config" ]; then
            cp "$theme_dir/config" "$HOME/.config/waybar/config"
        fi
        
        if [ -f "$theme_dir/style.css" ]; then
            cp "$theme_dir/style.css" "$HOME/.config/waybar/style.css"
        fi
        
        # Restart Waybar if it's running
        if pgrep -x "waybar" > /dev/null; then
            pkill waybar
            waybar &
            if [ "$VERBOSE" = true ]; then
                echo "Waybar restarted"
            fi
        fi
    else
        echo "Warning: Waybar theme directory not found at $theme_dir"
    fi
}

# Function to apply Dunst theme
apply_dunst_theme() {
    local theme_dir="$THEMES_DIR/$THEME_NAME/dunst"
    if [ -d "$theme_dir" ]; then
        if [ "$VERBOSE" = true ]; then
            echo "Applying Dunst theme from $theme_dir"
        fi
        
        if [ -f "$theme_dir/dunstrc" ]; then
            mkdir -p "$HOME/.config/dunst"
            cp "$theme_dir/dunstrc" "$HOME/.config/dunst/dunstrc"
            
            # Restart Dunst if it's running
            if pgrep -x "dunst" > /dev/null; then
                pkill dunst
                dunst &
                if [ "$VERBOSE" = true ]; then
                    echo "Dunst restarted"
                fi
            fi
        fi
    else
        echo "Warning: Dunst theme directory not found at $theme_dir"
    fi
}

# Function to create theme directory structure
create_theme_structure() {
    local new_theme="$1"
    local theme_dir="$THEMES_DIR/$new_theme"
    
    echo "Creating theme structure for '$new_theme' at $theme_dir"
    
    mkdir -p "$theme_dir"
    
    # Create directories for each supported app
    for app in "${SUPPORTED_APPS[@]}"; do
        mkdir -p "$theme_dir/$app"
        echo "Created $theme_dir/$app"
    done
    
    echo "Theme structure created. Add your theme files to these directories."
    echo "Run 'theme-switcher $new_theme' to apply the theme once configured."
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -l|--list)
            LIST_THEMES=true
            shift
            ;;
        -h|--help)
            HELP=true
            shift
            ;;
        -v|--verbose)
            VERBOSE=true
            shift
            ;;
        -c|--create)
            if [[ -z "$2" || "$2" == -* ]]; then
                echo "Error: --create requires a theme name"
                exit 1
            fi
            create_theme_structure "$2"
            exit 0
            ;;
        -*)
            echo "Unknown option: $1"
            print_usage
            exit 1
            ;;
        *)
            THEME_NAME="$1"
            shift
            ;;
    esac
done

# Show help if requested
if [ "$HELP" = true ]; then
    print_usage
    exit 0
fi

# List themes if requested
if [ "$LIST_THEMES" = true ]; then
    list_themes
    exit 0
fi

# Check if theme name is provided
if [ -z "$THEME_NAME" ]; then
    echo "Error: No theme name provided"
    print_usage
    exit 1
fi

# Check if theme exists
if [ ! -d "$THEMES_DIR/$THEME_NAME" ]; then
    echo "Error: Theme '$THEME_NAME' not found"
    list_themes
    exit 1
fi

# Apply theme to all supported applications
echo "Applying theme: $THEME_NAME"

apply_gtk_theme
apply_emacs_theme
# apply_alacritty_theme
# apply_hyprland_theme
# apply_waybar_theme
# apply_dunst_theme

# Save current theme
echo "$THEME_NAME" > "$HOME/.current_theme"

echo "Theme '$THEME_NAME' applied successfully"
