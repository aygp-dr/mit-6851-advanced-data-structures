#!/usr/bin/env bash

# MIT 6.851 Advanced Data Structures - Tmux Development Session
# Sets up a tmux session with Emacs configured for Scheme/Guile development

set -euo pipefail

# Source environment variables
if [ -f .envrc ]; then
    source .envrc
fi

# Set project defaults if not already set
PROJECT_NAME="${PROJECT_NAME:-mit-6851-advanced-data-structures}"
PROJECT_ROOT="${PROJECT_ROOT:-$(pwd)}"
EMACS_CONFIG="${EMACS_CONFIG:-${PROJECT_ROOT}/mit-6851-advanced-data-structures.el}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to print colored messages
info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1"
    exit 1
}

# Check dependencies
check_dependencies() {
    local deps=("tmux" "emacs" "guile3")
    for dep in "${deps[@]}"; do
        if ! command -v "$dep" &> /dev/null; then
            error "$dep is not installed. Please install it first."
        fi
    done
    info "All dependencies found"
}

# Kill existing session if it exists
kill_existing_session() {
    if tmux has-session -t "$PROJECT_NAME" 2>/dev/null; then
        warn "Existing session '$PROJECT_NAME' found. Killing it..."
        tmux kill-session -t "$PROJECT_NAME"
    fi
}

# Create tmux session with Emacs
create_tmux_session() {
    info "Creating tmux session: $PROJECT_NAME"
    
    # Start detached tmux session with Emacs
    tmux new-session -d -s "$PROJECT_NAME" -c "$PROJECT_ROOT" \
        "emacs -nw -Q -l $EMACS_CONFIG"
    
    # Create additional panes for development
    tmux split-window -v -t "$PROJECT_NAME" -c "$PROJECT_ROOT" \
        "guile3 --listen"
    tmux resize-pane -t "$PROJECT_NAME:0.1" -y 15
    
    # Create a third pane for shell
    tmux split-window -h -t "$PROJECT_NAME:0.1" -c "$PROJECT_ROOT"
    
    # Select the Emacs pane
    tmux select-pane -t "$PROJECT_NAME:0.0"
    
    info "Session created successfully"
}

# Get session TTY information
get_session_info() {
    info "Session information:"
    echo "  Session name: $PROJECT_NAME"
    echo "  Project root: $PROJECT_ROOT"
    echo "  Emacs config: $EMACS_CONFIG"
    echo ""
    echo "Pane TTYs:"
    tmux list-panes -t "$PROJECT_NAME" -F "  Pane #{pane_index}: #{pane_tty} (#{pane_current_command})"
    echo ""
    echo "To attach to session, run:"
    echo "  tmux attach-session -t $PROJECT_NAME"
    echo ""
    echo "Keyboard shortcuts inside tmux:"
    echo "  Ctrl-b d     - Detach from session"
    echo "  Ctrl-b 0/1/2 - Switch between panes"
    echo "  Ctrl-b z     - Toggle pane zoom"
    echo "  Ctrl-b [     - Enter scroll mode (q to exit)"
}

# Main execution
main() {
    info "MIT 6.851 Advanced Data Structures - Development Environment Setup"
    
    check_dependencies
    
    # Parse command line options
    case "${1:-start}" in
        start)
            kill_existing_session
            create_tmux_session
            get_session_info
            ;;
        attach)
            if tmux has-session -t "$PROJECT_NAME" 2>/dev/null; then
                tmux attach-session -t "$PROJECT_NAME"
            else
                error "No session named '$PROJECT_NAME' found. Run '$0 start' first."
            fi
            ;;
        info)
            if tmux has-session -t "$PROJECT_NAME" 2>/dev/null; then
                get_session_info
            else
                error "No session named '$PROJECT_NAME' found."
            fi
            ;;
        stop)
            kill_existing_session
            info "Session stopped"
            ;;
        *)
            echo "Usage: $0 {start|attach|info|stop}"
            echo ""
            echo "Commands:"
            echo "  start  - Start a new tmux session (default)"
            echo "  attach - Attach to existing session"
            echo "  info   - Show session information"
            echo "  stop   - Stop the tmux session"
            exit 1
            ;;
    esac
}

main "$@"