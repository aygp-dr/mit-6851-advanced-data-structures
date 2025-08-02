#!/bin/bash

# Update all session Makefiles to include experiments and aall targets

SESSIONS_DIR="../sessions"
cd "$(dirname "$0")" || exit 1

for session_dir in $SESSIONS_DIR/*/; do
    if [ -d "$session_dir" ] && [ -f "$session_dir/Makefile" ]; then
        makefile="$session_dir/Makefile"
        session_name=$(basename "$session_dir")
        
        # Check if already updated
        if grep -q "aall:" "$makefile"; then
            echo "✓ $session_name already has aall target"
            continue
        fi
        
        echo "Updating $session_name/Makefile..."
        
        # Create backup
        cp "$makefile" "$makefile.bak"
        
        # Update the Makefile
        sed -i '/^\.PHONY:.*run$/s/$/ experiments aall/' "$makefile"
        
        # Add new targets after the run target
        sed -i '/^run: test run-examples$/a\\n# Run all including experiments\naall: all run experiments\n\n# Run experiments\nexperiments:\n\t@echo "Running experiments for $(SESSION_NAME)..."\n\t@mkdir -p experiments/results\n\t@if [ -d experiments ] && [ "$$(ls -A experiments/*.scm 2>/dev/null)" ]; then \\\n\t\tfor exp in experiments/*.scm; do \\\n\t\t\techo "Running $$exp..."; \\\n\t\t\t$(GUILE) $$exp > experiments/results/$$(basename $$exp .scm)-results.txt 2>&1 || \\\n\t\t\t\techo "⚠️  Failed to run $$exp"; \\\n\t\tdone; \\\n\t\techo "✅ Experiments completed. Results in experiments/results/"; \\\n\telse \\\n\t\techo "No experiments found in experiments/ directory"; \\\n\tfi' "$makefile"
        
        echo "✓ Updated $session_name"
    fi
done

echo "✅ All session Makefiles updated"