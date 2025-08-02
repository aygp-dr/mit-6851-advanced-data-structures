# MIT 6.851 Advanced Data Structures - Makefile

.PHONY: all check-deps setup download-materials download-videos test clean help session-% list-sessions run run-all

# Default target
all: check-deps setup

# Help target
help:
	@echo "MIT 6.851 Advanced Data Structures - Build System"
	@echo ""
	@echo "Available targets:"
	@echo "  make check-deps      - Check for required dependencies"
	@echo "  make setup          - Setup environment and directories"
	@echo "  make download-materials - Download PDFs and mirror CSAIL site"
	@echo "  make download-videos - Download video lectures"
	@echo "  make scribe-template - Download LaTeX template and create org-mode version"
	@echo "  make export-scribe  - Export org-mode scribe notes to LaTeX"
	@echo "  make test           - Run Guile Scheme test suite"
	@echo "  make clean          - Clean temporary files"
	@echo "  make session-N      - Work on session N (e.g., make session-1)"
	@echo "  make list-sessions  - List all available sessions"
	@echo ""

# Check for required dependencies
check-deps:
	@echo "Checking required dependencies..."
	@which emacs >/dev/null 2>&1 || (echo "❌ emacs not found. Please install emacs." && exit 1)
	@which guile >/dev/null 2>&1 || (echo "❌ guile not found. Please install guile." && exit 1)
	@which sqlite3 >/dev/null 2>&1 || (echo "❌ sqlite3 not found. Please install sqlite3." && exit 1)
	@which python3 >/dev/null 2>&1 || (echo "❌ python3 not found. Please install python3." && exit 1)
	@echo "✅ All core dependencies found!"
	@echo ""
	@echo "Optional dependencies for downloading:"
	@which wget >/dev/null 2>&1 || echo "⚠️  wget not found (needed for mirroring)"
	@which yt-dlp >/dev/null 2>&1 || echo "⚠️  yt-dlp not found (needed for videos)"
	@echo ""

# Setup environment
setup: check-deps
	@echo "Setting up environment..."
	@if [ ! -f .env ]; then \
		cp .env.example .env; \
		echo "✅ Created .env from .env.example"; \
	else \
		echo "⚠️  .env already exists, skipping"; \
	fi
	@if [ ! -d .venv ]; then \
		python3 -m venv .venv; \
		echo "✅ Created Python virtual environment"; \
		. .venv/bin/activate && pip install --upgrade pip && pip install yt-dlp requests beautifulsoup4; \
		echo "✅ Installed Python dependencies"; \
	else \
		echo "⚠️  .venv already exists, skipping"; \
	fi
	@echo "✅ Setup complete!"

# Download course materials (PDFs and mirror)
download-materials:
	@echo "Downloading course materials..."
	@bash scripts/download-materials.sh

# Download video lectures
download-videos:
	@echo "Downloading video lectures..."
	@bash scripts/download-videos.sh

# Download scribe template and create org-mode version
scribe-template:
	@echo "Downloading scribe template..."
	@mkdir -p templates
	@wget -q -O templates/lec-template.tex https://courses.csail.mit.edu/6.851/spring12/lectures/lec-template.tex || \
		(echo "❌ Failed to download template" && exit 1)
	@echo "✅ Downloaded LaTeX template"
	@echo "Creating org-mode template..."
	@emacs --batch --eval "(progn \
		(require 'ox-latex) \
		(with-temp-file \"templates/lec-template.org\" \
			(insert \"#+TITLE: Lecture X: Title\\n\") \
			(insert \"#+AUTHOR: Scribe Name\\n\") \
			(insert \"#+DATE: \\n\") \
			(insert \"#+LATEX_CLASS: article\\n\") \
			(insert \"#+LATEX_HEADER: \\\\usepackage{amsmath,amssymb,amsthm}\\n\") \
			(insert \"#+LATEX_HEADER: \\\\usepackage{algorithm,algorithmic}\\n\") \
			(insert \"#+OPTIONS: toc:nil\\n\\n\") \
			(insert \"* Introduction\\n\\n\") \
			(insert \"Brief overview of today's lecture.\\n\\n\") \
			(insert \"* Main Content\\n\\n\") \
			(insert \"** Definition\\n\\n\") \
			(insert \"** Theorem\\n\\n\") \
			(insert \"** Algorithm\\n\\n\") \
			(insert \"* Conclusion\\n\\n\") \
			(insert \"Summary of key points.\\n\")))" 2>/dev/null
	@echo "✅ Created org-mode template"

# Export org-mode scribe notes to LaTeX
export-scribe:
	@if [ -z "$(ORG_FILE)" ]; then \
		echo "❌ Please specify ORG_FILE, e.g., make export-scribe ORG_FILE=scribes/lec01.org"; \
		exit 1; \
	fi
	@echo "Exporting $(ORG_FILE) to LaTeX..."
	@emacs --batch --eval "(progn \
		(require 'ox-latex) \
		(find-file \"$(ORG_FILE)\") \
		(org-latex-export-to-latex))" 2>/dev/null
	@echo "✅ Exported to LaTeX"

# Run tests
test:
	@echo "Running Guile Scheme tests..."
	@guile -L lib tests/run-tests.scm

# Run all sessions
run-all:
	@echo "Running all sessions..."
	@for dir in sessions/*/; do \
		if [ -d "$$dir" ] && [ -f "$$dir/Makefile" ]; then \
			SESSION=$$(basename $$dir); \
			echo ""; \
			echo "Running $$SESSION..."; \
			$(MAKE) -C $$dir run || echo "⚠️  Failed to run $$SESSION"; \
		fi \
	done
	@echo ""
	@echo "✅ Finished running all sessions"

# Alias for run-all
run: run-all

# Clean temporary files
clean:
	@echo "Cleaning temporary files..."
	@find . -name "*.go" -delete
	@find . -name "*~" -delete
	@rm -rf __pycache__
	@echo "✅ Cleaned temporary files"

# Database initialization
init-db:
	@echo "Initializing SQLite database..."
	@sqlite3 materials/course.db < scripts/schema.sql
	@echo "✅ Database initialized"

# Session-specific builds
session-%:
	@SESSION_NUM=$$(echo $@ | sed 's/session-//'); \
	SESSION_DIR=$$(printf "sessions/%02d-*" $$SESSION_NUM); \
	if [ -d "$$SESSION_DIR" ]; then \
		echo "Building session $$SESSION_NUM in $$SESSION_DIR..."; \
		$(MAKE) -C $$SESSION_DIR all; \
	else \
		echo "❌ Session $$SESSION_NUM not found"; \
		echo "Creating new session directory..."; \
		SESSION_NAME=$$(printf "%02d-session" $$SESSION_NUM); \
		mkdir -p "sessions/$$SESSION_NAME/{src,tests,notes}"; \
		echo "✅ Created sessions/$$SESSION_NAME"; \
		echo "Run 'make list-sessions' to see available sessions"; \
	fi

# List all sessions
list-sessions:
	@echo "Available sessions:"
	@for dir in sessions/*/; do \
		if [ -d "$$dir" ]; then \
			SESSION=$$(basename $$dir); \
			echo "  - $$SESSION"; \
		fi \
	done

# === Lean Configuration ===
LEAN_VERSION := 4.21.0
LEAN_RELEASE := v$(LEAN_VERSION)
TOOLS_DIR := tools/formal-methods
LEAN_ZIP := $(TOOLS_DIR)/lean-$(LEAN_VERSION)-linux.zip
LEAN_DIR := $(TOOLS_DIR)/lean-$(LEAN_VERSION)-linux
LEAN_BIN := $(LEAN_DIR)/bin/lean
LEAN_LINK := $(TOOLS_DIR)/lean4

# === Directory Creation (automatic with | order-only prerequisite) ===
$(TOOLS_DIR):
	@install -d $@

# === Download Target (file-based, idempotent) ===
$(LEAN_ZIP): | $(TOOLS_DIR)
	@echo "Downloading Lean $(LEAN_VERSION)..."
	@curl -L -o $@ \
		https://github.com/leanprover/lean4/releases/download/$(LEAN_RELEASE)/lean-$(LEAN_VERSION)-linux.zip

# === Extract Target (depends on zip file) ===
$(LEAN_BIN): $(LEAN_ZIP)
	@echo "Extracting Lean $(LEAN_VERSION)..."
	@cd $(TOOLS_DIR) && unzip -q $(notdir $<)
	@touch $@  # Update timestamp to prevent re-extraction

# === Symlink Target (for version-agnostic access) ===
$(LEAN_LINK): $(LEAN_BIN)
	@echo "Creating symlink to Lean $(LEAN_VERSION)..."
	@ln -sf $(notdir $(LEAN_DIR)) $@

# === Test Target (verifies installation) ===
$(TOOLS_DIR)/.lean-tested: $(LEAN_BIN)
	@echo "Testing Lean installation..."
	@echo '#check (1 + 1 : Nat)' | $< --stdin >/dev/null
	@$< --version
	@$(dir $<)/lake --version
	@touch $@

# === Public Targets (aliases) ===
.PHONY: lean-install lean-version lean-clean

lean-install: $(TOOLS_DIR)/.lean-tested $(LEAN_LINK)
	@echo "Lean $(LEAN_VERSION) ready at: $(LEAN_LINK)"

lean-version: $(LEAN_BIN)
	@$< --version 2>/dev/null || echo "Lean not installed"
	@$(dir $<)/lake --version 2>/dev/null || true

lean-clean:
	@rm -f $(LEAN_ZIP) $(TOOLS_DIR)/.lean-tested
	@rm -rf $(LEAN_DIR)
	@rm -f $(LEAN_LINK)

# === Pattern Rules for Future Versions ===
# Allow: gmake tools/formal-methods/lean-4.22.0-linux.zip
$(TOOLS_DIR)/lean-%-linux.zip: | $(TOOLS_DIR)
	@echo "Downloading Lean $*..."
	@curl -L -o $@ \
		https://github.com/leanprover/lean4/releases/download/v$*/lean-$*-linux.zip

# === Automatic Directory Creation for Any Path ===
# This allows: gmake any/path/to/file
# and creates directories as needed
%/:
	@install -d $@

# Precious files (don't delete even if Make is interrupted)
.PRECIOUS: $(LEAN_ZIP) $(TOOLS_DIR)/lean-%-linux.zip