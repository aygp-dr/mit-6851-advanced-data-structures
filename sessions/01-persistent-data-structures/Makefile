# Session 1: Persistent Data Structures Makefile

SESSION_NAME := 01-persistent-data-structures
ROOT_DIR := ../..

# Include paths
GUILE_LOAD_PATH := $(ROOT_DIR)/lib:$(CURDIR)/src
GUILE := guile --no-auto-compile -L $(GUILE_LOAD_PATH)

# Source files
ORG_NOTES := notes/session-01.org
SCHEME_SRC := $(wildcard src/*.scm)
TEST_SRC := $(wildcard tests/*.scm)

# Targets
.PHONY: all test tangle clean run-examples

all: tangle test

# Extract Scheme code from org-mode notes
tangle: $(ORG_NOTES)
	@echo "Extracting Scheme code from org-mode notes..."
	@emacs --batch --eval "(require 'org)" \
		--eval "(org-babel-tangle-file \"$(ORG_NOTES)\")" \
		2>/dev/null || echo "Note: Emacs org-mode required for tangling"

# Run tests
test:
	@echo "Running tests for $(SESSION_NAME)..."
	@for test in $(TEST_SRC); do \
		echo "Testing $$test..."; \
		$(GUILE) $$test || exit 1; \
	done

# Run example code
run-examples:
	@echo "Running examples for $(SESSION_NAME)..."
	@$(GUILE) src/examples.scm

# Download materials for this session
download-materials:
	@echo "Downloading materials for Session 1..."
	@mkdir -p materials
	@cd $(ROOT_DIR) && ./scripts/download-materials.sh --session 1

# Convert PDF notes to org-mode (requires pdftotext)
convert-notes:
	@echo "Converting PDF notes to org-mode..."
	@if [ -f materials/MIT6_851S12_lec01.pdf ]; then \
		pdftotext materials/MIT6_851S12_lec01.pdf - | \
		sed 's/^/#+TEXT: /' > notes/session-01-raw.org; \
		echo "Raw conversion complete. Manual editing required."; \
	else \
		echo "PDF not found. Run 'make download-materials' first."; \
	fi

clean:
	@echo "Cleaning generated files..."
	@rm -f src/*~ tests/*~ notes/*~
	@find . -name "*.go" -delete

help:
	@echo "Available targets:"
	@echo "  all              - Tangle org files and run tests"
	@echo "  tangle           - Extract Scheme code from org-mode notes"
	@echo "  test             - Run all tests"
	@echo "  run-examples     - Run example code"
	@echo "  download-materials - Download session materials"
	@echo "  convert-notes    - Convert PDF to org-mode (basic)"
	@echo "  clean            - Remove temporary files"
	@echo "  help             - Show this message"