# Session 2: Retroactive Data Structures Makefile

SESSION_NAME := 02-retroactive-data-structures
ROOT_DIR := ../..

# Include paths
GUILE_LOAD_PATH := $(ROOT_DIR)/lib:$(CURDIR)/src
GUILE := guile --no-auto-compile -L $(GUILE_LOAD_PATH)

# Source files
ORG_NOTES := notes/session-02.org
SCHEME_SRC := $(wildcard src/*.scm)
TEST_SRC := $(wildcard tests/*.scm)

# Targets
.PHONY: all test tangle clean run-examples run

all: test

# Run target - runs tests and examples
run: test run-examples

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
	@if [ -f src/examples.scm ]; then \
		$(GUILE) src/examples.scm; \
	else \
		echo "No examples.scm found"; \
	fi

# Download materials for this session
download-materials:
	@echo "Downloading materials for Session 2..."
	@mkdir -p materials
	@cd $(ROOT_DIR) && ./scripts/download-materials.sh --session 2

clean:
	@echo "Cleaning generated files..."
	@rm -f src/*~ tests/*~ notes/*~
	@find . -name "*.go" -delete

help:
	@echo "Available targets:"
	@echo "  all              - Run tests"
	@echo "  tangle           - Extract Scheme code from org-mode notes"
	@echo "  test             - Run all tests"
	@echo "  run-examples     - Run example code"
	@echo "  download-materials - Download session materials"
	@echo "  clean            - Remove temporary files"
	@echo "  help             - Show this message"