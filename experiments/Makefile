# MIT 6.851 Advanced Data Structures - Experiments Makefile

ROOT_DIR := ..
GUILE_LOAD_PATH := $(ROOT_DIR)/lib:$(ROOT_DIR)/sessions/*/src
GUILE := guile --no-auto-compile -L $(GUILE_LOAD_PATH)

# Experiment files
EXPERIMENTS := $(wildcard *.scm)
RESULTS_DIR := results

.PHONY: all clean run-all help

# Default target
all: run-all

# Create results directory
$(RESULTS_DIR):
	@mkdir -p $(RESULTS_DIR)

# Run all experiments
run-all: $(RESULTS_DIR)
	@echo "Running all experiments..."
	@for exp in $(EXPERIMENTS); do \
		if [ -f "$$exp" ]; then \
			echo ""; \
			echo "Running $$exp..."; \
			$(GUILE) $$exp > $(RESULTS_DIR)/$${exp%.scm}-results.txt 2>&1 || \
				echo "⚠️  Failed to run $$exp"; \
		fi \
	done
	@echo ""
	@echo "✅ All experiments completed. Results in $(RESULTS_DIR)/"

# Run specific experiment
run-%: %.scm $(RESULTS_DIR)
	@echo "Running $*.scm..."
	@$(GUILE) $*.scm | tee $(RESULTS_DIR)/$*-results.txt

# Clean results
clean:
	@echo "Cleaning experiment results..."
	@rm -rf $(RESULTS_DIR)
	@rm -f *.log *~

# Help
help:
	@echo "MIT 6.851 Experiments"
	@echo ""
	@echo "Available targets:"
	@echo "  make all         - Run all experiments"
	@echo "  make run-NAME    - Run specific experiment (e.g., make run-persistence-benchmark)"
	@echo "  make clean       - Remove results and temporary files"
	@echo "  make help        - Show this message"
	@echo ""
	@echo "Experiments should be .scm files in this directory"