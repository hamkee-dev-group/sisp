subdirs = src

all install uninstall clean realclean mostlyclean:
	@for f in $(subdirs); do \
		(cd $$f && $(MAKE) $@) \
	done

debug:
	$(MAKE) -C src debug

distclean:
	@for f in $(subdirs); do \
		(cd $$f && $(MAKE) $@) \
	done

# Optional static analysis tools.
CPPCHECK = cppcheck
ANALYZER_CC = gcc

# nullPointerOutOfMemory: unchecked malloc() is by design in this small interpreter.
# normalCheckLevelMaxBranches: informational note that this cppcheck counts toward exit.
# False positives are suppressed at the affected line with --inline-suppr.
CPPCHECK_SUPPRESS = --inline-suppr \
	--suppress=nullPointerOutOfMemory \
	--suppress=normalCheckLevelMaxBranches
CPPCHECK_FLAGS = --quiet --error-exitcode=1 --enable=unusedFunction \
	$(CPPCHECK_SUPPRESS) \
	--cppcheck-build-dir=.cppcheck-cache -I src

# Additional leak, lifetime, and unused-function analysis.
ANALYZE_SUPPRESS = $(CPPCHECK_SUPPRESS)

.PHONY: all install uninstall clean realclean mostlyclean distclean build debug test lint analyze validate

build: all

test: build
	./tests/run-all.sh

lint:
	@if command -v $(CPPCHECK) >/dev/null 2>&1; then \
		mkdir -p .cppcheck-cache; \
		$(CPPCHECK) $(CPPCHECK_FLAGS) src/*.c src/*.h; \
	else \
		echo "== SKIP lint: $(CPPCHECK) is not installed =="; \
	fi

analyze:
	@if $(ANALYZER_CC) -fanalyzer -fsyntax-only -I src src/pool.c >/dev/null 2>&1; then \
		echo "== $(ANALYZER_CC) -fanalyzer =="; \
		$(ANALYZER_CC) -fanalyzer -fsyntax-only -I src src/*.c; \
	else \
		echo "== SKIP -fanalyzer: $(ANALYZER_CC) does not support it =="; \
		echo "   (on macOS, gcc is Apple clang; install real gcc or set ANALYZER_CC)"; \
	fi
	@if command -v $(CPPCHECK) >/dev/null 2>&1; then \
		echo "== cppcheck (warning + unusedFunction) =="; \
		mkdir -p .cppcheck-analyze-cache; \
		$(CPPCHECK) --quiet --error-exitcode=1 --enable=warning,unusedFunction \
			$(ANALYZE_SUPPRESS) \
			--cppcheck-build-dir=.cppcheck-analyze-cache -I src src/*.c; \
	else \
		echo "== SKIP cppcheck: $(CPPCHECK) is not installed =="; \
	fi
	@echo "== analyze done =="

validate: lint test
