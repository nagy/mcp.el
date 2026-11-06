EMACS ?= emacs

# Unit tests run without network access or subprocesses.
test:
	$(EMACS) -Q --batch -L . -L test -l ert -l mcp-test.el \
		-f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -Q --batch --eval "(setq byte-compile-error-on-warn t)" \
		--eval "(add-to-list 'load-path \"./\")" \
		-f batch-byte-compile mcp-transport.el mcp.el mcp-hub.el

.PHONY: test compile
