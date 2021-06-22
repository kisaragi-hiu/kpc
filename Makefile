.cask: Cask
	cask install

test: .cask $(shell find . -name "*.el")
	cask exec buttercup -L . -L tests

.PHONY: test
