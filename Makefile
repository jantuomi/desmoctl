# Compiler and source definitions
CC = csc
CFLAGS = -O3 -static
SRC = desmoctl.scm
BIN = build/desmoctl

# Default PREFIX for install, can be overridden
PREFIX ?= /usr/bin/

# Default target
all: $(BIN)

# Rule for building the binary
$(BIN): $(SRC)
	mkdir -p build
	$(CC) $(CFLAGS) -o $(BIN) $(SRC)
	chmod +x $(BIN)

# Install target
install:
	cp $(BIN) $(PREFIX)

# Dependency installation target
deps:
	chicken-install -from-list requirements.list

# Clean target
clean:
	rm -rf build

.PHONY: all install deps clean

