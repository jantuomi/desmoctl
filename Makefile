# Compiler and source definitions
CC = csc
CFLAGS = -O3
SRC = desmoctl.scm
BIN = build/desmoctl

# Default PREFIX for install, can be overridden
PREFIX ?= /usr/bin/

# Default target - do nothing
all:
	@echo "Available targets:"
	@echo "  static    - Build the binary with static linking."
	@echo "  dynamic   - Build the binary without static linking."
	@echo "  install   - Copy the binary to the install PREFIX."
	@echo "  deps      - Install dependencies from requirements.list."
	@echo "  test      - Run unit tests and exit."
	@echo "  clean     - Remove built binary and other generated files."
	@echo ""
	@echo "Specify a target to make."

# Rule for building the binary with static linking
static: CFLAGS += -static
static: $(BIN)

# Rule for building the binary without static linking
dynamic: $(BIN)

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

test:
	csi -s test.scm

# Clean target
clean:
	rm -rf build

.PHONY: all static dynamic install deps clean
