# Makefile for Devout MCP Server

.PHONY: all build clean deps test dialyzer dialyzer-full plt xref shell release start stop check format

# Default target
all: build

# Build the project
build: deps
	@echo "Building Devout MCP Server..."
	@rebar3 compile

# Build with debug_info for Dialyzer compatibility
build-debug: deps
	@echo "Building Devout MCP Server with debug_info..."
	@rm -rf _build/default/lib/erlmcp/ebin
	@ERL_COMPILER_OPTIONS="[debug_info]" rebar3 compile

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	@rebar3 clean

# Get dependencies
deps:
	@echo "Getting dependencies..."
	@rebar3 get-deps

# Run tests
test:
	@echo "Running tests..."
	@rebar3 eunit
	@rebar3 ct

# Run dialyzer for type checking (uses existing PLT)
dialyzer:
	@echo "Running dialyzer..."
	@rebar3 dialyzer

# Full Dialyzer run (rebuild with debug_info + PLT + analysis)
dialyzer-full: build-debug
	@echo "Running full Dialyzer analysis..."
	@rebar3 dialyzer

# Run xref for cross-reference analysis
xref:
	@echo "Running xref..."
	@rebar3 xref

# Start development shell
shell: build
	@echo "Starting development shell..."
	@rebar3 shell

# Create release
release:
	@echo "Creating release..."
	@rebar3 release

# Start the MCP server
start: build
	@echo "Starting Devout MCP Server..."
	@rebar3 shell --apps devout --config config/sys.config

# Start the MCP server in stdio mode (for MCP client integration)
start-mcp: build
	@echo "Starting Devout MCP Server in stdio mode..."
	@erl -pa $(shell rebar3 path) \
		-eval "devout:start()" \
		-noshell \
		-noinput

# Stop the server (if running in background)
stop:
	@echo "Stopping Devout MCP Server..."
	@pkill -f "devout"

# Run all checks (now includes full Dialyzer)
check: dialyzer-full xref test
	@echo "All checks passed!"

# Format code (requires rebar3_format plugin)
format:
	@echo "Formatting code..."
	@rebar3 fmt

# Development targets
dev: build
	@echo "Starting development environment..."
	@rebar3 shell --config config/sys.config

# Development workflow with type checking
dev-full: clean build-debug dialyzer-full test
	@echo "Full development workflow completed!"

# Watch for changes and rebuild (requires rebar3_auto plugin)
watch:
	@echo "Watching for changes..."
	@rebar3 auto

# Generate documentation
docs:
	@echo "Generating documentation..."
	@rebar3 edoc

# Package for distribution
package: release
	@echo "Packaging for distribution..."
	@tar -czf devout-mcp-server.tar.gz -C _build/default/rel/devout .

# Install locally (requires root/sudo)
install: package
	@echo "Installing Devout MCP Server..."
	@sudo mkdir -p /usr/local/lib/devout
	@sudo tar -xzf devout-mcp-server.tar.gz -C /usr/local/lib/devout
	@sudo ln -sf /usr/local/lib/devout/bin/devout /usr/local/bin/devout-mcp

# Uninstall
uninstall:
	@echo "Uninstalling Devout MCP Server..."
	@sudo rm -rf /usr/local/lib/devout
	@sudo rm -f /usr/local/bin/devout-mcp

# Development helper - quick rebuild and start
quick: clean build start-mcp

# Test the rebar3 path command
test-path:
	@echo "Testing rebar3 path output:"
	@rebar3 path

# Show help
help:
	@echo "Available targets:"
	@echo "  all         - Build the project (default)"
	@echo "  build       - Compile the project"
	@echo "  build-debug - Compile with debug_info for Dialyzer"
	@echo "  clean       - Clean build artifacts"
	@echo "  deps        - Get dependencies"
	@echo "  test        - Run tests"
	@echo "  plt         - Build Dialyzer PLT (with debug_info)"
	@echo "  dialyzer    - Run type checking (uses existing PLT)"
	@echo "  dialyzer-full - Full Dialyzer run (rebuild + PLT + analysis)"
	@echo "  xref        - Run cross-reference analysis"
	@echo "  shell       - Start development shell"
	@echo "  release     - Create release"
	@echo "  start       - Start development server"
	@echo "  start-mcp   - Start server in MCP stdio mode"
	@echo "  start-mcp-alt - Alternative MCP start method"
	@echo "  start-mcp-debug - Start MCP server with debug output"
	@echo "  stop        - Stop server"
	@echo "  check       - Run all checks (includes full Dialyzer)"
	@echo "  format      - Format code"
	@echo "  dev         - Start development environment"
	@echo "  dev-full    - Full development workflow with type checking"
	@echo "  watch       - Watch for changes and rebuild"
	@echo "  docs        - Generate documentation"
	@echo "  package     - Package for distribution"
	@echo "  install     - Install locally"
	@echo "  uninstall   - Uninstall"
	@echo "  quick       - Quick rebuild and start"
	@echo "  test-path   - Test rebar3 path output"
	@echo "  help        - Show this help"