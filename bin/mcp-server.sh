#!/bin/bash

# Devout MCP Server startup script for Claude Desktop integration
# This script ensures the server starts properly from any directory

# Get the directory where this script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

# Change to the project directory
cd "$SCRIPT_DIR"

# Build the project if needed
make build > /dev/null 2>&1

# Start the MCP server
exec erl -pa _build/default/lib/*/ebin \
    -config config/sys.config \
    -eval "application:ensure_all_started(devout)" \
    -noshell \
    -noinput