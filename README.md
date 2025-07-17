# Devout - Devin Clone MCP Server

A Model Context Protocol (MCP) server implementation in Erlang that provides secure file system operations for AI assistants. This server is designed to be a foundation for building Devin-like AI coding assistants.

## Features

### File System Operations
- **Show Current Directory**: Display the current working directory
- **Change Directory**: Navigate to relative paths only
- **Create Files**: Create files with optional content
- **Delete Files**: Remove files from the file system
- **Create Directories**: Create directories with all necessary parent directories
- **Remove Directories**: Remove directories, optionally with all contents

### Security Features
- **Relative Path Only**: All operations are restricted to relative paths
- **Base Directory Restriction**: Operations are confined to the base directory and its subdirectories
- **Path Validation**: Comprehensive path validation to prevent directory traversal attacks
- **No Absolute Paths**: Absolute paths are explicitly forbidden

## Requirements

- Erlang/OTP 25 or later
- Rebar3 build tool
- Git (for dependencies)

## Installation

1. **Clone the repository**:
```bash
git clone <repository-url>
cd devout
```

2. **Build the project**:
```bash
make build
```

3. **Run tests** (optional):
```bash
make test
```

## Usage

### Starting the Server

For development:
```bash
make dev
```

For MCP client integration (stdio mode):
```bash
make start-mcp
```

### MCP Client Configuration

To use this server with Claude Desktop or other MCP clients, add the following to your MCP configuration:

```json
{
  "mcpServers": {
    "devout-fs": {
      "command": "/path/to/devout/_build/default/bin/devout",
      "args": ["--stdio"],
      "env": {
        "ERL_LIBS": "/path/to/devout/_build/default/lib"
      }
    }
  }
}
```

## Available Tools

### `show_cwd`
Shows the current working directory.

**Parameters**: None

**Example Response**:
```json
{
  "status": "success",
  "cwd": "/home/user/project"
}
```

### `change_cwd`
Changes the current working directory to a relative path.

**Parameters**:
- `path` (string): Relative path to change to

**Example**:
```json
{
  "path": "src/modules"
}
```

### `create_file`
Creates a new file with optional content.

**Parameters**:
- `path` (string): Relative path for the new file
- `content` (string, optional): Content for the file

**Example**:
```json
{
  "path": "src/hello.erl",
  "content": "-module(hello).\n-export([world/0]).\n\nworld() -> \"Hello, World!\"."
}
```

### `delete_file`
Deletes a file.

**Parameters**:
- `path` (string): Relative path of the file to delete

**Example**:
```json
{
  "path": "temp/old_file.txt"
}
```

### `create_directory`
Creates a directory and all necessary parent directories.

**Parameters**:
- `path` (string): Relative path for the new directory

**Example**:
```json
{
  "path": "src/modules/utils"
}
```

### `remove_directory`
Removes a directory, optionally with all contents.

**Parameters**:
- `path` (string): Relative path of the directory to remove
- `recursive` (boolean, optional): Remove directory and all contents recursively (default: false)

**Example**:
```json
{
  "path": "temp/old_dir",
  "recursive": true
}
```

## Architecture

The server follows OTP (Open Telecom Platform) design principles:

- **devout_app**: Application module that manages the lifecycle
- **devout_sup**: Supervisor that manages the server process
- **devout_server**: Main GenServer that handles MCP protocol communication
- **devout_fs_ops**: File system operations module
- **devout_path_validator**: Path validation and security module

## Security Considerations

This server is designed with security in mind:

1. **Path Restrictions**: Only relative paths are allowed
2. **Base Directory Confinement**: All operations are restricted to the base directory
3. **Input Validation**: All inputs are validated before processing
4. **No System Commands**: No shell command execution capabilities
5. **Logging**: All operations are logged for audit purposes

## Development

### Building
```bash
make build
```

### Testing
```bash
make test
```

### Type Checking
```bash
make dialyzer
```

### Code Analysis
```bash
make xref
```

### All Checks
```bash
make check
```

### Development Shell
```bash
make shell
```

## Configuration

Configuration is managed through `config/sys.config`. Key settings include:

- MCP server configuration
- Allowed operations
- Logging settings
- Base directory (set at runtime)

## Logging

The server uses Lager for logging with multiple backends:
- Console output for development
- File logging for production
- Structured logging for debugging

Log files are stored in the `logs/` directory.

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Run tests and checks: `make check`
5. Submit a pull request

## License

Apache License 2.0 - See LICENSE file for details.

## Roadmap

- [ ] Add file reading capabilities
- [ ] Add directory listing operations
- [ ] Add file/directory metadata operations
- [ ] Add file watching capabilities
- [ ] Add compression/decompression operations
- [ ] Add search and grep operations
- [ ] Add git integration
- [ ] Add remote file system support

## Support

For issues, questions, or contributions, please visit the [GitHub repository](https://github.com/erlsci/devout).
