# Devout

[![Build Status][gh-actions-badge]][gh-actions]

[![Project Logo][logo]][logo-large]

*A Devin Clone MCP Server*

A Model Context Protocol (MCP) server written in Erlang that provides file, directory, and version control operations for software projects.

## Features

### 🛠️ File Operations

- **new-dir**: Create directories with automatic parent creation
- **new-dirs**: Create directory structures with multiple children
- **move**: Safely move/rename files and directories
- **write**: Create/write files with append mode support
- **read**: Read file contents with size limits
- **list-files**: List directory contents with file types and sizes
- **show-cwd**: Display current working directory
- **change-cwd**: Navigate to relative directories

### 🔒 Security Features

- **Path Validation**: Only relative paths allowed, prevents directory traversal
- **Sandboxing**: All operations restricted to base directory
- **File Size Limits**: Configurable maximum file sizes
- **Extension Filtering**: Optional file type restrictions
- **Safe Error Handling**: No information disclosure through errors

### 📚 Resources & Prompts

- **devout://status**: Real-time service status and configuration
- **devout://help**: Comprehensive tool documentation
- **create_project**: Intelligent project structure generation (Erlang, web, API, library)

## Architecture

### OTP Design

- **Application**: `devout_app` - Standard OTP application callback
- **Supervisor**: `devout_sup` - Manages server process lifecycle
- **Server**: `devout_server` - Main gen_server coordinating with erlmcp
- **Operations**: `devout_fs` - File system operation implementations
- **Validation**: `devout_path_validator` - Security and path validation
- **Entry Point**: `devout_stdio_main` - Standalone stdio executable

### Security Model

```
┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
│   Claude AI     │───▶│   devout_server  │───▶│    devout_fs    │
└─────────────────┘    └──────────────────┘    └─────────────────┘
                                │                       │
                                ▼                       ▼
                       ┌──────────────────┐    ┌─────────────────┐
                       │ erlmcp_stdio     │    │ path_validator  │
                       │ (MCP Protocol)   │    │ (Security)      │
                       └──────────────────┘    └─────────────────┘
```

## Prerequisites

- **Erlang/OTP 25+**
- **Rebar3**
- **Git** (for dependencies)

## Quick Start

### 1. Build

```bash
git clone <repository-url>
cd devout
rebar3 get-deps
rebar3 compile
```

### 2. Test

```bash
rebar3 eunit
```

### 3. Configure Claude Desktop

Edit `claude_desktop_config.json`:

**macOS**: `~/Library/Application Support/Claude/claude_desktop_config.json`
**Windows**: `%APPDATA%\Claude\claude_desktop_config.json`
**Linux**: `~/.config/Claude/claude_desktop_config.json`

```json
{
  "mcpServers": {
    "devout": {
        "command": "/Users/oubiwann/lab/erlsci/devout/bin/start.sh",
        "args": []
    }
  }
}
```

### 4. Restart Claude Desktop

## Usage Examples

### 📁 Directory Operations

```
User: Create a new project structure called "my_app" with src, test, and docs directories

Claude: I'll create that project structure for you.

[Uses new-dirs tool: {"path": "my_app", "children": ["src", "test", "docs"]}]

Directory structure created successfully:
  - my_app
  - src
  - test
  - docs
```

### 🔍 Directory Exploration

```
User: What files are in the current directory?

Claude: I'll show you the files in the current directory.

[Uses list-files tool: {}]

Contents of .:
[FILE] .DS_Store (6148 bytes)
[DIR] .git
[DIR] .github
[FILE] .gitignore (164 bytes)
[FILE] LICENSE (11358 bytes)
[FILE] Makefile (4612 bytes)
[FILE] README.md (9895 bytes)
[DIR] _build
[DIR] bin
[DIR] config
[DIR] src
[DIR] test
```

### 📝 File Operations

```
User: Create a configuration file with some JSON content

Claude: I'll create a config file for you.

[Uses write tool: {"path": "my_app/config.json", "content": "{\n  \"port\": 3000,\n  \"debug\": true\n}"}]

Content written to file successfully: my_app/config.json (32 bytes)
```

### 🔍 Navigation & Reading

```
User: Show me the current directory and then read the config file

Claude: I'll show the current directory and read the config file.

[Uses show-cwd tool: {}]
Current working directory: /Users/developer/projects

[Uses read tool: {"path": "my_app/config.json"}]
Content of my_app/config.json (32 bytes):

{
  "port": 3000,
  "debug": true
}
```

### 🏗️ Project Generation

```
User: Use the create_project prompt for an Erlang application

Claude: I'll generate an Erlang project structure prompt.

[Uses create_project prompt: {"project_name": "my_erlang_app", "project_type": "erlang"}]

Create a project structure for 'my_erlang_app' (erlang project). Create an Erlang/OTP project structure with directories: src, include, test, priv, rebar.config. Use the devout tools to create the necessary directories and files.
```

## Configuration

Edit `config/sys.config` to customize behavior:

```erlang
[
    {devout, [
        {max_file_size, 10485760},          % 10MB limit
        {allowed_extensions, all},          % or [<<".txt">>, <<".md">>]
        {enable_recursive_delete, false},   % Safety first
        {allowed_operations, [              % Restrict available operations
            new_dir, new_dirs, move, write, read, list_files, show_cwd, change_cwd
        ]}
    ]}
].
```

## Security Features

### 🛡️ Path Security

- **Relative Paths Only**: Absolute paths rejected (`/etc/passwd` ❌)
- **Traversal Prevention**: Parent directory access blocked (`../../../etc` ❌)
- **Base Directory Enforcement**: Operations confined to working directory
- **Path Normalization**: Handles `./`, `//`, and other edge cases

### 📏 Resource Limits

- **File Size Limits**: Configurable maximum file sizes (default: 10MB)
- **Operation Whitelisting**: Restrict available operations per deployment
- **Extension Filtering**: Optional file type restrictions
- **Memory Protection**: Streaming for large files

### 🔒 Error Handling

- **Information Hiding**: Errors don't leak system details
- **Graceful Degradation**: Partial failures handled cleanly
- **Audit Logging**: All operations logged for security review
- **Input Sanitization**: All inputs validated before processing

## Development

### Running Tests

```bash
# Full test suite
rebar3 eunit

# Specific test modules
rebar3 eunit --module=devout_test

# With coverage
rebar3 cover
```

### Code Quality

```bash
# Static analysis
rebar3 dialyzer

# Cross references
rebar3 xref

# Linting
rebar3 lint
```

### Development Mode

```bash
# Interactive shell
rebar3 shell

# Start stdio server manually
devout_server:start_stdio().
```

### Adding Operations

1. **Add to allowed_operations** in `devout.app.src`
2. **Implement in devout_fs.erl** (if needed):

   ```erlang
   my_operation(Path) ->
       case devout_path_validator:validate_path(Path) of
           {ok, ValidPath} ->
               % Your operation here
               ok;
           {error, Reason} ->
               {error, Reason}
       end.
   ```

3. **Register tool in devout_app.erl**:

   ```erlang
   ok = erlmcp_stdio:add_tool(
       <<"my-operation">>,
       <<"Description">>,
       fun devout_server:handle_my_operation/1,
       SchemaMap
   ).
   ```

4. **Add handler in devout_server.erl**:

   ```erlang
   handle_my_operation(#{<<"path">> := Path}) ->
       case devout_fs:my_operation(Path) of
           ok -> <<"Success message">>;
           {error, Reason} -> format_error(Reason)
       end.
   ```

## Monitoring & Debugging

### Health Checks

```bash
# Check if application is running
erl -eval "io:format('~p~n', [application:which_applications()]), halt()."

# Verify processes
erl -eval "io:format('~p~n', [whereis(devout_server)]), halt()."
```

### Logging

Logs go to stderr to avoid interfering with MCP protocol on stdout:

- **Info**: Service lifecycle events
- **Warning**: Security violations, invalid paths
- **Error**: Operation failures, system errors

### Common Issues

| Issue | Cause | Solution |
|-------|-------|----------|
| Service not starting | Missing dependencies | `rebar3 get-deps && rebar3 compile` |
| Path rejected | Absolute/traversal path | Use relative paths only |
| File too large | Exceeds size limit | Check `max_file_size` config |
| Permission denied | File system permissions | Check directory ownership |
| Tool not found | Version mismatch | Verify erlmcp compatibility |

## Performance

- **Memory Usage**: ~10MB base + file buffers
- **Throughput**: 1000+ operations/second for small files
- **Latency**: <10ms for typical operations
- **Scalability**: Single-threaded, suitable for interactive use

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass: `rebar3 eunit`
5. Run static analysis: `rebar3 dialyzer`
6. Submit a pull request

### Code Style

- Use OTP principles and gen_server patterns
- Comprehensive error handling with proper types
- Security-first design - validate all inputs
- Document all public functions with edoc
- Follow Erlang naming conventions

## Acknowledgments

- Built on the excellent [erlmcp](https://github.com/erlsci/erlmcp) library
- Inspired by security practices from the Erlang/OTP ecosystem
- Thanks to the Claude Desktop team for MCP protocol specification

## License

Apache License 2.0 - see LICENSE file for details.

## External Resources

- [Get started with the Model Context Protocol (MCP)](https://modelcontextprotocol.io/introduction)

[//]: ---Named-Links---

[logo]: priv/images/project-logo.png
[logo-large]: priv/images/project-logo-large.png
[gh-actions-badge]: https://github.com/erlsci/devout/workflows/ci/badge.svg
[gh-actions]: https://github.com/erlsci/devout/actions?query=workflow%3Aci
