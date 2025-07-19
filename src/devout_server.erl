%%%-------------------------------------------------------------------
%%% @doc
%%% Devout MCP server - Main server that coordinates with erlmcp_stdio
%%% Combines the best of both approaches: simple erlmcp integration with
%%% robust security and error handling
%%% @end
%%%-------------------------------------------------------------------
-module(devout_server).
-behaviour(gen_server).

%% API
-export([start_link/0, start_stdio/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Tool handlers (exported for testing)
-export([handle_new_dir/1, handle_new_dirs/1, handle_move/1, handle_write/1, 
         handle_read/1, handle_show_cwd/1, handle_change_cwd/1]).

%% Utility functions (exported for testing)
-export([format_error/1]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    stdio_started = false :: boolean(),
    base_directory :: string(),
    server_config :: map()
}).

-type state() :: #state{}.

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_stdio() ->
    gen_server:call(?MODULE, start_stdio).

stop() ->
    gen_server:stop(?MODULE).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),
    ?LOG_INFO("Initializing Devout MCP server"),
    
    % Get configuration
    {ok, ServerConfig} = application:get_env(devout, mcp_server_config),
    {ok, BaseDir} = application:get_env(devout, base_directory),
    
    State = #state{
        server_config = ServerConfig,
        base_directory = BaseDir
    },
    
    ?LOG_INFO("Devout MCP server initialized with base directory: ~s", [BaseDir]),
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, state()) -> 
    {reply, term(), state()} | {stop, normal, ok, state()}.
handle_call(start_stdio, _From, #state{stdio_started = false} = State) ->
    case setup_stdio_server() of
        ok ->
            ?LOG_INFO("Stdio server started successfully"),
            {reply, ok, State#state{stdio_started = true}};
        {error, Reason} ->
            ?LOG_ERROR("Failed to start stdio server: ~p", [Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call(start_stdio, _From, #state{stdio_started = true} = State) ->
    {reply, {error, already_started}, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ?LOG_INFO("Devout MCP server terminating"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

setup_stdio_server() ->
    case erlmcp_stdio:start() of
        ok ->
            setup_tools(),
            setup_resources(),
            setup_prompts(),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

setup_tools() ->
    %% Add new-dir tool
    ok = erlmcp_stdio:add_tool(
        <<"new-dir">>, 
        <<"Create a new directory">>,
        fun handle_new_dir/1,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"path">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"Relative path for the directory to create">>
                }
            },
            <<"required">> => [<<"path">>]
        }
    ),
    
    %% Add new-dirs tool (create directory with children)
    ok = erlmcp_stdio:add_tool(
        <<"new-dirs">>, 
        <<"Create a directory with child directories">>,
        fun handle_new_dirs/1,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"path">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"Base directory path to create">>
                },
                <<"children">> => #{
                    <<"type">> => <<"array">>,
                    <<"items">> => #{<<"type">> => <<"string">>},
                    <<"description">> => <<"Child directory names to create">>
                }
            },
            <<"required">> => [<<"path">>]
        }
    ),
    
    %% Add move tool
    ok = erlmcp_stdio:add_tool(
        <<"move">>, 
        <<"Move or rename a file or directory">>,
        fun handle_move/1,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"source">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"Relative path of source file/directory">>
                },
                <<"destination">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"Relative path of destination">>
                }
            },
            <<"required">> => [<<"source">>, <<"destination">>]
        }
    ),
    
    %% Add write tool
    ok = erlmcp_stdio:add_tool(
        <<"write">>, 
        <<"Create or write to a file">>,
        fun handle_write/1,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"path">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"Relative path for the file">>
                },
                <<"content">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"Content to write to the file">>
                },
                <<"mode">> => #{
                    <<"type">> => <<"string">>,
                    <<"enum">> => [<<"write">>, <<"append">>],
                    <<"description">> => <<"Write mode: write (overwrite) or append">>,
                    <<"default">> => <<"write">>
                }
            },
            <<"required">> => [<<"path">>, <<"content">>]
        }
    ),
    
    %% Add read tool
    ok = erlmcp_stdio:add_tool(
        <<"read">>, 
        <<"Read a file">>,
        fun handle_read/1,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"path">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"Relative path of the file to read">>
                }
            },
            <<"required">> => [<<"path">>]
        }
    ),
    
    %% Add show_cwd tool (from devout)
    ok = erlmcp_stdio:add_tool(
        <<"show-cwd">>, 
        <<"Show current working directory">>,
        fun handle_show_cwd/1,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{},
            <<"required">> => []
        }
    ),
    
    %% Add change_cwd tool (from devout)
    ok = erlmcp_stdio:add_tool(
        <<"change-cwd">>, 
        <<"Change current working directory">>,
        fun handle_change_cwd/1,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"path">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"Relative path to change to">>
                }
            },
            <<"required">> => [<<"path">>]
        }
    ).

setup_resources() ->
    %% Add status resource
    ok = erlmcp_stdio:add_resource(
        <<"devout://status">>, 
        <<"Devout Status">>,
        fun handle_status_resource/1,
        <<"text/plain">>
    ),
    
    %% Add help resource
    ok = erlmcp_stdio:add_resource(
        <<"devout://help">>, 
        <<"Devout Help">>,
        fun handle_help_resource/1,
        <<"text/plain">>
    ).

setup_prompts() ->
    %% Add create_project prompt
    ok = erlmcp_stdio:add_prompt(
        <<"create_project">>, 
        <<"Create a project structure">>,
        fun handle_create_project_prompt/1,
        [
            #{
                <<"name">> => <<"project_name">>, 
                <<"description">> => <<"Name of the project">>, 
                <<"required">> => true
            },
            #{
                <<"name">> => <<"project_type">>, 
                <<"description">> => <<"Type of project (web, api, library, erlang)">>, 
                <<"required">> => false
            }
        ]
    ).

%%====================================================================
%% Tool handlers - Using devout_fs_ops for actual operations
%%====================================================================

handle_new_dir(#{<<"path">> := Path}) ->
    case devout_fs_ops:create_directory(Path) of
        ok ->
            <<"Directory created successfully: ", Path/binary>>;
        {error, Reason} ->
            ReasonBin = format_error(Reason),
            <<"Error creating directory: ", ReasonBin/binary>>
    end.

handle_new_dirs(#{<<"path">> := Path} = Args) ->
    Children = maps:get(<<"children">>, Args, []),
    
    %% First create the base directory
    case devout_fs_ops:create_directory(Path) of
        ok ->
            case create_children(Path, Children, []) of
                {ok, Results} ->
                    iolist_to_binary([<<"Directory structure created successfully:\n  - ">>, 
                                      Path, Results]);
                {error, Reason} ->
                    ReasonBin = format_error(Reason),
                    <<"Error creating child directories: ", ReasonBin/binary>>
            end;
        {error, Reason} ->
            ReasonBin = format_error(Reason),
            <<"Error creating base directory: ", ReasonBin/binary>>
    end.

create_children(_BasePath, [], Acc) ->
    {ok, lists:reverse(Acc)};
create_children(BasePath, [Child | Rest], Acc) ->
    ChildPath = <<BasePath/binary, "/", Child/binary>>,
    case devout_fs_ops:create_directory(ChildPath) of
        ok ->
            create_children(BasePath, Rest, [<<"\n  - ", Child/binary>> | Acc]);
        {error, Reason} ->
            {error, {child_creation_failed, Child, Reason}}
    end.

handle_move(#{<<"source">> := Source, <<"destination">> := Destination}) ->
    %% Use path validator to ensure both paths are safe
    case {devout_path_validator:validate_path(Source), 
          devout_path_validator:validate_path(Destination)} of
        {{ok, ValidSource}, {ok, ValidDest}} ->
            case file:rename(ValidSource, ValidDest) of
                ok ->
                    <<"Moved successfully: ", Source/binary, " -> ", Destination/binary>>;
                {error, Reason} ->
                    ReasonBin = format_error(Reason),
                    <<"Error moving file: ", ReasonBin/binary>>
            end;
        {{error, SourceErr}, _} ->
            ReasonBin = format_error(SourceErr),
            <<"Invalid source path: ", ReasonBin/binary>>;
        {_, {error, DestErr}} ->
            ReasonBin = format_error(DestErr),
            <<"Invalid destination path: ", ReasonBin/binary>>
    end.

handle_write(#{<<"path">> := Path, <<"content">> := Content} = Args) ->
    Mode = maps:get(<<"mode">>, Args, <<"write">>),
    
    %% Check file size limit
    {ok, MaxSize} = application:get_env(devout, max_file_size),
    case byte_size(Content) > MaxSize of
        true ->
            MaxSizeBin = integer_to_binary(MaxSize),
            <<"Error: Content exceeds maximum file size (", MaxSizeBin/binary, " bytes)">>;
        false ->
            case devout_fs_ops:create_file(Path, Content) of
                ok when Mode =:= <<"append">> ->
                    % Handle append mode by reading existing content first
                    handle_append_write(Path, Content);
                ok ->
                    Size = byte_size(Content),
                    SizeBin = integer_to_binary(Size),
                    <<"Content written to file successfully: ", Path/binary, 
                      " (", SizeBin/binary, " bytes)">>;
                {error, Reason} ->
                    ReasonBin = format_error(Reason),
                    <<"Error writing file: ", ReasonBin/binary>>
            end
    end.

handle_append_write(Path, Content) ->
    case devout_path_validator:validate_path(Path) of
        {ok, ValidPath} ->
            case file:write_file(ValidPath, Content, [append]) of
                ok ->
                    Size = byte_size(Content),
                    SizeBin = integer_to_binary(Size),
                    <<"Content appended to file successfully: ", Path/binary, 
                      " (", SizeBin/binary, " bytes)">>;
                {error, Reason} ->
                    ReasonBin = format_error(Reason),
                    <<"Error appending to file: ", ReasonBin/binary>>
            end;
        {error, Reason} ->
            ReasonBin = format_error(Reason),
            <<"Invalid path for append: ", ReasonBin/binary>>
    end.

handle_read(#{<<"path">> := Path}) ->
    case devout_path_validator:validate_path(Path) of
        {ok, ValidPath} ->
            case file:read_file(ValidPath) of
                {ok, Content} ->
                    Size = byte_size(Content),
                    SizeBin = integer_to_binary(Size),
                    case Content of
                        <<>> ->
                            <<"File is empty: ", Path/binary>>;
                        _ ->
                            <<"Content of ", Path/binary, " (", SizeBin/binary, " bytes):\n\n", Content/binary>>
                    end;
                {error, enoent} ->
                    <<"Error: File does not exist: ", Path/binary>>;
                {error, eisdir} ->
                    <<"Error: Path is a directory, not a file: ", Path/binary>>;
                {error, eacces} ->
                    <<"Error: Permission denied reading file: ", Path/binary>>;
                {error, Reason} ->
                    ReasonBin = format_error(Reason),
                    <<"Error reading file: ", ReasonBin/binary>>
            end;
        {error, Reason} ->
            ReasonBin = format_error(Reason),
            <<"Invalid path: ", ReasonBin/binary>>
    end.

handle_show_cwd(_Args) ->
    case devout_fs_ops:show_cwd() of
        {ok, Cwd} ->
            CwdBin = list_to_binary(Cwd),
            <<"Current working directory: ", CwdBin/binary>>;
        {error, Reason} ->
            ReasonBin = format_error(Reason),
            <<"Error getting current directory: ", ReasonBin/binary>>
    end.

handle_change_cwd(#{<<"path">> := Path}) ->
    case devout_fs_ops:change_cwd(Path) of
        {ok, NewCwd} ->
            NewCwdBin = list_to_binary(NewCwd),
            <<"Changed working directory to: ", NewCwdBin/binary>>;
        {error, Reason} ->
            ReasonBin = format_error(Reason),
            <<"Error changing directory: ", ReasonBin/binary>>
    end.

%%====================================================================
%% Resource handlers
%%====================================================================

handle_status_resource(_Uri) ->
    {ok, Cwd} = file:get_cwd(),
    CwdBin = list_to_binary(Cwd),
    {ok, AllowedOps} = application:get_env(devout, allowed_operations),
    OpsList = string:join([atom_to_list(Op) || Op <- AllowedOps], ", "),
    OpsListBin = list_to_binary(OpsList),
    
    <<"Devout MCP Service Status\n\n"
      "Current working directory: ", CwdBin/binary, "\n"
      "Service: Active\n"
      "Allowed operations: ", OpsListBin/binary, "\n"
      "Security: Path validation enabled\n">>.

handle_help_resource(_Uri) ->
    <<"Devout MCP Service Help\n\n"
      "Available Tools:\n"
      "- new-dir: Create a new directory\n"
      "  Usage: {\"path\": \"relative/path\"}\n\n"
      "- new-dirs: Create a directory with children\n"
      "  Usage: {\"path\": \"base/path\", \"children\": [\"dir1\", \"dir2\"]}\n\n"
      "- move: Move or rename a file or directory\n"
      "  Usage: {\"source\": \"old/path\", \"destination\": \"new/path\"}\n\n"
      "- write: Create or write to a file\n"
      "  Usage: {\"path\": \"file.txt\", \"content\": \"data\", \"mode\": \"write|append\"}\n\n"
      "- read: Read a file\n"
      "  Usage: {\"path\": \"file.txt\"}\n\n"
      "- show-cwd: Show current working directory\n"
      "  Usage: {}\n\n"
      "- change-cwd: Change current working directory\n"
      "  Usage: {\"path\": \"relative/path\"}\n\n"
      "Security Features:\n"
      "- All paths must be relative\n"
      "- Path traversal attacks prevented\n"
      "- Operations restricted to base directory\n"
      "- File size limits enforced\n">>.

%%====================================================================
%% Prompt handlers
%%====================================================================

handle_create_project_prompt(Args) ->
    ProjectName = maps:get(<<"project_name">>, Args, <<"my_project">>),
    ProjectType = maps:get(<<"project_type">>, Args, <<"web">>),
    
    {Instructions, Dirs} = case ProjectType of
        <<"erlang">> ->
            {<<"Create an Erlang/OTP project structure">>,
             [<<"src">>, <<"include">>, <<"test">>, <<"priv">>, <<"rebar.config">>]};
        <<"web">> ->
            {<<"Create a web project structure">>,
             [<<"src">>, <<"static">>, <<"templates">>, <<"config">>, <<"tests">>]};
        <<"api">> ->
            {<<"Create an API project structure">>,
             [<<"src">>, <<"tests">>, <<"docs">>, <<"config">>, <<"schemas">>]};
        <<"library">> ->
            {<<"Create a library project structure">>,
             [<<"src">>, <<"tests">>, <<"docs">>, <<"examples">>, <<"benchmarks">>]};
        _ ->
            {<<"Create a basic project structure">>,
             [<<"src">>, <<"tests">>, <<"docs">>]}
    end,
    
    DirsText = string:join([binary_to_list(D) || D <- Dirs], ", "),
    DirsTextBin = list_to_binary(DirsText),
    
    [#{
        <<"role">> => <<"user">>,
        <<"content">> => #{
            <<"type">> => <<"text">>,
            <<"text">> => <<"Create a project structure for '", ProjectName/binary,
                            "' (", ProjectType/binary, " project). ", Instructions/binary,
                            " with directories: ", DirsTextBin/binary,
                            ". Use the devout tools to create the necessary directories and files.">>
        }
    }].

%%====================================================================
%% Utility functions
%%====================================================================

format_error({child_creation_failed, Child, SubReason}) ->
    ChildBin = ensure_binary(Child),
    SubReasonBin = format_error(SubReason),
    <<"Failed to create child directory ", ChildBin/binary, ": ", SubReasonBin/binary>>;
format_error(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8).

ensure_binary(B) when is_binary(B) -> B.
