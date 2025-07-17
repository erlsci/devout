%%%-------------------------------------------------------------------
%%% @doc
%%% MCP Server implementation - Main server process that handles MCP protocol
%%% and exposes file system operations as tools
%%% @end
%%%-------------------------------------------------------------------
-module(devout_server).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    stop/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).

%% Server state
-record(state, {
    mcp_server :: pid(),
    server_config :: map(),
    base_directory :: string()
}).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the server
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    gen_server:call(?SERVER, stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args) -> {ok, State} | {ok, State, timeout() | hibernate} |
    {stop, Reason} | ignore when
    Args :: term(),
    State :: #state{},
    Reason :: term().
init([]) ->
    ?LOG_INFO("Initializing Devout MCP server"),
    
    % Get configuration
    {ok, ServerConfig} = application:get_env(devout, mcp_server_config),
    {ok, BaseDir} = application:get_env(devout, base_directory),
    
    % Create MCP server capabilities
    Capabilities = #{
        tools => create_tool_definitions(),
        resources => [],
        prompts => []
    },
    
    % Start MCP server
    case erlmcp_server:start_link({stdio, []}, Capabilities) of
        {ok, McpServer} ->
            % Register tools
            register_tools(McpServer),
            
            State = #state{
                mcp_server = McpServer,
                server_config = ServerConfig,
                base_directory = BaseDir
            },
            
            ?LOG_INFO("Devout MCP server initialized successfully"),
            {ok, State};
        Error ->
            ?LOG_ERROR("Failed to start MCP server: ~p", [Error]),
            {stop, Error}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request, From, State) -> {reply, Reply, State} |
    {reply, Reply, State, timeout() | hibernate} |
    {noreply, State} |
    {noreply, State, timeout() | hibernate} |
    {stop, Reason, Reply, State} |
    {stop, Reason, State} when
    Request :: term(),
    From :: {pid(), Tag :: term()},
    State :: #state{},
    Reply :: term(),
    Reason :: term().
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%%--------------------------------------------------------------------
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Msg, State) -> {noreply, State} |
    {noreply, State, timeout() | hibernate} |
    {stop, Reason, State} when
    Msg :: term(),
    State :: #state{},
    Reason :: term().
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info, State) -> {noreply, State} |
    {noreply, State, timeout() | hibernate} |
    {stop, Reason, State} when
    Info :: term(),
    State :: #state{},
    Reason :: term().
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason, State) -> ok when
    Reason :: normal | shutdown | {shutdown, term()} | term(),
    State :: #state{}.
terminate(_Reason, _State) ->
    ?LOG_INFO("Devout MCP server terminating"),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn, State, Extra) -> {ok, NewState} when
    OldVsn :: term() | {down, term()},
    State :: #state{},
    Extra :: term(),
    NewState :: #state{}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Create tool definitions for MCP
%% @end
%%--------------------------------------------------------------------
-spec create_tool_definitions() -> list().
create_tool_definitions() ->
    [
        #{
            name => <<"show_cwd">>,
            description => <<"Show current working directory">>,
            inputSchema => #{
                type => <<"object">>,
                properties => #{},
                required => []
            }
        },
        #{
            name => <<"change_cwd">>,
            description => <<"Change current working directory to a relative path">>,
            inputSchema => #{
                type => <<"object">>,
                properties => #{
                    path => #{
                        type => <<"string">>,
                        description => <<"Relative path to change to">>
                    }
                },
                required => [<<"path">>]
            }
        },
        #{
            name => <<"create_file">>,
            description => <<"Create a file with optional content">>,
            inputSchema => #{
                type => <<"object">>,
                properties => #{
                    path => #{
                        type => <<"string">>,
                        description => <<"Relative path for the new file">>
                    },
                    content => #{
                        type => <<"string">>,
                        description => <<"Content for the file (optional)">>
                    }
                },
                required => [<<"path">>]
            }
        },
        #{
            name => <<"delete_file">>,
            description => <<"Delete a file">>,
            inputSchema => #{
                type => <<"object">>,
                properties => #{
                    path => #{
                        type => <<"string">>,
                        description => <<"Relative path of the file to delete">>
                    }
                },
                required => [<<"path">>]
            }
        },
        #{
            name => <<"create_directory">>,
            description => <<"Create a directory and all necessary parent directories">>,
            inputSchema => #{
                type => <<"object">>,
                properties => #{
                    path => #{
                        type => <<"string">>,
                        description => <<"Relative path for the new directory">>
                    }
                },
                required => [<<"path">>]
            }
        },
        #{
            name => <<"remove_directory">>,
            description => <<"Remove a directory, optionally with all contents">>,
            inputSchema => #{
                type => <<"object">>,
                properties => #{
                    path => #{
                        type => <<"string">>,
                        description => <<"Relative path of the directory to remove">>
                    },
                    recursive => #{
                        type => <<"boolean">>,
                        description => <<"Remove directory and all contents recursively (default: false)">>
                    }
                },
                required => [<<"path">>]
            }
        }
    ].

%%--------------------------------------------------------------------
%% @doc
%% Register tools with the MCP server
%% @end
%%--------------------------------------------------------------------
-spec register_tools(McpServer) -> ok when
    McpServer :: pid().
register_tools(McpServer) ->
    % Show current working directory
    erlmcp_server:add_tool_with_schema(McpServer, <<"show_cwd">>, 
        fun(_Args) -> handle_show_cwd() end, 
        #{type => <<"object">>, properties => #{}}),
    
    % Change current working directory
    erlmcp_server:add_tool_with_schema(McpServer, <<"change_cwd">>,
        fun(Args) -> handle_change_cwd(Args) end,
        #{type => <<"object">>, 
          properties => #{path => #{type => <<"string">>}},
          required => [<<"path">>]}),
    
    % Create file
    erlmcp_server:add_tool_with_schema(McpServer, <<"create_file">>,
        fun(Args) -> handle_create_file(Args) end,
        #{type => <<"object">>,
          properties => #{
              path => #{type => <<"string">>},
              content => #{type => <<"string">>}
          },
          required => [<<"path">>]}),
    
    % Delete file
    erlmcp_server:add_tool_with_schema(McpServer, <<"delete_file">>,
        fun(Args) -> handle_delete_file(Args) end,
        #{type => <<"object">>,
          properties => #{path => #{type => <<"string">>}},
          required => [<<"path">>]}),
    
    % Create directory
    erlmcp_server:add_tool_with_schema(McpServer, <<"create_directory">>,
        fun(Args) -> handle_create_directory(Args) end,
        #{type => <<"object">>,
          properties => #{path => #{type => <<"string">>}},
          required => [<<"path">>]}),
    
    % Remove directory
    erlmcp_server:add_tool_with_schema(McpServer, <<"remove_directory">>,
        fun(Args) -> handle_remove_directory(Args) end,
        #{type => <<"object">>,
          properties => #{
              path => #{type => <<"string">>},
              recursive => #{type => <<"boolean">>}
          },
          required => [<<"path">>]}),
    
    ?LOG_INFO("All tools registered successfully"),
    ok.

%%====================================================================
%% Tool handlers
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Handle show_cwd tool call
%% @end
%%--------------------------------------------------------------------
-spec handle_show_cwd() -> binary().
handle_show_cwd() ->
    case devout_fs_ops:show_cwd() of
        {ok, Cwd} ->
            jsx:encode(#{status => success, cwd => list_to_binary(Cwd)});
        {error, Reason} ->
            jsx:encode(#{status => error, reason => format_error(Reason)})
    end.

%%--------------------------------------------------------------------
%% @doc
%% Handle change_cwd tool call
%% @end
%%--------------------------------------------------------------------
-spec handle_change_cwd(Args) -> binary() when
    Args :: map().
handle_change_cwd(#{<<"path">> := Path}) ->
    case devout_fs_ops:change_cwd(Path) of
        {ok, NewCwd} ->
            jsx:encode(#{status => success, new_cwd => list_to_binary(NewCwd)});
        {error, Reason} ->
            jsx:encode(#{status => error, reason => format_error(Reason)})
    end.

%%--------------------------------------------------------------------
%% @doc
%% Handle create_file tool call
%% @end
%%--------------------------------------------------------------------
-spec handle_create_file(Args) -> binary() when
    Args :: map().
handle_create_file(#{<<"path">> := Path} = Args) ->
    Content = maps:get(<<"content">>, Args, <<>>),
    case devout_fs_ops:create_file(Path, Content) of
        ok ->
            jsx:encode(#{status => success, message => <<"File created successfully">>});
        {error, Reason} ->
            jsx:encode(#{status => error, reason => format_error(Reason)})
    end.

%%--------------------------------------------------------------------
%% @doc
%% Handle delete_file tool call
%% @end
%%--------------------------------------------------------------------
-spec handle_delete_file(Args) -> binary() when
    Args :: map().
handle_delete_file(#{<<"path">> := Path}) ->
    case devout_fs_ops:delete_file(Path) of
        ok ->
            jsx:encode(#{status => success, message => <<"File deleted successfully">>});
        {error, Reason} ->
            jsx:encode(#{status => error, reason => format_error(Reason)})
    end.

%%--------------------------------------------------------------------
%% @doc
%% Handle create_directory tool call
%% @end
%%--------------------------------------------------------------------
-spec handle_create_directory(Args) -> binary() when
    Args :: map().
handle_create_directory(#{<<"path">> := Path}) ->
    case devout_fs_ops:create_directory(Path) of
        ok ->
            jsx:encode(#{status => success, message => <<"Directory created successfully">>});
        {error, Reason} ->
            jsx:encode(#{status => error, reason => format_error(Reason)})
    end.

%%--------------------------------------------------------------------
%% @doc
%% Handle remove_directory tool call
%% @end
%%--------------------------------------------------------------------
-spec handle_remove_directory(Args) -> binary() when
    Args :: map().
handle_remove_directory(#{<<"path">> := Path} = Args) ->
    Recursive = maps:get(<<"recursive">>, Args, false),
    case devout_fs_ops:remove_directory(Path, Recursive) of
        ok ->
            jsx:encode(#{status => success, message => <<"Directory removed successfully">>});
        {error, Reason} ->
            jsx:encode(#{status => error, reason => format_error(Reason)})
    end.

%%--------------------------------------------------------------------
%% @doc
%% Format error for JSON response
%% @end
%%--------------------------------------------------------------------
-spec format_error(Reason) -> binary() when
    Reason :: term().
format_error(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
format_error(Reason) when is_list(Reason) ->
    list_to_binary(Reason);
format_error(Reason) when is_binary(Reason) ->
    Reason;
format_error(Reason) ->
    list_to_binary(io_lib:format("~p", [Reason])).