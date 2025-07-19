%%%-------------------------------------------------------------------
%%% @doc
%%% Devout application module - Entry point for the Devout MCP server
%%% @end
%%%-------------------------------------------------------------------
-module(devout_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start_phase/3]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%% @end
%%--------------------------------------------------------------------
-spec start(StartType, StartArgs) -> {ok, pid()} | ignore | {error, Reason} when
    StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term(),
    Reason :: term().
start(_StartType, _StartArgs) ->
    ?LOG_INFO("Starting Devout MCP server application"),

    % Set the base directory to current working directory if not already set
    case application:get_env(devout, base_directory) of
        undefined ->
            {ok, Cwd} = file:get_cwd(),
            application:set_env(devout, base_directory, Cwd),
            ?LOG_INFO("Base directory set to: ~s", [Cwd]);
        {ok, ExistingDir} ->
            ?LOG_INFO("Using existing base directory: ~s", [ExistingDir])
    end,

    case devout_sup:start_link() of
        {ok, Pid} ->
            ?LOG_INFO("Devout supervisor started successfully"),
            {ok, Pid};
        Error ->
            ?LOG_ERROR("Failed to start Devout supervisor: ~p", [Error]),
            Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% This function is called for each start phase defined in the
%% application specification, after the application and all its
%% dependencies have been started.
%% @end
%%--------------------------------------------------------------------
-spec start_phase(Phase, StartType, PhaseArgs) -> ok | {error, Reason} when
    Phase :: atom(),
    StartType :: normal | {takeover, node()} | {failover, node()},
    PhaseArgs :: term(),
    Reason :: term().
start_phase(register_tools, _StartType, _PhaseArgs) ->
    ?LOG_INFO("Starting phase: register_tools"),
    
    % First, start the erlmcp stdio server
    case erlmcp_sup:start_stdio_server() of
        {ok, Pid} ->
            ?LOG_INFO("erlmcp stdio server started successfully with pid: ~p", [Pid]),
            
            % Register our tools
            case register_tools() of
                ok ->
                    ?LOG_INFO("Successfully registered tools with erlmcp_stdio"),
                    
                    % Monitor the stdio server and keep the application alive
                    spawn(fun() -> 
                        monitor(process, Pid),
                        receive
                            {'DOWN', _, process, Pid, Reason} ->
                                ?LOG_INFO("stdio server terminated: ~p", [Reason]),
                                init:stop()
                        end
                    end),
                    ok;
                {error, Reason} ->
                    ?LOG_ERROR("Failed to register tools: ~p", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            ?LOG_ERROR("Failed to start erlmcp stdio server: ~p", [Reason]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Register all tools, resources, and prompts with erlmcp_stdio
%% @end
%%--------------------------------------------------------------------
-spec register_tools() -> ok | {error, term()}.
register_tools() ->
    ?LOG_INFO("Registering tools, resources, and prompts with erlmcp_stdio"),
    try
        setup_tools(),
        setup_resources(), 
        setup_prompts(),
        ok
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to register tools: ~p:~p", [Error, Reason]),
            {error, {Error, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec stop(State) -> ok when
    State :: term().
stop(_State) ->
    ?LOG_INFO("Stopping Devout MCP server application"),
    ok.

%%====================================================================
%% Internal functions - Tool/Resource/Prompt registration
%%====================================================================

setup_tools() ->
    %% Add new-dir tool
    ok = erlmcp_stdio:add_tool(
        <<"new-dir">>, 
        <<"Create a new directory">>,
        fun devout_server:handle_new_dir/1,
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
        fun devout_server:handle_new_dirs/1,
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
        fun devout_server:handle_move/1,
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
        fun devout_server:handle_write/1,
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
        fun devout_server:handle_read/1,
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
    
    %% Add show_cwd tool
    ok = erlmcp_stdio:add_tool(
        <<"show-cwd">>, 
        <<"Show current working directory">>,
        fun devout_server:handle_show_cwd/1,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{},
            <<"required">> => []
        }
    ),
    
    %% Add change_cwd tool
    ok = erlmcp_stdio:add_tool(
        <<"change-cwd">>, 
        <<"Change current working directory">>,
        fun devout_server:handle_change_cwd/1,
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
        fun devout_server:handle_status_resource/1,
        <<"text/plain">>
    ),
    
    %% Add help resource
    ok = erlmcp_stdio:add_resource(
        <<"devout://help">>, 
        <<"Devout Help">>,
        fun devout_server:handle_help_resource/1,
        <<"text/plain">>
    ).

setup_prompts() ->
    %% Add create_project prompt
    ok = erlmcp_stdio:add_prompt(
        <<"create_project">>, 
        <<"Create a project structure">>,
        fun devout_server:handle_create_project_prompt/1,
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