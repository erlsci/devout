%%%-------------------------------------------------------------------
%%% @doc
%%% Devout main entry point - Starts the application and stdio server
%%% (This was originally devout_stdio_main.erl)
%%% @end
%%%-------------------------------------------------------------------
-module(devout).
-export([start/0, start/1]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% API
%%====================================================================

start() ->
    start([]).

start(_Args) ->
    %% Start the devout application
    case application:ensure_all_started(devout) of
        {ok, _Started} ->
            ?LOG_INFO("Successfully started devout application");
        {error, AppReason} ->
            ?LOG_ERROR("Failed to start devout application: ~p", [AppReason]),
            halt(1)
    end,

    %% Configure logging to stderr (so it doesn't interfere with stdio MCP protocol)
    ok = logger:remove_handler(default),
    ok = logger:add_handler(stderr_handler, logger_std_h, #{
        level => info,
        config => #{type => standard_error}
    }),
    ok = logger:set_primary_config(level, info),

    ?LOG_INFO("Starting Devout MCP server..."),

    %% Start the stdio server through our main server
    case devout_server:start_stdio() of
        ok ->
            ?LOG_INFO("Devout MCP server started successfully"),
            wait_for_shutdown();
        {error, ServerReason} ->
            ?LOG_ERROR("Failed to start Devout MCP server: ~p", [ServerReason]),
            halt(1)
    end.

%%====================================================================
%% Internal functions
%%====================================================================

wait_for_shutdown() ->
    %% Monitor the stdio server process to know when it's done
    case whereis(erlmcp_stdio_server) of
        undefined ->
            ?LOG_WARNING("Stdio server not found, exiting");
        Pid ->
            monitor(process, Pid),
            receive
                {'DOWN', _Ref, process, Pid, _Reason} ->
                    ?LOG_INFO("Stdio server terminated, exiting")
            end
    end.