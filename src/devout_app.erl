%%%-------------------------------------------------------------------
%%% @doc
%%% Devout application module - Entry point for the Devin clone MCP server
%%% @end
%%%-------------------------------------------------------------------
-module(devout_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

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
